library(ggplot2)
library(metafor)

# Colourblind-safe palette (Okabe-Ito) mapped to semantic roles
cols_review <- c(
  benefit   = "#0072B2",
  harm      = "#D55E00",
  neutral   = "#999999",
  clinical  = "#1a6373",
  patient_reported = "#d97706",
  pem       = "#D55E00",
  no_pem    = "#1a6373"
)

# Accent colors for specific chart elements
col_pooled  <- "#b45309"
col_line    <- "#a3d4de"
col_zero    <- "#a3d4de"
col_grid    <- "#daeef2"
col_text    <- "#44403c"
col_muted   <- "#6e6862"
col_faint   <- "#e7e5e4"

.plot_font <- tryCatch({
  if (requireNamespace("systemfonts", quietly = TRUE) &&
      any(grepl("DM Sans", systemfonts::system_fonts()$family))) "DM Sans" else ""
}, error = function(e) "")

theme_review <- theme_minimal(base_size = 12, base_family = .plot_font) +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.title = element_text(size = 13, face = "plain", colour = col_text,
                              margin = margin(b = 4)),
    plot.subtitle = element_text(size = 10, colour = col_muted,
                                  margin = margin(b = 8)),
    plot.title.position = "plot",
    axis.title = element_text(size = 10, colour = col_muted),
    axis.text = element_text(size = 9, colour = col_muted),
    axis.ticks = element_line(colour = col_faint, linewidth = 0.3),
    panel.grid.major.x = element_line(colour = col_grid, linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.justification = "left",
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA),
    legend.title = element_text(size = 9, colour = col_muted),
    legend.text = element_text(size = 9, colour = col_muted),
    legend.key.size = unit(0.8, "lines"),
    plot.margin = margin(12, 16, 8, 8),
    strip.text = element_text(size = 10, hjust = 0, colour = col_text)
  )

theme_lay <- theme_review +
  theme(
    plot.title = element_text(size = 16, face = "plain", colour = col_text),
    plot.subtitle = element_text(size = 12, colour = col_muted),
    axis.text = element_text(size = 11, colour = col_muted),
    legend.text = element_text(size = 11)
  )

forest_height <- function(k) min(25, max(4, k * 0.35 + 2))

subgroup_forest <- function(data,
                            group_col,
                            yi_col = "yi",
                            vi_col = "vi",
                            min_k = 2,
                            title = NULL,
                            subtitle = NULL,
                            xlab = "Standardised Mean Difference") {
  groups <- unique(data[[group_col]])
  groups <- groups[!is.na(groups)]

  results <- list()
  for (grp in groups) {
    sub <- data[data[[group_col]] == grp & !is.na(data[[group_col]]), ]
    if (nrow(sub) >= min_k) {
      fit <- tryCatch(
        rma(yi = sub[[yi_col]], vi = sub[[vi_col]], method = "REML"),
        error = function(e) NULL
      )
      if (!is.null(fit)) {
        results[[grp]] <- data.frame(
          group = grp,
          k = fit$k,
          yi = coef(fit),
          ci_lb = fit$ci.lb,
          ci_ub = fit$ci.ub,
          I2 = fit$I2,
          pval = fit$pval
        )
      }
    }
  }

  if (length(results) == 0) return(NULL)

  df <- do.call(rbind, results)
  df <- df[order(df$yi), ]
  df$label <- sprintf("%s (k=%d, I\u00b2=%.0f%%)", df$group, df$k, df$I2)
  df$label <- factor(df$label, levels = rev(df$label))

  ggplot(df, aes(x = yi, y = label)) +
    geom_vline(xintercept = 0, colour = col_zero, linewidth = 0.4) +
    geom_linerange(aes(xmin = ci_lb, xmax = ci_ub),
                   colour = col_muted, linewidth = 0.6) +
    geom_point(aes(size = k), colour = cols_review["clinical"],
               shape = 18, show.legend = FALSE) +
    scale_size_continuous(range = c(3, 7)) +
    geom_text(aes(x = ci_ub + 0.03 * diff(range(c(ci_lb, ci_ub))),
                  label = sprintf("%.2f [%.2f, %.2f]", yi, ci_lb, ci_ub)),
              hjust = 0, size = 2.8, colour = col_muted) +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.25))) +
    labs(x = xlab, y = NULL, title = title, subtitle = subtitle) +
    theme_review +
    theme(axis.text.y = element_text(size = 9))
}

bar_height <- function(k) min(20, max(4, k * 0.5 + 2))

forest_gg <- function(fit,
                      xlab = "Standardised Mean Difference (Hedges' g)",
                      title = NULL,
                      subtitle = NULL,
                      show_prediction = TRUE) {
  slab <- fit$slab %||% paste("Study", seq_len(fit$k))
  yi <- fit$yi
  vi <- fit$vi
  ci_lb <- yi - qnorm(0.975) * sqrt(vi)
  ci_ub <- yi + qnorm(0.975) * sqrt(vi)
  weights <- ifelse(is.finite(vi) & vi > 0, 1 / vi, 0)
  max_w <- max(weights, na.rm = TRUE)
  weights <- if (max_w > 0) weights / max_w else rep(1, length(weights))

  df <- data.frame(
    study = factor(slab, levels = rev(slab)),
    yi = yi,
    ci_lb = ci_lb,
    ci_ub = ci_ub,
    w = weights
  )

  summary_df <- data.frame(
    study = "Pooled estimate",
    yi = coef(fit),
    ci_lb = fit$ci.lb,
    ci_ub = fit$ci.ub
  )

  auto_subtitle <- sprintf(
    "k = %d, pooled = %.2f [%.2f, %.2f], I\u00b2 = %.0f%%",
    fit$k, coef(fit), fit$ci.lb, fit$ci.ub, fit$I2
  )

  p <- ggplot(df, aes(x = yi, y = study)) +
    geom_vline(xintercept = 0, colour = col_zero, linewidth = 0.4)

  if (show_prediction && !is.null(fit$cr.lb)) {
    p <- p + annotate(
      "rect",
      xmin = fit$cr.lb, xmax = fit$cr.ub,
      ymin = -Inf, ymax = Inf,
      alpha = 0.08, fill = cols_review["clinical"]
    )
  }

  p <- p +
    geom_linerange(aes(xmin = ci_lb, xmax = ci_ub),
                   colour = col_muted, linewidth = 0.4) +
    geom_point(aes(size = w), colour = cols_review["clinical"],
               shape = 15, show.legend = FALSE) +
    scale_size_continuous(range = c(1.5, 4)) +
    geom_hline(yintercept = 1.4, colour = col_faint, linewidth = 0.3) +
    geom_point(
      data = summary_df, aes(x = yi, y = study),
      shape = 18, size = 4.5, colour = col_pooled
    ) +
    geom_linerange(
      data = summary_df, aes(xmin = ci_lb, xmax = ci_ub),
      linewidth = 1, colour = col_pooled
    ) +
    geom_text(
      data = df,
      aes(x = ci_ub + 0.05 * diff(range(c(ci_lb, ci_ub))),
          label = sprintf("%.2f", yi)),
      hjust = 0, size = 2.8, colour = col_muted
    ) +
    labs(
      x = xlab, y = NULL,
      title = title,
      subtitle = subtitle %||% auto_subtitle
    ) +
    theme_review +
    theme(
      panel.grid.major.x = element_line(colour = col_grid, linewidth = 0.3),
      axis.text.y = element_text(size = 8)
    )

  p
}

prevalence_plot <- function(data, title = NULL, subtitle = NULL) {
  ggplot(data, aes(x = prevalence, y = study_label)) +
    geom_vline(xintercept = 0, colour = col_zero, linewidth = 0.3) +
    geom_linerange(aes(xmin = ci_lower, xmax = ci_upper),
                   colour = col_muted, linewidth = 0.4) +
    geom_point(size = 2.5, colour = cols_review["clinical"], shape = 15) +
    scale_x_continuous(labels = scales::percent_format()) +
    labs(x = "Prevalence", y = NULL, title = title, subtitle = subtitle) +
    theme_review +
    theme(axis.text.y = element_text(size = 8))
}

lay_bar_plot <- function(data, x, y, fill = NULL,
                         title = NULL, subtitle = NULL,
                         xlab = NULL, ylab = NULL) {
  p <- ggplot(data, aes(x = .data[[x]], y = .data[[y]])) +
    geom_col(
      aes(fill = if (!is.null(fill)) .data[[fill]] else NULL),
      width = 0.7
    ) +
    labs(title = title, subtitle = subtitle, x = xlab, y = ylab) +
    theme_lay

  if (!is.null(fill)) {
    p <- p + scale_fill_manual(values = cols_review, name = NULL)
  }
  p
}

lay_dumbbell <- function(data,
                         y, xmin, xmax, group,
                         title = NULL, subtitle = NULL) {
  segments <- data |>
    dplyr::summarise(
      xmin = min(c(.data[[xmin]], .data[[xmax]])),
      xmax = max(c(.data[[xmin]], .data[[xmax]])),
      .by = dplyr::all_of(y)
    )

  ggplot(data, aes(y = .data[[y]])) +
    geom_linerange(
      data = segments,
      aes(xmin = xmin, xmax = xmax),
      linewidth = 0.5, colour = col_muted
    ) +
    geom_point(aes(x = .data[[xmin]], colour = .data[[group]]), size = 3.5) +
    geom_point(aes(x = .data[[xmax]], colour = .data[[group]]), size = 3.5) +
    scale_colour_manual(values = cols_review, name = NULL) +
    labs(title = title, subtitle = subtitle, x = NULL, y = NULL) +
    theme_lay
}

grade_heatmap <- function(quality_data, title = "GRADE evidence certainty") {
  grade_levels <- c("Very Low", "Low", "Moderate", "High")
  grade_colours <- c(
    "Very Low" = "#b83744",
    "Low"      = "#d16d1c",
    "Moderate" = "#b58e2c",
    "High"     = "#2e8a43"
  )

  quality_data$grade <- factor(quality_data$grade, levels = grade_levels)

  ggplot(quality_data, aes(x = outcome, y = comparison, fill = grade)) +
    geom_tile(colour = "white", linewidth = 1.2) +
    geom_text(aes(label = grade), colour = "white", size = 3.5, fontface = "bold") +
    scale_fill_manual(values = grade_colours, drop = FALSE, name = NULL) +
    labs(title = title, x = NULL, y = NULL) +
    theme_review +
    theme(
      axis.text.x = element_text(angle = 35, hjust = 1, size = 9),
      panel.grid = element_blank(),
      legend.position = "none"
    )
}

rob_traffic_light <- function(rob_data, title = "Risk of bias") {
  rob_colours <- c(
    "Low"            = "#2e8a43",
    "Some concerns"  = "#E69F00",
    "High"           = "#D55E00",
    "Not applicable" = "#CCCCCC"
  )

  rob_data$judgement <- factor(
    rob_data$judgement,
    levels = c("Low", "Some concerns", "High", "Not applicable")
  )

  ggplot(rob_data, aes(x = domain, y = study_label, fill = judgement)) +
    geom_tile(colour = "white", linewidth = 0.8) +
    scale_fill_manual(values = rob_colours, name = NULL) +
    labs(title = title, x = NULL, y = NULL) +
    theme_review +
    theme(
      axis.text.x = element_text(angle = 35, hjust = 1, size = 8),
      axis.text.y = element_text(size = 8),
      panel.grid = element_blank(),
      legend.position = "top"
    )
}

funnel_gg <- function(fit, title = "Funnel plot") {
  yi <- fit$yi
  se <- sqrt(fit$vi)
  summary_est <- coef(fit)

  se_range <- range(se)
  ci_bound <- data.frame(
    se = seq(0, max(se) * 1.05, length.out = 100)
  )
  ci_bound$lower <- summary_est - qnorm(0.975) * ci_bound$se
  ci_bound$upper <- summary_est + qnorm(0.975) * ci_bound$se

  df <- data.frame(yi = yi, se = se)

  ggplot(df, aes(x = yi, y = se)) +
    geom_ribbon(data = ci_bound, aes(x = NULL, ymin = 0, ymax = se,
                                      xmin = lower, xmax = upper),
                fill = col_grid, colour = NA) +
    geom_vline(xintercept = summary_est, colour = col_muted,
               linewidth = 0.4, linetype = "dashed") +
    geom_point(size = 2, colour = cols_review["clinical"], alpha = 0.7) +
    scale_y_reverse(expand = expansion(mult = c(0.02, 0.05))) +
    labs(
      x = "Effect size", y = "Standard error",
      title = title,
      subtitle = sprintf("Pooled estimate = %.2f (dashed line)", summary_est)
    ) +
    theme_review +
    theme(panel.grid.major.x = element_line(colour = col_grid, linewidth = 0.3))
}

# --- Layperson-friendly visualisations ---

effect_label <- function(smd) {
  size <- dplyr::case_when(
    abs(smd) < 0.2 ~ "negligible",
    abs(smd) < 0.5 ~ "small",
    abs(smd) < 0.8 ~ "moderate",
    TRUE           ~ "large"
  )
  direction <- ifelse(smd <= 0, "benefit", "harm")
  ifelse(size == "negligible", "negligible",
         paste(size, direction))
}

effect_colour <- function(smd) {
  dplyr::case_when(
    abs(smd) < 0.2 ~ cols_review[["neutral"]],
    smd < 0        ~ cols_review[["benefit"]],
    TRUE           ~ cols_review[["harm"]]
  )
}

lay_lollipop <- function(data,
                          group_col,
                          yi_col = "yi",
                          vi_col = "vi",
                          min_k = 2,
                          title = "Treatment effects at a glance",
                          subtitle = "Each dot shows the average effect across studies. Farther left = more benefit.") {
  groups <- unique(data[[group_col]])
  groups <- groups[!is.na(groups)]

  results <- list()
  for (grp in groups) {
    sub <- data[data[[group_col]] == grp & !is.na(data[[group_col]]), ]
    if (nrow(sub) >= min_k) {
      fit <- tryCatch(
        rma(yi = sub[[yi_col]], vi = sub[[vi_col]], method = "REML"),
        error = function(e) NULL
      )
      if (!is.null(fit)) {
        results[[grp]] <- data.frame(
          group = grp, k = fit$k, yi = coef(fit),
          ci_lb = fit$ci.lb, ci_ub = fit$ci.ub
        )
      }
    }
  }

  if (length(results) == 0) return(NULL)

  df <- do.call(rbind, results)
  df <- df[order(df$yi), ]
  df$magnitude <- effect_label(df$yi)
  df$label <- paste0(df$group, " (", df$k, " studies)")
  df$label <- factor(df$label, levels = rev(df$label))
  df$col <- effect_colour(df$yi)

  ggplot(df, aes(x = yi, y = label)) +
    annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
             fill = "#1a6373", alpha = 0.06) +
    annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf,
             fill = "#D55E00", alpha = 0.06) +
    annotate("text", x = -0.02, y = Inf, label = "Favours treatment",
             hjust = 1, vjust = -0.5, size = 3, colour = col_muted) +
    annotate("text", x = 0.02, y = Inf, label = "Favours control",
             hjust = 0, vjust = -0.5, size = 3, colour = col_muted) +
    geom_vline(xintercept = 0, colour = col_zero, linewidth = 0.4) +
    geom_segment(aes(x = 0, xend = yi, yend = label),
                 colour = col_muted, linewidth = 0.5) +
    geom_point(colour = df$col, size = 4) +
    geom_text(aes(label = magnitude), hjust = ifelse(df$yi < 0, 1.5, -0.5),
              size = 3, colour = col_muted) +
    scale_x_continuous(expand = expansion(mult = c(0.15, 0.15))) +
    labs(x = NULL, y = NULL, title = title, subtitle = subtitle) +
    theme_lay +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.subtitle = element_text(size = 10, colour = col_muted)
    )
}

lay_waffle <- function(n_yes, n_total,
                       yes_label = "Measured",
                       no_label = "Not measured",
                       title = NULL,
                       subtitle = NULL) {
  pct <- round(n_yes / n_total * 100)
  grid_size <- 10
  cells <- expand.grid(x = 1:grid_size, y = 1:grid_size)
  cells <- cells[order(-cells$y, cells$x), ]
  cells$fill <- ifelse(seq_len(100) <= pct, yes_label, no_label)

  fill_vals <- c(cols_review[["pem"]], col_faint)
  names(fill_vals) <- c(yes_label, no_label)

  auto_subtitle <- sprintf("%d out of %d (%d%%)", n_yes, n_total, pct)

  ggplot(cells, aes(x = x, y = y, fill = fill)) +
    geom_tile(colour = "white", linewidth = 0.8, width = 0.9, height = 0.9) +
    scale_fill_manual(values = fill_vals, name = NULL) +
    coord_equal() +
    labs(title = title, subtitle = subtitle %||% auto_subtitle) +
    theme_void(base_size = 14, base_family = .plot_font) +
    theme(
      plot.background = element_rect(fill = "transparent", colour = NA),
      legend.position = "bottom",
      legend.background = element_rect(fill = "transparent", colour = NA),
      legend.key = element_rect(fill = "transparent", colour = NA),
      legend.text = element_text(size = 11, colour = col_muted),
      plot.title = element_text(size = 16, face = "plain", colour = col_text,
                                margin = margin(b = 4)),
      plot.subtitle = element_text(size = 12, colour = col_muted,
                                    margin = margin(b = 8)),
      plot.margin = margin(12, 16, 8, 8)
    )
}

lay_grade_bar <- function(grade_data,
                           outcome_col = "outcome",
                           certainty_col = "certainty",
                           title = "How confident are we in the evidence?",
                           subtitle = "Each bar shows how many outcomes fall into each certainty level") {
  grade_levels <- c("very low", "low", "moderate", "high")
  grade_labels <- c("Very uncertain", "Uncertain", "Moderate confidence", "Confident")
  grade_colours <- c(
    "Very uncertain"       = "#b83744",
    "Uncertain"            = "#d16d1c",
    "Moderate confidence"  = "#b58e2c",
    "Confident"            = "#2e8a43"
  )

  df <- grade_data
  df$cert_clean <- tolower(trimws(df[[certainty_col]]))
  df <- df[df$cert_clean %in% grade_levels, ]
  df$cert_label <- factor(
    grade_labels[match(df$cert_clean, grade_levels)],
    levels = grade_labels
  )

  counts <- as.data.frame(table(cert_label = df$cert_label))
  counts$pct <- counts$Freq / sum(counts$Freq)

  ggplot(counts, aes(x = Freq, y = "Evidence", fill = cert_label)) +
    geom_col(position = "stack", width = 0.6) +
    geom_text(
      aes(label = ifelse(Freq > 0, paste0(Freq, "\n(", round(pct * 100), "%)"), "")),
      position = position_stack(vjust = 0.5), size = 3.5, colour = "white",
      fontface = "bold"
    ) +
    scale_fill_manual(values = grade_colours, name = NULL, drop = FALSE) +
    labs(x = "Number of outcomes assessed", y = NULL,
         title = title, subtitle = subtitle) +
    theme_lay +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "bottom"
    )
}

lay_paradigm_compare <- function(cbt_data, biomed_data,
                                  domain_col = "outcome_domain",
                                  title = "Where is the evidence? Biomedical vs psychological",
                                  subtitle = "Number of study results by symptom area") {
  cbt_counts <- as.data.frame(table(
    domain = cbt_data[[domain_col]]
  ))
  cbt_counts$paradigm <- "Psychological / Behavioural"

  bio_counts <- as.data.frame(table(
    domain = biomed_data[[domain_col]]
  ))
  bio_counts$paradigm <- "Biomedical"

  df <- rbind(cbt_counts, bio_counts)
  df <- df[df$Freq > 0, ]

  totals <- aggregate(Freq ~ domain, df, sum)
  top_domains <- totals$domain[order(-totals$Freq)][1:min(12, nrow(totals))]
  df <- df[df$domain %in% top_domains, ]
  df$domain <- factor(df$domain, levels = rev(top_domains[order(
    totals$Freq[match(top_domains, totals$domain)]
  )]))

  ggplot(df, aes(x = Freq, y = domain, fill = paradigm)) +
    geom_col(position = "dodge", width = 0.7) +
    scale_fill_manual(values = c(
      "Psychological / Behavioural" = "#d97706",
      "Biomedical" = "#1a6373"
    ), name = NULL) +
    labs(x = "Number of study results", y = NULL,
         title = title, subtitle = subtitle) +
    theme_lay +
    theme(legend.position = "top")
}

lay_treatment_summary <- function(data,
                                    group_col,
                                    yi_col = "yi",
                                    vi_col = "vi",
                                    min_k = 2,
                                    title = "Does the treatment help?",
                                    subtitle = "Green = likely helpful, grey = uncertain, red = possibly harmful") {
  groups <- unique(data[[group_col]])
  groups <- groups[!is.na(groups)]

  results <- list()
  for (grp in groups) {
    sub <- data[data[[group_col]] == grp & !is.na(data[[group_col]]), ]
    if (nrow(sub) >= min_k) {
      fit <- tryCatch(
        rma(yi = sub[[yi_col]], vi = sub[[vi_col]], method = "REML"),
        error = function(e) NULL
      )
      if (!is.null(fit)) {
        smd <- coef(fit)
        verdict <- dplyr::case_when(
          fit$ci.ub < 0                    ~ "Likely helpful",
          fit$ci.lb > 0 & smd >= 0.3       ~ "Possibly harmful",
          fit$ci.lb > 0                    ~ "Unlikely to help",
          smd < 0 & abs(smd) >= 0.3        ~ "Possibly helpful",
          smd > 0 & abs(smd) >= 0.2        ~ "Possibly harmful",
          TRUE                             ~ "Uncertain"
        )
        verdict_col <- dplyr::case_when(
          verdict == "Likely helpful"   ~ "#2e8a43",
          verdict == "Possibly helpful" ~ "#6aaa5d",
          verdict == "Possibly harmful" ~ "#d16d1c",
          verdict == "Unlikely to help" ~ "#b83744",
          TRUE                          ~ "#999999"
        )
        results[[grp]] <- data.frame(
          group = grp, k = fit$k, yi = smd,
          ci_lb = fit$ci.lb, ci_ub = fit$ci.ub,
          verdict = verdict, verdict_col = verdict_col
        )
      }
    }
  }

  if (length(results) == 0) return(NULL)

  df <- do.call(rbind, results)
  df <- df[order(df$yi), ]
  df$label <- paste0(df$group, "\n(", df$k, " studies)")
  df$label <- factor(df$label, levels = rev(df$label))

  ggplot(df, aes(x = yi, y = label)) +
    geom_vline(xintercept = 0, colour = col_zero, linewidth = 0.4,
               linetype = "dashed") +
    geom_linerange(aes(xmin = ci_lb, xmax = ci_ub),
                   colour = col_muted, linewidth = 0.8) +
    geom_point(size = 5, colour = df$verdict_col, shape = 16) +
    geom_text(aes(label = verdict),
              hjust = ifelse(df$yi < 0, 1.3, -0.3),
              size = 3.2, colour = df$verdict_col, fontface = "bold") +
    scale_x_continuous(expand = expansion(mult = c(0.3, 0.3))) +
    labs(x = NULL, y = NULL, title = title, subtitle = subtitle) +
    theme_lay +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid.major.x = element_blank()
    )
}
