library(ggplot2)
library(metafor)

# Colourblind-safe palette (Okabe-Ito)
cols_review <- c(
  benefit   = "#0072B2",
  harm      = "#D55E00",
  neutral   = "#999999",
  clinical  = "#0072B2",
  patient_reported = "#E69F00",
  pem       = "#D55E00",
  no_pem    = "#0072B2"
)

theme_review <- theme_minimal(base_size = 12) +
  theme(
    text = element_text(family = ""),
    plot.title = element_text(size = 13, face = "plain", colour = "grey20",
                              margin = margin(b = 4)),
    plot.subtitle = element_text(size = 10, colour = "grey50",
                                  margin = margin(b = 8)),
    plot.title.position = "plot",
    axis.title = element_text(size = 10, colour = "grey40"),
    axis.text = element_text(size = 9, colour = "grey40"),
    axis.ticks = element_line(colour = "grey85", linewidth = 0.3),
    panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_text(size = 9, colour = "grey40"),
    legend.text = element_text(size = 9, colour = "grey40"),
    legend.key.size = unit(0.8, "lines"),
    plot.margin = margin(12, 16, 8, 8),
    strip.text = element_text(size = 10, hjust = 0, colour = "grey30")
  )

theme_lay <- theme_review +
  theme(
    plot.title = element_text(size = 16, face = "plain", colour = "grey20"),
    plot.subtitle = element_text(size = 12, colour = "grey50"),
    axis.text = element_text(size = 11, colour = "grey40"),
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
    geom_vline(xintercept = 0, colour = "grey75", linewidth = 0.4) +
    geom_linerange(aes(xmin = ci_lb, xmax = ci_ub),
                   colour = "grey55", linewidth = 0.6) +
    geom_point(aes(size = k), colour = cols_review["clinical"],
               shape = 18, show.legend = FALSE) +
    scale_size_continuous(range = c(3, 7)) +
    geom_text(aes(x = ci_ub + 0.03 * diff(range(c(ci_lb, ci_ub))),
                  label = sprintf("%.2f [%.2f, %.2f]", yi, ci_lb, ci_ub)),
              hjust = 0, size = 2.8, colour = "grey40") +
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
  weights <- 1 / vi
  weights <- weights / max(weights, na.rm = TRUE)

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
    geom_vline(xintercept = 0, colour = "grey75", linewidth = 0.4)

  if (show_prediction && !is.null(fit$cr.lb)) {
    p <- p + annotate(
      "rect",
      xmin = fit$cr.lb, xmax = fit$cr.ub,
      ymin = -Inf, ymax = Inf,
      alpha = 0.06, fill = cols_review["neutral"]
    )
  }

  p <- p +
    geom_linerange(aes(xmin = ci_lb, xmax = ci_ub),
                   colour = "grey60", linewidth = 0.4) +
    geom_point(aes(size = w), colour = cols_review["clinical"],
               shape = 15, show.legend = FALSE) +
    scale_size_continuous(range = c(1.5, 4)) +
    geom_hline(yintercept = 1.4, colour = "grey85", linewidth = 0.3) +
    geom_point(
      data = summary_df, aes(x = yi, y = study),
      shape = 18, size = 4.5, colour = cols_review["harm"]
    ) +
    geom_linerange(
      data = summary_df, aes(xmin = ci_lb, xmax = ci_ub),
      linewidth = 1, colour = cols_review["harm"]
    ) +
    geom_text(
      data = df,
      aes(x = ci_ub + 0.05 * diff(range(c(ci_lb, ci_ub))),
          label = sprintf("%.2f", yi)),
      hjust = 0, size = 2.8, colour = "grey50"
    ) +
    labs(
      x = xlab, y = NULL,
      title = title,
      subtitle = subtitle %||% auto_subtitle
    ) +
    theme_review +
    theme(
      panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3),
      axis.text.y = element_text(size = 8)
    )

  p
}

prevalence_plot <- function(data, title = NULL, subtitle = NULL) {
  ggplot(data, aes(x = prevalence, y = study_label)) +
    geom_vline(xintercept = 0, colour = "grey80", linewidth = 0.3) +
    geom_linerange(aes(xmin = ci_lower, xmax = ci_upper),
                   colour = "grey60", linewidth = 0.4) +
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
      linewidth = 0.5, colour = "grey70"
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
                fill = "grey93", colour = NA) +
    geom_vline(xintercept = summary_est, colour = "grey60",
               linewidth = 0.4, linetype = "dashed") +
    geom_point(size = 2, colour = cols_review["clinical"], alpha = 0.7) +
    scale_y_reverse(expand = expansion(mult = c(0.02, 0.05))) +
    labs(
      x = "Effect size", y = "Standard error",
      title = title,
      subtitle = sprintf("Pooled estimate = %.2f (dashed line)", summary_est)
    ) +
    theme_review +
    theme(panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3))
}
