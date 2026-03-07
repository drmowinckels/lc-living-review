library(ggplot2)
library(metafor)

theme_review <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "grey40"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.margin = margin(10, 15, 10, 10)
  )

theme_lay <- theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 13, color = "grey40"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    plot.margin = margin(15, 20, 15, 15)
  )

cols_review <- c(
  benefit = "#0072B2",
  harm = "#D55E00",
  neutral = "#999999",
  clinical = "#0072B2",
  patient_reported = "#E69F00",
  pem = "#D55E00",
  no_pem = "#0072B2"
)

forest_gg <- function(fit,
                      xlab = "Effect Size (Hedges' g)",
                      title = NULL,
                      show_prediction = TRUE) {
  slab <- fit$slab %||% paste("Study", seq_len(fit$k))
  yi <- fit$yi
  vi <- fit$vi
  ci_lb <- yi - qnorm(0.975) * sqrt(vi)
  ci_ub <- yi + qnorm(0.975) * sqrt(vi)

  df <- data.frame(
    study = factor(slab, levels = rev(slab)),
    yi = yi,
    ci_lb = ci_lb,
    ci_ub = ci_ub
  )

  summary_df <- data.frame(
    study = "Summary",
    yi = coef(fit),
    ci_lb = fit$ci.lb,
    ci_ub = fit$ci.ub
  )

  p <- ggplot(df, aes(x = yi, y = study)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_linerange(aes(xmin = ci_lb, xmax = ci_ub), linewidth = 0.5) +
    geom_point(size = 3, colour = cols_review["clinical"]) +
    geom_point(
      data = summary_df,
      aes(x = yi, y = study),
      shape = 18, size = 5, colour = cols_review["harm"]
    ) +
    geom_linerange(
      data = summary_df,
      aes(xmin = ci_lb, xmax = ci_ub),
      linewidth = 1.2, colour = cols_review["harm"]
    ) +
    labs(x = xlab, y = NULL, title = title) +
    theme_review

  if (show_prediction && !is.null(fit$cr.lb)) {
    p <- p + annotate(
      "rect",
      xmin = fit$cr.lb, xmax = fit$cr.ub,
      ymin = -Inf, ymax = Inf,
      alpha = 0.1, fill = cols_review["neutral"]
    )
  }

  p
}

prevalence_plot <- function(data, title = NULL) {
  ggplot(data, aes(x = prevalence, y = study_label)) +
    geom_vline(xintercept = 0, colour = "grey80") +
    geom_linerange(aes(xmin = ci_lower, xmax = ci_upper), linewidth = 0.5) +
    geom_point(size = 3, colour = cols_review["clinical"]) +
    scale_x_continuous(labels = scales::percent_format()) +
    labs(x = "Prevalence", y = NULL, title = title) +
    theme_review
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
      linewidth = 0.6, colour = "black"
    ) +
    geom_point(aes(x = .data[[xmin]], colour = .data[[group]]), size = 4) +
    geom_point(aes(x = .data[[xmax]], colour = .data[[group]]), size = 4) +
    scale_colour_manual(values = cols_review, name = NULL) +
    labs(title = title, subtitle = subtitle, x = NULL, y = NULL) +
    theme_lay
}

grade_heatmap <- function(quality_data, title = "GRADE Evidence Quality") {
  grade_levels <- c("Very Low", "Low", "Moderate", "High")
  grade_colours <- c(
    "Very Low" = "#D55E00",
    "Low" = "#E69F00",
    "Moderate" = "#56B4E9",
    "High" = "#009E73"
  )

  quality_data$grade <- factor(quality_data$grade, levels = grade_levels)

  ggplot(quality_data, aes(x = outcome, y = comparison, fill = grade)) +
    geom_tile(colour = "white", linewidth = 1) +
    geom_text(aes(label = grade), colour = "white", fontface = "bold") +
    scale_fill_manual(values = grade_colours, drop = FALSE, name = "GRADE") +
    labs(title = title, x = NULL, y = NULL) +
    theme_review +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )
}

rob_traffic_light <- function(rob_data, title = "Risk of Bias Assessment") {
  rob_colours <- c(
    "Low" = "#009E73",
    "Some concerns" = "#E69F00",
    "High" = "#D55E00",
    "Not applicable" = "#999999"
  )

  rob_data$judgement <- factor(
    rob_data$judgement,
    levels = c("Low", "Some concerns", "High", "Not applicable")
  )

  ggplot(rob_data, aes(x = domain, y = study_label, fill = judgement)) +
    geom_tile(colour = "white", linewidth = 0.8) +
    scale_fill_manual(values = rob_colours, name = "Judgement") +
    labs(title = title, x = NULL, y = NULL) +
    theme_review +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )
}

funnel_gg <- function(fit, title = "Funnel Plot") {
  yi <- fit$yi
  se <- sqrt(fit$vi)

  df <- data.frame(yi = yi, se = se)
  summary_est <- coef(fit)

  ggplot(df, aes(x = yi, y = se)) +
    geom_point(size = 2.5, colour = cols_review["clinical"]) +
    geom_vline(xintercept = summary_est, linetype = "dashed") +
    scale_y_reverse() +
    labs(x = "Effect Size", y = "Standard Error", title = title) +
    theme_review
}
