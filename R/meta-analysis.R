library(metafor)

run_meta <- function(data,
                     yi_col = "yi",
                     vi_col = "vi",
                     method = "REML",
                     slab_col = "study_label") {
  yi <- data[[yi_col]]
  vi <- data[[vi_col]]
  slab <- if (slab_col %in% names(data)) data[[slab_col]] else NULL

  rma(yi = yi, vi = vi, method = method, slab = slab)
}

run_prevalence_meta <- function(data,
                                xi_col = "events",
                                ni_col = "total",
                                method = "REML") {
  xi <- data[[xi_col]]
  ni <- data[[ni_col]]

  dat <- escalc(
    measure = "PLO",
    xi = xi,
    ni = ni,
    slab = data$study_label
  )

  fit <- rma(yi = dat$yi, vi = dat$vi, method = method)

  fit$back_transform <- function(est, ci.lb, ci.ub) {
    data.frame(
      prevalence = transf.ilogit(est),
      ci_lower = transf.ilogit(ci.lb),
      ci_upper = transf.ilogit(ci.ub)
    )
  }

  fit
}

run_subgroup_meta <- function(data,
                              yi_col = "yi",
                              vi_col = "vi",
                              moderator,
                              method = "REML") {
  yi <- data[[yi_col]]
  vi <- data[[vi_col]]
  mods <- data[[moderator]]

  rma(yi = yi, vi = vi, mods = ~ factor(mods), method = method)
}

compute_effect_sizes <- function(data, outcome_type = "continuous") {
  if (outcome_type == "continuous") {
    escalc(
      measure = "SMD",
      m1i = data$mean_treatment,
      sd1i = data$sd_treatment,
      n1i = data$n_treatment,
      m2i = data$mean_control,
      sd2i = data$sd_control,
      n2i = data$n_control,
      slab = data$study_label
    )
  } else if (outcome_type == "binary") {
    escalc(
      measure = "OR",
      ai = data$events_treatment,
      bi = data$n_treatment - data$events_treatment,
      ci = data$events_control,
      di = data$n_control - data$events_control,
      slab = data$study_label
    )
  }
}

leave_one_out <- function(fit) {
  leave1out(fit)
}

influence_diagnostics <- function(fit) {
  influence(fit)
}

publication_bias <- function(fit) {
  list(
    funnel = funnel(fit),
    egger = regtest(fit),
    trim_fill = trimfill(fit)
  )
}

heterogeneity_summary <- function(fit) {
  data.frame(
    Q = fit$QE,
    Q_df = fit$k - 1,
    Q_p = fit$QEp,
    I2 = fit$I2,
    tau2 = fit$tau2,
    tau = sqrt(fit$tau2),
    H2 = fit$H2
  )
}
