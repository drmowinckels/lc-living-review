library(metafor)

# --- Effect size direction harmonisation ---
#
# Convention: negative SMD = favours the intervention (treatment group improved).
#
# Most clinical scales are "lower is better" (fatigue severity, pain, depression
# scores), so escalc(measure = "SMD", m1i = treatment, m2i = control) naturally
# produces negative values when treatment helps. However, some scales are
# "higher is better" (SF-36 physical function, EQ-5D quality of life, 6-minute
# walk distance). For these, a positive raw SMD actually means treatment helped,
# which violates our convention. We flip the sign for these scales so that
# negative *always* means "favours treatment" across all outcomes.
#
# Additionally, pre-computed effect sizes from LLM extraction may be on raw
# scales (e.g. mean difference in metres, raw questionnaire points) rather than
# standardised (Hedges' g). These cannot be pooled with SMDs. We filter out
# pre-computed effect sizes with |yi| > 3, since real SMDs virtually never
# exceed this threshold.

.higher_is_better_patterns <- c(
  "SF-36", "SF-12", "EQ-5D", "EQ-VAS", "WHOQOL", "WHO-QoL",
  "WHO-5", "WHO5",
  "6MWD", "6-minute walk", "6-Minute Walk", "VO2", "MWT",
  "sit-to-stand", "MSTS",
  "handgrip", "grip strength",
  "Digit Span", "RAVLT",
  "Physical Functioning", "Physical Component", "Mental Component",
  "Social Functioning", "Role Physical", "Role Emotional",
  "Vitality", "General Health", "Mental Health",
  "FIM", "Barthel",
  "COPM", "Bell score",
  "FACIT", "Total effective rate"
)

is_higher_better <- function(outcome_measure, scale_direction = NULL) {
  vapply(seq_along(outcome_measure), function(i) {
    if (!is.null(scale_direction) && !is.na(scale_direction[i])) {
      return(scale_direction[i] == "higher_is_better")
    }
    m <- outcome_measure[i]
    if (is.na(m)) return(FALSE)
    any(vapply(.higher_is_better_patterns, function(pat) {
      grepl(pat, m, ignore.case = TRUE)
    }, logical(1)))
  }, logical(1))
}

is_standardised_es <- function(effect_size_type) {
  if (is.null(effect_size_type)) return(rep(TRUE, 0))
  tolower(trimws(effect_size_type)) %in% c("smd", "hedges_g", "cohen_d", NA)
}

run_meta <- function(data,
                     yi_col = "yi",
                     vi_col = "vi",
                     method = "REML",
                     slab_col = "study_label") {
  yi <- data[[yi_col]]
  vi <- data[[vi_col]]
  slab <- if (slab_col %in% names(data)) {
    data[[slab_col]]
  } else if (!is.null(attr(data, "slab"))) {
    attr(data, "slab")
  } else {
    paste0("Study ", seq_len(nrow(data)))
  }

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

prepare_forest_data <- function(effects) {
  rows <- list()

  has_raw <- all(c("mean_intervention", "sd_intervention", "n_intervention",
                    "mean_comparator", "sd_comparator", "n_comparator") %in% names(effects))
  if (has_raw) {
    raw <- effects |>
      dplyr::filter(
        !is.na(mean_intervention), !is.na(mean_comparator),
        !is.na(sd_intervention), !is.na(sd_comparator),
        !is.na(n_intervention), !is.na(n_comparator),
        n_intervention > 0, n_comparator > 0,
        sd_intervention > 0, sd_comparator > 0
      ) |>
      dplyr::rename(
        mean_treatment = mean_intervention, sd_treatment = sd_intervention,
        n_treatment = n_intervention, mean_control = mean_comparator,
        sd_control = sd_comparator, n_control = n_comparator
      ) |>
      dplyr::mutate(
        study_label = dplyr::if_else(
          !is.na(outcome_measure),
          paste0(study_id, ": ", outcome_measure),
          study_id
        ),
        row_key = paste0(study_id, outcome_measure)
      )
    if (nrow(raw) > 0) {
      es <- compute_effect_sizes(raw, outcome_type = "continuous")
      raw_sd <- if ("scale_direction" %in% names(raw)) raw$scale_direction else NULL
      flip <- is_higher_better(raw$outcome_measure, raw_sd)
      es$yi[flip] <- -es$yi[flip]
      rows[[1]] <- data.frame(
        yi = es$yi, vi = es$vi,
        study_label = es$study_label,
        study_id = raw$study_id,
        intervention_category = if ("intervention_category" %in% names(raw)) raw$intervention_category else NA,
        outcome_domain = if ("outcome_domain" %in% names(raw)) raw$outcome_domain else NA,
        outcome_measure = if ("outcome_measure" %in% names(raw)) raw$outcome_measure else NA,
        row_key = raw$row_key
      )
    }
  }

  has_es <- all(c("effect_size", "ci_lower", "ci_upper") %in% names(effects))
  if (has_es) {
    already <- if (length(rows) > 0) rows[[1]]$row_key else character(0)
    pre <- effects |>
      dplyr::filter(!is.na(effect_size), !is.na(ci_lower), !is.na(ci_upper)) |>
      dplyr::mutate(
        row_key = paste0(study_id, outcome_measure),
        study_label = dplyr::if_else(
          !is.na(outcome_measure),
          paste0(study_id, ": ", outcome_measure),
          study_id
        ),
        yi = effect_size,
        ci_z = qnorm((1 + dplyr::coalesce(
          if ("ci_level" %in% names(effects)) ci_level else NULL,
          0.95
        )) / 2),
        vi = ((ci_upper - ci_lower) / (2 * ci_z))^2
      ) |>
      dplyr::filter(!row_key %in% already)
    if (nrow(pre) > 0) {
      pre_sd <- if ("scale_direction" %in% names(pre)) pre$scale_direction else NULL
      flip_pre <- is_higher_better(pre$outcome_measure, pre_sd)
      pre$yi[flip_pre] <- -pre$yi[flip_pre]
      if ("effect_size_type" %in% names(pre)) {
        pre <- pre[is.na(pre$effect_size_type) | is_standardised_es(pre$effect_size_type), , drop = FALSE]
      }
      pre <- pre[abs(pre$yi) <= 3, , drop = FALSE]
    }
    if (nrow(pre) > 0) {
      rows[[length(rows) + 1]] <- data.frame(
        yi = pre$yi, vi = pre$vi,
        study_label = pre$study_label,
        study_id = pre$study_id,
        intervention_category = if ("intervention_category" %in% names(pre)) pre$intervention_category else NA,
        outcome_domain = if ("outcome_domain" %in% names(pre)) pre$outcome_domain else NA,
        outcome_measure = if ("outcome_measure" %in% names(pre)) pre$outcome_measure else NA,
        row_key = pre$row_key
      )
    }
  }

  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

compute_effect_sizes <- function(data, outcome_type = "continuous") {
  if (outcome_type == "continuous") {
    es <- escalc(
      measure = "SMD",
      m1i = data$mean_treatment,
      sd1i = data$sd_treatment,
      n1i = data$n_treatment,
      m2i = data$mean_control,
      sd2i = data$sd_control,
      n2i = data$n_control,
      slab = data$study_label
    )
    es$study_label <- data$study_label
  } else if (outcome_type == "binary") {
    es <- escalc(
      measure = "OR",
      ai = data$events_treatment,
      bi = data$n_treatment - data$events_treatment,
      ci = data$events_control,
      di = data$n_control - data$events_control,
      slab = data$study_label
    )
  }
  valid <- is.finite(es$yi) & is.finite(es$vi) & es$vi > 0
  es[valid, , drop = FALSE]
}

leave_one_out <- function(fit) {
  leave1out(fit)
}

influence_diagnostics <- function(fit) {
  influence(fit)
}

publication_bias <- function(fit) {
  if (fit$k < 3) return(list(funnel = NULL, egger = NULL, trim_fill = NULL))
  egger <- tryCatch(regtest(fit), error = function(e) NULL)
  tf <- tryCatch(trimfill(fit), error = function(e) NULL)
  list(
    egger = egger,
    trim_fill = tf,
    egger_p = if (!is.null(egger)) egger$pval else NA_real_,
    tf_k0 = if (!is.null(tf)) tf$k0 else NA_integer_,
    tf_estimate = if (!is.null(tf)) coef(tf) else NA_real_
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
