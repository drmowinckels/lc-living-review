#!/usr/bin/env Rscript

source(here::here("R/utils.R"))
source(here::here("R/extraction.R"))
source(here::here("R/meta-analysis.R"))

audit_data_quality <- function(fix = FALSE) {
  domains <- c("treatment_cbt_psych", "treatment_biomedical")
  all_issues <- list()

  for (domain in domains) {
    df <- read_extracted_data(domain)
    if (nrow(df) == 0) next
    issues <- list()

    if (all(c("ci_lower", "ci_upper") %in% names(df))) {
      idx <- which(!is.na(df$ci_lower) & !is.na(df$ci_upper) & df$ci_lower > df$ci_upper)
      if (length(idx) > 0) {
        issues$swapped_ci <- df[idx, c("study_id", "outcome_measure", "ci_lower", "ci_upper")]
        if (fix) {
          tmp <- df$ci_lower[idx]
          df$ci_lower[idx] <- df$ci_upper[idx]
          df$ci_upper[idx] <- tmp
        }
      }
    }

    if (all(c("effect_size", "ci_lower", "ci_upper") %in% names(df))) {
      idx <- which(
        !is.na(df$effect_size) & !is.na(df$ci_lower) & !is.na(df$ci_upper) &
        df$ci_lower != 0 & df$ci_upper != 0 &
        (df$effect_size < df$ci_lower - 0.01 | df$effect_size > df$ci_upper + 0.01)
      )
      if (length(idx) > 0) {
        issues$es_outside_ci <- df[idx, c("study_id", "outcome_measure", "effect_size", "ci_lower", "ci_upper")]
        if (fix) {
          df$ci_lower[idx] <- NA_real_
          df$ci_upper[idx] <- NA_real_
        }
      }
    }

    for (grp in c("intervention", "comparator")) {
      n_col <- paste0("n_", grp)
      m_col <- paste0("mean_", grp)
      s_col <- paste0("sd_", grp)
      if (all(c(n_col, m_col) %in% names(df))) {
        idx <- which(!is.na(df[[n_col]]) & df[[n_col]] == 0 & !is.na(df[[m_col]]) & df[[m_col]] != 0)
        if (length(idx) > 0) {
          issues[[paste0("n0_", grp)]] <- df[idx, c("study_id", "outcome_measure", n_col, m_col)]
          if (fix) df[[n_col]][idx] <- NA_integer_
        }
      }
      if (all(c(s_col, m_col) %in% names(df))) {
        idx <- which(!is.na(df[[s_col]]) & df[[s_col]] == 0 & !is.na(df[[m_col]]) & df[[m_col]] != 0)
        if (length(idx) > 0) {
          issues[[paste0("zero_sd_", grp)]] <- df[idx, c("study_id", "outcome_measure", s_col, m_col)]
          if (fix) df[[s_col]][idx] <- NA_real_
        }
      }
      if (s_col %in% names(df)) {
        idx <- which(!is.na(df[[s_col]]) & df[[s_col]] < 0)
        if (length(idx) > 0) {
          issues[[paste0("neg_sd_", grp)]] <- df[idx, c("study_id", "outcome_measure", s_col)]
          if (fix) df[[s_col]][idx] <- NA_real_
        }
      }
    }

    key_cols <- intersect(
      c("study_id", "intervention", "outcome_measure", "mean_intervention", "effect_size"),
      names(df)
    )
    dup_idx <- which(duplicated(df[, key_cols]))
    if (length(dup_idx) > 0) {
      issues$duplicates <- df[dup_idx, c("study_id", "intervention", "outcome_measure")]
      if (fix) df <- df[-dup_idx, ]
    }

    if ("effect_size_type" %in% names(df)) {
      smd_rows <- df[!is.na(df$effect_size) & !is.na(df$effect_size_type) &
                      df$effect_size_type %in% c("smd", "hedges_g"), ]
      extreme <- smd_rows[abs(smd_rows$effect_size) > 3, ]
      if (nrow(extreme) > 0) {
        issues$implausible_smd <- extreme[, c("study_id", "outcome_measure", "effect_size")]
      }
    }

    if (all(c("mean_intervention", "mean_comparator", "effect_size") %in% names(df))) {
      check <- df[!is.na(df$mean_intervention) & !is.na(df$mean_comparator) &
                  !is.na(df$effect_size) &
                  df$mean_intervention != 0 & df$mean_comparator != 0 &
                  df$effect_size != 0, ]
      if (nrow(check) > 0) {
        raw_sign <- sign(check$mean_intervention - check$mean_comparator)
        es_sign <- sign(check$effect_size)
        mismatch <- check[raw_sign != es_sign & raw_sign != 0, ]
        if (nrow(mismatch) > 0) {
          issues$direction_mismatch <- mismatch[, c(
            "study_id", "outcome_measure",
            "mean_intervention", "mean_comparator", "effect_size"
          )]
        }
      }
    }

    if (length(issues) > 0) {
      all_issues[[domain]] <- issues
      if (fix) write_extracted_data_raw(df, domain)
    }
  }

  all_issues
}

write_extracted_data_raw <- function(df, domain) {
  files <- list(
    treatment_cbt_psych = here::here("data/extracted/treatment_cbt_psych.parquet"),
    treatment_biomedical = here::here("data/extracted/treatment_biomedical.parquet")
  )
  path <- files[[domain]]
  if (!is.null(path)) arrow::write_parquet(df, path)
}

format_issue_summary <- function(issues) {
  lines <- character()
  for (domain in names(issues)) {
    lines <- c(lines, paste0("## ", domain))
    for (check_name in names(issues[[domain]])) {
      df <- issues[[domain]][[check_name]]
      n <- if (is.data.frame(df)) nrow(df) else length(df)
      lines <- c(lines, sprintf("- **%s**: %d rows", check_name, n))
      if (is.data.frame(df) && nrow(df) <= 10) {
        for (i in seq_len(nrow(df))) {
          lines <- c(lines, sprintf("  - %s: %s",
            df$study_id[i],
            if ("outcome_measure" %in% names(df) && !is.na(df$outcome_measure[i]))
              df$outcome_measure[i] else "(no measure)"))
        }
      }
    }
    lines <- c(lines, "")
  }
  paste(lines, collapse = "\n")
}

llm_review_issues <- function(issues) {
  summary_text <- format_issue_summary(issues)
  if (nchar(summary_text) == 0) return(NULL)

  prompt <- paste(
    "You are reviewing data quality issues in a systematic review database of",
    "Long COVID and ME/CFS treatment studies. The following issues were detected",
    "by automated checks. For each issue, classify it as:",
    "",
    "AUTO-FIX: safe to correct automatically (e.g. swapped CIs, duplicates)",
    "INVESTIGATE: needs human review (e.g. direction mismatches, implausible values)",
    "IGNORE: likely a false positive or not worth fixing",
    "",
    "For INVESTIGATE items, briefly explain what to check.",
    "For AUTO-FIX items, confirm the fix is safe.",
    "",
    "Issues found:",
    summary_text,
    "",
    "Respond with one line per issue in format:",
    "<domain> | <check_name> | <AUTO-FIX|INVESTIGATE|IGNORE> | <brief reason>",
    sep = "\n"
  )

  call_llm(prompt, max_tokens = 2000)
}

if (sys.nframe() == 0L) {
  cli::cli_h1("Monthly Data Quality Audit")

  issues <- audit_data_quality(fix = FALSE)

  if (length(issues) == 0) {
    cli::cli_alert_success("All checks passed — no issues found.")
    quit(status = 0)
  }

  cli::cli_h2("Issues Detected")
  cat(format_issue_summary(issues))

  has_key <- nzchar(Sys.getenv("GEMINI_API_KEY")) ||
    nzchar(Sys.getenv("ANTHROPIC_API_KEY"))

  if (has_key) {
    cli::cli_h2("LLM Review")
    review <- llm_review_issues(issues)
    if (!is.null(review)) {
      cli::cli_verbatim(review)

      auto_fix_ok <- grepl("AUTO-FIX", review, ignore.case = TRUE)
      if (any(auto_fix_ok)) {
        cli::cli_h2("Applying Auto-Fixes")
        audit_data_quality(fix = TRUE)
        cli::cli_alert_success("Auto-fixable issues corrected.")
      }
    }
  } else {
    cli::cli_alert_info("No LLM API key — skipping review. Set GEMINI_API_KEY or ANTHROPIC_API_KEY.")
    cli::cli_alert_info("Run with fix=TRUE to apply safe fixes: Rscript scripts/audit-data-quality.R --fix")

    if ("--fix" %in% commandArgs(trailingOnly = TRUE)) {
      audit_data_quality(fix = TRUE)
      cli::cli_alert_success("Structural fixes applied (swapped CIs, duplicates, zero SDs, N=0).")
    }
  }
}
