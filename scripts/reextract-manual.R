#!/usr/bin/env Rscript

source(here::here("R/utils.R"))
source(here::here("R/extraction.R"))

reextract_manual_studies <- function(
  max_studies = 20,
  max_consecutive_failures = 3
) {
  db <- read_study_database()
  domains <- c("treatment_cbt_psych", "treatment_biomedical")

  total_replaced <- 0L
  total_flagged <- 0L

  for (domain in domains) {
    df <- read_extracted_data(domain)
    if (nrow(df) == 0 || !"extraction_source" %in% names(df)) next

    manual_ids <- unique(df$study_id[
      is.na(df$extraction_source) | df$extraction_source == "manual"
    ])
    verified_ids <- unique(df$study_id[
      !is.na(df$extraction_source) & df$extraction_source == "human_verified"
    ])
    manual_ids <- setdiff(manual_ids, verified_ids)

    if (length(manual_ids) == 0) {
      cli::cli_alert_success("{domain}: all studies have consensus or human-verified extraction")
      next
    }

    studies <- db[db$study_id %in% manual_ids, ]
    studies <- studies[!is.na(studies$llm_relevant) & studies$llm_relevant, ]

    if (nrow(studies) == 0) {
      cli::cli_alert_info("{domain}: no eligible studies found in database")
      next
    }

    n_to_try <- min(nrow(studies), max_studies - total_replaced)
    if (n_to_try <= 0) break
    studies <- studies[seq_len(n_to_try), ]

    cli::cli_h2("{domain}: attempting {nrow(studies)} manual studies")

    consecutive_failures <- 0L
    for (i in seq_len(nrow(studies))) {
      sid <- studies$study_id[i]
      title <- studies$title[i]
      abstract <- if ("abstract" %in% names(studies)) studies$abstract[i] else NA_character_

      cli::cli_alert_info("[{i}/{nrow(studies)}] {sid}")

      fulltext <- tryCatch(
        fetch_study_content(
          studies$pmid[i],
          doi = if ("doi" %in% names(studies)) studies$doi[i] else NA
        ),
        error = function(e) NULL
      )

      result <- extract_study_consensus(
        title = title,
        abstract = if (is.na(abstract)) "" else abstract,
        search_topic = domain,
        fulltext_sections = fulltext,
        n_attempts = 5,
        min_agreement = 3
      )

      log_extraction(
        study_id = sid, topic = domain,
        consensus = result$consensus,
        n_attempts = result$n_attempts,
        n_parsed = result$n_parsed,
        n_agreeing = result$n_agreeing,
        had_supplement = !is.null(fulltext)
      )

      if (result$consensus && !is.null(result$data)) {
        new_df <- extraction_to_df(result$data, sid, domain)
        new_df$extraction_source <- "consensus"

        manual_rows <- df[df$study_id == sid, ]
        discrepancies <- compare_manual_vs_consensus(manual_rows, new_df)

        if (discrepancies$n_major == 0) {
          write_extracted_data(new_df, domain)
          cli::cli_alert_success("{sid}: consensus replaces manual ({discrepancies$summary})")
          total_replaced <- total_replaced + 1L
        } else {
          flag_for_review(sid, domain, discrepancies)
          cli::cli_alert_warning("{sid}: consensus differs from manual — flagged for review ({discrepancies$summary})")
          total_flagged <- total_flagged + 1L
        }
        consecutive_failures <- 0L
      } else if (result$n_parsed == 0) {
        consecutive_failures <- consecutive_failures + 1L
        if (consecutive_failures >= max_consecutive_failures) {
          cli::cli_alert_danger("Rate limited after {i} studies — stopping")
          break
        }
      } else {
        cli::cli_alert_warning("{sid}: no consensus ({result$n_agreeing}/{result$n_attempts})")
        consecutive_failures <- 0L
      }
    }
  }

  cli::cli_h2("Re-extraction summary")
  cli::cli_alert_info("{total_replaced} studies replaced with consensus data")
  cli::cli_alert_info("{total_flagged} studies flagged for human review")
  invisible(list(replaced = total_replaced, flagged = total_flagged))
}

compare_manual_vs_consensus <- function(manual_df, consensus_df) {
  n_manual <- nrow(manual_df)
  n_consensus <- nrow(consensus_df)
  row_diff <- abs(n_manual - n_consensus)

  major <- 0L
  minor <- 0L
  details <- character()

  if (row_diff > max(1, n_manual * 0.5)) {
    major <- major + 1L
    details <- c(details, sprintf("row count: %d manual vs %d consensus", n_manual, n_consensus))
  } else if (row_diff > 0) {
    minor <- minor + 1L
  }

  shared_measures <- intersect(
    manual_df$outcome_measure[!is.na(manual_df$outcome_measure)],
    consensus_df$outcome_measure[!is.na(consensus_df$outcome_measure)]
  )

  for (m in shared_measures) {
    m_row <- manual_df[!is.na(manual_df$outcome_measure) & manual_df$outcome_measure == m, ]
    c_row <- consensus_df[!is.na(consensus_df$outcome_measure) & consensus_df$outcome_measure == m, ]
    if (nrow(m_row) == 0 || nrow(c_row) == 0) next

    for (col in c("mean_intervention", "mean_comparator", "effect_size")) {
      if (!col %in% names(m_row) || !col %in% names(c_row)) next
      mv <- m_row[[col]][1]
      cv <- c_row[[col]][1]
      if (is.na(mv) || is.na(cv)) next
      if (mv == 0 && cv == 0) next
      rel_diff <- abs(mv - cv) / max(abs(mv), abs(cv), 1e-6)
      if (rel_diff > 0.3) {
        major <- major + 1L
        details <- c(details, sprintf("%s %s: manual=%.3f vs consensus=%.3f", m, col, mv, cv))
      } else if (rel_diff > 0.05) {
        minor <- minor + 1L
      }
    }

    if ("effect_size" %in% names(m_row) && "effect_size" %in% names(c_row)) {
      me <- m_row$effect_size[1]
      ce <- c_row$effect_size[1]
      if (!is.na(me) && !is.na(ce) && sign(me) != sign(ce) && me != 0 && ce != 0) {
        major <- major + 1L
        details <- c(details, sprintf("%s: sign flip manual=%.3f vs consensus=%.3f", m, me, ce))
      }
    }
  }

  summary_text <- sprintf("%d outcomes, %d major/%d minor discrepancies",
                          length(shared_measures), major, minor)

  list(
    n_major = major,
    n_minor = minor,
    details = details,
    summary = summary_text
  )
}

flag_for_review <- function(study_id, domain, discrepancies) {
  flag_file <- here::here("data/review-flags.csv")
  flag <- data.frame(
    date = Sys.Date(),
    study_id = study_id,
    domain = domain,
    reason = "manual_vs_consensus_discrepancy",
    details = paste(discrepancies$details, collapse = "; "),
    resolved = FALSE,
    stringsAsFactors = FALSE
  )
  if (file.exists(flag_file)) {
    existing <- read.csv(flag_file, stringsAsFactors = FALSE)
    already <- existing$study_id == study_id & existing$domain == domain & !existing$resolved
    if (any(already)) return(invisible(NULL))
    flag <- rbind(existing, flag)
  }
  write.csv(flag, flag_file, row.names = FALSE)
}

if (sys.nframe() == 0L) {
  cli::cli_h1("Re-extracting manually extracted studies")
  reextract_manual_studies()
}
