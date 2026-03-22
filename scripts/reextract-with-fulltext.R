#!/usr/bin/env Rscript

source(here::here("R/utils.R"))
source(here::here("R/extraction.R"))

reextract_with_fulltext <- function(
  limit = Inf,
  cache_dir = here::here("data/fulltext"),
  n_attempts = 3,
  min_agreement = 2,
  rate_limit_delay = 2
) {
  db <- read_study_database()
  cached <- tools::file_path_sans_ext(list.files(cache_dir, pattern = "[.]rds$"))

  if (length(cached) == 0) {
    cli::cli_alert_warning("No cached full texts. Run scripts/fetch-fulltext.R first.")
    return(invisible(NULL))
  }

  log_path <- here::here("data/extraction-log.csv")
  log <- if (file.exists(log_path)) {
    read.csv(log_path, stringsAsFactors = FALSE)
  } else {
    data.frame()
  }

  already_with_ft <- character()
  if (nrow(log) > 0 && "had_supplement" %in% names(log)) {
    already_with_ft <- log$study_id[log$had_supplement]
  }

  candidates <- db[db$study_id %in% cached & !db$study_id %in% already_with_ft, ]
  candidates <- candidates[!is.na(candidates$data_extracted) & candidates$data_extracted, ]

  if (nrow(candidates) == 0) {
    cli::cli_alert_success("No studies need re-extraction.")
    return(invisible(NULL))
  }

  n_todo <- min(nrow(candidates), limit)
  cli::cli_h2("Re-extracting {n_todo} studies with full text")

  stats <- list(success = 0L, failed = 0L, skipped = 0L)

  for (i in seq_len(n_todo)) {
    study <- candidates[i, ]
    sid <- study$study_id
    topic <- study$search_topic

    if (is.na(topic) || topic == "") {
      cli::cli_alert_warning("[{i}/{n_todo}] {sid}: no search_topic, skipping")
      stats$skipped <- stats$skipped + 1L
      next
    }

    ft <- readRDS(file.path(cache_dir, paste0(sid, ".rds")))
    ft_source <- ft$source %||% "cached"

    has_r <- !is.null(ft$results) && nchar(ft$results) > 100
    has_m <- !is.null(ft$methods) && nchar(ft$methods) > 100
    if (!has_r && !has_m) {
      cli::cli_alert_warning("[{i}/{n_todo}] {sid}: cached content too short, skipping")
      stats$skipped <- stats$skipped + 1L
      next
    }

    cli::cli_text("[{i}/{n_todo}] {sid} ({topic}, {ft_source})")

    result <- tryCatch(
      extract_with_consensus(
        title = study$title,
        abstract = study$abstract,
        search_topic = topic,
        fulltext_sections = ft,
        n_attempts = n_attempts,
        min_agreement = min_agreement,
        rate_limit_delay = rate_limit_delay
      ),
      error = function(e) {
        cli::cli_alert_danger("  Error: {conditionMessage(e)}")
        list(consensus = FALSE, data = NULL)
      }
    )

    if (!isTRUE(result$consensus) || is.null(result$data)) {
      cli::cli_alert_warning("  No consensus reached")
      stats$failed <- stats$failed + 1L
      log_extraction(
        study_id = sid, topic = topic,
        consensus = FALSE,
        n_attempts = n_attempts,
        n_parsed = result$n_parsed %||% 0L,
        n_agreeing = result$n_agreeing %||% 0L,
        had_supplement = TRUE
      )
      next
    }

    df <- extraction_to_df(result$data, study_id = sid, search_topic = topic)
    write_extracted_data(df, domain = topic)

    study_type <- study$llm_study_type %||% NA_character_
    if (!is.na(study_type) && !is.null(ft$methods)) {
      qa <- tryCatch(
        assess_quality_consensus(
          title = study$title,
          abstract = study$abstract,
          methods_text = ft$methods,
          study_type = study_type
        ),
        error = function(e) list(consensus = FALSE, data = NULL)
      )
      if (isTRUE(qa$consensus) && !is.null(qa$data)) {
        write_quality_data(qa$data, study_id = sid, search_topic = topic, study_type = study_type)
        cli::cli_alert_success("  Extracted + QA updated")
      } else {
        cli::cli_alert_success("  Extracted (QA failed)")
      }
    } else {
      cli::cli_alert_success("  Extracted")
    }

    log_extraction(
      study_id = sid, topic = topic,
      consensus = TRUE,
      n_attempts = n_attempts,
      n_parsed = result$n_parsed %||% 0L,
      n_agreeing = result$n_agreeing %||% 0L,
      had_supplement = TRUE
    )

    stats$success <- stats$success + 1L
    Sys.sleep(rate_limit_delay)
  }

  cli::cli_h2("Re-extraction Results")
  cli::cli_alert_success("Success: {stats$success}")
  cli::cli_alert_warning("No consensus: {stats$failed}")
  cli::cli_alert_info("Skipped: {stats$skipped}")

  invisible(stats)
}

args <- commandArgs(trailingOnly = TRUE)
limit <- if (length(args) > 0) as.integer(args[1]) else Inf
reextract_with_fulltext(limit = limit)
