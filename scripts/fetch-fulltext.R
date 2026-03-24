#!/usr/bin/env Rscript

source(here::here("R/utils.R"))
source(here::here("R/extraction.R"))

safe_filename <- function(x) {
  out <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  gsub("[^a-zA-Z0-9._-]", "", out)
}

fetch_all_fulltext <- function(
  limit = Inf,
  cache_dir = here::here("data/fulltext"),
  delay = 0.5
) {
  db <- read_study_database()
  rel <- db[!is.na(db$llm_relevant) & db$llm_relevant, ]

  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  already_cached <- tools::file_path_sans_ext(list.files(cache_dir, pattern = "\\.rds$"))
  rel$safe_id <- safe_filename(rel$study_id)
  todo <- rel[!rel$safe_id %in% already_cached, ]

  if (nrow(todo) == 0) {
    cli::cli_alert_success("All relevant studies already cached.")
    return(invisible(NULL))
  }

  n_todo <- min(nrow(todo), limit)
  cli::cli_h2("Fetching full text for {n_todo} studies")

  stats <- list(pmc = 0L, publisher_xml = 0L, pdf = 0L, failed = 0L)

  for (i in seq_len(n_todo)) {
    sid <- todo$study_id[i]
    safe_sid <- todo$safe_id[i]
    pmid <- todo$pmid[i]
    doi <- todo$doi[i]

    cli::cli_text("[{i}/{n_todo}] {safe_sid}")

    content <- tryCatch(
      fetch_study_content(
        pmid = if (!is.na(pmid) && pmid != "") pmid else NA,
        doi = if (!is.na(doi) && doi != "") doi else NA
      ),
      error = function(e) {
        cli::cli_alert_danger("Error fetching {safe_sid}: {conditionMessage(e)}")
        NULL
      }
    )

    if (is.null(content)) {
      stats$failed <- stats$failed + 1L
      next
    }

    src <- content$source %||% "unknown"
    if (src %in% names(stats)) {
      stats[[src]] <- stats[[src]] + 1L
    }

    saveRDS(content, file.path(cache_dir, paste0(safe_sid, ".rds")))
    Sys.sleep(delay)
  }

  cli::cli_h2("Results")
  cli::cli_alert_info("PMC XML: {stats$pmc}")
  cli::cli_alert_info("Publisher XML: {stats$publisher_xml}")
  cli::cli_alert_info("PDF text: {stats$pdf}")
  cli::cli_alert_warning("Failed: {stats$failed}")

  total_cached <- length(list.files(cache_dir, pattern = "\\.rds$"))
  cli::cli_alert_success("Total cached: {total_cached} of {nrow(rel)} relevant studies")

  invisible(stats)
}

args <- commandArgs(trailingOnly = TRUE)
limit <- if (length(args) > 0) as.integer(args[1]) else Inf
fetch_all_fulltext(limit = limit)
