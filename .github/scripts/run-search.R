#!/usr/bin/env Rscript

source("R/search.R")
source("R/utils.R")
source("R/screening.R")

last_search_date <- function(log_file = "data/search-log.csv") {
  if (!file.exists(log_file)) return(NULL)
  log <- read.csv(log_file, stringsAsFactors = FALSE)
  if (nrow(log) == 0) return(NULL)
  max(as.Date(log$date))
}

main <- function() {
  message("=== Living Review Search Pipeline ===")
  message(sprintf("Date: %s", Sys.Date()))

  existing <- read_study_database()
  last_date <- last_search_date()
  date_from <- if (!is.null(last_date)) {
    as.character(last_date)
  } else {
    "2020-01-01"
  }

  message(sprintf("Searching from: %s", date_from))

  new_results <- run_all_searches(date_from = date_from)

  if (nrow(new_results) == 0) {
    message("No new results found.")
    return(invisible(NULL))
  }

  message(sprintf("Found %d results across all topics", nrow(new_results)))

  new_results <- deduplicate_studies(new_results, existing)
  message(sprintf("After deduplication: %d new studies", nrow(new_results)))

  if (nrow(new_results) == 0) {
    message("All results already in database.")
    return(invisible(NULL))
  }

  message("Fetching abstracts...")
  pmids <- new_results$pmid[!is.na(new_results$pmid)]
  if (length(pmids) > 0) {
    abstracts <- fetch_abstracts(pmids)
    new_results$abstract[!is.na(new_results$pmid)] <- abstracts
  }

  has_api_key <- Sys.getenv("GEMINI_API_KEY") != ""

  if (has_api_key) {
    message("Running LLM screening...")
    for (topic in unique(new_results$search_topic)) {
      topic_studies <- new_results[new_results$search_topic == topic, ]
      screened <- screen_batch(topic_studies, search_topic = topic)
      new_results[new_results$search_topic == topic, names(screened)] <- screened
    }
  } else {
    message("No GEMINI_API_KEY set, skipping LLM screening.")
    new_results$llm_relevant <- NA
    new_results$llm_category <- NA
    new_results$llm_study_type <- NA
    new_results$llm_reporting_type <- NA
  }

  new_results$study_id <- mapply(
    generate_study_id,
    new_results$authors,
    new_results$year
  )
  new_results$human_verified <- FALSE
  new_results$included <- NA
  new_results$exclusion_reason <- NA_character_
  new_results$study_type <- NA_character_
  new_results$reporting_type <- NA_character_
  new_results$diagnostic_criteria <- NA_character_
  new_results$pem_assessed <- NA
  new_results$data_extracted <- FALSE
  new_results$notes <- NA_character_

  all_cols <- names(read_study_database())
  for (col in setdiff(all_cols, names(new_results))) {
    new_results[[col]] <- NA
  }
  new_results <- new_results[, all_cols]

  updated <- rbind(existing, new_results)
  write_study_database(updated)

  for (topic in unique(new_results$search_topic)) {
    n <- sum(new_results$search_topic == topic)
    log_search(topic, n)
  }

  n_relevant <- sum(new_results$llm_relevant, na.rm = TRUE)
  message(sprintf(
    "Pipeline complete. %d new studies added (%d flagged as relevant by LLM).",
    nrow(new_results), n_relevant
  ))

  summary_file <- "data/last-run-summary.json"
  summary <- list(
    date = as.character(Sys.Date()),
    new_studies = nrow(new_results),
    llm_relevant = n_relevant,
    topics = as.list(table(new_results$search_topic))
  )
  writeLines(jsonlite::toJSON(summary, auto_unbox = TRUE, pretty = TRUE), summary_file)
}

main()
