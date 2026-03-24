#!/usr/bin/env Rscript
# Run data extraction on relevant, unextracted studies.
# Usage:
#   Rscript scripts/extract.R                   # all relevant unextracted
#   Rscript scripts/extract.R --topic outcomes  # filter by search_topic
#   Rscript scripts/extract.R --limit 50        # cap number of studies
#   Rscript scripts/extract.R --reextract       # include already-extracted studies

source(here::here("R/utils.R"))
source(here::here("R/extraction.R"))

args <- commandArgs(trailingOnly = TRUE)

topic_arg <- {
  i <- match("--topic", args)
  if (!is.na(i) && length(args) >= i + 1) args[i + 1] else NULL
}
limit_arg <- {
  i <- match("--limit", args)
  if (!is.na(i) && length(args) >= i + 1) as.integer(args[i + 1]) else Inf
}
reextract <- "--reextract" %in% args

has_api_key <- switch(llm_provider(),
  claude  = nzchar(Sys.getenv("ANTHROPIC_API_KEY")),
  gemini  = nzchar(Sys.getenv("GEMINI_API_KEY")),
  mistral = nzchar(Sys.getenv("MISTRAL_API_KEY")),
  github  = nzchar(Sys.getenv("GITHUB_TOKEN")),
  FALSE
)
if (!has_api_key) {
  cli::cli_abort("No LLM API key found. Set GEMINI_API_KEY, ANTHROPIC_API_KEY, MISTRAL_API_KEY, or GITHUB_TOKEN.")
}

cli::cli_alert_info("LLM provider: {.val {llm_model_name()}}")

existing <- read_study_database()

eligible <- !is.na(existing$llm_relevant) & existing$llm_relevant &
  !is.na(existing$abstract) & existing$abstract != ""

if (!reextract) eligible <- eligible & !existing$data_extracted

if (!is.null(topic_arg)) eligible <- eligible & existing$search_topic == topic_arg

to_extract <- existing[eligible, , drop = FALSE]

if (nrow(to_extract) > limit_arg) {
  to_extract <- to_extract[seq_len(limit_arg), , drop = FALSE]
}

cli::cli_alert_info(
  "{nrow(to_extract)} studies to extract{if (!is.null(topic_arg)) paste0(' (topic: ', topic_arg, ')') else ''}"
)

if (nrow(to_extract) == 0) {
  cli::cli_alert_success("Nothing to extract.")
  quit(save = "no")
}

n_extracted <- 0L
for (topic in unique(to_extract$search_topic)) {
  topic_studies <- to_extract[to_extract$search_topic == topic, ]
  cli::cli_h2("Topic: {topic} ({nrow(topic_studies)} studies)")

  on_success <- function(study_id) {
    db <- read_study_database()
    idx <- match(study_id, db$study_id)
    if (!is.na(idx)) {
      db$data_extracted[idx] <- TRUE
      write_study_database(db)
    }
  }

  extracted <- extract_batch(
    topic_studies,
    search_topic = topic,
    on_success = on_success
  )
  n_extracted <- n_extracted + length(extracted)
}

cli::cli_alert_success("Extraction complete. {n_extracted} studies successfully extracted.")
