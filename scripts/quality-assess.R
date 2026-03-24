#!/usr/bin/env Rscript

source(here::here("R/utils.R"))
source(here::here("R/extraction.R"))

main <- function() {
  cli::cli_h1("Quality Assessment (incremental)")

  db <- read_study_database()
  extracted <- db[
    !is.na(db$llm_relevant) & db$llm_relevant & db$data_extracted,
  ]

  qa_path <- here::here("data/extracted/quality_ratings.parquet")
  already_rated <- character(0)
  if (file.exists(qa_path)) {
    existing_qa <- as.data.frame(arrow::read_parquet(qa_path))
    already_rated <- unique(existing_qa$study_id)
  }

  to_assess <- extracted[!extracted$study_id %in% already_rated, ]
  if (nrow(to_assess) == 0) {
    cli::cli_alert_success("All extracted studies already have quality ratings.")
    return(invisible())
  }

  cli::cli_alert_info("Assessing {nrow(to_assess)} new stud{?y/ies}")

  n_assessed <- 0L
  for (i in seq_len(nrow(to_assess))) {
    study <- to_assess[i, ]
    study_type <- study$llm_study_type
    if (is.na(study_type)) next

    fulltext <- NULL
    methods_text <- NULL
    if (!is.na(study$pmid) && study$pmid != "") {
      fulltext <- tryCatch(
        fetch_fulltext_sections(study$pmid),
        error = function(e) NULL
      )
      if (!is.null(fulltext)) methods_text <- fulltext$methods
    }

    cli::cli_progress_step(
      "[{i}/{nrow(to_assess)}] {study$study_id} ({study_type})"
    )

    result <- assess_quality_consensus(
      title = study$title,
      abstract = study$abstract,
      methods_text = methods_text,
      study_type = study_type
    )

    if (result$consensus && !is.null(result$data)) {
      qa_long <- quality_to_long(
        result$data, study$study_id, study$search_topic, result$rob_tool
      )
      write_quality_data(qa_long)
      n_assessed <- n_assessed + 1L
      overall_row <- qa_long[qa_long$domain_name == "overall", ]
      overall_judgement <- if (nrow(overall_row) > 0) overall_row$judgement[1] else "?"
      cli::cli_alert_success(
        "{study$study_id} ({result$rob_tool}): {overall_judgement}"
      )
    } else {
      cli::cli_alert_warning("{study$study_id}: no consensus")
    }
  }

  cli::cli_alert_success("Quality assessment complete: {n_assessed} new ratings")
}

main()
