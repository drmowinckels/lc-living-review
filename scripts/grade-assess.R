#!/usr/bin/env Rscript

source(here::here("R/utils.R"))
source(here::here("R/extraction.R"))

aggregate_grade_inputs <- function() {
  treatment_domains <- c("treatment_cbt_psych", "treatment_biomedical")
  other_domains <- c("biomarkers", "outcomes")
  qa_path <- here::here("data/extracted/quality_ratings.parquet")

  if (!file.exists(qa_path)) {
    cli::cli_alert_warning("No quality ratings found — run quality-assess.R first")
    return(NULL)
  }

  qa <- as.data.frame(arrow::read_parquet(qa_path))
  db <- read_study_database()

  grade_inputs <- list()

  for (domain in treatment_domains) {
    extract_path <- here::here("data/extracted", paste0(domain, ".parquet"))
    if (!file.exists(extract_path)) next

    ext <- as.data.frame(arrow::read_parquet(extract_path))
    ext$outcome_harmonized <- harmonize_outcomes(ext$outcome_domain)
    harmonized <- unique(ext$outcome_harmonized)
    harmonized <- harmonized[!is.na(harmonized)]

    domain_studies <- db[
      db$search_topic == domain & !is.na(db$llm_relevant) & db$llm_relevant,
    ]
    study_types <- table(domain_studies$llm_study_type)
    study_type_str <- paste(
      names(study_types), study_types,
      sep = ": ", collapse = ", "
    )

    domain_qa <- qa[qa$domain == domain, ]
    qa_summary <- if (nrow(domain_qa) > 0) {
      if ("overall_rob" %in% names(domain_qa)) {
        tbl <- table(domain_qa$overall_rob)
        paste(names(tbl), tbl, sep = ": ", collapse = ", ")
      } else ""
    } else ""

    for (outcome in harmonized) {
      outcome_studies <- ext[ext$outcome_harmonized == outcome, ]
      n_studies <- length(unique(outcome_studies$study_id))
      if (n_studies < 2) next

      label <- paste0(
        gsub("_", "/", sub("treatment_", "", domain)),
        ": ",
        tools::toTitleCase(gsub("_", " ", outcome))
      )

      grade_inputs[[label]] <- list(
        outcome = label,
        domain = domain,
        n_studies = n_studies,
        study_types = study_type_str,
        qa_summary = qa_summary
      )
    }
  }

  for (domain in other_domains) {
    extract_path <- here::here("data/extracted", paste0(domain, ".parquet"))
    if (!file.exists(extract_path)) next

    ext <- as.data.frame(arrow::read_parquet(extract_path))

    grouping_col <- if (domain == "biomarkers") "biomarker_type" else "outcome_domain"
    if (!grouping_col %in% names(ext)) next

    if (domain == "outcomes") {
      ext$outcome_harmonized <- harmonize_outcomes(ext[[grouping_col]])
      groups <- unique(ext$outcome_harmonized)
    } else {
      groups <- unique(ext[[grouping_col]])
    }
    groups <- groups[!is.na(groups)]

    domain_studies <- db[
      db$search_topic == domain & !is.na(db$llm_relevant) & db$llm_relevant,
    ]
    study_types <- table(domain_studies$llm_study_type)
    study_type_str <- paste(
      names(study_types), study_types,
      sep = ": ", collapse = ", "
    )

    for (grp in groups) {
      if (domain == "outcomes") {
        grp_rows <- ext[ext$outcome_harmonized == grp, ]
      } else {
        grp_rows <- ext[ext[[grouping_col]] == grp, ]
      }
      n_studies <- length(unique(grp_rows$study_id))
      if (n_studies < 2) next

      label <- paste0(
        tools::toTitleCase(domain), ": ",
        tools::toTitleCase(gsub("_", " ", grp))
      )

      grade_inputs[[label]] <- list(
        outcome = label,
        domain = domain,
        n_studies = n_studies,
        study_types = study_type_str,
        qa_summary = ""
      )
    }
  }

  grade_inputs
}

main <- function() {
  cli::cli_h1("GRADE Assessment")

  inputs <- aggregate_grade_inputs()
  if (is.null(inputs) || length(inputs) == 0) {
    cli::cli_alert_warning("No outcome domains to assess.")
    return(invisible())
  }

  cli::cli_alert_info("Assessing {length(inputs)} outcome domain{?s}")

  grade_path <- here::here("data/extracted/grade.parquet")
  if (file.exists(grade_path)) file.remove(grade_path)

  n_assessed <- 0L

  for (label in names(inputs)) {
    inp <- inputs[[label]]
    cli::cli_progress_step("GRADE: {label}")

    title_summary <- paste0(
      "Living review of ", inp$domain, " for Long COVID/ME/CFS"
    )
    abstract_summary <- paste0(
      "Body of evidence: ", inp$n_studies,
      " studies covering ", label,
      ". Study types: ", inp$study_types
    )
    if (nzchar(inp$qa_summary)) {
      abstract_summary <- paste0(
        abstract_summary,
        ". Quality ratings: ", inp$qa_summary
      )
    }

    result <- assess_grade_consensus(
      title = title_summary,
      abstract = abstract_summary,
      outcome = inp$outcome,
      n_studies = inp$n_studies,
      study_types = inp$study_types
    )

    if (result$consensus && !is.null(result$data)) {
      grade_row <- as.data.frame(result$data, stringsAsFactors = FALSE)
      grade_row$domain <- inp$domain
      grade_row$llm_model <- llm_model_name()
      write_grade_data(grade_row)
      n_assessed <- n_assessed + 1L
      cli::cli_alert_success("{label}: {result$data$certainty}")
    } else {
      cli::cli_alert_warning("{label}: no consensus")
    }
  }

  cli::cli_alert_success("GRADE complete: {n_assessed}/{length(inputs)} domains assessed")
}

main()
