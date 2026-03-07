read_study_database <- function(path = "data/studies.csv") {
  if (!file.exists(path)) {
    return(data.frame(
      study_id = character(),
      pmid = character(),
      doi = character(),
      title = character(),
      authors = character(),
      journal = character(),
      year = integer(),
      pub_date = character(),
      abstract = character(),
      search_topic = character(),
      date_added = character(),
      llm_relevant = logical(),
      llm_category = character(),
      llm_study_type = character(),
      llm_reporting_type = character(),
      human_verified = logical(),
      included = logical(),
      exclusion_reason = character(),
      study_type = character(),
      reporting_type = character(),
      diagnostic_criteria = character(),
      pem_assessed = logical(),
      data_extracted = logical(),
      notes = character(),
      stringsAsFactors = FALSE
    ))
  }
  read.csv(path, stringsAsFactors = FALSE)
}

write_study_database <- function(studies, path = "data/studies.csv") {
  write.csv(studies, path, row.names = FALSE)
}

generate_study_id <- function(authors, year) {
  first_author <- sub(",.*", "", authors)
  first_author <- sub(" .*", "", first_author)
  first_author <- tolower(first_author)
  paste0(first_author, year)
}

deduplicate_studies <- function(new_studies, existing_studies) {
  if (nrow(existing_studies) == 0) return(new_studies)
  if (nrow(new_studies) == 0) return(new_studies)

  new_studies$doi_clean <- tolower(trimws(new_studies$doi))
  existing_studies$doi_clean <- tolower(trimws(existing_studies$doi))

  by_doi <- !is.na(new_studies$doi_clean) &
    new_studies$doi_clean %in% existing_studies$doi_clean

  by_pmid <- !is.na(new_studies$pmid) &
    new_studies$pmid %in% existing_studies$pmid

  duplicates <- by_doi | by_pmid

  new_studies$doi_clean <- NULL
  new_studies[!duplicates, , drop = FALSE]
}

read_extracted_data <- function(domain) {
  files <- list(
    treatment_effectiveness = "data/extracted/treatment_effects.csv",
    prevalence = "data/extracted/prevalence_estimates.csv",
    pem_prevalence = "data/extracted/pem_prevalence.csv",
    recovery_trajectories = "data/extracted/recovery_outcomes.csv"
  )

  path <- files[[domain]]
  if (is.null(path) || !file.exists(path)) {
    return(data.frame())
  }
  read.csv(path, stringsAsFactors = FALSE)
}

read_quality_ratings <- function() {
  path <- "data/extracted/quality_ratings.csv"
  if (!file.exists(path)) return(data.frame())
  read.csv(path, stringsAsFactors = FALSE)
}
