#!/usr/bin/env Rscript
# Backfill missing journal fields for existing records in the study database.
# Strategy: EuropePMC (PMID-based) → CrossRef (DOI-based fallback)
# Saves after each batch so progress is never lost on crash.

source(here::here("R/utils.R"))
source(here::here("R/search.R"))

fetch_journals_europepmc <- function(pmids) {
  query <- paste(sprintf("EXT_ID:%s", pmids), collapse = " OR ")
  rows <- tryCatch(
    suppressMessages(europepmc::epmc_search(query, limit = length(pmids), output = "parsed")),
    error = function(e) NULL
  )
  Sys.sleep(0.5)
  if (is.null(rows) || nrow(rows) == 0 || !"pmid" %in% names(rows) || !"journalTitle" %in% names(rows)) {
    return(setNames(rep(NA_character_, length(pmids)), pmids))
  }
  result <- setNames(rep(NA_character_, length(pmids)), pmids)
  matched <- rows[rows$pmid %in% pmids, c("pmid", "journalTitle")]
  result[matched$pmid] <- matched$journalTitle
  result
}

fetch_journal_crossref <- function(doi) {
  tryCatch({
    url <- sprintf(
      "https://api.crossref.org/works/%s?mailto=living-review@project.org",
      utils::URLencode(doi, reserved = TRUE)
    )
    res <- jsonlite::fromJSON(url, simplifyVector = FALSE)
    titles <- res$message$`container-title`
    if (is.null(titles) || length(titles) == 0) NA_character_ else trimws(titles[[1]])
  }, error = function(e) NA_character_)
}

main <- function(batch_size_epmc = 200, batch_size_crossref = 50) {
  existing <- read_study_database()
  needs_journal <- is.na(existing$journal) | existing$journal == ""
  n_missing <- sum(needs_journal)

  cli::cli_alert_info("{n_missing} records missing journal field")
  if (n_missing == 0) {
    cli::cli_alert_success("Nothing to fix.")
    return(invisible(NULL))
  }

  has_pmid     <- needs_journal & !is.na(existing$pmid) & existing$pmid != ""
  has_doi_only <- needs_journal & !has_pmid & !is.na(existing$doi) & existing$doi != ""
  neither      <- needs_journal & !has_pmid & !has_doi_only

  cli::cli_alert_info(
    "{sum(has_pmid)} have PMID (EuropePMC), {sum(has_doi_only)} DOI-only (CrossRef), {sum(neither)} have neither"
  )

  if (any(has_pmid)) {
    cli::cli_h2("Fetching journals via EuropePMC (PMID, batched)")
    pmid_idx <- which(has_pmid)
    batches <- split(pmid_idx, ceiling(seq_along(pmid_idx) / batch_size_epmc))
    lapply(cli::cli_progress_along(batches, name = "EuropePMC"), function(k) {
      idx <- batches[[k]]
      result <- fetch_journals_europepmc(existing$pmid[idx])
      existing$journal[idx] <<- result[existing$pmid[idx]]
      write_study_database(existing)
    })
  }

  if (any(has_doi_only)) {
    cli::cli_h2("Fetching journals via CrossRef (DOI-only, batched)")
    doi_idx <- which(has_doi_only)
    batches <- split(doi_idx, ceiling(seq_along(doi_idx) / batch_size_crossref))
    lapply(cli::cli_progress_along(batches, name = "CrossRef"), function(k) {
      idx <- batches[[k]]
      journals <- vapply(existing$doi[idx], fetch_journal_crossref, character(1))
      existing$journal[idx] <<- journals
      write_study_database(existing)
    })
  }

  n_filled <- sum(!is.na(existing$journal[needs_journal]) & existing$journal[needs_journal] != "")
  cli::cli_alert_success("Filled {n_filled} of {n_missing} records")
}

main()
