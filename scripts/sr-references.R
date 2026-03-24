#!/usr/bin/env Rscript
source(here::here("R/utils.R"))
source(here::here("R/extraction.R"))

db <- read_study_database()

sr <- db[db$search_topic == "systematic_reviews" &
  !is.na(db$llm_relevant) & db$llm_relevant &
  !is.na(db$pmid) & db$pmid != "", ]
cli::cli_h1("Extracting references from {nrow(sr)} systematic reviews")

extract_references_from_pmc <- function(pmid) {
  pmcid <- pmid_to_pmcid(pmid)
  if (is.null(pmcid)) return(NULL)

  xml_doc <- tryCatch(
    suppressMessages(europepmc::epmc_ftxt(ext_id = pmcid)),
    error = function(e) NULL
  )
  if (is.null(xml_doc)) return(NULL)

  refs <- xml2::xml_find_all(xml_doc, "//ref")
  if (length(refs) == 0) return(NULL)

  ref_data <- lapply(refs, function(ref) {
    pub_ids <- xml2::xml_find_all(ref, ".//pub-id")
    pmid_node <- pub_ids[xml2::xml_attr(pub_ids, "pub-id-type") == "pmid"]
    doi_node <- pub_ids[xml2::xml_attr(pub_ids, "pub-id-type") == "doi"]

    ref_pmid <- if (length(pmid_node) > 0) xml2::xml_text(pmid_node[1]) else NA_character_
    ref_doi <- if (length(doi_node) > 0) xml2::xml_text(doi_node[1]) else NA_character_

    title_nodes <- xml2::xml_find_all(ref, ".//article-title")
    ref_title <- if (length(title_nodes) > 0) xml2::xml_text(title_nodes[1]) else NA_character_

    year_nodes <- xml2::xml_find_all(ref, ".//year")
    ref_year <- if (length(year_nodes) > 0) xml2::xml_text(year_nodes[1]) else NA_character_

    data.frame(
      ref_pmid = ref_pmid,
      ref_doi = ref_doi,
      ref_title = ref_title,
      ref_year = ref_year,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, ref_data)
}

all_refs <- list()
n_fetched <- 0L
n_failed <- 0L

limit <- as.integer(Sys.getenv("LIMIT", unset = "50"))

for (i in seq_len(min(nrow(sr), limit))) {
  sid <- sr$study_id[i]
  pmid <- sr$pmid[i]

  refs <- tryCatch(
    extract_references_from_pmc(pmid),
    error = function(e) NULL
  )

  if (is.null(refs)) {
    n_failed <- n_failed + 1L
    next
  }

  refs$source_study_id <- sid
  refs$source_pmid <- pmid
  all_refs[[sid]] <- refs
  n_fetched <- n_fetched + 1L

  if (i %% 10 == 0) {
    cli::cli_alert_info("[{i}/{min(nrow(sr), limit)}] {n_fetched} fetched, {n_failed} failed")
  }
  Sys.sleep(0.5)
}

if (length(all_refs) == 0) {
  cli::cli_alert_warning("No references extracted.")
  quit(save = "no")
}

ref_df <- do.call(rbind, all_refs)
cli::cli_alert_info("Total references extracted: {nrow(ref_df)}")
cli::cli_alert_info("From {n_fetched} systematic reviews")

ref_df <- ref_df[!is.na(ref_df$ref_pmid) | !is.na(ref_df$ref_doi), ]
cli::cli_alert_info("With identifiers (PMID or DOI): {nrow(ref_df)}")

db_pmids <- db$pmid[!is.na(db$pmid) & db$pmid != ""]
db_dois <- tolower(trimws(db$doi[!is.na(db$doi) & db$doi != ""]))

ref_df$in_database <- FALSE
for (j in seq_len(nrow(ref_df))) {
  if (!is.na(ref_df$ref_pmid[j]) && ref_df$ref_pmid[j] %in% db_pmids) {
    ref_df$in_database[j] <- TRUE
  } else if (!is.na(ref_df$ref_doi[j]) && tolower(trimws(ref_df$ref_doi[j])) %in% db_dois) {
    ref_df$in_database[j] <- TRUE
  }
}

n_in_db <- sum(ref_df$in_database)
n_missing <- sum(!ref_df$in_database)
cli::cli_alert_info("Already in database: {n_in_db}")
cli::cli_alert_warning("Missing from database: {n_missing}")

missing <- ref_df[!ref_df$in_database, ]
missing <- missing[!duplicated(paste(missing$ref_pmid, missing$ref_doi)), ]
cli::cli_alert_info("Unique missing references: {nrow(missing)}")

write.csv(ref_df, here::here("data/sr-references.csv"), row.names = FALSE)
write.csv(missing, here::here("data/sr-missing-references.csv"), row.names = FALSE)

cli::cli_alert_success("Saved to data/sr-references.csv and data/sr-missing-references.csv")

if (nrow(missing) > 0) {
  cli::cli_h2("Top sources of missing references")
  source_counts <- table(missing$source_study_id)
  top <- head(sort(source_counts, decreasing = TRUE), 10)
  for (sid in names(top)) {
    title <- db$title[db$study_id == sid][1]
    cli::cli_alert_info("{sid} ({top[sid]} missing): {title}")
  }

  cli::cli_h2("Missing references with PMIDs (can be fetched)")
  has_pmid <- missing[!is.na(missing$ref_pmid) & missing$ref_pmid != "", ]
  cli::cli_alert_info("{nrow(has_pmid)} missing references have PMIDs")

  if (nrow(has_pmid) > 0) {
    cli::cli_alert_info("Sample missing (first 20):")
    for (k in seq_len(min(20, nrow(has_pmid)))) {
      cli::cli_text("  PMID:{has_pmid$ref_pmid[k]} [{has_pmid$ref_year[k]}] {has_pmid$ref_title[k]}")
    }
  }
}
