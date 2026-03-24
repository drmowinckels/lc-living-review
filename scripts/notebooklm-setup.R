library(arrow)

db <- read_parquet("data/studies.parquet")
rel <- db[db$llm_relevant == TRUE & !is.na(db$llm_relevant), ]

nlm <- Sys.which("nlm")
if (nlm == "") nlm <- "~/.local/bin/nlm"

run_nlm <- function(...) {
  args <- paste(...)
  cmd <- paste(nlm, args)
  message(">> ", cmd)
  result <- system(cmd, intern = TRUE)
  result
}

create_notebook <- function(title) {
  out <- run_nlm("notebook create", shQuote(title))
  jsonlite::fromJSON(paste(out, collapse = "\n"))$id
}

add_url_source <- function(notebook_id, url, title = NULL) {
  title_flag <- if (!is.null(title)) paste("--title", shQuote(title)) else ""
  run_nlm("source add", notebook_id, "--url", shQuote(url), title_flag)
}

make_pubmed_url <- function(pmid) {
  paste0("https://pubmed.ncbi.nlm.nih.gov/", pmid, "/")
}

make_doi_url <- function(doi) {
  paste0("https://doi.org/", doi)
}

get_url <- function(row) {
  if (!is.na(row$pmid) && row$pmid != "") {
    return(make_pubmed_url(row$pmid))
  }
  if (!is.na(row$doi) && row$doi != "") {
    return(make_doi_url(row$doi))
  }
  return(NA_character_)
}

topics <- split(rel, rel$search_topic)

topic_labels <- c(
  treatment_cbt_psych = "LC/MECFS: CBT & Psychological Treatments",
  treatment_biomedical = "LC/MECFS: Biomedical Treatments",
  biomarkers = "LC/MECFS: Biomarkers",
  outcomes = "LC/MECFS: Outcomes & Trajectories",
  systematic_reviews = "LC/MECFS: Systematic Reviews"
)

max_sources <- 49

notebook_log <- data.frame(
  notebook_id = character(),
  title = character(),
  topic = character(),
  n_sources = integer(),
  stringsAsFactors = FALSE
)

for (topic_name in names(topics)) {
  studies <- topics[[topic_name]]
  label <- topic_labels[[topic_name]]

  if (nrow(studies) <= max_sources) {
    chunks <- list(studies)
    chunk_labels <- label
  } else {
    n_chunks <- ceiling(nrow(studies) / max_sources)
    chunk_idx <- rep(seq_len(n_chunks), each = max_sources)[seq_len(nrow(studies))]
    chunks <- split(studies, chunk_idx)
    chunk_labels <- paste(label, paste0("(Part ", seq_along(chunks), ")"))
  }

  for (i in seq_along(chunks)) {
    chunk <- chunks[[i]]
    title <- chunk_labels[i]

    message("\n=== Creating notebook: ", title, " (", nrow(chunk), " studies) ===")
    nb_id <- create_notebook(title)
    message("Notebook ID: ", nb_id)

    added <- 0
    for (j in seq_len(nrow(chunk))) {
      row <- chunk[j, ]
      url <- get_url(row)
      if (is.na(url)) {
        message("  SKIP: ", row$study_id, " - no PMID or DOI")
        next
      }
      study_title <- paste0(row$study_id, ": ", substr(row$title, 1, 80))
      message(sprintf("  [%d/%d] %s", j, nrow(chunk), row$study_id))
      tryCatch({
        add_url_source(nb_id, url, study_title)
        added <- added + 1
        Sys.sleep(1)
      }, error = function(e) {
        message("  ERROR: ", conditionMessage(e))
      })
    }

    notebook_log <- rbind(notebook_log, data.frame(
      notebook_id = nb_id,
      title = title,
      topic = topic_name,
      n_sources = added,
      stringsAsFactors = FALSE
    ))
    message("Added ", added, " sources to ", title)
  }
}

write.csv(notebook_log, "data/notebooklm-notebooks.csv", row.names = FALSE)
message("\n=== Done! Notebook log saved to data/notebooklm-notebooks.csv ===")
print(notebook_log)
