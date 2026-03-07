search_queries <- list(
  treatment_effectiveness = paste(
    '(("long COVID" OR "post-COVID" OR "PASC" OR "post-acute sequelae"',
    'OR "ME/CFS" OR "myalgic encephalomyelitis" OR "chronic fatigue syndrome")',
    'AND ("treatment" OR "intervention" OR "therapy" OR "rehabilitation"',
    'OR "pacing" OR "graded exercise" OR "GET" OR "CBT"',
    'OR "cognitive behavio*" OR "pharmacological" OR "drug"',
    'OR "supplement" OR "low-dose naltrexone" OR "LDN"',
    'OR "antihistamine" OR "immunoglobulin"))',
    'AND ("randomized" OR "randomised" OR "controlled trial" OR "RCT"',
    'OR "cohort" OR "observational" OR "survey" OR "patient-reported"',
    'OR "effectiveness" OR "efficacy" OR "outcome")'
  ),

  prevalence = paste(
    '(("long COVID" OR "post-COVID" OR "PASC"',
    'OR "ME/CFS" OR "myalgic encephalomyelitis" OR "chronic fatigue syndrome")',
    'AND ("prevalence" OR "incidence" OR "epidemiology"',
    'OR "population-based" OR "cross-sectional" OR "burden"))'
  ),

  pem_prevalence = paste(
    '(("post-exertional malaise" OR "PEM" OR "post-exertional symptom exacerbation"',
    'OR "PESE" OR "exercise intolerance" OR "activity intolerance")',
    'AND ("long COVID" OR "post-COVID" OR "PASC"',
    'OR "ME/CFS" OR "myalgic encephalomyelitis" OR "chronic fatigue syndrome")',
    'AND ("prevalence" OR "frequency" OR "characteris*" OR "phenotyp*"',
    'OR "two-day CPET" OR "cardiopulmonary exercise"))'
  ),

  recovery_trajectories = paste(
    '(("long COVID" OR "post-COVID" OR "PASC"',
    'OR "ME/CFS" OR "myalgic encephalomyelitis" OR "chronic fatigue syndrome")',
    'AND ("recovery" OR "trajectory" OR "prognosis" OR "natural history"',
    'OR "longitudinal" OR "follow-up" OR "prospective cohort"',
    'OR "remission" OR "improvement"))',
    'AND ("post-exertional malaise" OR "PEM" OR "fatigue"',
    'OR "functional status" OR "quality of life")'
  )
)

run_search <- function(query,
                       source = "MED",
                       date_from = NULL,
                       date_to = NULL,
                       page_size = 100,
                       max_results = 1000) {
  if (!requireNamespace("europepmc", quietly = TRUE)) {
    stop("Install europepmc: install.packages('europepmc')")
  }

  date_range <- NULL
  if (!is.null(date_from) || !is.null(date_to)) {
    from <- date_from %||% "2020-01-01"
    to <- date_to %||% format(Sys.Date(), "%Y-%m-%d")
    date_range <- paste0(
      'FIRST_PDATE:[', from, ' TO ', to, ']'
    )
    query <- paste(query, "AND", date_range)
  }

  results <- europepmc::epmc_search(
    query = query,
    limit = max_results,
    output = "parsed"
  )

  if (nrow(results) == 0) return(data.frame())

  data.frame(
    pmid = results$pmid %||% NA_character_,
    doi = results$doi %||% NA_character_,
    title = results$title %||% NA_character_,
    authors = results$authorString %||% NA_character_,
    journal = results$journalTitle %||% NA_character_,
    year = results$pubYear %||% NA_integer_,
    pub_date = results$firstPublicationDate %||% NA_character_,
    abstract = NA_character_,
    source = results$source %||% NA_character_,
    stringsAsFactors = FALSE
  )
}

fetch_abstracts <- function(pmids, batch_size = 50) {
  if (!requireNamespace("europepmc", quietly = TRUE)) {
    stop("Install europepmc: install.packages('europepmc')")
  }

  abstracts <- character(length(pmids))
  batches <- split(seq_along(pmids), ceiling(seq_along(pmids) / batch_size))

  for (batch in batches) {
    for (i in batch) {
      tryCatch({
        detail <- europepmc::epmc_details(ext_id = pmids[i], data_src = "MED")
        abstracts[i] <- detail$abstractText %||% NA_character_
      }, error = function(e) {
        abstracts[i] <<- NA_character_
      })
      Sys.sleep(0.2)
    }
  }
  abstracts
}

run_all_searches <- function(date_from = NULL, date_to = NULL) {
  all_results <- list()

  for (topic in names(search_queries)) {
    message(sprintf("Searching: %s", topic))
    results <- run_search(
      query = search_queries[[topic]],
      date_from = date_from,
      date_to = date_to
    )

    if (nrow(results) > 0) {
      results$search_topic <- topic
      results$date_added <- Sys.Date()
      all_results[[topic]] <- results
    }

    Sys.sleep(1)
  }

  if (length(all_results) == 0) return(data.frame())
  do.call(rbind, all_results)
}

log_search <- function(topic, n_results, log_file = "data/search-log.csv") {
  entry <- data.frame(
    date = Sys.Date(),
    topic = topic,
    n_results = n_results,
    query = search_queries[[topic]],
    stringsAsFactors = FALSE
  )

  if (file.exists(log_file)) {
    existing <- read.csv(log_file, stringsAsFactors = FALSE)
    entry <- rbind(existing, entry)
  }

  write.csv(entry, log_file, row.names = FALSE)
}
