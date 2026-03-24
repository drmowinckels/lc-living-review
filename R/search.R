.condition_terms <- paste(
  '("long COVID" OR "post-COVID" OR "PASC" OR "post-acute sequelae"',
  'OR "ME/CFS" OR "myalgic encephalomyelitis" OR "chronic fatigue syndrome"',
  'OR "post-viral fatigue" OR "CFIDS" OR "systemic exertion intolerance")'
)

.study_design_terms <- paste(
  '("randomized" OR "randomised" OR "controlled trial" OR "RCT"',
  'OR "cohort" OR "observational" OR "open-label" OR "pilot"',
  'OR "survey" OR "patient-reported"',
  'OR "effectiveness" OR "efficacy" OR "outcome")'
)

search_queries <- list(
  treatment_cbt_psych = paste(
    '(', .condition_terms,
    'AND ("CBT" OR "cognitive behavio*" OR "graded exercise" OR "GET"',
    'OR "pacing" OR "activity management" OR "rehabilitation"',
    'OR "occupational therapy" OR "self-management"',
    'OR "mindfulness" OR "acceptance and commitment"',
    'OR "psychotherapy" OR "psychological intervention"',
    'OR "behavioural activation" OR "behavioral activation"',
    'OR "lightning process" OR "neurolinguistic"))',
    'AND', .study_design_terms
  ),

  treatment_biomedical = paste(
    '(', .condition_terms,
    'AND ("pharmacological" OR "drug" OR "medication"',
    'OR "supplement" OR "nutraceutical"',
    'OR "low-dose naltrexone" OR "LDN"',
    'OR "rintatolimod" OR "ampligen"',
    'OR "rituximab" OR "immunoglobulin" OR "IVIg"',
    'OR "valganciclovir" OR "valacyclovir" OR "antiviral"',
    'OR "isoprinosine" OR "inosine pranobex"',
    'OR "hydrocortisone" OR "fludrocortisone" OR "corticosteroid"',
    'OR "coenzyme Q10" OR "CoQ10" OR "NADH" OR "D-ribose"',
    'OR "carnitine" OR "acetyl-L-carnitine"',
    'OR "methylphenidate" OR "modafinil" OR "aripiprazole"',
    'OR "galantamine" OR "pyridostigmine"',
    'OR "melatonin" OR "vitamin B12" OR "magnesium"',
    'OR "omega-3" OR "essential fatty acid"',
    'OR "antihistamine" OR "mast cell"',
    'OR "immunoadsorption" OR "plasmapheresis" OR "apheresis"',
    'OR "cyclophosphamide" OR "anakinra"',
    'OR "midodrine" OR "ivabradine" OR "saline infusion"',
    'OR "oxymatrine" OR "staphypan"))',
    'AND', .study_design_terms
  ),

  biomarkers = paste(
    '(', .condition_terms,
    'AND ("biomarker" OR "cytokine" OR "immune marker"',
    'OR "metabolomics" OR "proteomics" OR "transcriptomics"',
    'OR "natural killer" OR "T cell" OR "B cell"',
    'OR "autoantibody" OR "neuroinflammation"))'
  ),

  outcomes = paste(
    '(', .condition_terms,
    'AND ("prevalence" OR "incidence" OR "epidemiology"',
    'OR "population-based" OR "cross-sectional" OR "burden"',
    'OR "recovery" OR "trajectory" OR "prognosis" OR "natural history"',
    'OR "longitudinal" OR "follow-up" OR "prospective cohort"))'
  ),

  systematic_reviews = paste(
    '(', .condition_terms,
    'AND ("systematic review" OR "meta-analysis" OR "scoping review"',
    'OR "umbrella review" OR "rapid review" OR "Cochrane"))'
  )
)

build_date_query <- function(query, date_from = NULL, date_to = NULL) {
  if (is.null(date_from) && is.null(date_to)) return(query)
  from <- date_from %||% "2020-01-01"
  to <- date_to %||% format(Sys.Date(), "%Y-%m-%d")
  paste(query, "AND", sprintf("FIRST_PDATE:[%s TO %s]", from, to))
}

.parse_epmc_results <- function(results) {
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

search_page <- function(query, cursor = "*", page_size = 200) {
  if (!requireNamespace("europepmc", quietly = TRUE)) {
    stop("Install europepmc: install.packages('europepmc')")
  }
  tryCatch({
    resp <- europepmc::epmc_search(
      query = query,
      limit = page_size,
      output = "parsed",
      cursorMark = cursor
    )
    list(
      results = .parse_epmc_results(resp),
      hit_count = attr(resp, "hit_count") %||% nrow(resp),
      next_cursor = attr(resp, "next_cursor")
    )
  }, error = function(e) {
    warning(sprintf("Europe PMC search error: %s", conditionMessage(e)))
    NULL
  })
}

run_search <- function(query,
                       source = "MED",
                       date_from = NULL,
                       date_to = NULL,
                       page_size = 100,
                       max_results = 1000) {
  if (!requireNamespace("europepmc", quietly = TRUE)) {
    stop("Install europepmc: install.packages('europepmc')")
  }

  full_query <- build_date_query(query, date_from, date_to)

  results <- europepmc::epmc_search(
    query = full_query,
    limit = max_results,
    output = "parsed"
  )

  .parse_epmc_results(results)
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

log_search <- function(topic, n_retrieved, n_duplicates = NA_integer_,
                       n_new = NA_integer_, n_screened = NA_integer_,
                       n_relevant = NA_integer_,
                       log_file = here::here("data/search-log.csv")) {
  entry <- data.frame(
    date = Sys.Date(),
    topic = topic,
    n_retrieved = n_retrieved,
    n_duplicates = n_duplicates,
    n_new = n_new,
    n_screened = n_screened,
    n_relevant = n_relevant,
    query = search_queries[[topic]] %||% NA_character_,
    stringsAsFactors = FALSE
  )

  if (file.exists(log_file)) {
    existing <- read.csv(log_file, stringsAsFactors = FALSE)
    for (col in setdiff(names(entry), names(existing))) {
      existing[[col]] <- NA
    }
    for (col in setdiff(names(existing), names(entry))) {
      entry[[col]] <- NA
    }
    entry <- rbind(existing[, names(entry)], entry)
  }

  write.csv(entry, log_file, row.names = FALSE)
}
