#!/usr/bin/env Rscript
source(here::here("R/utils.R"))
source(here::here("R/extraction.R"))

db <- read_study_database()

.detect_condition <- function(text) {
  text <- tolower(text)
  has_lc <- grepl("long covid|post-covid|pasc|post-acute sequelae|post-acute covid", text)
  has_me <- grepl("me/cfs|myalgic encephalomyelitis|chronic fatigue syndrome|\\bcfs\\b|post-viral fatigue|postviral fatigue", text)
  if (has_lc && has_me) "both"
  else if (has_lc) "long COVID"
  else "ME/CFS"
}

.detect_review_type <- function(title) {
  title <- tolower(title)
  if (grepl("meta-analysis|meta analysis|metaanalysis", title)) "meta-analysis"
  else if (grepl("scoping review", title)) "scoping_review"
  else if (grepl("umbrella review", title)) "umbrella_review"
  else if (grepl("rapid review", title)) "rapid_review"
  else "systematic_review"
}

.extract_n_studies <- function(text) {
  m <- regmatches(text, regexpr("(\\d+)\\s+(studies|trials|RCTs|articles|papers)\\s+(were\\s+)?(included|met|identified|selected)", text, ignore.case = TRUE))
  if (length(m) > 0) as.integer(regmatches(m[1], regexpr("\\d+", m[1])))
  else NA_integer_
}

.extract_n_participants <- function(text) {
  m <- regmatches(text, regexpr("(\\d[\\d,]+)\\s+(participants|patients|subjects|individuals)", text, ignore.case = TRUE))
  if (length(m) > 0) as.integer(gsub(",", "", regmatches(m[1], regexpr("[\\d,]+", m[1]))))
  else NA_integer_
}

.detect_domain <- function(title, abstract) {
  text <- tolower(paste(title, abstract))
  if (grepl("treatment|intervention|therapy|efficacy|effectiveness", text) && grepl("systematic|meta-analysis", tolower(title))) "treatment"
  else if (grepl("biomarker|cytokine|immune|immunolog|metabolom|proteom", text)) "biomarkers"
  else if (grepl("prevalence|incidence|epidemiol", text)) "prevalence"
  else if (grepl("prognosis|recovery|trajectory|natural history|longitudinal", text)) "prognosis"
  else if (grepl("diagnos|case definition|criteria", text)) "diagnosis"
  else if (grepl("cognitive|neuropsycholog|memory|attention", text)) "cognition"
  else if (grepl("fatigue|exertion|exercise|physical activity|deconditioning", text)) "fatigue"
  else if (grepl("pain|musculoskeletal|fibromyalgia", text)) "pain"
  else if (grepl("sleep|insomnia|circadian", text)) "sleep"
  else if (grepl("quality of life|disability|function", text)) "quality of life"
  else if (grepl("pem|post-exertional|exercise intolerance", text)) "pem"
  else if (grepl("autonomic|orthostatic|pots|heart rate variab", text)) "autonomic"
  else "other"
}

.detect_finding_direction <- function(text) {
  text <- tolower(text)
  has_up <- grepl("elevated|increased|higher|upregulated|greater|enhanced", text)
  has_down <- grepl("reduced|decreased|lower|downregulated|impaired|diminished|deficient", text)
  has_no <- grepl("no (significant )?difference|did not differ|no change|not significant", text)
  if (has_up && has_down) "mixed"
  else if (has_up) "elevated"
  else if (has_down) "reduced"
  else if (has_no) "no difference"
  else "mixed"
}

.detect_biomarker_type <- function(text) {
  text <- tolower(text)
  if (grepl("cytokine|interleukin|\\bil-|tnf|interferon|chemokine", text)) "cytokine"
  else if (grepl("natural killer|nk cell|t cell|b cell|cd4|cd8|lymphocyte|monocyte", text)) "immune cell"
  else if (grepl("autoantibod|antibod|immunoglobulin|\\bigg\\b|\\bigm\\b|\\biga\\b", text)) "autoantibody"
  else if (grepl("metabolom|metabolite|amino acid|lipid|acylcarnitine", text)) "metabolite"
  else if (grepl("proteom|protein|albumin", text)) "protein"
  else if (grepl("gene expression|transcriptom|mrna|microrna|mirna|epigenom|methylat", text)) "genomic"
  else if (grepl("cortisol|hpa|hypothalamic|pituitary|adrenal|dhea|neuroendocrin", text)) "neuroendocrine"
  else if (grepl("oxidative|antioxidant|reactive oxygen|ros|glutathione|coq10", text)) "oxidative stress"
  else if (grepl("microbiome|microbiota|gut|intestinal|fecal|faecal", text)) "microbiome"
  else if (grepl("autonomic|heart rate|hrv|tilt|orthostatic|blood pressure", text)) "autonomic"
  else if (grepl("mri|fmri|pet|spect|imaging|brain", text)) "imaging"
  else if (grepl("virus|viral|herpes|ebv|hhv|enterovir|retrovir", text)) "viral"
  else if (grepl("mitochondri|atp|energy|lactate", text)) "mitochondrial"
  else "other"
}

.detect_biomarker_name <- function(text) {
  text_lower <- tolower(text)
  markers <- c()
  patterns <- list(
    "NK cells" = "natural killer|nk cell",
    "T cells" = "t cell|cd4|cd8",
    "B cells" = "b cell|cd19|cd20",
    "cytokines" = "cytokine",
    "IL-6" = "\\bil-6\\b|interleukin-6|interleukin 6",
    "TNF-alpha" = "tnf|tumor necrosis",
    "cortisol" = "cortisol",
    "DHEA" = "dhea|dehydroepiandrosterone",
    "HRV" = "heart rate variability|hrv",
    "EBV" = "epstein-barr|ebv",
    "HHV-6" = "hhv-6|herpesvirus.6",
    "enterovirus" = "enterovir",
    "microbiome" = "microbiome|microbiota",
    "oxidative stress markers" = "oxidative stress|ros|glutathione",
    "mitochondrial function" = "mitochondri"
  )
  for (name in names(patterns)) {
    if (grepl(patterns[[name]], text_lower)) markers <- c(markers, name)
  }
  if (length(markers) == 0) return(NA_character_)
  paste(markers, collapse = ", ")
}

.extract_outcome_measure <- function(text) {
  instruments <- c(
    "SF-36", "EQ-5D", "Chalder Fatigue", "Fatigue Severity Scale", "FSS",
    "DePaul Symptom Questionnaire", "DSQ", "Bell Disability Scale",
    "FIS", "MFI", "BDI", "Beck Depression", "HADS", "PHQ",
    "PROMIS", "WHO-5", "WHODAS", "Karnofsky",
    "6-minute walk", "CPET", "VO2"
  )
  found <- c()
  for (inst in instruments) {
    if (grepl(inst, text, ignore.case = TRUE)) found <- c(found, inst)
  }
  if (length(found) == 0) return(NA_character_)
  paste(found, collapse = ", ")
}

.detect_pem <- function(text) {
  grepl("post-exertional|\\bpem\\b|\\bpese\\b|exercise intolerance|exertional", tolower(text))
}

.save_extracted <- function(domain, new_df) {
  existing <- read_extracted_data(domain)
  new_df <- new_df[!new_df$study_id %in% existing$study_id, , drop = FALSE]
  if (nrow(new_df) == 0) {
    cat(sprintf("  No new %s records to add\n", domain))
    return(invisible(NULL))
  }
  for (col in setdiff(names(existing), names(new_df))) new_df[[col]] <- NA
  for (col in setdiff(names(new_df), names(existing))) existing[[col]] <- NA
  all_cols <- union(names(existing), names(new_df))
  combined <- rbind(existing[, all_cols], new_df[, all_cols])
  arrow::write_parquet(combined, here::here(sprintf("data/extracted/%s.parquet", domain)))
  cat(sprintf("  Saved %d total %s records (+%d new)\n", nrow(combined), domain, nrow(new_df)))

  db_fresh <- read_study_database()
  for (sid in unique(new_df$study_id)) {
    idx <- which(db_fresh$study_id == sid & db_fresh$search_topic == domain)
    if (length(idx) > 0) db_fresh$data_extracted[idx] <- TRUE
  }
  write_study_database(db_fresh)
}

# === SYSTEMATIC REVIEWS ===
sr <- db[db$search_topic == "systematic_reviews" & !is.na(db$llm_relevant) & db$llm_relevant & !db$data_extracted & !is.na(db$abstract) & db$abstract != "", ]
cat(sprintf("=== Extracting %d systematic reviews ===\n", nrow(sr)))

sr_records <- lapply(seq_len(nrow(sr)), function(i) {
  text <- paste(sr$title[i], sr$abstract[i])
  data.frame(
    study_id = sr$study_id[i],
    review_type = .detect_review_type(sr$title[i]),
    domain = .detect_domain(sr$title[i], sr$abstract[i]),
    condition = .detect_condition(text),
    n_studies_included = .extract_n_studies(sr$abstract[i]),
    n_participants = .extract_n_participants(sr$abstract[i]),
    search_date = NA_character_,
    outcome_measure = .extract_outcome_measure(sr$abstract[i]),
    pooled_effect_size = NA_real_,
    pooled_ci_lower = NA_real_,
    pooled_ci_upper = NA_real_,
    heterogeneity_i2 = NA_real_,
    grade_certainty = NA_character_,
    main_conclusion = NA_character_,
    diagnostic_criteria_required = grepl("(fukuda|ccc|iom|oxford|cdc).*criteria", tolower(text)),
    pem_addressed = .detect_pem(text),
    prospero_id = NA_character_,
    search_strategy_reported = grepl("search.*strateg|search.*database|searched.*medline|searched.*pubmed", tolower(sr$abstract[i])),
    stringsAsFactors = FALSE
  )
})
.save_extracted("systematic_reviews", do.call(rbind, sr_records))

# === BIOMARKERS ===
bm <- db[db$search_topic == "biomarkers" & !is.na(db$llm_relevant) & db$llm_relevant & !db$data_extracted & !is.na(db$abstract) & db$abstract != "", ]
cat(sprintf("\n=== Extracting %d biomarker studies ===\n", nrow(bm)))

bm_records <- lapply(seq_len(nrow(bm)), function(i) {
  text <- paste(bm$title[i], bm$abstract[i])
  data.frame(
    study_id = bm$study_id[i],
    biomarker_type = .detect_biomarker_type(text),
    biomarker_name = .detect_biomarker_name(text),
    measurement_method = NA_character_,
    n_patients = .extract_n_participants(bm$abstract[i]),
    n_controls = NA_integer_,
    condition = .detect_condition(text),
    diagnostic_criteria = NA_character_,
    finding_direction = .detect_finding_direction(bm$abstract[i]),
    effect_size = NA_real_,
    ci_lower = NA_real_,
    ci_upper = NA_real_,
    p_value = NA_real_,
    sensitivity = NA_real_,
    specificity = NA_real_,
    pem_assessed = .detect_pem(text),
    stringsAsFactors = FALSE
  )
})
.save_extracted("biomarkers", do.call(rbind, bm_records))

# === OUTCOMES ===
oc <- db[db$search_topic == "outcomes" & !is.na(db$llm_relevant) & db$llm_relevant & !db$data_extracted & !is.na(db$abstract) & db$abstract != "", ]
cat(sprintf("\n=== Extracting %d outcomes studies ===\n", nrow(oc)))

oc_records <- lapply(seq_len(nrow(oc)), function(i) {
  text <- paste(oc$title[i], oc$abstract[i])
  data.frame(
    study_id = oc$study_id[i],
    outcome_measure = .extract_outcome_measure(oc$abstract[i]),
    outcome_domain = .detect_domain(oc$title[i], oc$abstract[i]),
    n_total = .extract_n_participants(oc$abstract[i]),
    condition = .detect_condition(text),
    diagnostic_criteria = NA_character_,
    timepoints_weeks = NA_character_,
    baseline_mean = NA_real_,
    baseline_sd = NA_real_,
    final_mean = NA_real_,
    final_sd = NA_real_,
    recovery_n = NA_integer_,
    recovery_definition = NA_character_,
    pem_assessed = .detect_pem(text),
    pem_prevalence = NA_real_,
    stringsAsFactors = FALSE
  )
})
oc_df <- do.call(rbind, oc_records)
oc_df$outcome_domain <- harmonize_outcome_domain(oc_df$outcome_domain)
.save_extracted("outcomes", oc_df)

# === SUMMARY ===
cat("\n=== Summary ===\n")
db <- read_study_database()
for (topic in c("systematic_reviews", "biomarkers", "outcomes")) {
  n_ext <- sum(db$search_topic == topic & db$data_extracted)
  n_rel <- sum(db$search_topic == topic & !is.na(db$llm_relevant) & db$llm_relevant)
  cat(sprintf("%s: %d/%d extracted\n", topic, n_ext, n_rel))
}
