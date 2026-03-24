screening_prompt <- function(title, abstract, search_topic) {
  criteria <- list(
    treatment_cbt_psych = paste(
      "Behavioural, psychological, or rehabilitation intervention study for",
      "Long COVID, post-COVID condition, ME/CFS, or post-viral fatigue.",
      "Includes CBT, graded exercise therapy, pacing, activity management,",
      "self-management programmes, occupational therapy, mindfulness,",
      "acceptance and commitment therapy, or similar non-pharmacological",
      "behavioural interventions. Must report outcomes. Must have original data."
    ),
    treatment_biomedical = paste(
      "Pharmacological, supplement, or biomedical intervention study for",
      "Long COVID, post-COVID condition, ME/CFS, or post-viral fatigue.",
      "Includes drugs (e.g., LDN, rituximab, antivirals, corticosteroids,",
      "stimulants, antihistamines, immunoglobulin), supplements (e.g., CoQ10,",
      "NADH, carnitine, vitamins), or procedures (e.g., plasmapheresis,",
      "immunoadsorption, vagus nerve stimulation). Must report outcomes.",
      "Must have original data (not a narrative review or editorial)."
    ),
    biomarkers = paste(
      "Study reporting biomarkers, immune markers, cytokines, metabolomics,",
      "proteomics, or other biological measurements in Long COVID or ME/CFS.",
      "Must have original data."
    ),
    outcomes = paste(
      "Prevalence, epidemiology, or longitudinal outcome study reporting",
      "symptom prevalence rates, recovery trajectories, prognosis, or natural",
      "history of Long COVID or ME/CFS. Must have original data."
    ),
    systematic_reviews = paste(
      "Systematic review, meta-analysis, scoping review, or Cochrane review",
      "of Long COVID or ME/CFS research. Must use systematic search methods."
    )
  )

  sprintf(
    paste(
      "You are screening studies for a living systematic review on Long COVID and ME/CFS.",
      "",
      "INCLUSION CRITERIA for domain '%s':",
      "%s",
      "",
      "EXCLUSION CRITERIA (applies to all domains):",
      "- Case reports or case series with n < 10",
      "- Narrative reviews, editorials, commentaries without original data",
      "- Acute COVID-19 only (no post-acute follow-up)",
      "- Animal studies",
      "",
      "STUDY TO SCREEN:",
      "Title: %s",
      "Abstract: %s",
      "",
      "Respond with a JSON object (no markdown, no code fences):",
      "{",
      '  "relevant": true/false,',
      '  "confidence": "high"/"medium"/"low",',
      '  "category": "%s" or other domain if better fit,',
      '  "study_type": "RCT"/"cohort"/"cross-sectional"/"survey"/"case-control"/"qualitative"/"systematic_review"/"meta-analysis",',
      '  "reporting_type": "clinical"/"patient_reported"/"mixed",',
      '  "reason": "brief explanation"',
      "}",
      sep = "\n"
    ),
    search_topic,
    criteria[[search_topic]],
    title,
    ifelse(is.na(abstract) || abstract == "", "[No abstract available]", abstract),
    search_topic
  )
}

screen_study <- function(title, abstract, search_topic) {
  prompt <- screening_prompt(title, abstract, search_topic)
  text <- call_llm(prompt, max_tokens = 300)

  if (is.null(text)) {
    return(list(relevant = NA, confidence = NA, category = NA,
                study_type = NA, reporting_type = NA, reason = "All providers failed"))
  }

  text <- gsub("^```json\\s*|^```\\s*|\\s*```$", "", trimws(text))

  tryCatch(
    jsonlite::fromJSON(text),
    error = function(e) {
      warning(sprintf("JSON parse error for: %s", title))
      list(relevant = NA, confidence = NA, category = NA,
           study_type = NA, reporting_type = NA, reason = "Parse error")
    }
  )
}

screen_batch <- function(studies, search_topic, save_fn = NULL,
                         rate_limit_delay = 1.5) {
  results <- vector("list", nrow(studies))

  for (i in seq_len(nrow(studies))) {
    cli::cli_alert_info(
      "[{i}/{nrow(studies)}] Screening: {.val {studies$title[i]}}"
    )

    results[[i]] <- screen_study(
      title = studies$title[i],
      abstract = studies$abstract[i],
      search_topic = search_topic
    )

    if (!is.null(save_fn)) save_fn(i, results[[i]])
    Sys.sleep(rate_limit_delay)
  }

  screening_results <- do.call(rbind, lapply(results, function(r) {
    data.frame(
      llm_relevant = if (is.na(r$relevant)) NA else isTRUE(r$relevant),
      llm_category = r$category %||% NA_character_,
      llm_study_type = r$study_type %||% NA_character_,
      llm_reporting_type = r$reporting_type %||% NA_character_,
      llm_confidence = r$confidence %||% NA_character_,
      llm_reason = r$reason %||% NA_character_,
      llm_model = llm_model_name(),
      stringsAsFactors = FALSE
    )
  }))

  cbind(studies, screening_results)
}
