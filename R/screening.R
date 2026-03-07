screening_prompt <- function(title, abstract, search_topic) {
  criteria <- list(
    treatment_effectiveness = paste(
      "Treatment or intervention study for Long COVID, post-COVID condition,",
      "ME/CFS, or post-viral fatigue. Must report outcomes of a specific",
      "intervention (pacing, GET, CBT, pharmacological, supplements, rehabilitation).",
      "Must have original data (not a narrative review or editorial)."
    ),
    prevalence = paste(
      "Prevalence or epidemiology study reporting symptom prevalence rates",
      "in Long COVID or ME/CFS populations. Must report point or period",
      "prevalence with sample sizes. Must have original data."
    ),
    pem_prevalence = paste(
      "Study reporting on post-exertional malaise (PEM) or post-exertional",
      "symptom exacerbation (PESE) in Long COVID or ME/CFS. Must report",
      "PEM prevalence, characterisation, or objective measurement (e.g., CPET)."
    ),
    recovery_trajectories = paste(
      "Longitudinal study reporting recovery rates, prognosis, or natural",
      "history of Long COVID or ME/CFS. Must have follow-up data over time.",
      "Studies stratifying by PEM status are especially relevant."
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

screen_study_gemini <- function(title, abstract, search_topic,
                                api_key = Sys.getenv("GEMINI_API_KEY"),
                                model = "gemini-2.5-flash") {
  if (api_key == "") stop("Set GEMINI_API_KEY environment variable")

  prompt <- screening_prompt(title, abstract, search_topic)

  body <- list(
    contents = list(
      list(parts = list(list(text = prompt)))
    ),
    generationConfig = list(
      maxOutputTokens = 300,
      temperature = 0
    )
  )

  url <- sprintf(
    "https://generativelanguage.googleapis.com/v1beta/models/%s:generateContent?key=%s",
    model, api_key
  )

  resp <- httr::POST(
    url = url,
    httr::add_headers(`content-type` = "application/json"),
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "raw"
  )

  if (httr::status_code(resp) != 200) {
    warning(sprintf("API error %d for: %s", httr::status_code(resp), title))
    return(list(relevant = NA, confidence = NA, category = NA,
                study_type = NA, reporting_type = NA, reason = "API error"))
  }

  content <- httr::content(resp, as = "text", encoding = "UTF-8")
  parsed <- jsonlite::fromJSON(content)
  text <- parsed$candidates[[1]]$content$parts[[1]]$text

  tryCatch(
    jsonlite::fromJSON(text),
    error = function(e) {
      warning(sprintf("JSON parse error for: %s", title))
      list(relevant = NA, confidence = NA, category = NA,
           study_type = NA, reporting_type = NA, reason = "Parse error")
    }
  )
}

screen_batch <- function(studies, search_topic, rate_limit_delay = 1.5) {
  results <- vector("list", nrow(studies))

  for (i in seq_len(nrow(studies))) {
    message(sprintf("[%d/%d] Screening: %s", i, nrow(studies), studies$title[i]))

    results[[i]] <- screen_study_gemini(
      title = studies$title[i],
      abstract = studies$abstract[i],
      search_topic = search_topic
    )

    Sys.sleep(rate_limit_delay)
  }

  screening_results <- do.call(rbind, lapply(results, function(r) {
    data.frame(
      llm_relevant = isTRUE(r$relevant),
      llm_category = r$category %||% NA_character_,
      llm_study_type = r$study_type %||% NA_character_,
      llm_reporting_type = r$reporting_type %||% NA_character_,
      llm_confidence = r$confidence %||% NA_character_,
      llm_reason = r$reason %||% NA_character_,
      stringsAsFactors = FALSE
    )
  }))

  cbind(studies, screening_results)
}
