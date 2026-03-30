search_ctgov <- function(condition_terms = NULL, max_results = 1000) {
  base_url <- "https://clinicaltrials.gov/api/v2/studies"

  conditions <- condition_terms %||% paste(
    "long COVID", "post-COVID", "PASC",
    "ME/CFS", "myalgic encephalomyelitis", "chronic fatigue syndrome",
    "post-viral fatigue", "systemic exertion intolerance disease",
    sep = " OR "
  )

  params <- list(
    `query.cond` = conditions,
    pageSize = min(max_results, 1000),
    countTotal = "true",
    fields = paste(
      "NCTId",
      "BriefTitle",
      "OfficialTitle",
      "OverallStatus",
      "StartDate",
      "CompletionDate",
      "PrimaryCompletionDate",
      "ResultsFirstPostDate",
      "StudyType",
      "Phase",
      "EnrollmentCount",
      "InterventionName",
      "InterventionType",
      "ConditionMeshTerm",
      "LeadSponsorName",
      "ReferencePMID",
      "ReferenceType",
      "ReferenceCitation",
      sep = "|"
    )
  )

  all_studies <- list()
  next_token <- NULL
  page <- 1

  repeat {
    if (!is.null(next_token)) params$pageToken <- next_token

    resp <- tryCatch(
      httr2::request(base_url) |>
        httr2::req_url_query(!!!params) |>
        httr2::req_perform() |>
        httr2::resp_body_json(),
      error = function(e) {
        warning(sprintf("ClinicalTrials.gov API error: %s", conditionMessage(e)))
        return(NULL)
      }
    )

    if (is.null(resp)) break

    studies <- resp$studies
    if (length(studies) == 0) break

    all_studies <- c(all_studies, studies)
    message(sprintf("  Page %d: %d studies (total so far: %d)", page, length(studies), length(all_studies)))

    next_token <- resp$nextPageToken
    if (is.null(next_token) || length(all_studies) >= max_results) break

    page <- page + 1
    Sys.sleep(0.5)
  }

  parse_ctgov_results(all_studies)
}

parse_ctgov_results <- function(studies) {
  rows <- lapply(studies, function(s) {
    proto <- s$protocolSection
    id_mod <- proto$identificationModule
    status_mod <- proto$statusModule
    design_mod <- proto$designModule
    arms_mod <- proto$armsInterventionsModule
    conditions_mod <- proto$conditionsModule
    sponsor_mod <- proto$sponsorCollaboratorsModule
    refs_mod <- proto$referencesModule
    results_mod <- s$resultsSection

    interventions <- if (!is.null(arms_mod$interventions)) {
      paste(vapply(arms_mod$interventions, function(x) x$name %||% "", character(1)), collapse = "; ")
    } else NA_character_

    intervention_types <- if (!is.null(arms_mod$interventions)) {
      paste(unique(vapply(arms_mod$interventions, function(x) x$type %||% "", character(1))), collapse = "; ")
    } else NA_character_

    refs <- refs_mod$references %||% list()
    result_pmids <- character(0)
    all_pmids <- character(0)
    for (ref in refs) {
      pmid <- ref$pmid %||% NA_character_
      if (!is.na(pmid)) {
        all_pmids <- c(all_pmids, pmid)
        if (identical(ref$type, "RESULT")) {
          result_pmids <- c(result_pmids, pmid)
        }
      }
    }

    data.frame(
      nct_id = id_mod$nctId %||% NA_character_,
      title = id_mod$briefTitle %||% NA_character_,
      official_title = id_mod$officialTitle %||% NA_character_,
      status = status_mod$overallStatus %||% NA_character_,
      start_date = status_mod$startDateStruct$date %||% NA_character_,
      completion_date = status_mod$completionDateStruct$date %||% NA_character_,
      primary_completion_date = status_mod$primaryCompletionDateStruct$date %||% NA_character_,
      has_results = !is.null(results_mod),
      results_date = status_mod$resultsFirstPostDateStruct$date %||% NA_character_,
      study_type = design_mod$studyType %||% NA_character_,
      phases = paste(design_mod$phases %||% "N/A", collapse = "/"),
      enrollment = design_mod$enrollmentInfo$count %||% NA_integer_,
      interventions = interventions,
      intervention_types = intervention_types,
      sponsor = sponsor_mod$leadSponsor$name %||% NA_character_,
      n_references = length(refs),
      n_result_publications = length(result_pmids),
      result_pmids = paste(result_pmids, collapse = "; "),
      all_pmids = paste(all_pmids, collapse = "; "),
      has_linked_publication = length(all_pmids) > 0,
      has_result_publication = length(result_pmids) > 0,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

parse_ctgov_date <- function(x) {
  full <- as.Date(x, format = "%Y-%m-%d")
  partial <- as.Date(paste0(x, "-01"), format = "%Y-%m-%d")
  ifelse(is.na(full), partial, full) |> as.Date(origin = "1970-01-01")
}

classify_registry_status <- function(registry_df) {
  registry_df$years_since_completion <- NA_real_
  registry_df$publication_status <- NA_character_

  completion <- parse_ctgov_date(registry_df$completion_date)
  primary_completion <- parse_ctgov_date(registry_df$primary_completion_date)
  best_completion <- dplyr::coalesce(primary_completion, completion)

  registry_df$years_since_completion <- as.numeric(
    difftime(Sys.Date(), best_completion, units = "days")
  ) / 365.25

  has_any_pub <- registry_df$has_results |
    registry_df$has_linked_publication |
    registry_df$has_result_publication

  completed_statuses <- c("COMPLETED", "UNKNOWN", "AVAILABLE",
                           "NO_LONGER_AVAILABLE", "TEMPORARILY_NOT_AVAILABLE")

  registry_df$publication_status <- dplyr::case_when(
    has_any_pub ~ "results_posted",
    registry_df$status %in% c("WITHDRAWN", "TERMINATED") ~ "terminated",
    registry_df$status %in% c("NOT_YET_RECRUITING", "RECRUITING", "ENROLLING_BY_INVITATION",
                               "ACTIVE_NOT_RECRUITING", "SUSPENDED") ~ "ongoing",
    registry_df$status %in% completed_statuses &
      !is.na(registry_df$years_since_completion) & registry_df$years_since_completion >= 2 ~ "completed_no_results",
    registry_df$status %in% completed_statuses &
      !is.na(registry_df$years_since_completion) & registry_df$years_since_completion >= 0 ~ "recently_completed",
    registry_df$status %in% completed_statuses &
      is.na(registry_df$years_since_completion) ~ "completed_no_results",
    TRUE ~ "unknown"
  )

  registry_df
}

filter_relevant_trials <- function(registry_df) {
  if ("llm_relevant" %in% names(registry_df) && any(!is.na(registry_df$llm_relevant))) {
    registry_df$relevant <- registry_df$llm_relevant
    registry_df$exclusion_reason_registry <- ifelse(
      !registry_df$relevant & !is.na(registry_df$relevant),
      registry_df$llm_reason,
      NA_character_
    )
    return(registry_df)
  }

  title_lower <- tolower(registry_df$title)
  official_lower <- tolower(registry_df$official_title %||% "")
  interv_lower <- tolower(registry_df$interventions %||% "")
  combined <- paste(title_lower, official_lower, interv_lower)

  vaccine_pattern <- paste(
    "vaccin", "immunization", "immunisation", "booster dose",
    "booster immuni", "mrna-1273", "bnt162", "chadox", "ad26",
    "coronavac", "sinopharm", "sinovac", "covaxin", "sputnik",
    "immunogenicity of", "antibody response to vaccine",
    "vla2001", "zf2001", "covax", "cho cell",
    sep = "|"
  )

  acute_prophylaxis_pattern <- paste(
    "pre-exposure prophylaxis", "post-exposure prophylaxis",
    "prophylaxis of covid", "prevention of covid",
    "prevention of sars", "prophylaxis for covid",
    "hydroxychloroquine.*prophylaxis", "ivermectin.*prophylaxis",
    sep = "|"
  )

  serology_pattern <- paste(
    "seroprevalence", "serosurveillance", "antibody titer",
    "antibody titre", "antibody persistence", "seroconversion",
    "immunoassay", "anti-rbd", "anti-spike",
    "immune response after.*vaccin",
    sep = "|"
  )

  offtopic_pattern <- paste(
    "burnout.*health.*worker", "ptsd.*health.*worker",
    "stress.*health.*worker.*pandemic",
    "psychological.*impact.*pandemic.*staff",
    "cryopreservation.*semen", "semen.*covid",
    "pregnancy.*covid.*vaccine", "pregnant.*vaccin",
    "nursing home.*vaccin", "transplant.*covid.*vaccin",
    "cancer.*vaccin.*covid",
    sep = "|"
  )

  is_irrelevant <- grepl(vaccine_pattern, combined, perl = TRUE) |
    grepl(acute_prophylaxis_pattern, combined, perl = TRUE) |
    grepl(serology_pattern, combined, perl = TRUE) |
    grepl(offtopic_pattern, combined, perl = TRUE)

  lc_mecfs_pattern <- paste(
    "long covid", "post.covid", "pasc", "post.acute sequel",
    "me/cfs", "myalgic encephalomyelitis", "chronic fatigue syndrome",
    "post.viral fatigue", "systemic exertion intolerance",
    "post.covid.19 syndrome", "long.term.*covid",
    sep = "|"
  )
  is_lc_mecfs <- grepl(lc_mecfs_pattern, combined, perl = TRUE)

  registry_df$relevant <- !is_irrelevant | is_lc_mecfs
  registry_df$exclusion_reason_registry <- dplyr::case_when(
    grepl(vaccine_pattern, combined, perl = TRUE) & !is_lc_mecfs ~ "vaccine_immunogenicity",
    grepl(acute_prophylaxis_pattern, combined, perl = TRUE) & !is_lc_mecfs ~ "acute_prophylaxis",
    grepl(serology_pattern, combined, perl = TRUE) & !is_lc_mecfs ~ "serology",
    grepl(offtopic_pattern, combined, perl = TRUE) & !is_lc_mecfs ~ "off_topic",
    TRUE ~ NA_character_
  )

  registry_df
}

registry_screening_prompt <- function(title, official_title, interventions,
                                     intervention_types, status, enrollment) {
  description <- paste0(
    "Title: ", title, "\n",
    if (!is.na(official_title)) paste0("Official title: ", official_title, "\n") else "",
    if (!is.na(interventions)) paste0("Interventions: ", interventions, "\n") else "",
    if (!is.na(intervention_types)) paste0("Intervention types: ", intervention_types, "\n") else "",
    "Status: ", status, "\n",
    if (!is.na(enrollment)) paste0("Enrollment: ", enrollment, "\n") else ""
  )

  paste0(
    "You are screening clinical trial registrations for a living systematic review ",
    "on Long COVID and ME/CFS.\n\n",
    "INCLUDE trials that study:\n",
    "- Long COVID / post-COVID condition / post-acute sequelae of SARS-CoV-2 (PASC)\n",
    "- ME/CFS (myalgic encephalomyelitis / chronic fatigue syndrome)\n",
    "- Post-viral fatigue syndromes with persistent symptoms\n",
    "- Treatments, rehabilitation, biomarkers, prevalence, or outcomes for the above\n\n",
    "EXCLUDE trials that:\n",
    "- Study acute COVID-19 treatment or prevention only (no post-acute focus)\n",
    "- Study COVID-19 vaccine immunogenicity, efficacy, or safety (unless specifically ",
    "studying vaccine effects on Long COVID or ME/CFS)\n",
    "- Study fatigue as a symptom of OTHER conditions (cancer, fibromyalgia, MS, ",
    "lupus, dialysis, transplant rejection, HIV, Parkinson's, etc.)\n",
    "- Study COVID-19 serology, antibody persistence, or seroprevalence only\n",
    "- Study COVID-19 in pregnancy, neonates, or transplant immunology only\n",
    "- Study healthcare worker burnout/PTSD during the pandemic\n",
    "- Are general COVID-19 epidemiology without a Long COVID / post-acute focus\n\n",
    "TRIAL TO SCREEN:\n", description, "\n",
    "Respond with a JSON object (no markdown, no code fences):\n",
    '{"relevant": true/false, "confidence": "high"/"medium"/"low", ',
    '"reason": "brief explanation"}'
  )
}

screen_registry <- function(registry_df, rate_limit_delay = 0.5) {
  source(here::here("R/utils.R"))

  needs_screening <- is.na(registry_df$llm_relevant)
  idx <- which(needs_screening)
  cat(sprintf("Screening %d / %d registry entries\n", length(idx), nrow(registry_df)))

  for (ii in seq_along(idx)) {
    i <- idx[ii]
    if (ii %% 50 == 0 || ii == 1) {
      cat(sprintf("  [%d/%d] %s\n", ii, length(idx),
                  substr(registry_df$title[i], 1, 70)))
    }

    prompt <- registry_screening_prompt(
      title = registry_df$title[i],
      official_title = registry_df$official_title[i],
      interventions = registry_df$interventions[i],
      intervention_types = registry_df$intervention_types[i],
      status = registry_df$status[i],
      enrollment = registry_df$enrollment[i]
    )

    text <- call_llm(prompt, max_tokens = 200)

    if (is.null(text)) {
      registry_df$llm_relevant[i] <- NA
      registry_df$llm_confidence[i] <- NA_character_
      registry_df$llm_reason[i] <- "All providers failed"
      next
    }

    text <- gsub("^```json\\s*|^```\\s*|\\s*```$", "", trimws(text))
    parsed <- tryCatch(
      jsonlite::fromJSON(text),
      error = function(e) list(relevant = NA, confidence = NA, reason = "Parse error")
    )

    registry_df$llm_relevant[i] <- if (is.na(parsed$relevant)) NA else isTRUE(parsed$relevant)
    registry_df$llm_confidence[i] <- parsed$confidence %||% NA_character_
    registry_df$llm_reason[i] <- parsed$reason %||% NA_character_

    Sys.sleep(rate_limit_delay)
  }

  registry_df
}

match_registry_to_publications <- function(registry_df, studies_df) {
  registry_df$matched_pmid <- NA_character_
  registry_df$matched_doi <- NA_character_
  registry_df$matched_title <- NA_character_

  published <- studies_df[
    !is.na(studies_df$is_protocol) & !studies_df$is_protocol &
    (studies_df$included == TRUE | is.na(studies_df$included)),
  ]

  for (i in seq_len(nrow(registry_df))) {
    nct <- registry_df$nct_id[i]
    reg_title <- tolower(registry_df$title[i])

    nct_match <- grep(nct, published$title, ignore.case = TRUE)
    if (length(nct_match) == 0) {
      nct_match <- grep(nct, published$abstract, ignore.case = TRUE)
    }

    if (length(nct_match) > 0) {
      registry_df$matched_pmid[i] <- published$pmid[nct_match[1]]
      registry_df$matched_doi[i] <- published$doi[nct_match[1]]
      registry_df$matched_title[i] <- published$title[nct_match[1]]
      next
    }

    reg_words <- strsplit(gsub("[^a-z0-9 ]", "", reg_title), "\\s+")[[1]]
    reg_words <- reg_words[nchar(reg_words) > 3]
    if (length(reg_words) < 3) next

    for (j in seq_len(nrow(published))) {
      pub_title <- tolower(published$title[j])
      shared <- sum(reg_words %in% strsplit(gsub("[^a-z0-9 ]", "", pub_title), "\\s+")[[1]])
      if (shared / length(reg_words) > 0.5) {
        registry_df$matched_pmid[i] <- published$pmid[j]
        registry_df$matched_doi[i] <- published$doi[j]
        registry_df$matched_title[i] <- published$title[j]
        break
      }
    }
  }

  registry_df$has_publication <- !is.na(registry_df$matched_pmid) | !is.na(registry_df$matched_doi)
  registry_df
}

match_protocols_to_publications <- function(studies_df) {
  protocols <- studies_df[!is.na(studies_df$is_protocol) & studies_df$is_protocol, ]
  published <- studies_df[!is.na(studies_df$is_protocol) & !studies_df$is_protocol, ]

  if (nrow(protocols) == 0) return(protocols)

  protocols$matched_publication_id <- NA_character_
  protocols$has_published_results <- FALSE

  for (i in seq_len(nrow(protocols))) {
    proto_title <- tolower(protocols$title[i])
    proto_authors <- tolower(protocols$authors[i] %||% "")

    proto_words <- strsplit(gsub("[^a-z0-9 ]", "", proto_title), "\\s+")[[1]]
    proto_words <- proto_words[nchar(proto_words) > 3]
    proto_words <- setdiff(proto_words, c("protocol", "study", "trial", "randomised",
                                           "randomized", "controlled", "design",
                                           "methods", "rationale"))

    if (length(proto_words) < 3) next

    best_match <- 0
    best_idx <- NA

    for (j in seq_len(nrow(published))) {
      pub_title <- tolower(published$title[j])
      pub_words <- strsplit(gsub("[^a-z0-9 ]", "", pub_title), "\\s+")[[1]]
      shared <- sum(proto_words %in% pub_words)
      score <- shared / length(proto_words)

      if (score > best_match && score > 0.4) {
        best_match <- score
        best_idx <- j
      }
    }

    if (!is.na(best_idx)) {
      protocols$matched_publication_id[i] <- published$study_id[best_idx]
      protocols$has_published_results[i] <- TRUE
    }
  }

  protocols
}
