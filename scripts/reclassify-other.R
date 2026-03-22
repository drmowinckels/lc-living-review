#!/usr/bin/env Rscript

source(here::here("R/utils.R"))
source(here::here("R/extraction.R"))

reclassify_prompt <- function(title, abstract, current_raw, canonical) {
  paste(
    "You are classifying outcome domains for a systematic review of Long COVID and ME/CFS.",
    "",
    "The allowed outcome domain categories are:",
    paste(canonical, collapse = ", "),
    "",
    sprintf("Study title: %s", title),
    sprintf("Abstract: %s", if (is.na(abstract) || abstract == "") "[No abstract]" else substr(abstract, 1, 3000)),
    "",
    sprintf("The current outcome_domain value is: \"%s\"", current_raw),
    "This did not match any canonical category.",
    "",
    "Based on the study content, pick the single best-fitting canonical category from the list above.",
    "If none fit, respond with \"other\".",
    "",
    "Respond with ONLY the category name, nothing else.",
    sep = "\n"
  )
}

reclassify_other_domains <- function() {
  db <- read_study_database()
  canonical <- paste(unlist(.outcome_domain_enum), collapse = ", ")
  domains <- c("treatment_cbt_psych", "treatment_biomedical", "outcomes")

  total_updated <- 0L

  for (d in domains) {
    path <- here::here("data/extracted", paste0(d, ".parquet"))
    if (!file.exists(path)) next
    df <- as.data.frame(arrow::read_parquet(path))
    if (!"outcome_domain" %in% names(df)) next

    idx <- which(!is.na(df$outcome_domain) & df$outcome_domain == "other")
    if (length(idx) == 0) next

    cli::cli_h2("{d}: {length(idx)} rows to reclassify")

    for (i in idx) {
      study_id <- df$study_id[i]
      study_row <- db[db$study_id == study_id, ]
      if (nrow(study_row) == 0) {
        cli::cli_alert_warning("Study {study_id} not in database, skipping")
        next
      }

      raw_val <- df$outcome_domain_raw[i]
      if (is.na(raw_val)) raw_val <- "other"

      prompt <- reclassify_prompt(
        study_row$title[1],
        study_row$abstract[1],
        raw_val,
        canonical
      )

      result <- call_llm(prompt, max_tokens = 50)
      if (is.null(result)) {
        cli::cli_alert_danger("LLM failed for {study_id}")
        next
      }

      new_domain <- tolower(trimws(result))
      new_domain <- gsub("^[\"']|[\"']$", "", new_domain)
      valid <- unlist(.outcome_domain_enum)
      if (!new_domain %in% valid) {
        cli::cli_alert_warning("{study_id}: LLM returned \"{new_domain}\" — not valid, keeping other")
        next
      }

      if (new_domain == "other") {
        cli::cli_text("  {study_id}: confirmed as other (raw: {raw_val})")
        df$outcome_domain_raw[i] <- raw_val
      } else {
        cli::cli_alert_success("{study_id}: \"{raw_val}\" -> \"{new_domain}\"")
        df$outcome_domain_raw[i] <- raw_val
        df$outcome_domain[i] <- new_domain
        total_updated <- total_updated + 1L
      }

      Sys.sleep(0.5)
    }

    arrow::write_parquet(df, path)
    cli::cli_alert_info("Saved {.file {basename(path)}}")
  }

  cli::cli_alert_success("Reclassified {total_updated} values across all domains")
}

reclassify_other_domains()
