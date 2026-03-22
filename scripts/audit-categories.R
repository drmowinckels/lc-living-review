#!/usr/bin/env Rscript

source(here::here("R/utils.R"))
source(here::here("R/extraction.R"))

audit_outcome_domains <- function(suggest = TRUE) {
  domains <- c("treatment_cbt_psych", "treatment_biomedical", "outcomes")
  raw_vals <- character()
  mapped_vals <- character()

  for (d in domains) {
    df <- read_extracted_data(d)
    if (!all(c("outcome_domain", "outcome_domain_raw") %in% names(df))) next
    raw_vals <- c(raw_vals, df$outcome_domain_raw)
    mapped_vals <- c(mapped_vals, df$outcome_domain)
  }

  is_other <- !is.na(mapped_vals) & mapped_vals == "other"
  other_raw <- raw_vals[is_other]
  other_raw <- other_raw[!is.na(other_raw) & other_raw != "other"]

  cli::cli_h2("Outcome Domain Audit")
  cli::cli_alert_info("{sum(is_other)} of {length(mapped_vals)} values mapped to 'other'")

  if (length(other_raw) == 0) {
    cli::cli_alert_success("No novel unmapped domains to review.")
    return(invisible(NULL))
  }

  counts <- sort(table(other_raw), decreasing = TRUE)
  cli::cli_h3("Unmapped raw values")
  for (i in seq_along(counts)) {
    cli::cli_text("  {names(counts)[i]}: {counts[i]}")
  }

  has_key <- nzchar(Sys.getenv("GEMINI_API_KEY")) ||
    nzchar(Sys.getenv("ANTHROPIC_API_KEY"))
  if (!suggest || !has_key) {
    cli::cli_alert_info("Set an LLM API key to get mapping suggestions.")
    return(invisible(counts))
  }

  canonical <- paste(unlist(.outcome_domain_enum), collapse = ", ")
  unmapped <- paste(sprintf("- \"%s\" (n=%d)", names(counts), as.integer(counts)),
                    collapse = "\n")

  prompt <- paste(
    "You maintain a controlled vocabulary for outcome domains in a systematic",
    "review of Long COVID and ME/CFS. The canonical categories are:",
    canonical,
    "",
    "The following outcome_domain values were extracted by an LLM but did not",
    "match any category or mapping entry:",
    unmapped,
    "",
    "For each value, respond with exactly ONE of these formats:",
    "MAP: \"<raw value>\" -> \"<existing canonical category>\"",
    "NEW: \"<proposed category>\" — <brief justification>",
    "DROP: \"<raw value>\" — not a meaningful outcome domain",
    "",
    "Be conservative: only propose NEW categories if no existing one fits.",
    "Respond with one line per value, no preamble.",
    sep = "\n"
  )

  cli::cli_h3("LLM Suggestions")
  result <- call_llm(prompt, max_tokens = 1500)
  if (!is.null(result)) {
    cli::cli_verbatim(result)
  } else {
    cli::cli_alert_danger("LLM call failed.")
  }

  invisible(counts)
}

audit_outcome_domains()
