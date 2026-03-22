`%||%` <- function(x, y) if (!is.null(x)) x else y

is_ci <- function() nzchar(Sys.getenv("CI"))

llm_provider <- function() {
  if (is_ci()) return("gemini")
  p <- Sys.getenv("LLM_PROVIDER")
  if (nzchar(p)) return(p)
  if (nzchar(Sys.getenv("ANTHROPIC_API_KEY"))) return("claude")
  if (nzchar(Sys.getenv("MISTRAL_API_KEY")))   return("mistral")
  if (nzchar(Sys.getenv("GITHUB_TOKEN")))       return("github")
  "gemini"
}

.provider_model <- function(provider) {
  switch(provider,
    claude  = "claude-sonnet-4-6",
    gemini  = "gemini-2.5-flash",
    mistral = "mistral-small-latest",
    github  = "gpt-4o-mini",
    "unknown"
  )
}

llm_model_name <- function() .provider_model(llm_provider())

.available_providers <- function() {
  primary <- llm_provider()
  has_key <- c(
    claude  = nzchar(Sys.getenv("ANTHROPIC_API_KEY")),
    gemini  = nzchar(Sys.getenv("GEMINI_API_KEY")),
    mistral = nzchar(Sys.getenv("MISTRAL_API_KEY")),
    github  = nzchar(Sys.getenv("GITHUB_TOKEN"))
  )
  c(primary, setdiff(names(has_key[has_key]), primary))
}

.rate_limit_cond <- function(provider) {
  structure(
    class = c("rate_limit_error", "error", "condition"),
    list(message = sprintf("Rate limit reached for: %s", provider), provider = provider)
  )
}

.llm_req <- function(url) {
  httr2::request(url) |>
    httr2::req_timeout(60) |>
    httr2::req_error(is_error = \(r) FALSE)
}

.call_openai_compat <- function(prompt, url, api_key, model, max_tokens, provider) {
  body <- jsonlite::toJSON(list(
    model = model,
    temperature = 0,
    max_tokens = max_tokens,
    messages = list(list(role = "user", content = prompt))
  ), auto_unbox = TRUE)
  resp <- .llm_req(url) |>
    httr2::req_headers(Authorization = paste("Bearer", api_key)) |>
    httr2::req_body_raw(body, type = "application/json") |>
    httr2::req_perform()
  if (httr2::resp_status(resp) == 429) stop(.rate_limit_cond(provider))
  if (httr2::resp_status(resp) != 200) return(NULL)
  parsed <- jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = FALSE)
  parsed$choices[[1]]$message$content
}

call_claude_api <- function(prompt,
                             api_key = Sys.getenv("ANTHROPIC_API_KEY"),
                             model = .provider_model("claude"),
                             max_tokens = 2000) {
  if (api_key == "") stop("Set ANTHROPIC_API_KEY environment variable")
  body <- jsonlite::toJSON(list(
    model = model,
    max_tokens = max_tokens,
    temperature = 0,
    messages = list(list(role = "user", content = prompt))
  ), auto_unbox = TRUE)
  resp <- .llm_req("https://api.anthropic.com/v1/messages") |>
    httr2::req_headers(
      `x-api-key`         = api_key,
      `anthropic-version` = "2023-06-01"
    ) |>
    httr2::req_body_raw(body, type = "application/json") |>
    httr2::req_perform()
  if (httr2::resp_status(resp) == 429) stop(.rate_limit_cond("claude"))
  if (httr2::resp_status(resp) != 200) return(NULL)
  parsed <- jsonlite::fromJSON(httr2::resp_body_string(resp))
  parsed$content[[1]]$text
}

call_gemini_api <- function(prompt,
                             api_key = Sys.getenv("GEMINI_API_KEY"),
                             model = .provider_model("gemini"),
                             max_tokens = 2000) {
  if (api_key == "") stop("Set GEMINI_API_KEY environment variable")
  body <- jsonlite::toJSON(list(
    contents = list(list(parts = list(list(text = prompt)))),
    generationConfig = list(maxOutputTokens = max_tokens, temperature = 0)
  ), auto_unbox = TRUE)
  url <- sprintf(
    "https://generativelanguage.googleapis.com/v1beta/models/%s:generateContent?key=%s",
    model, api_key
  )
  resp <- .llm_req(url) |>
    httr2::req_body_raw(body, type = "application/json") |>
    httr2::req_perform()
  if (httr2::resp_status(resp) == 429) stop(.rate_limit_cond("gemini"))
  if (httr2::resp_status(resp) != 200) return(NULL)
  parsed <- jsonlite::fromJSON(httr2::resp_body_string(resp))
  parsed$candidates[[1]]$content$parts[[1]]$text
}

call_mistral_api <- function(prompt,
                              api_key = Sys.getenv("MISTRAL_API_KEY"),
                              model = .provider_model("mistral"),
                              max_tokens = 2000) {
  if (api_key == "") stop("Set MISTRAL_API_KEY environment variable")
  .call_openai_compat(
    prompt, "https://api.mistral.ai/v1/chat/completions",
    api_key, model, max_tokens, "mistral"
  )
}

call_github_api <- function(prompt,
                             api_key = Sys.getenv("GITHUB_TOKEN"),
                             model = .provider_model("github"),
                             max_tokens = 2000) {
  if (api_key == "") stop("Set GITHUB_TOKEN environment variable")
  .call_openai_compat(
    prompt, "https://models.inference.ai.azure.com/chat/completions",
    api_key, model, max_tokens, "github"
  )
}

.call_provider <- function(provider, prompt, max_tokens) {
  switch(provider,
    claude  = call_claude_api(prompt, max_tokens = max_tokens),
    gemini  = call_gemini_api(prompt, max_tokens = max_tokens),
    mistral = call_mistral_api(prompt, max_tokens = max_tokens),
    github  = call_github_api(prompt, max_tokens = max_tokens),
    stop(sprintf("Unknown provider: %s", provider))
  )
}

call_llm <- function(prompt, max_tokens = 2000) {
  for (provider in .available_providers()) {
    result <- tryCatch(
      .call_provider(provider, prompt, max_tokens),
      rate_limit_error = function(e) {
        cli::cli_alert_warning("Rate limit on {.val {provider}}, trying next provider...")
        NULL
      },
      error = function(e) {
        cli::cli_alert_warning("Error with {.val {provider}}: {conditionMessage(e)}")
        NULL
      }
    )
    if (!is.null(result)) return(result)
  }
  cli::cli_alert_danger("All providers exhausted or failed.")
  NULL
}

.check_arrow <- function() {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Install arrow: install.packages('arrow')")
  }
}

.migrate_csv_to_parquet <- function(parquet_path) {
  csv_path <- sub("\\.parquet$", ".csv", parquet_path)
  if (!file.exists(csv_path)) return(invisible(FALSE))
  .check_arrow()
  cli::cli_alert_info("Migrating {.file {basename(csv_path)}} to parquet...")
  arrow::write_parquet(read.csv(csv_path, stringsAsFactors = FALSE), parquet_path)
  cli::cli_alert_success("Migration complete. You may delete {.file {csv_path}}.")
  invisible(TRUE)
}

read_study_database <- function(path = here::here("data/studies.parquet")) {
  if (!file.exists(path)) .migrate_csv_to_parquet(path)
  if (!file.exists(path)) {
    return(data.frame(
      study_id = character(),
      pmid = character(),
      doi = character(),
      title = character(),
      authors = character(),
      journal = character(),
      year = integer(),
      pub_date = character(),
      abstract = character(),
      search_topic = character(),
      date_added = character(),
      llm_relevant = logical(),
      llm_category = character(),
      llm_study_type = character(),
      llm_reporting_type = character(),
      llm_confidence = character(),
      llm_reason = character(),
      llm_model = character(),
      human_verified = logical(),
      included = logical(),
      exclusion_reason = character(),
      study_type = character(),
      reporting_type = character(),
      diagnostic_criteria = character(),
      pem_assessed = logical(),
      data_extracted = logical(),
      notes = character(),
      stringsAsFactors = FALSE
    ))
  }
  .check_arrow()
  as.data.frame(arrow::read_parquet(path))
}

write_study_database <- function(studies, path = here::here("data/studies.parquet")) {
  .check_arrow()
  arrow::write_parquet(studies, path)
}

generate_study_id <- function(authors, year) {
  first_author <- sub(",.*", "", authors)
  first_author <- sub(" .*", "", first_author)
  first_author <- tolower(first_author)
  paste0(first_author, year)
}

deduplicate_studies <- function(new_studies, existing_studies) {
  if (nrow(new_studies) == 0) return(new_studies)

  new_studies$doi_clean <- tolower(trimws(new_studies$doi))
  new_studies$pmid_clean <- trimws(new_studies$pmid)

  dup_key <- ifelse(
    !is.na(new_studies$doi_clean) & new_studies$doi_clean != "",
    new_studies$doi_clean,
    ifelse(
      !is.na(new_studies$pmid_clean) & new_studies$pmid_clean != "",
      paste0("pmid:", new_studies$pmid_clean),
      paste0("title:", tolower(new_studies$title))
    )
  )

  first_occurrence <- !duplicated(dup_key)
  new_studies <- new_studies[first_occurrence, , drop = FALSE]

  if (nrow(existing_studies) > 0) {
    existing_studies$doi_clean <- tolower(trimws(existing_studies$doi))

    by_doi <- !is.na(new_studies$doi_clean) & new_studies$doi_clean != "" &
      new_studies$doi_clean %in% existing_studies$doi_clean

    by_pmid <- !is.na(new_studies$pmid_clean) & new_studies$pmid_clean != "" &
      new_studies$pmid_clean %in% existing_studies$pmid

    no_id <- (is.na(new_studies$doi_clean) | new_studies$doi_clean == "") &
      (is.na(new_studies$pmid_clean) | new_studies$pmid_clean == "")
    by_title <- no_id &
      tolower(trimws(new_studies$title)) %in% tolower(trimws(existing_studies$title))

    new_studies <- new_studies[!(by_doi | by_pmid | by_title), , drop = FALSE]
  }

  new_studies$doi_clean <- NULL
  new_studies$pmid_clean <- NULL
  new_studies
}

read_extracted_data <- function(domain) {
  files <- list(
    treatment_cbt_psych = here::here("data/extracted/treatment_cbt_psych.parquet"),
    treatment_biomedical = here::here("data/extracted/treatment_biomedical.parquet"),
    biomarkers = here::here("data/extracted/biomarkers.parquet"),
    outcomes = here::here("data/extracted/outcomes.parquet"),
    systematic_reviews = here::here("data/extracted/systematic_reviews.parquet")
  )

  path <- files[[domain]]
  if (is.null(path)) return(data.frame())
  if (!file.exists(path)) .migrate_csv_to_parquet(path)
  if (!file.exists(path)) return(data.frame())
  .check_arrow()
  as.data.frame(arrow::read_parquet(path))
}

prisma_counts <- function(
  db_path   = here::here("data/studies.parquet"),
  log_path  = here::here("data/search-log.csv")
) {
  db  <- read_study_database(db_path)
  log <- if (file.exists(log_path)) read.csv(log_path, stringsAsFactors = FALSE) else data.frame()

  n_retrieved  <- if (nrow(log) > 0 && "n_retrieved"  %in% names(log)) sum(log$n_retrieved,  na.rm = TRUE) else NA_integer_
  n_duplicates <- if (nrow(log) > 0 && "n_duplicates" %in% names(log)) sum(log$n_duplicates, na.rm = TRUE) else NA_integer_

  n_records      <- nrow(db)
  n_screened     <- sum(!is.na(db$llm_relevant))
  n_excluded     <- sum(!is.na(db$llm_relevant) & !db$llm_relevant, na.rm = TRUE)
  n_eligible     <- sum(db$llm_relevant, na.rm = TRUE)
  n_extracted    <- sum(db$llm_relevant & db$data_extracted, na.rm = TRUE)
  n_not_extracted <- n_eligible - n_extracted

  list(
    n_retrieved     = n_retrieved,
    n_duplicates    = n_duplicates,
    n_records       = n_records,
    n_screened      = n_screened,
    n_excluded      = n_excluded,
    n_eligible      = n_eligible,
    n_extracted     = n_extracted,
    n_not_extracted = n_not_extracted
  )
}

read_quality_ratings <- function() {
  path <- here::here("data/extracted/quality_ratings.parquet")
  if (!file.exists(path)) .migrate_csv_to_parquet(path)
  if (!file.exists(path)) return(data.frame())
  .check_arrow()
  as.data.frame(arrow::read_parquet(path))
}

read_grade_data <- function() {
  path <- here::here("data/extracted/grade.parquet")
  if (!file.exists(path)) return(data.frame())
  .check_arrow()
  as.data.frame(arrow::read_parquet(path))
}

display_table <- function(df, caption = NULL) {
  df[] <- lapply(df, function(col) {
    if (is.character(col)) {
      col[is.na(col) | trimws(col) == ""] <- "\u2014"
    } else if (is.numeric(col)) {
      col[is.na(col)] <- NA_real_
    }
    col
  })
  tbl <- gt::gt(df) |>
    gt::sub_missing(missing_text = "\u2014") |>
    gt::cols_align(align = "left") |>
    gt::tab_options(
      table.width = gt::pct(100),
      table.font.size = gt::px(14),
      heading.align = "left",
      column_labels.font.weight = "bold",
      column_labels.border.bottom.color = "grey40",
      table_body.hlines.color = "grey90",
      table.border.top.style = "none",
      table.border.bottom.style = "none"
    )
  if (!is.null(caption)) {
    tbl <- tbl |> gt::tab_header(title = caption)
  }
  tbl
}
