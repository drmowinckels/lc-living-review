#!/usr/bin/env Rscript

source(here::here("R/utils.R"))
source(here::here("R/search.R"))
source(here::here("R/screening.R"))
source(here::here("R/extraction.R"))

future::plan(future::multisession, workers = future::availableCores() / 2)
progressr::handlers(global = TRUE)

last_search_date <- function(log_file = here::here("data/search-log.csv")) {
  if (!file.exists(log_file)) {
    return(NULL)
  }
  log <- read.csv(log_file, stringsAsFactors = FALSE)
  if (nrow(log) == 0) {
    return(NULL)
  }
  max(as.Date(log$date))
}

main <- function() {
  cli::cli_h1("Living Review Search Pipeline")
  cli::cli_alert_info("Date: {Sys.Date()}")

  existing <- read_study_database()
  all_cols <- names(existing)

  date_from <- if (nzchar(Sys.getenv("DATE_FROM"))) {
    Sys.getenv("DATE_FROM")
  } else {
    last_date <- last_search_date()
    if (!is.null(last_date)) as.character(last_date) else "2020-01-01"
  }
  has_api_key <- switch(
    llm_provider(),
    claude = nzchar(Sys.getenv("ANTHROPIC_API_KEY")),
    gemini = nzchar(Sys.getenv("GEMINI_API_KEY")),
    mistral = nzchar(Sys.getenv("MISTRAL_API_KEY")),
    github = nzchar(Sys.getenv("GITHUB_TOKEN")),
    FALSE
  )
  if (has_api_key) {
    cli::cli_alert_info("LLM provider: {.val {llm_model_name()}}")
  } else {
    cli::cli_alert_warning(
      "No API key found — screening disabled. Set GEMINI_API_KEY, ANTHROPIC_API_KEY, MISTRAL_API_KEY, or GITHUB_TOKEN."
    )
  }

  topics_env <- Sys.getenv("TOPICS", unset = "")
  active_topics <- if (nzchar(topics_env)) {
    trimws(strsplit(topics_env, ",")[[1]])
  } else {
    names(search_queries)
  }

  if (has_api_key) {
    unscreened <- existing[
      is.na(existing$llm_relevant) &
        !is.na(existing$abstract) &
        existing$abstract != "" &
        existing$search_topic %in% active_topics,
    ]
    if (nrow(unscreened) > 0) {
      cli::cli_h2(
        "Resuming screening for {nrow(unscreened)} unscreened record{?s}"
      )
      for (topic in unique(unscreened$search_topic)) {
        batch <- unscreened[unscreened$search_topic == topic, ]
        cli::cli_alert_info(
          "Screening {nrow(batch)} {.val {topic}} record{?s}..."
        )
        llm_cols <- c(
          "llm_relevant",
          "llm_category",
          "llm_study_type",
          "llm_reporting_type",
          "llm_confidence",
          "llm_reason",
          "llm_model"
        )
        batch_idx <- match(batch$study_id, existing$study_id)

        save_fn <- function(i, r) {
          row_idx <- batch_idx[i]
          existing$llm_relevant[row_idx] <<- if (is.na(r$relevant)) NA else isTRUE(r$relevant)
          existing$llm_category[row_idx] <<- r$categories %||% NA_character_
          existing$llm_study_type[row_idx] <<- r$study_type %||% NA_character_
          existing$llm_reporting_type[row_idx] <<- r$reporting_type %||%
            NA_character_
          existing$llm_confidence[row_idx] <<- r$confidence %||% NA_character_
          existing$llm_reason[row_idx] <<- r$reason %||% NA_character_
          existing$llm_model[row_idx] <<- llm_model_name()
          if (i %% 10 == 0) write_study_database(existing)
        }

        screened <- screen_batch(
          batch[, setdiff(names(batch), llm_cols), drop = FALSE],
          search_topic = topic,
          save_fn = save_fn
        )
        idx <- match(screened$study_id, existing$study_id)
        existing[idx, llm_cols] <- screened[, llm_cols]
        write_study_database(existing)
        n_done <- sum(!is.na(screened$llm_relevant))
        n_rel <- sum(screened$llm_relevant == TRUE, na.rm = TRUE)
        cli::cli_alert_success(
          "{n_done} screened, {n_rel} relevant for {.val {topic}}"
        )
      }
    }
  }

  skip_search <- as.logical(Sys.getenv("SKIP_SEARCH", unset = "FALSE"))
  if (isTRUE(skip_search)) {
    cli::cli_alert_info("SKIP_SEARCH=TRUE — jumping to extraction.")
  }

  cli::cli_alert_info("Searching from: {.val {date_from}}")
  cli::cli_alert_info("Topics: {.val {active_topics}}")

  n_total_retrieved <- 0L
  n_total_duplicates <- 0L
  n_total_new <- 0L
  n_total_screened <- 0L
  n_total_relevant <- 0L

  for (topic in active_topics) {
    if (isTRUE(skip_search)) {
      break
    }
    cli::cli_h2("Topic: {topic}")

    full_query <- build_date_query(
      search_queries[[topic]],
      date_from = date_from
    )
    cursor <- "*"
    pb_ready <- FALSE
    n_topic_retrieved <- 0L
    n_topic_duplicates <- 0L
    n_topic_new <- 0L
    n_topic_screened <- 0L
    n_topic_relevant <- 0L

    repeat {
      page <- search_page(full_query, cursor = cursor)

      if (is.null(page)) {
        cli::cli_alert_warning(
          "API error, stopping pagination for {.val {topic}}"
        )
        break
      }

      if (!pb_ready && page$hit_count > 0) {
        n_pages <- ceiling(page$hit_count / 200)
        cli::cli_alert_info(
          "{page$hit_count} results across {n_pages} page{?s}"
        )
        cli::cli_progress_bar("Pages", total = n_pages)
        pb_ready <- TRUE
      }

      if (nrow(page$results) == 0) {
        break
      }

      results <- page$results
      n_retrieved_page <- nrow(results)
      results$search_topic <- topic
      results$date_added <- Sys.Date()
      results <- deduplicate_studies(results, existing)
      n_topic_retrieved <- n_topic_retrieved + n_retrieved_page
      n_topic_duplicates <- n_topic_duplicates +
        (n_retrieved_page - nrow(results))

      if (nrow(results) > 0) {
        needs_abstract <- !is.na(results$pmid) &
          results$pmid != "" &
          (is.na(results$abstract) | results$abstract == "")
        if (any(needs_abstract)) {
          results$abstract[needs_abstract] <- fetch_abstracts(results$pmid[
            needs_abstract
          ])
        }

        results$llm_relevant <- NA
        results$llm_category <- NA_character_
        results$llm_study_type <- NA_character_
        results$llm_reporting_type <- NA_character_
        results$llm_confidence <- NA_character_
        results$llm_reason <- NA_character_
        results$llm_model <- NA_character_
        results$study_id <- mapply(
          generate_study_id,
          results$authors,
          results$year
        )
        results$human_verified <- FALSE
        results$included <- NA
        results$exclusion_reason <- NA_character_
        results$study_type <- NA_character_
        results$reporting_type <- NA_character_
        results$diagnostic_criteria <- NA_character_
        results$pem_assessed <- NA
        results$data_extracted <- FALSE
        results$notes <- NA_character_

        for (col in setdiff(all_cols, names(results))) {
          results[[col]] <- NA
        }
        results <- results[, all_cols]

        existing <- rbind(existing, results)
        write_study_database(existing)

        if (has_api_key) {
          llm_cols <- c(
            "llm_relevant",
            "llm_category",
            "llm_study_type",
            "llm_reporting_type",
            "llm_confidence",
            "llm_reason",
            "llm_model"
          )
          cli::cli_alert_info("Screening {nrow(results)} new record{?s}...")
          screened <- screen_batch(
            results[, setdiff(names(results), llm_cols), drop = FALSE],
            search_topic = topic
          )
          n <- nrow(existing)
          m <- nrow(results)
          existing[(n - m + 1):n, llm_cols] <- screened[, llm_cols]
          write_study_database(existing)
          results[, llm_cols] <- screened[, llm_cols]
          n_topic_screened <- n_topic_screened + nrow(results)
        }

        n_topic_relevant <- n_topic_relevant +
          sum(results$llm_relevant, na.rm = TRUE)
        n_topic_new <- n_topic_new + nrow(results)
      }

      if (pb_ready) {
        cli::cli_progress_update(
          status = sprintf(
            "%d new, %d dupes",
            nrow(results),
            n_retrieved_page - nrow(results)
          )
        )
      }

      next_cursor <- page$next_cursor
      if (is.null(next_cursor) || next_cursor == cursor) {
        break
      }
      cursor <- next_cursor
      Sys.sleep(0.3)
    }
    if (pb_ready) {
      cli::cli_progress_done()
    }

    log_search(
      topic,
      n_topic_retrieved,
      n_topic_duplicates,
      n_topic_new,
      n_topic_screened,
      n_topic_relevant
    )
    n_total_retrieved <- n_total_retrieved + n_topic_retrieved
    n_total_duplicates <- n_total_duplicates + n_topic_duplicates
    n_total_new <- n_total_new + n_topic_new
    n_total_screened <- n_total_screened + n_topic_screened
    n_total_relevant <- n_total_relevant + n_topic_relevant
    cli::cli_alert_success(
      "{n_topic_new} new ({n_topic_duplicates} dupes), {n_topic_relevant} relevant for {.val {topic}}"
    )
  }

  if (n_total_new == 0 && !isTRUE(skip_search)) {
    cli::cli_alert_warning("No new results found across all topics.")
  }

  existing <- read_study_database()

  n_extracted <- 0L
  if (has_api_key) {
    cli::cli_h2("LLM Data Extraction")
    to_extract <- existing[
      !is.na(existing$llm_relevant) &
        existing$llm_relevant &
        !is.na(existing$abstract) &
        existing$abstract != "" &
        !existing$data_extracted,
    ]
    cli::cli_alert_info(
      "Attempting extraction for {nrow(to_extract)} relevant stud{?y/ies}"
    )

    on_success <- function(study_id) {
      idx <- match(study_id, existing$study_id)
      if (!is.na(idx)) {
        existing$data_extracted[idx] <<- TRUE
        write_study_database(existing)
      }
    }

    for (topic in unique(to_extract$search_topic)) {
      topic_studies <- to_extract[to_extract$search_topic == topic, ]
      cli::cli_alert_info(
        "Extracting {nrow(topic_studies)} {.val {topic}} stud{?y/ies}"
      )
      extracted <- extract_batch(
        topic_studies,
        search_topic = topic,
        on_success = on_success
      )
      n_extracted <- n_extracted + length(extracted)
    }
  }

  cli::cli_alert_success(
    "Pipeline complete. {n_total_new} new, {n_total_relevant} relevant, {n_extracted} extracted."
  )

  writeLines(
    jsonlite::toJSON(
      list(
        date = as.character(Sys.Date()),
        n_retrieved = n_total_retrieved,
        n_duplicates = n_total_duplicates,
        n_added = n_total_new,
        n_screened = n_total_screened,
        n_relevant = n_total_relevant,
        n_extracted = n_extracted
      ),
      auto_unbox = TRUE,
      pretty = TRUE
    ),
    here::here("data/last-run-summary.json")
  )
}

main()
