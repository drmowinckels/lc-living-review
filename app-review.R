library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(here)

source(here("R/utils.R"))
source(here("R/extraction.R"))

ui <- page_sidebar(
  title = "Study Review Tool",
  theme = bs_theme(bootswatch = "flatly"),

  sidebar = sidebar(
    width = 350,
    title = "Filters",
    selectInput(
      "filter_status", "Review Status",
      choices = c(
        "All" = "all",
        "Needs review (LLM relevant)" = "llm_relevant",
        "Needs review (unscreened)" = "unscreened",
        "Human verified" = "verified",
        "Included" = "included",
        "Excluded" = "excluded"
      ),
      selected = "llm_relevant"
    ),
    selectInput(
      "filter_topic", "Topic",
      choices = c("All" = "all"),
      selected = "all"
    ),
    hr(),
    actionButton("save_all", "Save Database", class = "btn-success w-100"),
    hr(),
    verbatimTextOutput("db_summary")
  ),

  navset_card_tab(
    nav_panel(
      "Review",
      layout_columns(
        col_widths = c(8, 4),

        card(
          card_header("Study Details"),
          uiOutput("study_details")
        ),

        card(
          card_header("Decision"),
          radioButtons(
            "decision", "Include?",
            choices = c("Pending" = "pending", "Include" = "include", "Exclude" = "exclude"),
            selected = "pending"
          ),
          conditionalPanel(
            condition = "input.decision == 'exclude'",
            selectInput(
              "exclusion_reason", "Exclusion Reason",
              choices = c(
                "",
                "Not original research",
                "Wrong population",
                "Wrong outcome",
                "Case report / n < 10",
                "Acute COVID only",
                "Animal study",
                "Duplicate",
                "Language",
                "Other"
              )
            )
          ),
          hr(),
          selectInput(
            "study_type_input", "Study Type",
            choices = c(
              "" = "",
              "RCT", "Non-randomised trial", "Prospective cohort",
              "Retrospective cohort", "Cross-sectional", "Case-control",
              "Survey", "Qualitative", "Systematic review", "Meta-analysis"
            )
          ),
          selectInput(
            "reporting_type_input", "Reporting Type",
            choices = c("" = "", "Clinical" = "clinical", "Patient-reported" = "patient_reported", "Mixed" = "mixed")
          ),
          selectInput(
            "diagnostic_criteria_input", "Diagnostic Criteria",
            choices = c(
              "" = "",
              "WHO Long COVID", "CCC (Canada)", "Fukuda",
              "IOM/SEID", "ICC", "Oxford",
              "Self-reported", "Clinical diagnosis", "Other", "Not specified"
            )
          ),
          checkboxInput("pem_assessed_input", "PEM Assessed?", value = FALSE),
          textAreaInput("notes_input", "Notes", rows = 3),
          hr(),
          layout_columns(
            col_widths = c(6, 6),
            actionButton("save_decision", "Save & Next", class = "btn-primary w-100"),
            actionButton("skip_study", "Skip", class = "btn-secondary w-100")
          )
        )
      ),
      hr(),
      textOutput("progress_text")
    ),

    nav_panel(
      "Extract Data",
      layout_columns(
        col_widths = c(5, 7),

        card(
          card_header("Included Studies (not yet extracted)"),
          DTOutput("extract_study_list"),
          hr(),
          textOutput("extract_progress")
        ),

        card(
          card_header(
            class = "d-flex justify-content-between align-items-center",
            "Extraction",
            actionButton("run_extraction", "Run LLM Extraction", class = "btn-warning btn-sm")
          ),
          uiOutput("extract_study_info"),
          hr(),
          uiOutput("extraction_result_ui"),
          hr(),
          layout_columns(
            col_widths = c(6, 6),
            actionButton("accept_extraction", "Accept & Save", class = "btn-success w-100"),
            actionButton("reject_extraction", "Reject", class = "btn-danger w-100")
          )
        )
      )
    ),

    nav_panel(
      "Action Queue",
      layout_columns(
        col_widths = c(4, 8),

        card(
          card_header("Summary"),
          uiOutput("queue_summary"),
          hr(),
          selectInput(
            "queue_filter", "Show",
            choices = c(
              "All actions" = "all",
              "Needs manual full-text download" = "needs manual full-text download",
              "Needs extraction review (high % missing)" = "needs extraction review (high % missing)",
              "Needs extraction verification" = "needs extraction verification",
              "Needs human screening verification" = "needs human screening verification",
              "Protocol (low priority)" = "protocol (low priority)"
            ),
            selected = "all"
          )
        ),

        card(
          card_header(
            class = "d-flex justify-content-between align-items-center",
            "Priority Queue",
            downloadButton("download_queue", "Export CSV", class = "btn-outline-secondary btn-sm")
          ),
          DTOutput("queue_table")
        )
      )
    ),

    nav_panel(
      "Browse All",
      DTOutput("browse_table")
    )
  )
)

server <- function(input, output, session) {
  db <- reactiveVal(read_study_database())
  current_idx <- reactiveVal(1)
  unsaved_changes <- reactiveVal(FALSE)

  observe({
    topics <- unique(db()$search_topic)
    topics <- topics[!is.na(topics)]
    choices <- c("All" = "all", setNames(topics, gsub("_", " ", topics) |> tools::toTitleCase()))
    updateSelectInput(session, "filter_topic", choices = choices)
  })

  filtered_indices <- reactive({
    studies <- db()
    idx <- seq_len(nrow(studies))

    if (input$filter_topic != "all") {
      idx <- idx[studies$search_topic[idx] == input$filter_topic]
    }

    switch(input$filter_status,
      "llm_relevant" = idx[!studies$human_verified[idx] %in% TRUE &
                           studies$llm_relevant[idx] %in% TRUE],
      "unscreened" = idx[is.na(studies$llm_relevant[idx]) & !studies$human_verified[idx] %in% TRUE],
      "verified" = idx[studies$human_verified[idx] %in% TRUE],
      "included" = idx[studies$included[idx] %in% TRUE],
      "excluded" = idx[studies$human_verified[idx] %in% TRUE & !studies$included[idx] %in% TRUE],
      idx
    )
  })

  current_study <- reactive({
    indices <- filtered_indices()
    ci <- current_idx()
    if (length(indices) == 0 || ci > length(indices)) return(NULL)
    db()[indices[ci], ]
  })

  observe({
    study <- current_study()
    if (is.null(study)) return()

    if (study$human_verified %in% TRUE) {
      sel <- if (study$included %in% TRUE) "include" else "exclude"
    } else {
      sel <- "pending"
    }
    updateRadioButtons(session, "decision", selected = sel)

    updateSelectInput(session, "exclusion_reason",
                      selected = study$exclusion_reason %||% "")
    updateSelectInput(session, "study_type_input",
                      selected = study$study_type %||% "")
    updateSelectInput(session, "reporting_type_input",
                      selected = study$reporting_type %||% "")
    updateSelectInput(session, "diagnostic_criteria_input",
                      selected = study$diagnostic_criteria %||% "")
    updateCheckboxInput(session, "pem_assessed_input",
                        value = isTRUE(study$pem_assessed))
    updateTextAreaInput(session, "notes_input",
                        value = study$notes %||% "")
  })

  output$study_details <- renderUI({
    study <- current_study()
    if (is.null(study)) {
      return(tags$p(tags$em("No studies matching current filters."), class = "text-muted"))
    }

    doi_url <- if (!is.na(study$doi) && study$doi != "") {
      sprintf("https://doi.org/%s", study$doi)
    } else {
      NULL
    }

    llm_badge <- if (!is.na(study$llm_relevant)) {
      if (study$llm_relevant) {
        tags$span("LLM: Relevant", class = "badge bg-success")
      } else {
        tags$span("LLM: Excluded", class = "badge bg-danger")
      }
    }

    abstract_text <- if (!is.na(study$abstract) && study$abstract != "") {
      study$abstract
    } else {
      tags$em("No abstract available", class = "text-muted")
    }

    tagList(
      tags$h4(study$title),
      tags$p(
        tags$strong(study$authors),
        tags$br(),
        tags$em(study$journal), " (", study$year, ")"
      ),
      if (!is.null(doi_url)) {
        tags$p(tags$a(href = doi_url, doi_url, target = "_blank"))
      },
      tags$p(
        llm_badge,
        if (!is.na(study$llm_study_type) && study$llm_study_type != "") {
          tags$span(paste("LLM type:", study$llm_study_type), class = "badge bg-info ms-1")
        },
        if (!is.na(study$llm_reporting_type) && study$llm_reporting_type != "") {
          tags$span(paste("LLM reporting:", study$llm_reporting_type), class = "badge bg-info ms-1")
        }
      ),
      tags$p(
        tags$span(paste("Topic:", gsub("_", " ", study$search_topic) |> tools::toTitleCase()),
                  class = "badge bg-secondary"),
        tags$span(paste("Added:", study$date_added), class = "badge bg-light text-dark ms-1")
      ),
      tags$hr(),
      tags$h5("Abstract"),
      tags$div(abstract_text, style = "max-height: 400px; overflow-y: auto; line-height: 1.6;")
    )
  })

  observeEvent(input$save_decision, {
    indices <- filtered_indices()
    ci <- current_idx()
    if (length(indices) == 0 || ci > length(indices)) return()

    row_idx <- indices[ci]
    studies <- db()

    studies$human_verified[row_idx] <- TRUE
    studies$included[row_idx] <- input$decision == "include"
    studies$exclusion_reason[row_idx] <- if (input$decision == "exclude") {
      input$exclusion_reason
    } else {
      NA_character_
    }
    studies$study_type[row_idx] <- if (input$study_type_input != "") {
      input$study_type_input
    } else {
      NA_character_
    }
    studies$reporting_type[row_idx] <- if (input$reporting_type_input != "") {
      input$reporting_type_input
    } else {
      NA_character_
    }
    studies$diagnostic_criteria[row_idx] <- if (input$diagnostic_criteria_input != "") {
      input$diagnostic_criteria_input
    } else {
      NA_character_
    }
    studies$pem_assessed[row_idx] <- input$pem_assessed_input
    studies$notes[row_idx] <- if (input$notes_input != "") {
      input$notes_input
    } else {
      NA_character_
    }

    db(studies)
    unsaved_changes(TRUE)

    new_indices <- filtered_indices()
    if (ci <= length(new_indices)) {
      current_idx(ci)
    } else if (length(new_indices) > 0) {
      current_idx(1)
    }
  })

  observeEvent(input$skip_study, {
    indices <- filtered_indices()
    ci <- current_idx()
    if (ci < length(indices)) {
      current_idx(ci + 1)
    } else {
      current_idx(1)
    }
  })

  observeEvent(input$save_all, {
    write_study_database(db())
    unsaved_changes(FALSE)
    showNotification("Database saved.", type = "message")
  })

  output$progress_text <- renderText({
    indices <- filtered_indices()
    ci <- current_idx()
    total <- length(indices)
    if (total == 0) return("No studies to review.")

    unsaved <- if (unsaved_changes()) " (unsaved changes)" else ""
    sprintf("Study %d of %d in current filter%s", min(ci, total), total, unsaved)
  })

  output$db_summary <- renderText({
    studies <- db()
    n <- nrow(studies)
    n_verified <- sum(studies$human_verified %in% TRUE)
    n_included <- sum(studies$included %in% TRUE)
    n_excluded <- sum(studies$human_verified %in% TRUE & !studies$included %in% TRUE)
    n_llm_relevant <- sum(studies$llm_relevant %in% TRUE & !studies$human_verified %in% TRUE)
    n_unscreened <- sum(is.na(studies$llm_relevant) & !studies$human_verified %in% TRUE)

    sprintf(
      "Database: %d studies\n\nReviewed: %d\n  Included: %d\n  Excluded: %d\n\nPending:\n  LLM relevant: %d\n  Unscreened: %d",
      n, n_verified, n_included, n_excluded, n_llm_relevant, n_unscreened
    )
  })

  # --- Extract Data tab ---
  extract_selected_row <- reactiveVal(NULL)
  extraction_data <- reactiveVal(NULL)

  extractable_studies <- reactive({
    studies <- db()
    which(studies$included %in% TRUE & !studies$data_extracted %in% TRUE)
  })

  output$extract_study_list <- renderDT({
    studies <- db()
    idx <- extractable_studies()
    if (length(idx) == 0) {
      return(datatable(data.frame(Message = "All included studies have been extracted."),
                       rownames = FALSE, options = list(dom = "t")))
    }
    display <- data.frame(
      row_idx = idx,
      year = studies$year[idx],
      title = ifelse(nchar(studies$title[idx]) > 60,
                     paste0(substr(studies$title[idx], 1, 57), "..."),
                     studies$title[idx]),
      topic = gsub("_", " ", studies$search_topic[idx]) |> tools::toTitleCase(),
      type = studies$study_type[idx] %||% "",
      stringsAsFactors = FALSE
    )
    datatable(display, selection = "single", rownames = FALSE,
              options = list(pageLength = 15, dom = "lftp", scrollX = TRUE))
  })

  observeEvent(input$extract_study_list_rows_selected, {
    sel <- input$extract_study_list_rows_selected
    if (is.null(sel) || length(sel) == 0) return()
    idx <- extractable_studies()
    extract_selected_row(idx[sel])
    extraction_data(NULL)
  })

  output$extract_study_info <- renderUI({
    row <- extract_selected_row()
    if (is.null(row)) return(tags$p(tags$em("Select a study from the list."), class = "text-muted"))

    study <- db()[row, ]
    doi_url <- if (!is.na(study$doi) && study$doi != "") {
      sprintf("https://doi.org/%s", study$doi)
    }

    tagList(
      tags$h5(study$title),
      tags$p(tags$strong(study$authors), tags$br(),
             tags$em(study$journal), " (", study$year, ")"),
      if (!is.null(doi_url)) tags$p(tags$a(href = doi_url, doi_url, target = "_blank")),
      tags$p(
        tags$span(paste("Topic:", gsub("_", " ", study$search_topic) |> tools::toTitleCase()),
                  class = "badge bg-secondary"),
        tags$span(paste("Type:", study$study_type %||% "unknown"), class = "badge bg-info ms-1")
      )
    )
  })

  observeEvent(input$run_extraction, {
    row <- extract_selected_row()
    if (is.null(row)) {
      showNotification("Select a study first.", type = "warning")
      return()
    }

    study <- db()[row, ]
    topic <- study$search_topic

    if (!topic %in% names(.extraction_schemas)) {
      showNotification(
        paste("No extraction schema for topic:", topic),
        type = "error"
      )
      return()
    }

    showNotification("Running LLM extraction...", type = "message", duration = NULL, id = "extracting")

    prompt <- extraction_prompt(study$title, study$abstract, topic)
    response <- call_llm(prompt, max_tokens = 4000)
    parsed <- parse_extraction_response(response)

    removeNotification("extracting")

    if (is.null(parsed) || length(parsed) == 0) {
      showNotification("Extraction failed or returned empty results.", type = "error")
      return()
    }

    result_df <- tryCatch(
      dplyr::bind_rows(lapply(parsed, as.data.frame, stringsAsFactors = FALSE)),
      error = function(e) NULL
    )

    if (is.null(result_df)) {
      showNotification("Could not parse extraction into table.", type = "error")
      return()
    }

    result_df$study_id <- study$study_id
    extraction_data(result_df)
    showNotification(sprintf("Extracted %d outcome rows.", nrow(result_df)), type = "message")
  })

  output$extraction_result_ui <- renderUI({
    ext <- extraction_data()
    if (is.null(ext)) return(tags$p(tags$em("No extraction results yet."), class = "text-muted"))
    DTOutput("extraction_table")
  })

  output$extraction_table <- renderDT({
    ext <- extraction_data()
    req(ext)
    datatable(ext, rownames = FALSE, editable = TRUE,
              options = list(scrollX = TRUE, pageLength = 10, dom = "t"))
  })

  observeEvent(input$extraction_table_cell_edit, {
    info <- input$extraction_table_cell_edit
    ext <- extraction_data()
    ext[info$row, info$col + 1] <- DT::coerceValue(info$value, ext[info$row, info$col + 1])
    extraction_data(ext)
  })

  observeEvent(input$accept_extraction, {
    ext <- extraction_data()
    row <- extract_selected_row()
    if (is.null(ext) || is.null(row)) {
      showNotification("Nothing to save.", type = "warning")
      return()
    }

    study <- db()[row, ]
    topic <- study$search_topic
    domain_path <- here::here("data", "extracted", paste0(topic, ".parquet"))

    existing <- tryCatch(read_extracted_data(topic), error = function(e) data.frame())
    updated <- dplyr::bind_rows(existing, ext)

    .check_arrow()
    arrow::write_parquet(updated, domain_path)

    studies <- db()
    studies$data_extracted[row] <- TRUE
    db(studies)
    unsaved_changes(TRUE)

    extraction_data(NULL)
    extract_selected_row(NULL)
    showNotification("Extraction saved.", type = "message")
  })

  observeEvent(input$reject_extraction, {
    extraction_data(NULL)
    showNotification("Extraction discarded.", type = "message")
  })

  output$extract_progress <- renderText({
    studies <- db()
    n_included <- sum(studies$included %in% TRUE)
    n_extracted <- sum(studies$data_extracted %in% TRUE)
    sprintf("%d / %d included studies extracted", n_extracted, n_included)
  })

  # --- Action Queue tab ---
  priority_data <- reactive({
    db()
    compute_review_priority(db = db())
  })

  output$queue_summary <- renderUI({
    pq <- priority_data()
    counts <- table(pq$action)
    items <- lapply(names(counts), function(a) {
      col <- switch(a,
        "needs manual full-text download" = "warning",
        "needs extraction review (high % missing)" = "danger",
        "needs extraction verification" = "info",
        "needs human screening verification" = "primary",
        "protocol (low priority)" = "secondary",
        "light"
      )
      tags$span(
        sprintf("%s: %d", tools::toTitleCase(a), counts[a]),
        class = sprintf("badge bg-%s d-block mb-1", col)
      )
    })
    tagList(
      tags$p(tags$strong(sprintf("%d studies in queue", nrow(pq)))),
      tags$div(items)
    )
  })

  output$queue_table <- renderDT({
    pq <- priority_data()
    if (input$queue_filter != "all") {
      pq <- pq[pq$action == input$queue_filter, ]
    }
    display <- pq |>
      transmute(
        Priority = priority_score,
        ID = study_id,
        Type = study_type,
        Topic = gsub("_", " ", search_topic) |> tools::toTitleCase(),
        Action = action,
        Missing = ifelse(is.na(extraction_na_pct), "", sprintf("%.0f%%", extraction_na_pct * 100)),
        FT = ifelse(has_fulltext, "\u2713", "\u2717"),
        Verified = ifelse(human_verified, "\u2713", ""),
        Title = ifelse(nchar(title) > 60, paste0(substr(title, 1, 57), "..."), title)
      )
    datatable(
      display,
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 25, scrollX = TRUE, order = list(list(0, "desc"))),
      escape = FALSE
    ) |>
      formatStyle("Priority", backgroundColor = styleInterval(
        c(30, 60, 80),
        c("#e8f5e9", "#fff3e0", "#fde8e8", "#f8d7da")
      )) |>
      formatStyle("Action", fontWeight = "bold")
  })

  output$download_queue <- downloadHandler(
    filename = function() paste0("review-queue-", Sys.Date(), ".csv"),
    content = function(file) {
      pq <- priority_data()
      if (input$queue_filter != "all") {
        pq <- pq[pq$action == input$queue_filter, ]
      }
      write.csv(pq, file, row.names = FALSE)
    }
  )

  output$browse_table <- renderDT({
    studies <- db()
    display <- studies |>
      filter(!is.na(doi) & doi != "") |>
      transmute(
        year,
        title = ifelse(
          nchar(title) > 80,
          paste0(substr(title, 1, 77), "..."),
          title
        ),
        first_author = sub(",.*", "", authors) |> sub(" .*", "", x = _),
        journal,
        doi = sprintf('<a href="https://doi.org/%s" target="_blank">link</a>', doi),
        topic = gsub("_", " ", search_topic) |> tools::toTitleCase(),
        status = case_when(
          data_extracted ~ "Extracted",
          included %in% TRUE ~ "Included",
          human_verified %in% TRUE & !included %in% TRUE ~ "Excluded",
          llm_relevant %in% TRUE ~ "LLM relevant",
          !is.na(llm_relevant) & !llm_relevant ~ "LLM excluded",
          .default = "Unscreened"
        ),
        added = date_added
      )

    datatable(
      display,
      escape = FALSE,
      filter = "top",
      options = list(pageLength = 50, scrollX = TRUE),
      rownames = FALSE
    )
  })
}

shinyApp(ui, server)
