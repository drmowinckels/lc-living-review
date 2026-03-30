library(targets)

tar_option_set(
  packages = c("arrow", "dplyr", "metafor", "ggplot2", "purrr",
               "glue", "cli", "gt", "scales"),
  format = "rds"
)

tar_source("R/utils.R")
tar_source("R/meta-analysis.R")
tar_source("R/plots.R")

run_domain_metas <- function(forest_df, group_col = "intervention_category") {
  if (nrow(forest_df) < 2) return(list(overall = NULL, subgroups = list()))

  overall <- tryCatch(run_meta(forest_df), error = function(e) NULL)

  subgroups <- list()
  if (group_col %in% names(forest_df)) {
    groups <- unique(forest_df[[group_col]][!is.na(forest_df[[group_col]])])
    for (g in groups) {
      sub <- forest_df[!is.na(forest_df[[group_col]]) & forest_df[[group_col]] == g, ]
      if (nrow(sub) >= 2) {
        subgroups[[g]] <- tryCatch(run_meta(sub), error = function(e) NULL)
      }
    }
  }

  list(overall = overall, subgroups = subgroups)
}

run_domain_pub_bias <- function(metas) {
  if (is.null(metas$overall) || metas$overall$k < 10) return(NULL)
  tryCatch(publication_bias(metas$overall), error = function(e) NULL)
}

list(
  # --- Data inputs (file targets — rebuild when parquets change) ---
  tar_target(studies_file, "data/studies.parquet", format = "file"),
  tar_target(cbt_file, "data/extracted/treatment_cbt_psych.parquet", format = "file"),
  tar_target(bio_file, "data/extracted/treatment_biomedical.parquet", format = "file"),
  tar_target(quality_file, "data/extracted/quality_ratings.parquet", format = "file"),
  tar_target(grade_file, "data/extracted/grade.parquet", format = "file"),
  tar_target(outcomes_file, "data/extracted/outcomes.parquet", format = "file"),
  tar_target(biomarkers_file, "data/extracted/biomarkers.parquet", format = "file"),

  # --- Read data ---
  tar_target(studies, as.data.frame(arrow::read_parquet(studies_file))),
  tar_target(cbt_effects, as.data.frame(arrow::read_parquet(cbt_file))),
  tar_target(bio_effects, as.data.frame(arrow::read_parquet(bio_file))),
  tar_target(quality, {
    if (file.exists(quality_file)) as.data.frame(arrow::read_parquet(quality_file))
    else data.frame()
  }),
  tar_target(grade, {
    if (file.exists(grade_file)) as.data.frame(arrow::read_parquet(grade_file))
    else data.frame()
  }),
  tar_target(outcomes, {
    if (file.exists(outcomes_file)) as.data.frame(arrow::read_parquet(outcomes_file))
    else data.frame()
  }),
  tar_target(biomarkers, {
    if (file.exists(biomarkers_file)) as.data.frame(arrow::read_parquet(biomarkers_file))
    else data.frame()
  }),

  # --- Prepare forest data (direction harmonisation, type filtering) ---
  tar_target(cbt_forest, prepare_forest_data(cbt_effects)),
  tar_target(bio_forest, prepare_forest_data(bio_effects)),

  # --- Prevention data (ratio-scale, separate from SMD forest) ---
  tar_target(bio_prevention, {
    if (!"effect_size_type" %in% names(bio_effects)) return(data.frame())
    bio_effects |>
      dplyr::filter(
        effect_size_type == "ratio",
        !is.na(effect_size), !is.na(ci_lower), !is.na(ci_upper)
      ) |>
      dplyr::mutate(
        study_label = dplyr::if_else(
          !is.na(outcome_measure),
          paste0(study_id, ": ", outcome_measure),
          study_id
        ),
        intervention_category = dplyr::coalesce(intervention_category, intervention)
      )
  }),

  # --- Meta-analyses ---
  tar_target(cbt_metas, run_domain_metas(cbt_forest, "intervention_category")),
  tar_target(bio_metas, run_domain_metas(bio_forest, "intervention_category")),
  tar_target(cbt_metas_domain, run_domain_metas(cbt_forest, "outcome_domain")),
  tar_target(bio_metas_domain, run_domain_metas(bio_forest, "outcome_domain")),

  # --- Publication bias ---
  tar_target(cbt_pub_bias, run_domain_pub_bias(cbt_metas)),
  tar_target(bio_pub_bias, run_domain_pub_bias(bio_metas)),

  # --- Study counts for badges ---
  tar_target(cbt_study_counts, {
    topic <- studies |>
      dplyr::filter(!is.na(search_topic), search_topic == "treatment_cbt_psych",
                    !is.na(llm_relevant), llm_relevant)
    list(
      n_results = nrow(topic) - sum(topic$is_protocol %||% FALSE, na.rm = TRUE),
      n_rct = sum(grepl("RCT|random", topic$llm_study_type %||% "", ignore.case = TRUE) &
                    !topic$is_protocol, na.rm = TRUE),
      n_protocols = sum(topic$is_protocol %||% FALSE, na.rm = TRUE)
    )
  }),
  tar_target(bio_study_counts, {
    topic <- studies |>
      dplyr::filter(!is.na(search_topic), search_topic == "treatment_biomedical",
                    !is.na(llm_relevant), llm_relevant)
    list(
      n_results = nrow(topic) - sum(topic$is_protocol %||% FALSE, na.rm = TRUE),
      n_rct = sum(grepl("RCT|random", topic$llm_study_type %||% "", ignore.case = TRUE) &
                    !topic$is_protocol, na.rm = TRUE),
      n_protocols = sum(topic$is_protocol %||% FALSE, na.rm = TRUE)
    )
  }),

  # --- Render the Quarto site ---
  # tar_quarto inspects chunk options before execution, which breaks
  # !expr guards like `eval: !expr has_data`. Use a plain target
  # that shells out to quarto render instead.
  tar_target(site, {
    list(studies, cbt_forest, bio_forest, bio_prevention, quality, grade,
         outcomes, biomarkers, cbt_study_counts, bio_study_counts)
    status <- system2("quarto", "render", stdout = TRUE, stderr = TRUE)
    if (!file.exists("_site/index.html")) stop("quarto render failed:\n", paste(status, collapse = "\n"))
    file.path("_site", "index.html")
  }, format = "file")
)
