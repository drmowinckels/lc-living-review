#!/usr/bin/env Rscript
source(here::here("R/search.R"))
source(here::here("R/utils.R"))

results <- read.csv("data/pre2010_cbt_to_screen.csv", stringsAsFactors = FALSE)
results$search_topic <- "treatment_cbt_psych"
results$date_added <- as.character(Sys.Date())

relevant_idx <- c(1,3,4,5,6,7,10,12,13,18,19,24,27,28,35,43,50,55,60,65)
study_types <- c("cohort","cohort","RCT","RCT","cohort","RCT","cohort","RCT","RCT",
                 "survey","RCT","cohort","qualitative","RCT","pilot","cohort","cohort","pilot","RCT","cohort")
report_types <- c("patient_reported","patient_reported","clinical","mixed","patient_reported","clinical",
                  "patient_reported","clinical","patient_reported","patient_reported","patient_reported",
                  "patient_reported","patient_reported","patient_reported","patient_reported","patient_reported",
                  "mixed","clinical","patient_reported","patient_reported")
reasons <- c(
  "Pacing self-management case series in CFS",
  "Study of deterioration during CBT for CFS",
  "CBT effect on cognitive impairment in CFS",
  "Cost-effectiveness of CBT and GET for CFS",
  "Adolescent CFS inpatient rehabilitation",
  "Graded exercise vs resistance training adolescent CFS",
  "Therapeutic alliance in CFS treatment",
  "Pain physiology education vs pacing RCT",
  "Guided self-instructions minimal CBT RCT",
  "Patient survey on treatment experiences",
  "Psychiatric disorders effect on CBT outcome",
  "Long-term exercise programme FMS/CFIDS",
  "Focus group beneficial/harmful physical activity",
  "RCT QoL outcomes in CFS rehabilitation",
  "Exercise limits to prevent PEM",
  "CBT group therapy waiting list controlled",
  "CBT case study self-report vs behavioural",
  "Submaximal exercise with vibration",
  "Short-term group therapy RCT",
  "Group CBT pragmatic evaluation"
)

results$llm_relevant <- FALSE
results$llm_category <- "treatment_cbt_psych"
results$llm_study_type <- NA_character_
results$llm_reporting_type <- NA_character_
results$llm_confidence <- "high"
results$llm_reason <- "Excluded: not relevant treatment study"
results$llm_model <- "manual-claude-screening"

for (j in seq_along(relevant_idx)) {
  i <- relevant_idx[j]
  results$llm_relevant[i] <- TRUE
  results$llm_study_type[i] <- study_types[j]
  results$llm_reporting_type[i] <- report_types[j]
  results$llm_reason[i] <- reasons[j]
}

results$study_id <- mapply(generate_study_id, results$authors, results$year)
results$human_verified <- FALSE
results$included <- NA
results$exclusion_reason <- NA_character_
results$study_type <- NA_character_
results$reporting_type <- NA_character_
results$diagnostic_criteria <- NA_character_
results$pem_assessed <- NA
results$data_extracted <- FALSE
results$notes <- NA_character_
results$is_protocol <- FALSE

message("Reading database...")
existing <- read_study_database()
all_cols <- names(existing)
for (col in setdiff(all_cols, names(results))) results[[col]] <- NA
for (col in setdiff(names(results), all_cols)) results[[col]] <- NULL
results <- results[, all_cols]

message("Writing updated database...")
updated <- rbind(existing, results)
write_study_database(updated)

n_relevant <- sum(results$llm_relevant, na.rm = TRUE)
message(sprintf("Added %d studies (%d relevant, %d excluded)", nrow(results), n_relevant, sum(!results$llm_relevant)))
message(sprintf("Database: %d studies", nrow(updated)))
