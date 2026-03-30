#!/usr/bin/env Rscript
source(here::here("R/utils.R"))
source(here::here("R/extraction.R"))

rows <- list(
  # 1. Nijs 2009: Pacing self-management case series (n=7)
  list(study_id="nijs2009", intervention_category="Pacing",
       outcome_domain="symptoms (general)", outcome_measure="CFS Symptom List",
       mean_intervention=NA, sd_intervention=NA, n_intervention=7L,
       mean_comparator=NA, sd_comparator=NA, n_comparator=NA,
       dose="3 weeks, 3 individual sessions",
       pem_assessed=FALSE, adverse_events_reported=FALSE, adverse_events_detail=NA,
       notes="Pre-post: symptom severity and daily functioning improved (p=.043). N=7 case series."),

  # 2. Heins 2010: CBT deterioration study (pooled from 3 RCTs)
  list(study_id="heins2010", intervention_category="CBT",
       outcome_domain="fatigue", outcome_measure="Fatigue deterioration rate",
       mean_intervention=NA, sd_intervention=NA, n_intervention=NA,
       mean_comparator=NA, sd_comparator=NA, n_comparator=NA,
       dose=NA, pem_assessed=FALSE, adverse_events_reported=TRUE,
       adverse_events_detail="Deterioration 2-12% in CBT vs 7-17% in controls; no significant difference",
       notes="Pooled 3 RCTs. CBT not more harmful than no treatment. Deterioration reflects natural symptom variation."),

  # 3. Knoop 2007: CBT effect on cognition (2 RCTs pooled)
  list(study_id="knoop2007", intervention_category="CBT",
       outcome_domain="cognition", outcome_measure="Self-reported cognitive impairment",
       mean_intervention=NA, sd_intervention=NA, n_intervention=NA,
       mean_comparator=NA, sd_comparator=NA, n_comparator=NA,
       dose=NA, pem_assessed=FALSE, adverse_events_reported=FALSE, adverse_events_detail=NA,
       notes="CBT reduced self-reported cognitive impairment vs controls (significant). Neuropsychological test performance did NOT improve."),

  # 4. McCrone 2004: Cost-effectiveness CBT vs GET vs usual care (n=132)
  list(study_id="mccrone2004", intervention_category="CBT",
       outcome_domain="fatigue", outcome_measure="Fatigue (4-point improvement)",
       mean_intervention=NA, sd_intervention=NA, n_intervention=NA,
       mean_comparator=NA, sd_comparator=NA, n_comparator=NA,
       dose=NA, pem_assessed=FALSE, adverse_events_reported=FALSE, adverse_events_detail=NA,
       notes="CBT and GET similar cost-effectiveness. Combined therapy better outcomes than usual care (+booklet). Costs ~149 GBP higher. 81.9% chance cost-effective at 500 GBP/4-point fatigue improvement."),

  # 5. Gordon 2009: Adolescent inpatient rehab (n=16)
  list(study_id="gordon2009", intervention_category="Exercise",
       outcome_domain="physical function", outcome_measure="VO2peak",
       mean_intervention=NA, sd_intervention=NA, n_intervention=16L,
       mean_comparator=NA, sd_comparator=NA, n_comparator=NA,
       dose="4-week inpatient, daily supervised exercise",
       pem_assessed=FALSE, adverse_events_reported=FALSE, adverse_events_detail=NA,
       notes="Pre-post: VO2peak +17% (p=.01), time to fatigue +18% (p=.02), push-ups +70%. Fatigue severity -13% (p=.01), depression -42% (p=.02)."),
  list(study_id="gordon2009", intervention_category="Exercise",
       outcome_domain="fatigue", outcome_measure="Fatigue Severity",
       mean_intervention=NA, sd_intervention=NA, n_intervention=16L,
       mean_comparator=NA, sd_comparator=NA, n_comparator=NA,
       dose="4-week inpatient, daily supervised exercise",
       pem_assessed=FALSE, adverse_events_reported=FALSE, adverse_events_detail=NA,
       notes="Pre-post: fatigue severity improved 13% (p=.01)"),
  list(study_id="gordon2009", intervention_category="Exercise",
       outcome_domain="depression", outcome_measure="Depression index",
       mean_intervention=NA, sd_intervention=NA, n_intervention=16L,
       mean_comparator=NA, sd_comparator=NA, n_comparator=NA,
       dose="4-week inpatient, daily supervised exercise",
       pem_assessed=FALSE, adverse_events_reported=FALSE, adverse_events_detail=NA,
       notes="Pre-post: depression improved 42% (p=.02)"),

  # 6. Gordon 2010: GET vs resistance training adolescent RCT (n=22)
  list(study_id="gordon2010", intervention_category="Graded Exercise Therapy",
       outcome_domain="physical function", outcome_measure="Treadmill time to fatigue",
       mean_intervention=NA, sd_intervention=NA, n_intervention=11L,
       mean_comparator=NA, sd_comparator=NA, n_comparator=11L,
       dose="4 weeks, 5 days/week, 20-40 min aerobic vs 16 resistance exercises",
       pem_assessed=FALSE, adverse_events_reported=FALSE, adverse_events_detail=NA,
       notes="No significant difference between groups. Both improved physical capacity and QoL. Fatigue severity and depression improved only with aerobic training."),
  list(study_id="gordon2010", intervention_category="Graded Exercise Therapy",
       outcome_domain="fatigue", outcome_measure="Fatigue severity",
       mean_intervention=NA, sd_intervention=NA, n_intervention=11L,
       mean_comparator=NA, sd_comparator=NA, n_comparator=11L,
       dose="4 weeks aerobic vs resistance",
       pem_assessed=FALSE, adverse_events_reported=FALSE, adverse_events_detail=NA,
       notes="Fatigue improved with aerobic training only"),

  # 7. Meeus 2010: Pain education vs pacing RCT (n=48)
  list(study_id="meeus2010", intervention_category="Rehabilitation",
       outcome_domain="cognition", outcome_measure="Neurophysiology of Pain Test",
       mean_intervention=NA, sd_intervention=NA, n_intervention=22L,
       mean_comparator=NA, sd_comparator=NA, n_comparator=24L,
       dose="Single 30-min session",
       pem_assessed=FALSE, adverse_events_reported=FALSE, adverse_events_detail=NA,
       notes="Pain education vs pacing education. Education group: better pain understanding (p<.001, d=2.53) and less ruminating (p=.009, d=0.79)."),
  list(study_id="meeus2010", intervention_category="Rehabilitation",
       outcome_domain="pain", outcome_measure="Pain Catastrophizing Scale - ruminating",
       mean_intervention=NA, sd_intervention=NA, n_intervention=22L,
       mean_comparator=NA, sd_comparator=NA, n_comparator=24L,
       dose="Single 30-min session", pem_assessed=FALSE,
       adverse_events_reported=FALSE, adverse_events_detail=NA,
       notes="Pain education reduced ruminating vs pacing (p=.009, d=0.79)"),

  # 8. Knoop 2008: Guided self-instructions RCT (n=171)
  list(study_id="knoop2008", intervention_category="CBT",
       outcome_domain="fatigue", outcome_measure="Fatigue severity",
       mean_intervention=NA, sd_intervention=NA, n_intervention=85L,
       mean_comparator=NA, sd_comparator=NA, n_comparator=86L,
       dose="Self-instructions booklet + email contact",
       pem_assessed=FALSE, adverse_events_reported=FALSE, adverse_events_detail=NA,
       notes="ITT: significant decrease in fatigue and disability. Disability negatively correlated with outcome. Effective for less severe CFS."),
  list(study_id="knoop2008", intervention_category="CBT",
       outcome_domain="physical function", outcome_measure="Disability",
       mean_intervention=NA, sd_intervention=NA, n_intervention=85L,
       mean_comparator=NA, sd_comparator=NA, n_comparator=86L,
       dose="Self-instructions booklet + email contact",
       pem_assessed=FALSE, adverse_events_reported=FALSE, adverse_events_detail=NA,
       notes="Significant decrease in disability after self-instruction vs waiting list"),

  # 9. Bjorkum 2009: Patient survey (n=828)
  list(study_id="bjorkum2009", intervention_category="Pacing",
       outcome_domain="symptoms (general)", outcome_measure="Patient-rated usefulness",
       mean_intervention=NA, sd_intervention=NA, n_intervention=828L,
       mean_comparator=NA, sd_comparator=NA, n_comparator=NA,
       dose=NA, pem_assessed=TRUE, adverse_events_reported=TRUE,
       adverse_events_detail="79% reported graded training worsened health status",
       notes="Survey: pacing useful 96%, rest 97%. CBT useful 57%. GET worsened 79%. Critical patient perspective data."),

  # 10. Prins 2005: Psychiatric disorders and CBT outcome (n=270)
  list(study_id="prins2005", intervention_category="CBT",
       outcome_domain="fatigue", outcome_measure="Fatigue severity",
       mean_intervention=NA, sd_intervention=NA, n_intervention=270L,
       mean_comparator=NA, sd_comparator=NA, n_comparator=NA,
       dose=NA, pem_assessed=FALSE, adverse_events_reported=FALSE, adverse_events_detail=NA,
       notes="Psychiatric disorders (lifetime 50%, current 32%) did NOT affect CBT outcome. No significant differences in fatigue/function between those with/without psychiatric diagnoses."),

  # 11. Karper 2003: Long-term exercise programme (2 groups, 2-3 years)
  list(study_id="karper2003", intervention_category="Exercise",
       outcome_domain="physical function", outcome_measure="Physical fitness composite",
       mean_intervention=NA, sd_intervention=NA, n_intervention=NA,
       mean_comparator=NA, sd_comparator=NA, n_comparator=NA,
       dose="50-70 min/day, 5 days/week, 2-3 years",
       pem_assessed=FALSE, adverse_events_reported=FALSE, adverse_events_detail=NA,
       notes="Pre-post cohort. Both groups improved on physical fitness, psychosocial, and symptom measures over 1 year."),

  # 12. Taylor 2004: Rehabilitation RCT (n=47)
  list(study_id="taylor2004", intervention_category="Rehabilitation",
       outcome_domain="quality of life", outcome_measure="Quality of Life Index",
       mean_intervention=NA, sd_intervention=NA, n_intervention=23L,
       mean_comparator=NA, sd_comparator=NA, n_comparator=24L,
       dose="Integrative consumer-driven rehabilitation programme",
       pem_assessed=FALSE, adverse_events_reported=FALSE, adverse_events_detail=NA,
       notes="Significant condition x time interaction for QoL (d=0.66) and symptom severity (d=0.71)."),
  list(study_id="taylor2004", intervention_category="Rehabilitation",
       outcome_domain="symptoms (general)", outcome_measure="CFS Symptom Rating Scale",
       mean_intervention=NA, sd_intervention=NA, n_intervention=23L,
       mean_comparator=NA, sd_comparator=NA, n_comparator=24L,
       dose="Integrative consumer-driven rehabilitation programme",
       pem_assessed=FALSE, adverse_events_reported=FALSE, adverse_events_detail=NA,
       notes="Significant condition x time for symptom severity (d=0.71)"),

  # 13. Nijs 2008: Exercise limits to prevent PEM (n=24)
  list(study_id="nijs2008", intervention_category="Pacing",
       outcome_domain="pem", outcome_measure="SF-36 subscales + CFS Symptom List",
       mean_intervention=NA, sd_intervention=NA, n_intervention=24L,
       mean_comparator=NA, sd_comparator=NA, n_comparator=NA,
       dose="Single walking test with HR and duration limits",
       pem_assessed=TRUE, adverse_events_reported=TRUE,
       adverse_events_detail="Fatigue increased immediately post-exercise (p=.006) but returned to baseline at 24h. Pain increase retained at 24h (p=.03). 14/24 had clinically meaningful pain change. 6/24 slight health worsening.",
       notes="Exercise limits prevented major health status changes but not short-term symptom increases. Key PEM prevention study."),

  # 14. Bazelmans 2005: CBT group therapy (n=67)
  list(study_id="bazelmans2005", intervention_category="CBT",
       outcome_domain="fatigue", outcome_measure="CIS Fatigue",
       mean_intervention=NA, sd_intervention=NA, n_intervention=31L,
       mean_comparator=NA, sd_comparator=NA, n_comparator=36L,
       dose="12 sessions x 2h over 6 months",
       pem_assessed=FALSE, adverse_events_reported=FALSE, adverse_events_detail=NA,
       notes="Moderate effect on fatigue favouring CBGT. Functional impairment effect opposite to expected. Patients who improved had less complaints at baseline."),

  # 15. Friedberg 2009: CBT case study (n=11)
  list(study_id="friedberg2009", intervention_category="CBT",
       outcome_domain="physical function", outcome_measure="Actigraphy + 6MWT",
       mean_intervention=NA, sd_intervention=NA, n_intervention=11L,
       mean_comparator=NA, sd_comparator=NA, n_comparator=NA,
       dose="6-32 sessions outpatient CBT",
       pem_assessed=FALSE, adverse_events_reported=FALSE, adverse_events_detail=NA,
       notes="9/11 reported improvement. But actigraphy showed increases (3), decreases (4), and no change (2). Self-report improvement did not match objective activity changes."),

  # 16. Saggini 2006: Exercise + vibration pilot (n=10)
  list(study_id="saggini2006", intervention_category="Exercise",
       outcome_domain="physical function", outcome_measure="CYBEX 6000 dynamometer",
       mean_intervention=NA, sd_intervention=NA, n_intervention=10L,
       mean_comparator=NA, sd_comparator=NA, n_comparator=NA,
       dose="6 months submaximal aerobic + vibration (Galileo 2000)",
       pem_assessed=FALSE, adverse_events_reported=FALSE, adverse_events_detail=NA,
       notes="Pre-post: overall improvement in pain thresholds, VAS, and muscle performance. N=10 pilot."),

  # 17. Soderberg 2001: Group therapy RCT (n=14)
  list(study_id="soderberg2001", intervention_category="CBT",
       outcome_domain="quality of life", outcome_measure="Self-rating QoL",
       mean_intervention=NA, sd_intervention=NA, n_intervention=7L,
       mean_comparator=NA, sd_comparator=NA, n_comparator=7L,
       dose="10 sessions x 1.5h/week",
       pem_assessed=FALSE, adverse_events_reported=FALSE, adverse_events_detail=NA,
       notes="Small RCT (n=14). Most valuable aspect: sharing experiences. >50% reported improved psychological wellbeing. Recommends longer treatment period."),

  # 18. Barber 2007: Group CBT pragmatic evaluation (no abstract)
  list(study_id="barber2007", intervention_category="CBT",
       outcome_domain="fatigue", outcome_measure=NA,
       mean_intervention=NA, sd_intervention=NA, n_intervention=NA,
       mean_comparator=NA, sd_comparator=NA, n_comparator=NA,
       dose=NA, pem_assessed=FALSE, adverse_events_reported=FALSE, adverse_events_detail=NA,
       notes="No abstract available. Group CBT pragmatic evaluation.")
)

df <- do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))
df$search_topic <- "treatment_cbt_psych"
df$llm_model <- "manual-claude-extraction"

write_extracted_data(df, "treatment_cbt_psych")
cat(sprintf("Wrote %d effect sizes for %d studies\n", nrow(df), length(unique(df$study_id))))

db <- read_study_database()
for (sid in unique(df$study_id)) {
  idx <- which(db$study_id == sid & db$llm_model == "manual-claude-screening" &
               !is.na(db$llm_model) & !db$data_extracted %in% TRUE)
  if (length(idx) > 0) db$data_extracted[idx[1]] <- TRUE
}
write_study_database(db)

cat("All studies marked as extracted\n")
