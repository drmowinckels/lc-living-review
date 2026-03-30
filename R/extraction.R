.null_str <- list("string", "null")
.null_num <- list("number", "null")
.null_int <- list("integer", "null")

.outcome_domain_enum <- list(
  "fatigue",
  "physical function",
  "quality of life",
  "depression",
  "anxiety",
  "cognition",
  "sleep",
  "pain",
  "respiratory",
  "cardiovascular",
  "pem",
  "symptoms (general)",
  "recovery",
  "prevalence",
  "mortality",
  "self-efficacy",
  "gastrointestinal",
  "autonomic",
  "neurological",
  "biomarker",
  "safety",
  "work and social",
  "other"
)

.outcome_domain_map <- c(
  "fatigue severity"                    = "fatigue",
  "mental fatigue"                      = "fatigue",
  "physical fatigue"                    = "fatigue",
  "cognitive fatigue"                   = "fatigue",
  "chronic fatigue"                     = "fatigue",
  "general fatigue"                     = "fatigue",
  "fatigue (multidimensional)"          = "fatigue",
  "fatigue (total)"                     = "fatigue",
  "exhaustion"                          = "fatigue",
  "psychosocial fatigue"                = "fatigue",
  "cognitive state fatigue"             = "fatigue",
  "physical fatigability"               = "fatigue",
  "fatigue management"                  = "fatigue",
  "fatigue management and coping"       = "fatigue",
  "fatigue and total symptom burden"    = "fatigue",
  "energy"                              = "fatigue",

  "function"                            = "physical function",
  "physical functioning"                = "physical function",
  "physical performance"                = "physical function",
  "functional performance"              = "physical function",
  "functional capacity"                 = "physical function",
  "functional status"                   = "physical function",
  "functional impairment"               = "physical function",
  "functioning"                         = "physical function",
  "daily functioning"                   = "physical function",
  "exercise capacity"                   = "physical function",
  "exercise tolerance"                  = "physical function",
  "physical activity"                   = "physical function",
  "physical ability"                    = "physical function",
  "physical capacity"                   = "physical function",
  "activities of daily living"          = "physical function",
  "activity/participation"              = "physical function",
  "body composition"                    = "physical function",
  "muscular endurance"                  = "physical function",
  "muscle strength"                     = "physical function",
  "grip strength"                       = "physical function",
  "lower limb strength"                 = "physical function",
  "physical strength"                   = "physical function",
  "strength"                            = "physical function",
  "balance"                             = "physical function",
  "cardiopulmonary fitness"             = "physical function",
  "cardiorespiratory fitness"           = "physical function",
  "cardiovascular fitness"              = "physical function",
  "physical performance capacity"       = "physical function",
  "maximum power output"                = "physical function",
  "physical health"                     = "physical function",
  "sport motivation"                    = "physical function",
  "participation"                       = "physical function",
  "daily activities and self-management" = "physical function",
  "routine activities"                  = "physical function",
  "participation in routine activities" = "physical function",
  "health and functioning"              = "physical function",
  "functional health"                   = "physical function",
  "cognitive/functional independence"   = "physical function",
  "symptoms and function"               = "physical function",
  "rehabilitation"                      = "physical function",

  "qol"                                 = "quality of life",
  "health-related quality of life"      = "quality of life",
  "respiratory quality of life"         = "quality of life",
  "well-being"                          = "quality of life",
  "health status"                       = "quality of life",
  "general health"                      = "quality of life",
  "overall health"                      = "quality of life",
  "health utility"                      = "quality of life",
  "health and well-being"               = "quality of life",
  "mental and physical health"          = "quality of life",
  "mental wellbeing and physical health" = "quality of life",
  "positive mental well-being"          = "quality of life",
  "quality of life and questionnaires"  = "quality of life",
  "satisfaction with performance"       = "quality of life",

  "anxiety and depression"              = "anxiety",
  "anxiety/depression"                  = "anxiety",

  "cognitive function"                  = "cognition",
  "cognitive"                           = "cognition",
  "cognitive impairment"                = "cognition",
  "brain fog"                           = "cognition",
  "cognitive problems"                  = "cognition",
  "cognitive complaints"                = "cognition",
  "cognitive complaints impact on participation" = "cognition",
  "cognitive control"                   = "cognition",
  "processing speed"                    = "cognition",
  "attention"                           = "cognition",
  "sustained attention"                 = "cognition",
  "memory disorders"                    = "cognition",
  "neurocognitive function"             = "cognition",
  "subjective cognitive complaints"     = "cognition",
  "task-switching"                      = "cognition",

  "sleep quality"                       = "sleep",
  "insomnia"                            = "sleep",
  "sleep disorders"                     = "sleep",

  "chronic pain"                        = "pain",
  "pain intensity"                      = "pain",
  "pain interference"                   = "pain",
  "pain threshold"                      = "pain",

  "dyspnea"                             = "respiratory",
  "dyspnoea"                            = "respiratory",
  "pulmonary function"                  = "respiratory",
  "lung function"                       = "respiratory",
  "respiratory symptoms"                = "respiratory",
  "respiratory muscle strength"         = "respiratory",
  "respiratory function"                = "respiratory",
  "respiratory muscle function"         = "respiratory",
  "inspiratory muscle strength"         = "respiratory",
  "breathing pattern"                   = "respiratory",
  "pulmonary"                           = "respiratory",
  "pulmonary imaging"                   = "respiratory",
  "interstitial lung abnormalities"     = "respiratory",
  "voice and speech"                    = "respiratory",

  "cardiac"                             = "cardiovascular",
  "cardiac function"                    = "cardiovascular",
  "heart rate variability"              = "cardiovascular",
  "cardiovascular outcomes"             = "cardiovascular",
  "orthostatic cardiovascular response" = "cardiovascular",
  "deep venous thrombosis"              = "cardiovascular",
  "pericardial effusion"                = "cardiovascular",

  "post-exertional malaise"             = "pem",
  "post-exertional symptom exacerbation" = "pem",

  "symptoms"                            = "symptoms (general)",
  "somatic symptoms"                    = "symptoms (general)",
  "symptom severity"                    = "symptoms (general)",
  "multi-symptom"                       = "symptoms (general)",
  "multiple symptoms"                   = "symptoms (general)",
  "symptom burden"                      = "symptoms (general)",
  "symptom count"                       = "symptoms (general)",
  "symptom reduction"                   = "symptoms (general)",
  "symptom improvement"                 = "symptoms (general)",
  "symptom resolution"                  = "symptoms (general)",
  "symptom frequency"                   = "symptoms (general)",
  "symptom intensity"                   = "symptoms (general)",
  "symptom interference"                = "symptoms (general)",
  "overall symptoms"                    = "symptoms (general)",
  "somatic symptom severity"            = "symptoms (general)",
  "long covid symptom count"            = "symptoms (general)",
  "long covid symptom severity"         = "symptoms (general)",
  "long covid symptoms"                 = "symptoms (general)",
  "overall improvement"                 = "symptoms (general)",
  "global impression"                   = "symptoms (general)",
  "global change"                       = "symptoms (general)",
  "clinical response"                   = "symptoms (general)",
  "treatment response"                  = "symptoms (general)",
  "self-rated improvement"              = "symptoms (general)",
  "small fiber neuropathy symptoms"     = "symptoms (general)",

  "long covid incidence"                = "prevalence",
  "pasc prevalence"                     = "prevalence",
  "pasc incidence"                      = "prevalence",
  "long covid risk"                     = "prevalence",
  "long covid diagnosis"                = "prevalence",
  "long covid status"                   = "prevalence",
  "long covid prevalence"               = "prevalence",
  "long covid"                          = "prevalence",
  "long covid (overall)"                = "prevalence",
  "pasc (long covid)"                   = "prevalence",
  "pasc status"                         = "prevalence",
  "global pasc"                         = "prevalence",
  "global pasc severity"                = "prevalence",
  "me/cfs diagnosis"                    = "prevalence",
  "post-acute sequelae (overall)"       = "prevalence",

  "death"                               = "mortality",
  "hospitalization"                     = "mortality",
  "hospital admission"                  = "mortality",
  "readmission"                         = "mortality",
  "clinical deterioration"              = "mortality",

  "resilience"                          = "self-efficacy",
  "coping"                              = "self-efficacy",
  "coping and management of cfs/me"     = "self-efficacy",
  "mindfulness"                         = "self-efficacy",
  "hope"                                = "self-efficacy",
  "optimism"                            = "self-efficacy",
  "problem-solving"                     = "self-efficacy",
  "illness management and adjustment"   = "self-efficacy",
  "illness perception"                  = "self-efficacy",
  "problem awareness"                   = "self-efficacy",
  "motivation and adherence"            = "self-efficacy",
  "kinesiophobia"                       = "self-efficacy",

  "autonomic function"                  = "autonomic",
  "autonomic"                           = "autonomic",
  "autonomic symptoms"                  = "autonomic",
  "dizziness"                           = "autonomic",

  "neurological/neuropsychiatric symptoms" = "neurological",
  "neuropsychiatric symptoms"           = "neurological",
  "neuropathy"                          = "neurological",
  "neuroimaging"                        = "neurological",

  "biomarkers"                          = "biomarker",
  "autoantibodies"                      = "biomarker",
  "autoimmune"                          = "biomarker",
  "autoimmune diseases"                 = "biomarker",
  "immunological biomarker"             = "biomarker",
  "molecular biomarkers"                = "biomarker",
  "ion channel function"                = "biomarker",

  "side effects"                        = "safety",
  "feasibility"                         = "safety",
  "feasibility and acceptability"       = "safety",
  "feasibility/acceptability"           = "safety",

  "work ability"                        = "work and social",
  "work and social adjustment"          = "work and social",
  "social function"                     = "work and social",
  "social participation"                = "work and social",
  "social engagement"                   = "work and social",
  "social and occupational functioning" = "work and social",
  "social/recreational activities"      = "work and social",
  "school attendance"                   = "work and social",
  "education access"                    = "work and social",
  "sick leave"                          = "work and social",
  "relationship quality"                = "work and social",
  "healthcare utilization"              = "work and social",
  "health care utilization"             = "work and social",

  "mental health"                       = "depression",
  "mood"                                = "depression",
  "psychological distress"              = "depression",
  "psychosocial"                        = "depression",
  "psychosocial distress"               = "depression",
  "emotional wellbeing"                 = "depression",
  "emotional disorders/mental health diagnoses" = "depression",
  "mental functioning"                  = "depression",
  "subjective distress"                 = "depression",
  "negative affect"                     = "depression",
  "affect"                              = "depression",
  "apathy"                              = "depression",
  "burnout"                             = "depression",
  "anger"                               = "depression",
  "loneliness"                          = "depression",
  "somatization"                        = "depression",
  "interpretation bias"                 = "depression",
  "positive affect"                     = "depression",

  "ptsd"                                = "anxiety",
  "ptsd symptoms"                       = "anxiety",
  "stress"                              = "anxiety",
  "perceived stress"                    = "anxiety",
  "physiological stress"                = "anxiety",
  "post-traumatic stress"               = "anxiety",
  "impact of traumatic events"          = "anxiety",
  "perceived exertion"                  = "anxiety"
)

.intervention_category_map <- c(
  "cbt" = "CBT",
  "group cbt" = "CBT",
  "i-cbt" = "CBT",
  "cbt (internet-based)" = "CBT",
  "cbt (all forms)" = "CBT",
  "cbt (group face-to-face)" = "CBT",
  "cbt (individual face-to-face)" = "CBT",
  "cbt for fatigue" = "CBT",
  "cbt-act" = "CBT",
  "cbt-f" = "CBT",
  "cbt-i" = "CBT",
  "cbt-related treatments" = "CBT",
  "individually tailored icbt" = "CBT",
  "internet-based cbt (imp-fitnet)" = "CBT",
  "individual face-to-face cbt" = "CBT",
  "self-directed cbt" = "CBT",
  "fit after covid (cbt)" = "CBT",
  "iact + tau" = "CBT",
  "v-cbsm" = "CBT",

  "get" = "Graded Exercise Therapy",
  "graded exercise therapy" = "Graded Exercise Therapy",
  "graded exercise therapy (get)" = "Graded Exercise Therapy",
  "graded-exercise-related therapies" = "Graded Exercise Therapy",
  "guided graded exercise self-help (ges)" = "Graded Exercise Therapy",
  "individualised symptom-titrated exercise" = "Graded Exercise Therapy",
  "symptom-titrated exercise rehabilitation programme" = "Graded Exercise Therapy",
  "individualized controlled exercise" = "Graded Exercise Therapy",

  "act" = "ACT",
  "video-based case conceptualization + web-based act + tau" = "ACT",

  "exercise therapy" = "Exercise",
  "aerobic exercise training" = "Exercise",
  "conventional exercise therapy" = "Exercise",
  "conventional exercise therapy (aerobic)" = "Exercise",
  "exercise rehabilitation" = "Exercise",
  "exercise-based therapies" = "Exercise",
  "resistance exercise" = "Exercise",
  "resistance training" = "Exercise",
  "semi-supervised exercise program" = "Exercise",
  "self-paced aquatic exercise" = "Exercise",
  "unsupervised aerobic exercise training" = "Exercise",
  "eccentric cycling (ecc)" = "Exercise",
  "supervised multicomponent exercise (concurrent training)" = "Exercise",
  "multicomponent training (balance/aerobic/resistance)" = "Exercise",
  "physical activity (active subjects)" = "Exercise",
  "physical exercise via covidreapp" = "Exercise",
  "fatiguewalk" = "Exercise",

  "pulmonary rehabilitation" = "Pulmonary Rehabilitation",
  "in-patient pulmonary rehabilitation" = "Pulmonary Rehabilitation",
  "respiratory telerehabilitation" = "Pulmonary Rehabilitation",
  "respiratory muscle training" = "Pulmonary Rehabilitation",
  "aerobic exercise + respiratory muscle training" = "Pulmonary Rehabilitation",
  "pulmonary rehabilitation + progressive muscle relaxation" = "Pulmonary Rehabilitation",
  "pulmonary rehabilitation / respiratory muscle training" = "Pulmonary Rehabilitation",

  "rehabilitation interventions" = "Rehabilitation",
  "inpatient rehabilitation" = "Rehabilitation",
  "inpatient post-covid rehabilitation" = "Rehabilitation",
  "multidisciplinary hospital-based rehabilitation" = "Rehabilitation",
  "multidisciplinary group telerehabilitation" = "Rehabilitation",
  "multidisciplinary indoor rehabilitation" = "Rehabilitation",
  "multidisciplinary outpatient rehabilitation" = "Rehabilitation",
  "multimodal post-covid rehabilitation" = "Rehabilitation",
  "rehabilitation (any form targeting post-covid)" = "Rehabilitation",
  "physical and mental health rehabilitation programme" = "Rehabilitation",
  "spa resort rehabilitation program" = "Rehabilitation",

  "mindfulness-based stress reduction" = "Mindfulness",
  "online mindfulness-based stress reduction (mbsr)" = "Mindfulness",
  "online mindful walking" = "Mindfulness",
  "tele-mindfulness based intervention" = "Mindfulness",
  "mindfulness program" = "Mindfulness",

  "qigong" = "Qigong/Tai Chi",
  "tcme (qigong/tai chi)" = "Qigong/Tai Chi",
  "daoyin (traditional chinese exercise)" = "Qigong/Tai Chi",
  "prolong life with nine turn (plwnt) qigong" = "Qigong/Tai Chi",
  "plwnt qigong" = "Qigong/Tai Chi",
  "qigong tuina therapy" = "Qigong/Tai Chi",

  "yoga" = "Yoga",
  "tele-yoga" = "Yoga",
  "recumbent isometric yoga" = "Yoga",
  "recumbent isometric yoga + conventional therapy" = "Yoga",
  "remote yoga and self-management" = "Yoga",
  "vr-enhanced mindfulness and yoga" = "Yoga",
  "ai chi" = "Yoga",

  "pacing strategies" = "Pacing",
  "pacing" = "Pacing",
  "pacing interventions (video, app, book)" = "Pacing",
  "who borg cr-10 pacing protocol" = "Pacing",
  "structured who borg cr-10 5-phase pacing protocol" = "Pacing",

  "occupational therapy" = "Occupational Therapy",
  "online occupational therapy (prerecorded videos)" = "Occupational Therapy",
  "online occupational therapy (teletherapy)" = "Occupational Therapy",

  "ldn" = "Low-Dose Naltrexone",
  "low-dose naltrexone (ldn)" = "Low-Dose Naltrexone",
  "low dose naltrexone (ldn)" = "Low-Dose Naltrexone",
  "low-dose naltrexone" = "Low-Dose Naltrexone",

  "nirmatrelvir" = "Nirmatrelvir/Ritonavir",
  "nirmatrelvir-ritonavir" = "Nirmatrelvir/Ritonavir",
  "nirmatrelvir/ritonavir" = "Nirmatrelvir/Ritonavir",

  "fluvoxamine" = "Fluvoxamine",
  "fluoxetine" = "SSRIs",
  "ssris" = "SSRIs",

  "covid-19 vaccination" = "COVID-19 Vaccination",
  "full vaccination" = "COVID-19 Vaccination",
  "sars-cov-2 vaccination" = "COVID-19 Vaccination",
  "covid vaccination (1 dose)" = "COVID-19 Vaccination",
  "covid vaccination (2 doses)" = "COVID-19 Vaccination",
  "covid vaccination (3 doses)" = "COVID-19 Vaccination",
  "covid vaccination (4 doses)" = "COVID-19 Vaccination",

  "coq10 plus nadh" = "CoQ10/NADH",
  "coq10 + selenium" = "CoQ10/NADH",
  "coenzyme q10" = "CoQ10/NADH",
  "nadh" = "CoQ10/NADH",

  "creatine monohydrate" = "Creatine",
  "creatine supplementation + breathing exercises" = "Creatine",

  "nicotinamide riboside (nr)" = "Nicotinamide Riboside",

  "therapeutic apheresis" = "Apheresis",
  "therapeutic plasmapheresis" = "Apheresis",
  "immunoadsorption" = "Apheresis",
  "immunoadsorption (repeat)" = "Apheresis",

  "repetitive transcranial magnetic stimulation" = "Brain Stimulation",
  "high-frequency rtms to dlpfc" = "Brain Stimulation",
  "anodal transcranial direct current stimulation (tdcs)" = "Brain Stimulation",
  "tdcs + brainhq" = "Brain Stimulation",

  "intermittent hypoxia exposure (ihe)" = "Hypoxia Therapy",
  "intermittent hypoxia-hyperoxia conditioning (ihhc)" = "Hypoxia Therapy",

  "major ozone autohemotherapy" = "Ozone Therapy",
  "oxygen-ozone autohemotherapy (o2-o3-aht)" = "Ozone Therapy",

  "multi-strain probiotic" = "Probiotics",
  "oral bacteriotherapy (probiotics)" = "Probiotics",

  "metformin" = "Metformin",
  "rapamycin" = "Rapamycin",
  "methylphenidate" = "Methylphenidate",
  "dexamphetamine" = "Dexamphetamine",
  "minocycline" = "Minocycline",
  "hydrocortisone" = "Hydrocortisone",
  "mirtazapine" = "Mirtazapine",
  "pirfenidone" = "Pirfenidone"
)

harmonize_intervention <- function(x) {
  x_clean <- tolower(trimws(x))
  bogus <- x_clean %in% c("false", "true", "na", "n/a", "none", "null",
                           "unknown", "not reported", "")
  x[bogus] <- NA_character_
  mapped <- .intervention_category_map[x_clean]
  x[!is.na(mapped)] <- mapped[!is.na(mapped)]
  x
}

.outcome_measure_map <- c(
  "sf-36 physical function"            = "SF-36 Physical Functioning",
  "sf-36 physical functioning"         = "SF-36 Physical Functioning",
  "sf-36 physical function (sf36-pf)"  = "SF-36 Physical Functioning",
  "sf36-pf"                            = "SF-36 Physical Functioning",
  "sf-36"                              = "SF-36",
  "sf-12"                              = "SF-12",
  "eq-5d-5l"                           = "EQ-5D-5L",
  "eq-5d"                              = "EQ-5D-5L",
  "6mwt"                               = "6-Minute Walk Test",
  "6-minute walk test"                 = "6-Minute Walk Test",
  "6-minute walking test (6mwt)"       = "6-Minute Walk Test",
  "6-minute walk test (6mwt)"          = "6-Minute Walk Test",
  "6mwd"                               = "6-Minute Walk Test",
  "chalder fatigue scale"              = "Chalder Fatigue Scale",
  "chalder fatigue questionnaire"      = "Chalder Fatigue Scale",
  "chalder fatigue scale (cfs)"        = "Chalder Fatigue Scale",
  "fatigue severity scale"             = "Fatigue Severity Scale",
  "fatigue severity scale (fss)"       = "Fatigue Severity Scale",
  "fss"                                = "Fatigue Severity Scale",
  "fatigue assessment scale"           = "Fatigue Assessment Scale",
  "fatigue assessment scale (fas)"     = "Fatigue Assessment Scale",
  "fatigue assessment scale (fas) total" = "Fatigue Assessment Scale",
  "fas"                                = "Fatigue Assessment Scale",
  "facit-fatigue"                      = "FACIT-Fatigue",
  "facit fatigue"                      = "FACIT-Fatigue",
  "facit fatigue scale"                = "FACIT-Fatigue",
  "mfi-20"                             = "MFI-20",
  "mfi-20 general fatigue"             = "MFI-20 General Fatigue",
  "mfi-20 mental fatigue"              = "MFI-20 Mental Fatigue",
  "cis-fatigue"                        = "CIS-Fatigue",
  "checklist individual strength, subscale fatigue severity" = "CIS-Fatigue",
  "phq-9"                              = "PHQ-9",
  "gad-7"                              = "GAD-7",
  "hads"                               = "HADS",
  "hads anxiety"                       = "HADS-Anxiety",
  "hads depression"                    = "HADS-Depression",
  "dass-21"                            = "DASS-21",
  "dass-21 (depression anxiety stress scales)" = "DASS-21",
  "beck depression inventory"          = "Beck Depression Inventory",
  "beck depression inventory ii (bdi-ii)" = "Beck Depression Inventory",
  "bdi-ii"                             = "Beck Depression Inventory",
  "psqi"                               = "PSQI",
  "insomnia severity index"            = "Insomnia Severity Index",
  "insomnia severity index (isi)"      = "Insomnia Severity Index",
  "gses"                               = "GSES",
  "sss-8"                              = "SSS-8",
  "brief fatigue inventory"            = "Brief Fatigue Inventory",
  "pain visual analogue scale"         = "Pain VAS",
  "pain vas"                           = "Pain VAS",
  "vas fatigue"                        = "Fatigue VAS",
  "fatigue vas"                        = "Fatigue VAS",
  "vas pain"                           = "Pain VAS",
  "total fatigue score"                = "Total Fatigue Score",
  "moca"                               = "MoCA",
  "performance status (ps)"            = "Performance Status",
  "qalys"                              = "QALYs"
)

harmonize_outcome_measure <- function(x) {
  x_clean <- tolower(trimws(x))
  bogus <- x_clean %in% c("false", "true", "na", "n/a", "none", "null",
                           "unknown", "not reported", "")
  x[bogus] <- NA_character_
  mapped <- .outcome_measure_map[x_clean]
  x[!is.na(mapped)] <- mapped[!is.na(mapped)]
  x
}

harmonize_outcome_domain <- function(x) {
  x <- tolower(trimws(x))
  bogus <- x %in% c("false", "true", "na", "n/a", "none", "null", "unknown", "not reported", "")
  x[bogus] <- NA_character_
  mapped <- .outcome_domain_map[x]
  x[!is.na(mapped)] <- mapped[!is.na(mapped)]
  canonical <- unlist(.outcome_domain_enum)
  x[!is.na(x) & !x %in% canonical] <- "other"
  x
}

sanitize_extracted <- function(df) {
  bogus_vals <- c("false", "true", "na", "n/a", "none", "null",
                  "unknown", "not reported", "not available", "")
  char_cols <- names(df)[vapply(df, is.character, logical(1))]
  for (col in char_cols) {
    df[[col]][tolower(trimws(df[[col]])) %in% bogus_vals] <- NA_character_
  }
  if (all(c("effect_size", "ci_lower", "ci_upper") %in% names(df))) {
    bogus_es <- !is.na(df$effect_size) & df$effect_size == 0 &
                !is.na(df$ci_lower) & df$ci_lower == 0 &
                !is.na(df$ci_upper) & df$ci_upper == 0
    df$effect_size[bogus_es] <- NA_real_
    df$ci_lower[bogus_es] <- NA_real_
    df$ci_upper[bogus_es] <- NA_real_
    swapped_ci <- !is.na(df$ci_lower) & !is.na(df$ci_upper) & df$ci_lower > df$ci_upper
    if (any(swapped_ci)) {
      tmp <- df$ci_lower[swapped_ci]
      df$ci_lower[swapped_ci] <- df$ci_upper[swapped_ci]
      df$ci_upper[swapped_ci] <- tmp
    }
    es_outside <- !is.na(df$effect_size) & !is.na(df$ci_lower) & !is.na(df$ci_upper) &
                  df$ci_lower != 0 & df$ci_upper != 0 &
                  (df$effect_size < df$ci_lower - 0.01 | df$effect_size > df$ci_upper + 0.01)
    if (any(es_outside)) {
      df$ci_lower[es_outside] <- NA_real_
      df$ci_upper[es_outside] <- NA_real_
    }
  }
  for (grp in c("intervention", "comparator")) {
    n_col <- paste0("n_", grp)
    m_col <- paste0("mean_", grp)
    s_col <- paste0("sd_", grp)
    if (all(c(n_col, m_col) %in% names(df))) {
      n0_with_data <- !is.na(df[[n_col]]) & df[[n_col]] == 0 & !is.na(df[[m_col]]) & df[[m_col]] != 0
      if (any(n0_with_data)) df[[n_col]][n0_with_data] <- NA_integer_
    }
    if (all(c(s_col, m_col) %in% names(df))) {
      zero_sd <- !is.na(df[[s_col]]) & df[[s_col]] == 0 & !is.na(df[[m_col]]) & df[[m_col]] != 0
      if (any(zero_sd)) df[[s_col]][zero_sd] <- NA_real_
    }
    if (s_col %in% names(df)) {
      neg_sd <- !is.na(df[[s_col]]) & df[[s_col]] < 0
      if (any(neg_sd)) df[[s_col]][neg_sd] <- NA_real_
    }
  }
  if ("effect_size_type" %in% names(df)) {
    est <- tolower(trimws(df$effect_size_type))
    est[est %in% c("smd", "cohen_d")] <- "smd"
    est[est == "hedges_g"] <- "hedges_g"
    est[est %in% c("odds_ratio", "hazard_ratio", "risk_ratio",
                    "or", "hr", "rr", "aor", "ahr")] <- "ratio"
    est[est %in% c("mean_difference", "mean_diff", "md",
                    "raw_difference")] <- "mean_diff"
    est[!is.na(est) & !est %in% c("smd", "hedges_g", "ratio", "mean_diff")] <- "other"
    df$effect_size_type <- est
  }
  if ("outcome_domain" %in% names(df)) {
    df$outcome_domain_raw <- df$outcome_domain
    df$outcome_domain <- harmonize_outcome_domain(df$outcome_domain)
  }
  if ("outcome_measure" %in% names(df)) {
    df$outcome_measure <- harmonize_outcome_measure(df$outcome_measure)
  }
  if ("intervention" %in% names(df)) {
    df$intervention_category <- harmonize_intervention(df$intervention)
  }
  df
}

.treatment_props_shared <- list(
  intervention = list(
    type = .null_str,
    description = "Specific intervention name, e.g. CBT, GET, ACT, LDN, IVIG"
  ),
  intervention_category = list(
    type = .null_str,
    description = "Broad category: CBT, Exercise, Graded Exercise Therapy, Rehabilitation, Mindfulness, Pacing, Low-Dose Naltrexone, Nirmatrelvir/Ritonavir, CoQ10/NADH, Apheresis, Brain Stimulation, etc."
  ),
  comparator = list(
    type = .null_str,
    description = "Control/comparison group, e.g. usual care, waitlist, pacing, sham, placebo"
  ),
  study_design = list(
    type = .null_str,
    description = "RCT, open-label trial, cohort, case-control, pre-post, cross-sectional"
  ),
  blinding = list(
    type = .null_str,
    description = "double-blind, single-blind, open-label, sham-controlled, or null if not stated"
  ),
  condition = list(
    type = .null_str,
    enum = list("long COVID", "ME/CFS", "both", "post-viral fatigue"),
    description = "Which condition was studied"
  ),
  n_intervention = list(
    type = .null_int,
    description = "Participants in intervention arm"
  ),
  n_comparator = list(
    type = .null_int,
    description = "Participants in comparator arm"
  ),
  outcome_measure = list(
    type = .null_str,
    description = "Outcome instrument, e.g. Chalder Fatigue Scale, SF-36 Physical Functioning, 6-Minute Walk Test, PHQ-9"
  ),
  outcome_domain = list(
    type = "string",
    enum = .outcome_domain_enum,
    description = "Outcome category. Pick the closest match from the allowed values."
  ),
  scale_direction = list(
    type = .null_str,
    enum = list("lower_is_better", "higher_is_better"),
    description = "REQUIRED for every outcome. Does a LOWER score mean improvement (e.g. fatigue severity, pain VAS, depression scales, symptom counts) or does a HIGHER score mean improvement (e.g. SF-36, 6MWD, QoL, grip strength, walk distance, well-being VAS)?"
  ),
  mean_intervention = list(
    type = .null_num,
    description = "Post-intervention mean in intervention group"
  ),
  sd_intervention = list(
    type = .null_num,
    description = "Post-intervention SD in intervention group"
  ),
  mean_comparator = list(
    type = .null_num,
    description = "Post-intervention mean in comparator group"
  ),
  sd_comparator = list(
    type = .null_num,
    description = "Post-intervention SD in comparator group"
  ),
  effect_size = list(
    type = .null_num,
    description = "Pre-computed effect size if reported (SMD, Hedges g, Cohen d, OR, HR, RR, or raw mean difference). Place it here regardless of type — use effect_size_type to indicate the scale."
  ),
  effect_size_type = list(
    type = .null_str,
    enum = list("SMD", "hedges_g", "cohen_d", "odds_ratio", "hazard_ratio", "risk_ratio", "mean_difference", "other"),
    description = "REQUIRED when effect_size is provided. Classify the effect size scale: SMD/hedges_g/cohen_d for standardised mean differences; odds_ratio/hazard_ratio/risk_ratio for ratio measures (OR, HR, RR, aOR, aHR — where 1.0 = no effect); mean_difference for unstandardised score differences."
  ),
  ci_lower = list(type = .null_num, description = "Lower bound of the confidence interval"),
  ci_upper = list(type = .null_num, description = "Upper bound of the confidence interval"),
  ci_level = list(
    type = .null_num,
    description = "Confidence level as a proportion (0.95 for 95% CI, 0.90 for 90% CI). Default to 0.95 if not explicitly stated."
  ),
  p_value = list(type = .null_num),
  followup_weeks = list(type = .null_num),
  pem_assessed = list(type = "boolean"),
  diagnostic_criteria = list(
    type = .null_str,
    description = "ME/CFS criteria, e.g. Fukuda, CCC, IOM, WHO Long COVID definition, none"
  ),
  adverse_events_reported = list(type = "boolean"),
  adverse_events_detail = list(type = .null_str)
)

.extraction_schemas <- list(
  treatment_cbt_psych = list(
    type = "array",
    description = "One object per reported outcome. Use null for any value not explicitly stated.",
    items = list(
      type = "object",
      required = list(
        "intervention",
        "comparator",
        "outcome_measure",
        "outcome_domain",
        "scale_direction",
        "effect_size_type",
        "pem_assessed",
        "adverse_events_reported"
      ),
      properties = .treatment_props_shared
    )
  ),
  treatment_biomedical = list(
    type = "array",
    description = "One object per reported outcome. Use null for any value not explicitly stated.",
    items = list(
      type = "object",
      required = list(
        "intervention",
        "comparator",
        "outcome_measure",
        "outcome_domain",
        "effect_size_type",
        "pem_assessed",
        "adverse_events_reported"
      ),
      properties = c(
        list(
          dose = list(
            type = .null_str,
            description = "Dosage/protocol if reported"
          )
        ),
        .treatment_props_shared
      )
    )
  ),
  biomarkers = list(
    type = "array",
    description = "One object per biomarker or biomarker group. Use null for any value not explicitly stated.",
    items = list(
      type = "object",
      required = list(
        "biomarker_type",
        "biomarker_name",
        "condition",
        "finding_direction",
        "pem_assessed"
      ),
      properties = list(
        biomarker_type = list(
          type = .null_str,
          description = "Category, e.g. cytokine, autoantibody, metabolite, immune cell, imaging"
        ),
        biomarker_name = list(
          type = .null_str,
          description = "Specific biomarker(s) measured"
        ),
        measurement_method = list(
          type = .null_str,
          description = "Assay/technique used"
        ),
        n_patients = list(type = .null_int),
        n_controls = list(
          type = .null_int,
          description = "Healthy controls; null if none"
        ),
        condition = list(
          type = "string",
          enum = list("long COVID", "ME/CFS", "both")
        ),
        diagnostic_criteria = list(type = .null_str),
        finding_direction = list(
          type = "string",
          enum = list("elevated", "reduced", "no difference", "mixed")
        ),
        effect_size = list(type = .null_num),
        ci_lower = list(type = .null_num, description = "Lower 95% CI bound"),
        ci_upper = list(type = .null_num, description = "Upper 95% CI bound"),
        p_value = list(type = .null_num),
        sensitivity = list(
          type = .null_num,
          description = "Diagnostic sensitivity 0-1; null if not reported"
        ),
        specificity = list(
          type = .null_num,
          description = "Diagnostic specificity 0-1; null if not reported"
        ),
        pem_assessed = list(type = "boolean")
      )
    )
  ),
  outcomes = list(
    type = "array",
    description = "One object per outcome measure. Use null for any value not explicitly stated.",
    items = list(
      type = "object",
      required = list(
        "outcome_measure",
        "outcome_domain",
        "condition",
        "pem_assessed"
      ),
      properties = list(
        outcome_measure = list(
          type = .null_str,
          description = "Instrument/measure name"
        ),
        outcome_domain = list(
          type = "string",
          enum = .outcome_domain_enum,
          description = "Outcome category. Pick the closest match from the allowed values."
        ),
        n_total = list(type = .null_int),
        condition = list(
          type = "string",
          enum = list("long COVID", "ME/CFS", "both")
        ),
        diagnostic_criteria = list(type = .null_str),
        timepoints_weeks = list(
          type = .null_str,
          description = "Follow-up timepoints in weeks, comma-separated"
        ),
        baseline_mean = list(type = .null_num),
        baseline_sd = list(type = .null_num),
        final_mean = list(
          type = .null_num,
          description = "Mean at last follow-up"
        ),
        final_sd = list(type = .null_num, description = "SD at last follow-up"),
        recovery_n = list(
          type = .null_int,
          description = "Number who recovered; null if not reported"
        ),
        recovery_definition = list(
          type = .null_str,
          description = "How recovery was defined; null if not reported"
        ),
        pem_assessed = list(type = "boolean"),
        pem_prevalence = list(
          type = .null_num,
          description = "Proportion with PEM 0-1; null if not reported"
        )
      )
    )
  ),

  systematic_reviews = list(
    type = "array",
    description = "One object per synthesised outcome or domain. Use null for any value not explicitly stated.",
    items = list(
      type = "object",
      required = list(
        "review_type",
        "domain",
        "condition",
        "n_studies_included",
        "pem_addressed"
      ),
      properties = list(
        review_type = list(
          type = "string",
          enum = list("systematic_review", "meta-analysis", "scoping_review", "umbrella_review", "rapid_review")
        ),
        domain = list(
          type = .null_str,
          description = "Topic synthesised, e.g. CBT, biomarkers, prevalence, prognosis"
        ),
        condition = list(
          type = "string",
          enum = list("long COVID", "ME/CFS", "both")
        ),
        n_studies_included = list(type = .null_int, description = "Number of primary studies included"),
        n_participants = list(type = .null_int, description = "Total participants across included studies"),
        search_date = list(type = .null_str, description = "Latest date of literature search, YYYY-MM format"),
        outcome_measure = list(type = .null_str, description = "Primary outcome or measure synthesised"),
        pooled_effect_size = list(type = .null_num, description = "Pooled effect size (SMD, OR, RR, etc.)"),
        pooled_ci_lower = list(type = .null_num),
        pooled_ci_upper = list(type = .null_num),
        heterogeneity_i2 = list(type = .null_num, description = "I² statistic 0-100; null if not reported"),
        grade_certainty = list(
          type = .null_str,
          enum = list("high", "moderate", "low", "very low", NULL),
          description = "GRADE certainty of evidence; null if not assessed"
        ),
        main_conclusion = list(type = .null_str, description = "One sentence summarising the main finding"),
        diagnostic_criteria_required = list(
          type = "boolean",
          description = "Whether included studies were required to use validated diagnostic criteria"
        ),
        pem_addressed = list(type = "boolean", description = "Whether PEM was explicitly addressed"),
        prospero_id = list(
          type = .null_str,
          description = "PROSPERO registration ID (e.g. CRD42021...) ; null if not registered"
        ),
        search_strategy_reported = list(
          type = "boolean",
          description = "Whether a full search strategy is reported"
        )
      )
    )
  )
)

extraction_prompt <- function(
  title,
  abstract,
  search_topic,
  fulltext_sections = NULL
) {
  schema_json <- jsonlite::toJSON(
    .extraction_schemas[[search_topic]],
    auto_unbox = TRUE,
    pretty = TRUE
  )

  extra_sections <- ""
  if (!is.null(fulltext_sections)) {
    if (
      !is.null(fulltext_sections$results) &&
        nchar(fulltext_sections$results) > 0
    ) {
      extra_sections <- paste0(
        extra_sections,
        sprintf(
          "\n\nRESULTS SECTION:\n%s",
          substr(fulltext_sections$results, 1, 6000)
        )
      )
    }
    if (
      !is.null(fulltext_sections$tables) && nchar(fulltext_sections$tables) > 0
    ) {
      extra_sections <- paste0(
        extra_sections,
        sprintf(
          "\n\nTABLES:\n%s",
          substr(fulltext_sections$tables, 1, 6000)
        )
      )
    }
    if (
      !is.null(fulltext_sections$supplements) &&
        nchar(fulltext_sections$supplements) > 0
    ) {
      extra_sections <- paste0(
        extra_sections,
        sprintf(
          "\n\nSUPPLEMENTARY DATA:\n%s",
          substr(fulltext_sections$supplements, 1, 4000)
        )
      )
    }
  }

  paste(
    "You are extracting quantitative data from a research study for a systematic review.",
    "Extract ONLY values explicitly stated in the text, tables, or supplementary data.",
    "Use null for any field not reported. Do NOT calculate, estimate, or infer values.",
    "Pay special attention to TABLES and RESULTS sections — these contain the key numbers.",
    "",
    paste0("DOMAIN: ", search_topic),
    "",
    "RESPONSE SCHEMA (return a JSON array matching this schema exactly):",
    schema_json,
    "",
    "STUDY:",
    paste0("Title: ", title),
    paste0(
      "Abstract: ",
      ifelse(
        is.na(abstract) || abstract == "",
        "[No abstract available]",
        abstract
      )
    ),
    extra_sections,
    "",
    "Respond with ONLY a valid JSON array — no markdown, no code fences, no explanation.",
    sep = "\n"
  )
}


parse_extraction_response <- function(text) {
  if (is.null(text)) {
    return(NULL)
  }
  text <- trimws(text)
  text <- sub("^```json?\\s*", "", text)
  text <- sub("\\s*```$", "", text)

  tryCatch(
    {
      result <- jsonlite::fromJSON(text, simplifyVector = FALSE)
      if (!is.list(result)) {
        return(NULL)
      }
      if (!is.null(names(result))) {
        result <- list(result)
      }
      result
    },
    error = function(e) NULL
  )
}

numeric_fields <- list(
  treatment_cbt_psych = c(
    "n_intervention",
    "n_comparator",
    "mean_intervention",
    "sd_intervention",
    "mean_comparator",
    "sd_comparator",
    "effect_size",
    "ci_lower",
    "ci_upper",
    "p_value",
    "followup_weeks"
  ),
  treatment_biomedical = c(
    "n_intervention",
    "n_comparator",
    "mean_intervention",
    "sd_intervention",
    "mean_comparator",
    "sd_comparator",
    "effect_size",
    "ci_lower",
    "ci_upper",
    "p_value",
    "followup_weeks"
  ),
  biomarkers = c(
    "n_patients",
    "n_controls",
    "effect_size",
    "ci_lower",
    "ci_upper",
    "p_value",
    "sensitivity",
    "specificity"
  ),
  outcomes = c(
    "n_total",
    "baseline_mean",
    "baseline_sd",
    "final_mean",
    "final_sd",
    "recovery_n",
    "pem_prevalence"
  ),
  systematic_reviews = c(
    "n_studies_included",
    "n_participants",
    "pooled_effect_size",
    "pooled_ci_lower",
    "pooled_ci_upper",
    "heterogeneity_i2"
  )
)

.fields_agree <- function(row_a, row_b, fields, tolerance) {
  all(vapply(
    fields,
    function(field) {
      val_a <- row_a[[field]]
      val_b <- row_b[[field]]
      if (is.null(val_a) && is.null(val_b)) {
        return(TRUE)
      }
      if (is.null(val_a) || is.null(val_b)) {
        return(FALSE)
      }
      a <- suppressWarnings(as.numeric(val_a))
      b <- suppressWarnings(as.numeric(val_b))
      if (is.na(a) && is.na(b)) {
        return(TRUE)
      }
      if (is.na(a) || is.na(b)) {
        return(FALSE)
      }
      if (a == 0 && b == 0) {
        return(TRUE)
      }
      abs(a - b) / max(abs(a), abs(b)) <= tolerance
    },
    logical(1)
  ))
}

compare_extractions <- function(
  attempt_a,
  attempt_b,
  domain,
  tolerance = 0.01
) {
  fields <- numeric_fields[[domain]]
  if (is.null(fields) || length(attempt_a) != length(attempt_b)) {
    return(FALSE)
  }
  all(vapply(
    seq_along(attempt_a),
    function(i) {
      .fields_agree(attempt_a[[i]], attempt_b[[i]], fields, tolerance)
    },
    logical(1)
  ))
}

.count_agreements <- function(parsed, compare_fn) {
  vapply(
    seq_along(parsed),
    function(i) {
      sum(vapply(
        seq_along(parsed)[-i],
        function(j) {
          compare_fn(parsed[[i]], parsed[[j]])
        },
        logical(1)
      ))
    },
    integer(1)
  )
}

.rob2_schema <- list(
  type = "object",
  required = list(
    "overall_rob", "overall_rationale"
  ),
  properties = list(
    d1_randomisation        = list(type = "string", enum = list("low", "some concerns", "high")),
    d1_rationale            = list(type = .null_str),
    d2_deviations           = list(type = "string", enum = list("low", "some concerns", "high"), description = "Domain 2: Deviations from intended interventions"),
    d2_rationale            = list(type = .null_str),
    d3_missing_data         = list(type = "string", enum = list("low", "some concerns", "high")),
    d3_rationale            = list(type = .null_str),
    d4_outcome_measurement  = list(type = "string", enum = list("low", "some concerns", "high")),
    d4_rationale            = list(type = .null_str),
    d5_selection_reporting  = list(type = "string", enum = list("low", "some concerns", "high"), description = "Domain 5: Selection of the reported result"),
    d5_rationale            = list(type = .null_str),
    overall_rob             = list(type = "string", enum = list("low", "some concerns", "high")),
    overall_rationale       = list(type = "string", description = "Brief explanation of overall rating")
  )
)

.robins_schema <- list(
  type = "object",
  required = list(
    "overall_rob", "overall_rationale"
  ),
  properties = list(
    d1_confounding          = list(type = "string", enum = list("low", "moderate", "serious", "critical")),
    d1_rationale            = list(type = .null_str),
    d2_selection            = list(type = "string", enum = list("low", "moderate", "serious", "critical"), description = "Domain 2: Selection of participants"),
    d2_rationale            = list(type = .null_str),
    d3_classification       = list(type = "string", enum = list("low", "moderate", "serious", "critical"), description = "Domain 3: Classification of interventions"),
    d3_rationale            = list(type = .null_str),
    d4_deviations           = list(type = "string", enum = list("low", "moderate", "serious", "critical")),
    d4_rationale            = list(type = .null_str),
    d5_missing_data         = list(type = "string", enum = list("low", "moderate", "serious", "critical")),
    d5_rationale            = list(type = .null_str),
    d6_outcome_measurement  = list(type = "string", enum = list("low", "moderate", "serious", "critical")),
    d6_rationale            = list(type = .null_str),
    d7_reporting            = list(type = "string", enum = list("low", "moderate", "serious", "critical"), description = "Domain 7: Selection of the reported result"),
    d7_rationale            = list(type = .null_str),
    overall_rob             = list(type = "string", enum = list("low", "moderate", "serious", "critical")),
    overall_rationale       = list(type = "string", description = "Brief explanation of overall rating")
  )
)

.amstar2_schema <- list(
  type = "object",
  required = list("overall_confidence", "overall_rationale"),
  properties = list(
    d1_pico                = list(type = "string", enum = list("yes", "partial yes", "no")),
    d1_rationale           = list(type = .null_str),
    d2_protocol            = list(type = "string", enum = list("yes", "partial yes", "no"), description = "Protocol registered before review?"),
    d2_rationale           = list(type = .null_str),
    d3_study_design        = list(type = "string", enum = list("yes", "no"), description = "Justified study design selection?"),
    d3_rationale           = list(type = .null_str),
    d4_search_strategy     = list(type = "string", enum = list("yes", "partial yes", "no"), description = "Comprehensive search strategy? (critical)"),
    d4_rationale           = list(type = .null_str),
    d7_rob_assessment      = list(type = "string", enum = list("yes", "partial yes", "no"), description = "Satisfactory risk of bias assessment? (critical)"),
    d7_rationale           = list(type = .null_str),
    d9_synthesis           = list(type = "string", enum = list("yes", "no"), description = "Appropriate methods for meta-analysis? (critical)"),
    d9_rationale           = list(type = .null_str),
    d11_rob_impact         = list(type = "string", enum = list("yes", "no"), description = "Accounted for RoB in synthesis? (critical)"),
    d11_rationale          = list(type = .null_str),
    d13_rob_discussion     = list(type = "string", enum = list("yes", "no"), description = "Discussed RoB in interpretation?"),
    d13_rationale          = list(type = .null_str),
    d15_publication_bias   = list(type = "string", enum = list("yes", "no"), description = "Investigated publication bias? (critical)"),
    d15_rationale          = list(type = .null_str),
    overall_confidence     = list(type = "string", enum = list("high", "moderate", "low", "critically low")),
    overall_rationale      = list(type = "string", description = "Brief explanation of confidence rating")
  )
)

.casp_qualitative_schema <- list(
  type = "object",
  required = list("overall_quality", "overall_rationale"),
  properties = list(
    d1_clear_aims          = list(type = "string", enum = list("yes", "can't tell", "no")),
    d1_rationale           = list(type = .null_str),
    d2_methodology         = list(type = "string", enum = list("yes", "can't tell", "no"), description = "Qualitative methodology appropriate?"),
    d2_rationale           = list(type = .null_str),
    d3_design              = list(type = "string", enum = list("yes", "can't tell", "no"), description = "Research design appropriate?"),
    d3_rationale           = list(type = .null_str),
    d4_recruitment         = list(type = "string", enum = list("yes", "can't tell", "no"), description = "Recruitment strategy appropriate?"),
    d4_rationale           = list(type = .null_str),
    d5_data_collection     = list(type = "string", enum = list("yes", "can't tell", "no")),
    d5_rationale           = list(type = .null_str),
    d6_reflexivity         = list(type = "string", enum = list("yes", "can't tell", "no"), description = "Researcher-participant relationship considered?"),
    d6_rationale           = list(type = .null_str),
    d7_ethical             = list(type = "string", enum = list("yes", "can't tell", "no")),
    d7_rationale           = list(type = .null_str),
    d8_analysis            = list(type = "string", enum = list("yes", "can't tell", "no"), description = "Data analysis sufficiently rigorous?"),
    d8_rationale           = list(type = .null_str),
    d9_findings            = list(type = "string", enum = list("yes", "can't tell", "no"), description = "Clear statement of findings?"),
    d9_rationale           = list(type = .null_str),
    d10_value              = list(type = "string", enum = list("yes", "can't tell", "no"), description = "Valuable contribution?"),
    d10_rationale          = list(type = .null_str),
    overall_quality        = list(type = "string", enum = list("high", "moderate", "low")),
    overall_rationale      = list(type = "string", description = "Brief explanation of quality rating")
  )
)

.grade_schema <- list(
  type = "object",
  required = list("outcome", "starting_certainty", "certainty", "certainty_rationale"),
  properties = list(
    outcome                      = list(type = "string", description = "Name of the outcome assessed"),
    n_studies                    = list(type = .null_int),
    n_participants               = list(type = .null_int),
    effect_estimate              = list(type = .null_str, description = "Effect estimate with 95% CI as a string, e.g. \"SMD -0.52 (95% CI -0.71 to -0.33)\""),
    starting_certainty           = list(type = "string", enum = list("high", "low"), description = "high for RCTs, low for observational"),
    downgrade_risk_of_bias       = list(type = "integer", enum = list(0L, -1L, -2L)),
    downgrade_inconsistency      = list(type = "integer", enum = list(0L, -1L, -2L)),
    downgrade_indirectness       = list(type = "integer", enum = list(0L, -1L, -2L)),
    downgrade_imprecision        = list(type = "integer", enum = list(0L, -1L, -2L)),
    downgrade_publication_bias   = list(type = "integer", enum = list(0L, -1L)),
    upgrade_large_effect         = list(type = "integer", enum = list(0L, 1L, 2L)),
    upgrade_dose_response        = list(type = "integer", enum = list(0L, 1L)),
    upgrade_residual_confounding = list(type = "integer", enum = list(0L, 1L)),
    certainty                    = list(type = "string", enum = list("high", "moderate", "low", "very low")),
    certainty_rationale          = list(type = "string", description = "Brief explanation of certainty rating")
  )
)

quality_tool_for_type <- function(study_type) {
  switch(
    study_type,
    RCT = "rob2",
    "systematic_review" = , "meta-analysis" = "amstar2",
    qualitative = "casp_qualitative",
    "robins"
  )
}

quality_prompt <- function(title, abstract, methods_text, study_type,
                           rob_tool = quality_tool_for_type(study_type)) {
  schema <- switch(
    rob_tool,
    rob2 = .rob2_schema,
    robins = .robins_schema,
    amstar2 = .amstar2_schema,
    casp_qualitative = .casp_qualitative_schema
  )
  schema_json <- jsonlite::toJSON(schema, auto_unbox = TRUE, pretty = TRUE)

  tool_description <- switch(
    rob_tool,
    rob2 = "Use the Cochrane Risk of Bias 2 (RoB 2) tool for randomised controlled trials.",
    robins = "Use the ROBINS-I tool for non-randomised studies of interventions.",
    amstar2 = "Use the AMSTAR 2 tool for systematic reviews and meta-analyses. Focus on the critical domains (items 4, 7, 9, 11, 15). Overall confidence is critically low if any critical domain is 'no', low if more than one non-critical domain is 'no'.",
    casp_qualitative = "Use the CASP Qualitative Checklist for qualitative studies."
  )

  paste(
    "You are assessing methodological quality of a study for a systematic review.",
    tool_description,
    "Base your assessment solely on what is reported in the abstract and methods below.",
    "",
    "RESPONSE SCHEMA (return a JSON object matching this schema exactly):",
    schema_json,
    "",
    paste0("STUDY TYPE: ", study_type),
    paste0("Title: ", title),
    paste0("Abstract: ", ifelse(is.na(abstract) || abstract == "", "[No abstract available]", abstract)),
    "",
    "METHODS SECTION:",
    ifelse(is.null(methods_text) || methods_text == "", "[Methods section not available]", substr(methods_text, 1, 8000)),
    "",
    "Respond with ONLY a valid JSON object — no markdown, no code fences, no explanation.",
    sep = "\n"
  )
}

assess_quality_consensus <- function(
  title,
  abstract,
  methods_text,
  study_type,
  n_attempts = 3,
  min_agreement = 2,
  rate_limit_delay = 2
) {
  rob_tool <- quality_tool_for_type(study_type)
  prompt <- quality_prompt(title, abstract, methods_text, study_type, rob_tool)

  parse_quality <- function(text) {
    tryCatch(
      {
        if (is.null(text)) {
          return(NULL)
        }
        text <- trimws(text)
        text <- sub("^```json?\\s*", "", text)
        text <- sub("\\s*```$", "", text)
        jsonlite::fromJSON(text, simplifyVector = FALSE)
      },
      error = function(e) NULL
    )
  }

  parsed <- Filter(
    Negate(is.null),
    lapply(seq_len(n_attempts), function(k) {
      result <- parse_quality(call_llm(prompt))
      Sys.sleep(rate_limit_delay)
      result
    })
  )

  if (length(parsed) < min_agreement) {
    return(list(data = NULL, consensus = FALSE, n_agreeing = 0L, rob_tool = rob_tool))
  }

  key_fields <- switch(
    rob_tool,
    rob2 = c("overall_rob", "d1_randomisation", "d2_deviations", "d3_missing_data"),
    robins = c("overall_rob", "d1_confounding", "d2_selection", "d5_missing_data"),
    amstar2 = c("overall_confidence", "d4_search_strategy", "d7_rob_assessment"),
    casp_qualitative = c("overall_quality", "d2_methodology", "d8_analysis")
  )

  agreement_counts <- .count_agreements(parsed, function(a, b) {
    all(vapply(key_fields, function(f) identical(a[[f]], b[[f]]), logical(1)))
  })

  best_idx <- which.max(agreement_counts)
  n_agreeing <- agreement_counts[best_idx] + 1L

  if (n_agreeing >= min_agreement) {
    result_data <- parsed[[best_idx]]
    result_data$rob_tool <- rob_tool
    return(list(
      data = result_data,
      consensus = TRUE,
      n_agreeing = n_agreeing,
      rob_tool = rob_tool
    ))
  }
  list(data = NULL, consensus = FALSE, n_agreeing = n_agreeing, rob_tool = rob_tool)
}

grade_prompt <- function(title, abstract, outcome, n_studies, study_types) {
  schema_json <- jsonlite::toJSON(.grade_schema, auto_unbox = TRUE, pretty = TRUE)

  paste(
    "You are applying the GRADE approach to assess the certainty of evidence for a systematic review.",
    "GRADE certainty starts HIGH for RCTs and LOW for observational studies.",
    "Downgrade for: risk of bias (-1/-2), inconsistency (-1/-2), indirectness (-1/-2), imprecision (-1/-2), publication bias (-1).",
    "Upgrade for (observational only): large effect (+1/+2), dose-response (+1), residual confounding (+1).",
    "Final certainty: high (0 net), moderate (-1), low (-2), very low (-3 or more).",
    "",
    "RESPONSE SCHEMA (return a JSON object matching this schema exactly):",
    schema_json,
    "",
    paste0("Title/Review: ", title),
    paste0("Abstract: ", ifelse(is.na(abstract) || abstract == "", "[No abstract available]", abstract)),
    paste0("Outcome being assessed: ", outcome),
    paste0("Number of studies: ", n_studies),
    paste0("Study types in body of evidence: ", study_types),
    "",
    "Respond with ONLY a valid JSON object — no markdown, no code fences, no explanation.",
    sep = "\n"
  )
}

assess_grade_consensus <- function(
  title,
  abstract,
  outcome,
  n_studies,
  study_types,
  n_attempts = 3,
  min_agreement = 2,
  rate_limit_delay = 2
) {
  prompt <- grade_prompt(title, abstract, outcome, n_studies, study_types)

  parse_grade <- function(text) {
    tryCatch(
      {
        if (is.null(text)) return(NULL)
        text <- trimws(text)
        text <- sub("^```json?\\s*", "", text)
        text <- sub("\\s*```$", "", text)
        jsonlite::fromJSON(text, simplifyVector = FALSE)
      },
      error = function(e) NULL
    )
  }

  parsed <- Filter(
    Negate(is.null),
    lapply(seq_len(n_attempts), function(k) {
      result <- parse_grade(call_llm(prompt))
      Sys.sleep(rate_limit_delay)
      result
    })
  )

  if (length(parsed) < min_agreement) {
    return(list(data = NULL, consensus = FALSE, n_agreeing = 0L))
  }

  agreement_counts <- .count_agreements(parsed, function(a, b) {
    identical(a[["certainty"]], b[["certainty"]])
  })

  best_idx <- which.max(agreement_counts)
  n_agreeing <- agreement_counts[best_idx] + 1L

  if (n_agreeing >= min_agreement) {
    return(list(
      data = parsed[[best_idx]],
      consensus = TRUE,
      n_agreeing = n_agreeing
    ))
  }
  list(data = NULL, consensus = FALSE, n_agreeing = n_agreeing)
}

extract_study_consensus <- function(
  title,
  abstract,
  search_topic,
  fulltext_sections = NULL,
  n_attempts = 5,
  min_agreement = 3,
  rate_limit_delay = 2
) {
  prompt <- extraction_prompt(title, abstract, search_topic, fulltext_sections)

  parsed <- Filter(
    Negate(is.null),
    lapply(seq_len(n_attempts), function(k) {
      result <- parse_extraction_response(call_llm(prompt))
      Sys.sleep(rate_limit_delay)
      result
    })
  )

  if (length(parsed) < min_agreement) {
    return(list(
      data = NULL,
      consensus = FALSE,
      n_attempts = n_attempts,
      n_parsed = length(parsed),
      n_agreeing = 0L
    ))
  }

  agreement_counts <- .count_agreements(parsed, function(a, b) {
    compare_extractions(a, b, search_topic)
  })

  best_idx <- which.max(agreement_counts)
  n_agreeing <- agreement_counts[best_idx] + 1L

  if (n_agreeing >= min_agreement) {
    return(list(
      data = parsed[[best_idx]],
      consensus = TRUE,
      n_attempts = n_attempts,
      n_parsed = length(parsed),
      n_agreeing = n_agreeing
    ))
  }

  list(
    data = NULL,
    consensus = FALSE,
    n_attempts = n_attempts,
    n_parsed = length(parsed),
    n_agreeing = n_agreeing
  )
}

extract_section_text <- function(xml_doc, section_types) {
  if (!requireNamespace("xml2", quietly = TRUE)) {
    return(NULL)
  }

  sections <- xml2::xml_find_all(xml_doc, ".//sec")
  for (sec in sections) {
    title_node <- xml2::xml_find_first(sec, ".//title")
    if (is.na(title_node)) {
      next
    }
    title_text <- tolower(xml2::xml_text(title_node))
    if (
      any(vapply(section_types, function(s) grepl(s, title_text), logical(1)))
    ) {
      return(xml2::xml_text(sec))
    }
  }

  sec_type_attr <- xml2::xml_find_all(xml_doc, ".//sec[@sec-type]")
  for (sec in sec_type_attr) {
    sec_type <- xml2::xml_attr(sec, "sec-type")
    if (
      any(vapply(
        section_types,
        function(s) grepl(s, tolower(sec_type)),
        logical(1)
      ))
    ) {
      return(xml2::xml_text(sec))
    }
  }
  NULL
}

extract_tables_from_xml <- function(xml_doc) {
  if (!requireNamespace("xml2", quietly = TRUE)) {
    return(NULL)
  }

  tables <- xml2::xml_find_all(xml_doc, ".//table-wrap")
  if (length(tables) == 0) {
    return(NULL)
  }

  vapply(
    seq_along(tables),
    function(i) {
      tbl <- tables[[i]]
      label <- xml2::xml_text(xml2::xml_find_first(tbl, ".//label"))
      caption <- xml2::xml_text(xml2::xml_find_first(tbl, ".//caption"))

      thead <- xml2::xml_find_all(tbl, ".//thead//th")
      header <- if (length(thead) > 0) {
        paste(xml2::xml_text(thead), collapse = " | ")
      } else {
        ""
      }

      rows <- xml2::xml_find_all(tbl, ".//tbody//tr")
      row_texts <- vapply(
        rows,
        function(r) {
          paste(
            xml2::xml_text(xml2::xml_find_all(r, ".//td")),
            collapse = " | "
          )
        },
        character(1)
      )

      paste(
        c(
          sprintf(
            "=== %s: %s ===",
            label %||% paste("Table", i),
            caption %||% ""
          ),
          header,
          paste(rep("-", 40), collapse = ""),
          row_texts
        ),
        collapse = "\n"
      )
    },
    character(1)
  )
}

pmid_to_pmcid <- function(pmid) {
  url <- sprintf("https://www.ncbi.nlm.nih.gov/pmc/utils/idconv/v1.0/?ids=%s&format=json", pmid)
  resp <- tryCatch(
    httr2::request(url) |> httr2::req_timeout(10) |>
      httr2::req_error(is_error = \(r) FALSE) |> httr2::req_perform(),
    error = function(e) NULL
  )
  if (is.null(resp) || httr2::resp_status(resp) != 200) return(NULL)
  body <- jsonlite::fromJSON(httr2::resp_body_string(resp))
  pmcid <- tryCatch(body$records$pmcid[1], error = function(e) NA)
  if (is.null(pmcid) || is.na(pmcid) || pmcid == "") NULL else pmcid
}

fetch_fulltext_sections <- function(pmid) {
  if (is.na(pmid) || pmid == "") {
    return(NULL)
  }

  pmcid <- pmid_to_pmcid(pmid)
  if (is.null(pmcid)) return(NULL)

  xml_doc <- tryCatch(
    suppressMessages(europepmc::epmc_ftxt(ext_id = pmcid)),
    error = function(e) NULL
  )
  if (is.null(xml_doc)) {
    return(NULL)
  }

  results_text <- extract_section_text(xml_doc, c("result", "finding"))
  methods_text <- extract_section_text(
    xml_doc,
    c("method", "material", "procedure", "design")
  )

  tables <- extract_tables_from_xml(xml_doc)
  tables_text <- if (!is.null(tables) && length(tables) > 0) {
    paste(tables, collapse = "\n\n")
  }

  if (is.null(results_text) && is.null(methods_text) && is.null(tables_text)) {
    return(NULL)
  }

  list(
    results = results_text,
    methods = methods_text,
    tables = tables_text
  )
}

fetch_supplement_files <- function(pmid) {
  if (is.na(pmid) || pmid == "") {
    return(NULL)
  }

  supp_list <- tryCatch(
    suppressMessages(europepmc::epmc_supplementary_files(ext_id = pmid)),
    error = function(e) NULL
  )
  if (is.null(supp_list) || nrow(supp_list) == 0) {
    return(NULL)
  }

  table_files <- supp_list[
    grepl("\\.(csv|tsv|xlsx?|docx?)$", supp_list$fileName, ignore.case = TRUE),
  ]
  if (nrow(table_files) == 0) {
    return(NULL)
  }

  tmpdir <- tempdir()
  file_subset <- table_files[seq_len(min(nrow(table_files), 3)), ]

  collected_text <- Filter(
    Negate(is.null),
    lapply(seq_len(nrow(file_subset)), function(i) {
      fname <- file_subset$fileName[i]
      dest <- file.path(tmpdir, fname)

      dl_ok <- tryCatch(
        {
          utils::download.file(
            file_subset$url[i],
            dest,
            quiet = TRUE,
            mode = "wb"
          )
          TRUE
        },
        error = function(e) FALSE
      )
      if (!dl_ok) {
        return(NULL)
      }

      text <- tryCatch(
        {
          if (grepl("\\.csv$", fname, ignore.case = TRUE)) {
            paste(
              utils::capture.output(print(read.csv(
                dest,
                stringsAsFactors = FALSE,
                nrows = 200
              ))),
              collapse = "\n"
            )
          } else if (grepl("\\.tsv$", fname, ignore.case = TRUE)) {
            paste(
              utils::capture.output(print(read.delim(
                dest,
                stringsAsFactors = FALSE,
                nrows = 200
              ))),
              collapse = "\n"
            )
          } else if (
            grepl("\\.xlsx?$", fname, ignore.case = TRUE) &&
              requireNamespace("readxl", quietly = TRUE)
          ) {
            paste(
              utils::capture.output(print(readxl::read_excel(
                dest,
                n_max = 200
              ))),
              collapse = "\n"
            )
          } else {
            NULL
          }
        },
        error = function(e) NULL
      )
      unlink(dest)
      if (!is.null(text)) sprintf("=== %s ===\n%s", fname, text) else NULL
    })
  )

  if (length(collected_text) == 0) {
    return(NULL)
  }
  paste(collected_text, collapse = "\n\n")
}

fetch_study_content <- function(pmid, doi = NA) {
  sections <- NULL
  supplements <- NULL

  if (!is.na(pmid) && pmid != "") {
    sections <- fetch_fulltext_sections(pmid)
    supplements <- fetch_supplement_files(pmid)
  }

  if (is.null(sections) && !is.na(doi) && doi != "") {
    sections <- fetch_fulltext_via_doi(doi)
  }

  if (is.null(sections) && is.null(supplements)) {
    return(NULL)
  }

  list(
    results = if (!is.null(sections)) sections$results,
    methods = if (!is.null(sections)) sections$methods,
    tables = if (!is.null(sections)) sections$tables,
    supplements = supplements,
    source = attr(sections, "source") %||% "pmc"
  )
}

.unpaywall_email <- "a.m.mowinckel@psykologi.uio.no"

unpaywall_lookup <- function(doi, email = .unpaywall_email) {
  url <- sprintf("https://api.unpaywall.org/v2/%s?email=%s", doi, email)
  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_timeout(15) |>
      httr2::req_error(is_error = \(r) FALSE) |>
      httr2::req_perform(),
    error = function(e) NULL
  )
  if (is.null(resp) || httr2::resp_status(resp) != 200) return(NULL)
  jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = FALSE)
}

.publisher_xml_url <- function(doi) {
  if (grepl("10\\.1371/", doi)) {
    return(sprintf("https://journals.plos.org/plosone/article/file?id=%s&type=manuscript", doi))
  }
  NULL
}

.parse_publisher_xml <- function(xml_text) {
  doc <- tryCatch(xml2::read_xml(xml_text), error = function(e) NULL)
  if (is.null(doc)) return(NULL)

  extract <- function(section_hints) {
    for (hint in section_hints) {
      nodes <- xml2::xml_find_all(doc, sprintf(
        "//*[contains(translate(@sec-type,'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz'), '%s')]",
        hint
      ))
      if (length(nodes) == 0) {
        nodes <- xml2::xml_find_all(doc, sprintf(
          "//sec[title[contains(translate(., 'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz'), '%s')]]",
          hint
        ))
      }
      if (length(nodes) > 0) {
        return(paste(xml2::xml_text(nodes), collapse = "\n\n"))
      }
    }
    NULL
  }

  results_text <- extract(c("result", "finding"))
  methods_text <- extract(c("method", "material", "procedure"))
  tables_text <- tryCatch({
    tbls <- xml2::xml_find_all(doc, "//table-wrap")
    if (length(tbls) > 0) paste(xml2::xml_text(tbls), collapse = "\n\n") else NULL
  }, error = function(e) NULL)

  if (is.null(results_text) && is.null(methods_text) && is.null(tables_text)) {
    body <- xml2::xml_find_all(doc, "//body")
    if (length(body) > 0) {
      full <- xml2::xml_text(body[[1]])
      if (nchar(full) > 500) {
        results_text <- full
      }
    }
  }

  if (is.null(results_text) && is.null(methods_text) && is.null(tables_text)) {
    return(NULL)
  }

  out <- list(results = results_text, methods = methods_text, tables = tables_text)
  attr(out, "source") <- "publisher_xml"
  out
}

.parse_pdf_text <- function(pdf_path) {
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    cli::cli_alert_warning("Install pdftools for PDF text extraction")
    return(NULL)
  }
  pages <- tryCatch(pdftools::pdf_text(pdf_path), error = function(e) NULL)
  if (is.null(pages) || length(pages) == 0) return(NULL)

  full <- paste(pages, collapse = "\n\n")
  if (nchar(full) < 500) return(NULL)

  find_section <- function(text, patterns) {
    for (pat in patterns) {
      match <- regexpr(
        sprintf("(?i)\\n\\s*%s\\s*\\n", pat),
        text, perl = TRUE
      )
      if (match > 0) {
        start <- match + attr(match, "match.length")
        rest <- substring(text, start)
        end <- regexpr("(?i)\\n\\s*(discussion|conclusion|acknowledge|reference|funding|declaration)\\s*\\n",
                        rest, perl = TRUE)
        if (end > 0) rest <- substring(rest, 1, end - 1)
        return(trimws(rest))
      }
    }
    NULL
  }

  results_text <- find_section(full, c("results?", "findings?"))
  methods_text <- find_section(full, c("methods?", "materials? and methods?", "study design"))

  if (is.null(results_text) && is.null(methods_text)) {
    results_text <- full
  }

  out <- list(results = results_text, methods = methods_text, tables = NULL)
  attr(out, "source") <- "pdf"
  out
}

fetch_fulltext_via_doi <- function(doi) {
  pub_xml_url <- .publisher_xml_url(doi)
  if (!is.null(pub_xml_url)) {
    resp <- tryCatch(
      httr2::request(pub_xml_url) |>
        httr2::req_timeout(15) |>
        httr2::req_error(is_error = \(r) FALSE) |>
        httr2::req_perform(),
      error = function(e) NULL
    )
    if (!is.null(resp) && httr2::resp_status(resp) == 200) {
      ct <- httr2::resp_content_type(resp)
      if (grepl("xml", ct, ignore.case = TRUE)) {
        sections <- .parse_publisher_xml(httr2::resp_body_string(resp))
        if (!is.null(sections)) {
          cli::cli_alert_success("Got publisher XML for {.val {doi}}")
          return(sections)
        }
      }
    }
  }

  oa <- unpaywall_lookup(doi)
  if (is.null(oa) || !isTRUE(oa$is_oa)) return(NULL)

  pdf_urls <- Filter(Negate(is.null), lapply(oa$oa_locations, `[[`, "url_for_pdf"))
  if (length(pdf_urls) == 0) return(NULL)

  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)
  dl_ok <- FALSE
  for (pdf_url in pdf_urls) {
    dl_ok <- tryCatch({
      utils::download.file(pdf_url, tmp, quiet = TRUE, mode = "wb")
      file.exists(tmp) && file.size(tmp) > 1000
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (dl_ok) break
  }
  if (!dl_ok || !file.exists(tmp) || file.size(tmp) < 1000) return(NULL)

  cli::cli_alert_success("Got PDF for {.val {doi}}")
  .parse_pdf_text(tmp)
}

.outcome_harmonization <- list(
  fatigue = c(
    "fatigue", "fatigue severity", "mental fatigue", "physical fatigue",
    "psychosocial fatigue", "exhaustion", "fatigue (multidimensional)",
    "cognitive state fatigue", "fatigue and total symptom burden",
    "energy"
  ),
  physical_function = c(
    "physical function", "physical functioning", "physical performance",
    "functional status", "functioning", "functional health",
    "functional impairment", "functional performance", "daily functioning",
    "physical ability", "physical strength", "muscular endurance",
    "muscle strength", "functional capacity", "exercise capacity",
    "cardiopulmonary fitness", "cardiorespiratory fitness",
    "physical performance capacity", "maximum power output",
    "inspiratory muscle strength", "activity/participation",
    "participation", "routine activities", "health and functioning",
    "symptoms and function", "health status", "health and well-being",
    "overall improvement", "overall symptoms", "functional health"
  ),
  quality_of_life = c(
    "quality of life", "health-related quality of life",
    "quality of life and questionnaires", "health utility",
    "mental and physical health"
  ),
  depression = c("depression", "mood"),
  anxiety = c("anxiety"),
  anxiety_depression = c("anxiety and depression"),
  cognition = c(
    "cognition", "cognitive function", "cognitive problems",
    "cognitive complaints", "brain fog", "neurocognitive function",
    "working memory", "cognitive state fatigue"
  ),
  sleep = c("sleep", "sleep quality", "insomnia"),
  pain = c("pain", "pain intensity", "chronic pain"),
  pem = c(
    "post-exertional malaise", "post-exertional symptom exacerbation",
    "fatigue, PEM, and orthostatic intolerance recovery"
  ),
  long_covid_risk = c(
    "Long COVID incidence", "long COVID risk", "long COVID (overall)",
    "PASC (long COVID)", "post-acute sequelae (overall)",
    "long COVID symptoms", "long COVID symptom count",
    "long COVID symptom severity", "PASC symptom severity",
    "PCS symptom severity"
  ),
  psychological_wellbeing = c(
    "perceived stress", "self-efficacy", "emotional wellbeing",
    "subjective distress", "post-traumatic stress", "PTSD symptoms",
    "hope", "optimism", "kinesiophobia", "mindfulness",
    "affect", "loneliness", "problem awareness", "problem-solving",
    "interpretation bias", "illness management and adjustment",
    "symptom understanding and control"
  ),
  respiratory = c(
    "dyspnea", "lung function", "pulmonary function",
    "respiratory symptoms"
  ),
  autonomic = c(
    "autonomic function", "autonomic symptoms",
    "heart rate variability", "orthostatic cardiovascular response",
    "sinus tachycardia risk", "sinus bradycardia risk",
    "arrhythmia risk", "atrial fibrillation risk"
  ),
  mortality = c("mortality", "hospitalization"),
  symptoms = c(
    "symptoms", "symptom burden", "symptom frequency",
    "symptom intensity", "symptom improvement", "somatic symptoms",
    "side effects", "treatment response"
  )
)

harmonize_outcome <- function(outcome_domain) {
  if (is.na(outcome_domain)) return(NA_character_)
  od_lower <- tolower(outcome_domain)
  for (group in names(.outcome_harmonization)) {
    if (od_lower %in% tolower(.outcome_harmonization[[group]])) {
      return(group)
    }
  }
  od_lower
}

harmonize_outcomes <- function(outcome_domains) {
  vapply(outcome_domains, harmonize_outcome, character(1), USE.NAMES = FALSE)
}

extraction_to_df <- function(extracted_list, study_id, search_topic) {
  rows <- lapply(extracted_list, function(item) {
    item[vapply(item, is.null, logical(1))] <- NA
    as.data.frame(item, stringsAsFactors = FALSE)
  })
  all_cols <- unique(unlist(lapply(rows, names)))
  rows <- lapply(rows, function(df) {
    df[setdiff(all_cols, names(df))] <- NA
    df[, all_cols, drop = FALSE]
  })
  df <- do.call(rbind, rows)
  df$study_id <- study_id
  df$search_topic <- search_topic
  df$llm_model <- llm_model_name()
  sanitize_extracted(df)
}

write_extracted_data <- function(new_data, domain, replace_study = FALSE) {
  if (is.null(new_data) || !is.data.frame(new_data) || nrow(new_data) == 0) return(invisible(NULL))

  files <- list(
    treatment_cbt_psych = here::here(
      "data/extracted/treatment_cbt_psych.parquet"
    ),
    treatment_biomedical = here::here(
      "data/extracted/treatment_biomedical.parquet"
    ),
    biomarkers = here::here("data/extracted/biomarkers.parquet"),
    outcomes = here::here("data/extracted/outcomes.parquet"),
    systematic_reviews = here::here("data/extracted/systematic_reviews.parquet"),
    grade = here::here("data/extracted/grade.parquet")
  )

  path <- files[[domain]]
  if (is.null(path)) {
    return(invisible(NULL))
  }

  if (!"extraction_source" %in% names(new_data)) {
    new_data$extraction_source <- "consensus"
  }

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  if (file.exists(path)) {
    existing <- as.data.frame(arrow::read_parquet(path))
    if (!"extraction_source" %in% names(existing)) {
      existing$extraction_source <- NA_character_
    }
    for (col in setdiff(names(new_data), names(existing))) {
      existing[[col]] <- NA
    }
    for (col in setdiff(names(existing), names(new_data))) {
      new_data[[col]] <- NA
    }
    new_study_ids <- unique(new_data$study_id)
    if (replace_study) {
      existing <- existing[!existing$study_id %in% new_study_ids, , drop = FALSE]
    } else {
      existing <- existing[
        !existing$study_id %in% new_study_ids |
        (!is.na(existing$extraction_source) &
         existing$extraction_source == "human_verified"),
        ,
        drop = FALSE
      ]
      new_data <- new_data[
        !new_data$study_id %in%
          existing$study_id[!is.na(existing$extraction_source) &
                            existing$extraction_source == "human_verified"],
        ,
        drop = FALSE
      ]
    }
    new_data <- rbind(existing, new_data[, names(existing), drop = FALSE])
  }

  dedup_cols <- intersect(
    c("study_id", "intervention", "outcome_measure", "mean_intervention", "effect_size"),
    names(new_data)
  )
  new_data <- new_data[!duplicated(new_data[, dedup_cols]), , drop = FALSE]

  arrow::write_parquet(new_data, path)
}

.qa_domain_names <- list(
  rob2 = c(
    d1_randomisation = "randomisation", d2_deviations = "deviations",
    d3_missing_data = "missing_data", d4_outcome_measurement = "outcome_measurement",
    d5_selection_reporting = "selection_reporting", overall_rob = "overall"
  ),
  robins = c(
    d1_confounding = "confounding", d2_selection = "selection",
    d3_classification = "classification", d4_deviations = "deviations",
    d5_missing_data = "missing_data", d6_outcome_measurement = "outcome_measurement",
    d7_reporting = "reporting", overall_rob = "overall"
  ),
  amstar2 = c(
    d1_pico = "pico", d2_protocol = "protocol", d3_study_design = "study_design",
    d4_search_strategy = "search_strategy", d7_rob_assessment = "rob_assessment",
    d9_synthesis = "synthesis", d11_rob_impact = "rob_impact",
    d13_rob_discussion = "rob_discussion", d15_publication_bias = "publication_bias",
    overall_confidence = "overall"
  ),
  casp_qualitative = c(
    d1_clear_aims = "clear_aims", d2_methodology = "methodology",
    d3_design = "design", d4_recruitment = "recruitment",
    d5_data_collection = "data_collection", d6_reflexivity = "reflexivity",
    d7_ethical = "ethical", d8_analysis = "analysis",
    d9_findings = "findings", d10_value = "value",
    overall_quality = "overall"
  )
)

quality_to_long <- function(qa_result, study_id, search_topic, rob_tool) {
  mapping <- .qa_domain_names[[rob_tool]]
  if (is.null(mapping)) return(data.frame())

  rationale_map <- setNames(
    paste0(sub("^(d\\d+_|overall_).*", "\\1", names(mapping)), "rationale"),
    names(mapping)
  )
  rationale_map[grepl("^overall", names(rationale_map))] <- "overall_rationale"

  rows <- lapply(names(mapping), function(col) {
    judgement <- qa_result[[col]]
    if (is.null(judgement) || is.na(judgement)) return(NULL)
    rat_col <- rationale_map[[col]]
    rationale <- qa_result[[rat_col]]
    data.frame(
      study_id = study_id,
      search_topic = search_topic,
      rob_tool = rob_tool,
      domain_name = mapping[[col]],
      judgement = judgement,
      rationale = if (is.null(rationale)) NA_character_ else rationale,
      llm_model = llm_model_name(),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, Filter(Negate(is.null), rows))
}

write_quality_data <- function(
  new_data,
  path = here::here("data/extracted/quality_ratings.parquet")
) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  expected_cols <- c("study_id", "search_topic", "rob_tool",
                     "domain_name", "judgement", "rationale", "llm_model")

  for (col in setdiff(expected_cols, names(new_data))) {
    new_data[[col]] <- NA_character_
  }
  new_data <- new_data[, intersect(expected_cols, names(new_data)), drop = FALSE]

  if (file.exists(path)) {
    existing <- as.data.frame(arrow::read_parquet(path))
    for (col in setdiff(names(new_data), names(existing))) {
      existing[[col]] <- NA
    }
    for (col in setdiff(names(existing), names(new_data))) {
      new_data[[col]] <- NA
    }
    new_data <- rbind(existing, new_data[, names(existing), drop = FALSE])
  }

  arrow::write_parquet(new_data, path)
}

write_grade_data <- function(
  new_data,
  path = here::here("data/extracted/grade.parquet")
) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  if (file.exists(path)) {
    existing <- as.data.frame(arrow::read_parquet(path))
    for (col in setdiff(names(new_data), names(existing))) {
      existing[[col]] <- NA
    }
    for (col in setdiff(names(existing), names(new_data))) {
      new_data[[col]] <- NA
    }
    new_data <- rbind(existing, new_data[, names(existing), drop = FALSE])
  }

  arrow::write_parquet(new_data, path)
}

log_extraction <- function(
  study_id,
  topic,
  consensus,
  n_attempts,
  n_parsed,
  n_agreeing,
  had_supplement,
  fetch_status = NA_character_,
  log_file = here::here("data/extraction-log.csv")
) {
  entry <- data.frame(
    date = as.character(Sys.Date()),
    study_id = study_id,
    topic = topic,
    consensus = consensus,
    n_attempts = as.integer(n_attempts),
    n_parsed = as.integer(n_parsed),
    n_agreeing = as.integer(n_agreeing),
    had_supplement = had_supplement,
    fetch_status = fetch_status,
    stringsAsFactors = FALSE
  )

  if (file.exists(log_file)) {
    entry <- rbind(read.csv(log_file, stringsAsFactors = FALSE), entry)
  }

  write.csv(entry, log_file, row.names = FALSE)
}

.extract_one <- function(
  i,
  studies,
  search_topic,
  n_attempts,
  min_agreement,
  try_supplements
) {
  cli::cli_progress_step(
    "[{i}/{nrow(studies)}] Extracting: {.val {studies$title[i]}}"
  )

  fulltext <- NULL
  had_fulltext <- FALSE
  fetch_status <- "no_id"
  has_id <- (!is.na(studies$pmid[i]) && studies$pmid[i] != "") ||
    (!is.na(studies$doi[i]) && studies$doi[i] != "")
  if (try_supplements && has_id) {
    cli::cli_alert_info("Fetching full text, tables, and supplements...")
    fulltext <- tryCatch(
      fetch_study_content(
        pmid = studies$pmid[i] %||% NA,
        doi = studies$doi[i] %||% NA
      ),
      error = function(e) {
        cli::cli_alert_warning("Fulltext fetch failed: {conditionMessage(e)}")
        NULL
      }
    )
    had_fulltext <- !is.null(fulltext)
    if (had_fulltext) {
      parts <- c(
        if (!is.null(fulltext$results)) "results",
        if (!is.null(fulltext$methods)) "methods",
        if (!is.null(fulltext$tables)) "tables",
        if (!is.null(fulltext$supplements)) "supplements"
      )
      fetch_status <- paste(parts, collapse = "+")
      cli::cli_alert_success("Found: {paste(parts, collapse = ', ')}")
    } else {
      fetch_status <- "fetch_failed"
    }
  }

  result <- extract_study_consensus(
    title = studies$title[i],
    abstract = studies$abstract[i],
    search_topic = search_topic,
    fulltext_sections = fulltext,
    n_attempts = n_attempts,
    min_agreement = min_agreement
  )

  study_type <- studies$llm_study_type[i] %||% NA_character_
  quality_result <- list(data = NULL, consensus = FALSE, n_agreeing = 0L)
  if (!is.na(study_type) && had_fulltext) {
    cli::cli_alert_info("Assessing study quality from methods section...")
    quality_result <- assess_quality_consensus(
      title = studies$title[i],
      abstract = studies$abstract[i],
      methods_text = fulltext$methods,
      study_type = study_type
    )
    if (quality_result$consensus) {
      cli::cli_alert_success(
        "Quality assessment consensus ({quality_result$n_agreeing} agreeing)"
      )
      quality_df <- quality_to_long(
        quality_result$data,
        study_id = studies$study_id[i],
        search_topic = search_topic,
        rob_tool = quality_result$rob_tool
      )
      write_quality_data(quality_df)
    }
  }

  log_extraction(
    study_id = studies$study_id[i],
    topic = search_topic,
    consensus = result$consensus,
    n_attempts = result$n_attempts,
    n_parsed = result$n_parsed,
    n_agreeing = result$n_agreeing,
    had_supplement = had_fulltext,
    fetch_status = fetch_status
  )

  df <- NULL
  if (result$consensus && !is.null(result$data)) {
    cli::cli_alert_success(
      "Consensus reached ({result$n_agreeing}/{result$n_attempts} agreeing)"
    )
    df <- extraction_to_df(result$data, studies$study_id[i], search_topic)
    write_extracted_data(df, search_topic)
  } else {
    cli::cli_alert_warning(
      "No consensus ({result$n_agreeing}/{result$n_attempts} agreeing, {result$n_parsed} parsed)"
    )
  }

  list(
    df = df,
    consensus = result$consensus,
    n_parsed = result$n_parsed,
    study_id = studies$study_id[i]
  )
}

already_attempted <- function(study_ids, topic, max_attempts = 2,
                              log_file = here::here("data/extraction-log.csv")) {
  if (!file.exists(log_file)) return(rep(FALSE, length(study_ids)))
  log <- read.csv(log_file, stringsAsFactors = FALSE)
  log <- log[log$topic == topic, ]
  if (nrow(log) == 0) return(rep(FALSE, length(study_ids)))

  vapply(study_ids, function(sid) {
    entries <- log[log$study_id == sid, ]
    if (nrow(entries) == 0) return(FALSE)
    any(entries$consensus) || nrow(entries) >= max_attempts
  }, logical(1))
}

extract_batch <- function(
  studies,
  search_topic,
  n_attempts = 5,
  min_agreement = 3,
  try_supplements = TRUE,
  max_consecutive_failures = 3,
  on_success = NULL
) {
  skip <- already_attempted(studies$study_id, search_topic)
  if (any(skip)) {
    cli::cli_alert_info("Skipping {sum(skip)} already-attempted stud{?y/ies}")
    studies <- studies[!skip, , drop = FALSE]
  }
  if (nrow(studies) == 0) {
    cli::cli_alert_success("Nothing to extract.")
    return(list())
  }

  extracted <- list()
  consecutive_failures <- 0L
  n_attempted <- 0L

  for (i in seq_len(nrow(studies))) {
    n_attempted <- i
    result <- .extract_one(
      i, studies, search_topic,
      n_attempts, min_agreement, try_supplements
    )

    if (!is.null(result$df)) {
      extracted <- c(extracted, list(result$df))
      consecutive_failures <- 0L
      if (!is.null(on_success)) on_success(result$study_id)
    } else if (result$n_parsed == 0) {
      consecutive_failures <- consecutive_failures + 1L
      if (consecutive_failures >= max_consecutive_failures) {
        cli::cli_alert_danger(
          "Stopping: {max_consecutive_failures} consecutive studies with 0 LLM responses (rate limited?)"
        )
        break
      }
    } else {
      consecutive_failures <- 0L
    }
  }

  cli::cli_alert_info(
    "Extraction complete: {length(extracted)}/{n_attempted} succeeded"
  )
  extracted
}
