library(ggplot2)
library(dplyr)
library(forcats)
library(patchwork)

theme_horingssvar <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "grey40"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.margin = margin(10, 15, 10, 10)
  )

# --- Figure 1: Wormgoor & Rodenburg 2023 ---
# Health worsening by PEM-focus (Figure 3, significant results only)

wormgoor <- tibble::tribble(
  ~timepoint                                   , ~group              , ~pct , ~sig ,
  "Hospital\n2 weeks post"                     , "PEM not addressed" , 35.5 , FALSE,
  "Hospital\n2 weeks post"                     , "PEM addressed"     , 17.5 , FALSE,
  "Hospital\n3-6 months post"                  , "PEM not addressed" , 32.3 , FALSE,
  "Hospital\n3-6 months post"                  , "PEM addressed"     , 15.5 , FALSE,
  "Hospital\nat survey"                        , "PEM not addressed" , 35.5 , FALSE,
  "Hospital\nat survey"                        , "PEM addressed"     , 17.2 , FALSE,
  "Rehabilitation\ndirectly after ***"         , "PEM not addressed" , 35.9 , TRUE ,
  "Rehabilitation\ndirectly after ***"         , "PEM addressed"     , 16.2 , TRUE ,
  "Rehabilitation\n1 month after ***"          , "PEM not addressed" , 54.5 , TRUE ,
  "Rehabilitation\n1 month after ***"          , "PEM addressed"     , 29.0 , TRUE
) |>
  mutate(
    timepoint = factor(timepoint, levels = c(
      "Rehabilitation\n1 month after ***",
      "Rehabilitation\ndirectly after ***",
      "Hospital\nat survey",
      "Hospital\n3-6 months post",
      "Hospital\n2 weeks post"
    )),
    group = factor(group, levels = c("PEM addressed", "PEM not addressed"))
  )

wormgoor_segments <- wormgoor |>
  summarise(xmin = min(pct), xmax = max(pct), .by = timepoint)

wormgoor <- wormgoor |>
  mutate(is_min = pct == min(pct), .by = timepoint)

p_wormgoor <- ggplot(wormgoor, aes(y = timepoint)) +
  geom_linerange(
    data = wormgoor_segments,
    aes(xmin = xmin, xmax = xmax),
    linewidth = 0.6,
    colour = "black"
  ) +
  geom_point(aes(x = pct, colour = group), size = 4) +
  geom_text(
    aes(
      x = pct,
      label = paste0(pct, "%"),
      hjust = ifelse(is_min, 1.3, -0.3)
    ),
    size = 4,
    show.legend = FALSE
  ) +
  scale_x_continuous(
    limits = c(0, 65),
    labels = \(x) paste0(x, "%")
  ) +
  scale_colour_manual(
    values = c(
      "PEM addressed" = "#0072B2",
      "PEM not addressed" = "#D55E00"
    ),
    name = NULL
  ) +
  guides(colour = guide_legend(override.aes = list(size = 4))) +
  labs(
    title = "Health worsening by PEM-focus in care",
    caption = "Data: Wormgoor & Rodenburg (2023), Figure 3. Hospital n = 88-89; rehabilitation n = 788.\n*** p < 0.001 (Mann-Whitney U-test). Hospital comparisons not statistically significant.",
    x = NULL,
    y = NULL
  ) +
  theme_horingssvar

ggsave(
  "fig-wormgoor-pem-care.png",
  p_wormgoor,
  width = 9,
  height = 5,
  dpi = 300,
  bg = "white"
)


# --- Figure 2: Eckey et al. 2025 (PNAS) ---
# Net Assessment Scores for selected treatments

eckey <- tibble::tribble(
  ~treatment                    , ~nas  ,
  "Pacing"                      ,  75.2 ,
  "Fluids/electrolytes"         ,  68.6 ,
  "Compression stockings"       ,  62.0 ,
  "Antihistamines"              ,  51.6 ,
  "Nattokinase/lumbrokinase"    ,  49.9 ,
  "Low-dose naltrexone"         ,  49.4 ,
  "Beta-blockers or ivabradine" ,  47.1 ,
  "Melatonin"                   ,  43.3 ,
  "Coenzyme Q10"                ,  33.0 ,
  "Vitamin C (oral, reference)" ,  24.1 ,
  "CBT"                         ,   2.1 ,
  "Graded exercise therapy"     , -72.2
) |>
  mutate(
    treatment = fct_rev(fct_inorder(treatment)),
    direction = if_else(nas >= 0, "benefit", "harm")
  )

p_eckey <- ggplot(eckey, aes(x = nas, y = treatment)) +
  geom_vline(xintercept = 0, linewidth = 0.4) +
  geom_linerange(
    aes(xmin = 0, xmax = nas),
    linewidth = 0.6,
    colour = "black"
  ) +
  geom_point(
    aes(colour = direction),
    size = 4,
    show.legend = FALSE
  ) +
  geom_text(
    aes(
      label = ifelse(nas >= 0, paste0("+", nas), nas),
      hjust = ifelse(nas >= 0, -0.15, 1.15)
    ),
    size = 4,
    show.legend = FALSE
  ) +
  scale_x_continuous(limits = c(-85, 90)) +
  scale_colour_manual(
    values = c("benefit" = "#0072B2", "harm" = "#D55E00"),
    guide = "none"
  ) +
  labs(
    title = "Patient-reported treatment outcomes in ME/CFS and Long COVID",
    subtitle = "Net Assessment Score (NAS): % reporting benefit minus % reporting harm.\nVitamin C (oral) served as the reference treatment.",
    caption = "Data: Eckey et al. (2025, PNAS). Survey of 3,925 patients across 150+ treatments.",
    x = "Net Assessment Score (%)",
    y = NULL
  ) +
  theme_horingssvar

ggsave(
  "fig-eckey-treatments.png",
  p_eckey,
  width = 8,
  height = 5.5,
  dpi = 300,
  bg = "white"
)


# --- Figure 3: CO-FLOW PEM vs non-PEM ---

coflow <- tibble::tribble(
  ~measure                                    , ~group   , ~value ,
  "Self-reported\nrecovery"                   , "PEM"    ,     55 ,
  "Self-reported\nrecovery"                   , "No PEM" ,     88 ,
  "Concurrent fatigue +\ncognitive + dyspnea" , "PEM"    ,     42 ,
  "Concurrent fatigue +\ncognitive + dyspnea" , "No PEM" ,      6
) |>
  mutate(
    measure = fct_rev(fct_inorder(measure)),
    group = factor(group, levels = c("No PEM", "PEM"))
  )

coflow_segments <- coflow |>
  summarise(xmin = min(value), xmax = max(value), .by = measure)

coflow <- coflow |>
  mutate(is_min = value == min(value), .by = measure)

p_coflow <- ggplot(coflow, aes(y = measure)) +
  geom_linerange(
    data = coflow_segments,
    aes(xmin = xmin, xmax = xmax),
    linewidth = 0.6,
    colour = "black"
  ) +
  geom_point(aes(x = value, colour = group), size = 4) +
  geom_text(
    aes(
      x = value,
      label = paste0(value, "%"),
      hjust = ifelse(is_min, 1.3, -0.3)
    ),
    size = 4,
    show.legend = FALSE
  ) +
  scale_x_continuous(
    limits = c(0, 100),
    labels = \(x) paste0(x, "%")
  ) +
  scale_colour_manual(
    values = c("No PEM" = "#0072B2", "PEM" = "#D55E00"),
    name = NULL
  ) +
  guides(colour = guide_legend(override.aes = list(size = 4))) +
  labs(
    title = "Post-exertional malaise predicts worse recovery at 3 years",
    caption = "Data: CO-FLOW study (2025). Prospective cohort, n = 292 post-hospitalisation COVID-19 patients.",
    x = NULL,
    y = NULL
  ) +
  theme_horingssvar

ggsave(
  "fig-coflow-pem.png",
  p_coflow,
  width = 8,
  height = 3.5,
  dpi = 300,
  bg = "white"
)

message("All figures saved.")
