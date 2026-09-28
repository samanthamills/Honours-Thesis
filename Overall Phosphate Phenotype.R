# ============================================================
# OVERALL PHOSPHATE-SOLUBILISATION PHENOTYPE
# ORIGINAL PHOSPHATE MEDIA ONLY
# ============================================================

# ------------------------------------------------------------
# 1. LOAD PACKAGES
# ------------------------------------------------------------

library(readxl)
library(dplyr)
library(tidyr)
library(writexl)

# ------------------------------------------------------------
# 2. SET DATA LOCATION
# ------------------------------------------------------------

data_dir <- "/Users/samanthamills/Honours Project (R Studio)/Lab Result Graphing/Data"

file_path <- file.path(
  data_dir,
  "Raw_Phosphate_Results.xlsx"
)

# ------------------------------------------------------------
# 3. READ ORIGINAL PHOSPHATE MEDIA
# ------------------------------------------------------------

al_ph <- read_excel(
  file_path,
  sheet = "Al-ph"
)
ca_ph <- read_excel(
  file_path,
  sheet = "Ca-ph"
)
fe_ph <- read_excel(
  file_path,
  sheet = "Fe-ph"
)

# ------------------------------------------------------------
# 4. CALCULATE MEAN VALUES FOR EACH STRAIN
# ------------------------------------------------------------

# Al-ph
al_summary <- al_ph %>%
  rowwise() %>%
  mutate(
    `Al Colony size` = mean(
      c_across(starts_with("Colony size")),
      na.rm = TRUE
    ),
    `Al Organic acid` = mean(
      c_across(starts_with("Organic Acid Production")),
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>%
  select(
    BW_Strain,
    `Al Colony size`,
    `Al Organic acid`
  )

# Ca-ph
ca_summary <- ca_ph %>%
  rowwise() %>%
  mutate(
    `Ca Colony size` = mean(
      c_across(starts_with("Colony size")),
      na.rm = TRUE
    ),
    `Ca Zone of clearing` = mean(
      c_across(starts_with("Zone of clearing")),
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>%
  select(
    BW_Strain,
    `Ca Colony size`,
    `Ca Zone of clearing`
  )

# Fe-ph
fe_summary <- fe_ph %>%
  rowwise() %>%
  mutate(
    `Fe Colony size` = mean(
      c_across(starts_with("Colony size")),
      na.rm = TRUE
    ),
    `Fe Organic acid` = mean(
      c_across(starts_with("Organic Acid Production")),
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>%
  select(
    BW_Strain,
    `Fe Colony size`,
    `Fe Organic acid`
  )

# ------------------------------------------------------------
# 5. COMBINE ALL SIX TRAITS
# ------------------------------------------------------------

trait_data <- al_summary %>%
  full_join(
    ca_summary,
    by = "BW_Strain"
  ) %>%
  full_join(
    fe_summary,
    by = "BW_Strain"
  )

# ------------------------------------------------------------
# 6. CONVERT TO LONG FORMAT
# ------------------------------------------------------------
#
# This creates:
# BW_Strain | Trait | Value
#
# IMPORTANT:
# Zero values are retained as zero.
# Only NA values are treated as missing.
# ------------------------------------------------------------

trait_long <- trait_data %>%
  pivot_longer(
    cols = -BW_Strain,
    names_to = "Trait",
    values_to = "Value"
  )

# ------------------------------------------------------------
# 7. SCALE EACH TRAIT INDEPENDENTLY
# ------------------------------------------------------------
#
# Each trait is scaled relative to its own maximum.
#
# 0    = 0
# >0   = relative value above zero
# NA   = remains NA
#
# This means organic acid, colony size and clearing-zone
# measurements do not compete based on their raw units.
# ------------------------------------------------------------

trait_long <- trait_long %>%
  group_by(Trait) %>%
  mutate(
    Trait_Max = ifelse(
      all(is.na(Value)),
      NA_real_,
      max(Value, na.rm = TRUE)
    ),
    Relative_Value = case_when(
      is.na(Value) ~ NA_real_,
      Value == 0 ~ 0,
      is.na(Trait_Max) ~ NA_real_,
      Trait_Max == 0 ~ 0,
      TRUE ~ 0.001 + (Value / Trait_Max) * 0.999
    )
  ) %>%
  ungroup()

# ------------------------------------------------------------
# 8. CALCULATE OVERALL SCORE
# ------------------------------------------------------------
#
# All six traits are given equal weighting.
#
# A genuine zero contributes 0.
# Missing values (NA) are excluded.
# ------------------------------------------------------------

overall_scores <- trait_long %>%
  group_by(BW_Strain) %>%
  summarise(
    Overall_Score = ifelse(
      sum(!is.na(Relative_Value)) > 0,
      mean(Relative_Value, na.rm = TRUE),
      NA_real_
    ),
    Number_of_Measurements =
      sum(!is.na(Relative_Value)),
    
    .groups = "drop"
  )

# ------------------------------------------------------------
# 9. CREATE LOW / MEDIUM / HIGH CATEGORIES
# ------------------------------------------------------------

minimum_measurements <- 4

# Calculate the lower and upper tertile cut-offs
valid_scores <- overall_scores %>%
  filter(
    Number_of_Measurements >= minimum_measurements,
    !is.na(Overall_Score)
  )
low_cutoff <- quantile(
  valid_scores$Overall_Score,
  probs = 1/3,
  na.rm = TRUE
)
high_cutoff <- quantile(
  valid_scores$Overall_Score,
  probs = 2/3,
  na.rm = TRUE
)

# Apply the categories
overall_scores <- overall_scores %>%
  mutate(
    Phosphate_Solubilisation_Rating = case_when(
      Number_of_Measurements < minimum_measurements ~
        "Insufficient data",
      is.na(Overall_Score) ~
        "Insufficient data",
      Overall_Score <= low_cutoff ~
        "Low",
      Overall_Score <= high_cutoff ~
        "Medium",
      TRUE ~
        "High"
    )
  )

# ------------------------------------------------------------
# 10. ADD INDIVIDUAL TRAIT VALUES
# ------------------------------------------------------------

relative_values <- trait_long %>%
  select(
    BW_Strain,
    Trait,
    Relative_Value
  ) %>%
  pivot_wider(
    names_from = Trait,
    values_from = Relative_Value
  )
overall_scores <- overall_scores %>%
  left_join(
    relative_values,
    by = "BW_Strain"
  )

# ------------------------------------------------------------
# 11. ORDER BY OVERALL SCORE
# ------------------------------------------------------------

overall_scores <- overall_scores %>%
  arrange(
    desc(Overall_Score)
  )

# ------------------------------------------------------------
# 12. DISPLAY RESULTS
# ------------------------------------------------------------

print(overall_scores)

# ------------------------------------------------------------
# 13. SAVE RESULTS AS EXCEL
# ------------------------------------------------------------

write_xlsx(
  list(
    "Final Ratings" = overall_scores,
    "Mean Measurements" = trait_data,
    "Scaling Calculations" = trait_long
  ),
  file.path(
    "/Users/samanthamills/Honours Project (R Studio)/Lab Result Graphing/Output",
    "Original_Media_Phosphate_Solubilisation_Ratings.xlsx"
  )
)