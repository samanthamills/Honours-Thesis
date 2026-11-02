# ============================================================
# PHOSPHATE SOLUBILISATION INDEX
# High / Medium / Low + Overall Ranking
# ============================================================

# ------------------
# 1. Setting up
# ------------------

setwd("/Users/samanthamills/Honours Project (R Studio)/Lab Result Graphing")

library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)


# ------------------
# 2. File locations
# ------------------

input_file <- "Data/Raw_Phosphate_Results.xlsx"

output_file <- "Output/Phosphate_Solubilisation_Index_High_Medium_Low.xlsx"


# ------------------
# 3. Sheet names
# ------------------

# Change these if your actual sheet names are different

al_sheet <- "Al-ph"
ca_sheet <- "Ca-ph"
fe_sheet <- "Fe-ph"


# ============================================================
# 4. ALUMINIUM / IRON INDEX
# ============================================================
#
# Index =
#
# (Colony diameter + Organic acid production)
# --------------------------------------------
#             Colony diameter
#
# ============================================================

calculate_al_fe_index <- function(sheet_name, phosphate_type) {
  
  data <- read_excel(input_file, sheet = sheet_name)
  
  long_data <- data %>%
    select(
      BW_Strain,
      starts_with("Colony size"),
      starts_with("Organic Acid Production")
    ) %>%
    pivot_longer(
      cols = -BW_Strain,
      names_to = c(".value", "Replicate"),
      names_pattern = "(Colony size|Organic Acid Production)([1-6])"
    )
  
  index_data <- long_data %>%
    mutate(
      Index = ifelse(
        is.na(`Colony size`) |
          is.na(`Organic Acid Production`) |
          `Colony size` == 0,
        NA,
        (`Colony size` + `Organic Acid Production`) /
          `Colony size`
      ),
      Phosphate = phosphate_type
    )
  
  strain_summary <- index_data %>%
    group_by(BW_Strain, Phosphate) %>%
    summarise(
      Mean_Index = mean(Index, na.rm = TRUE),
      SD_Index = sd(Index, na.rm = TRUE),
      N = sum(!is.na(Index)),
      .groups = "drop"
    )
  
  return(list(
    replicate_data = index_data,
    summary = strain_summary
  ))
}


# ============================================================
# 5. CALCIUM INDEX
# ============================================================
#
# Index =
#
# Colony diameter + (2 × Zone of clearing)
# -----------------------------------------
#             Colony diameter
#
# ============================================================

calculate_ca_index <- function(sheet_name) {
  
  data <- read_excel(input_file, sheet = sheet_name)
  
  long_data <- data %>%
    select(
      BW_Strain,
      starts_with("Colony size"),
      starts_with("Zone of clearing")
    ) %>%
    pivot_longer(
      cols = -BW_Strain,
      names_to = c(".value", "Replicate"),
      names_pattern = "(Colony size|Zone of clearing)([1-6])"
    )
  
  index_data <- long_data %>%
    mutate(
      Index = ifelse(
        is.na(`Colony size`) |
          is.na(`Zone of clearing`) |
          `Colony size` == 0,
        NA,
        (`Colony size` + (2 * `Zone of clearing`)) /
          `Colony size`
      ),
      Phosphate = "Ca"
    )
  
  strain_summary <- index_data %>%
    group_by(BW_Strain, Phosphate) %>%
    summarise(
      Mean_Index = mean(Index, na.rm = TRUE),
      SD_Index = sd(Index, na.rm = TRUE),
      N = sum(!is.na(Index)),
      .groups = "drop"
    )
  
  return(list(
    replicate_data = index_data,
    summary = strain_summary
  ))
}


# ============================================================
# 6. Calculate all three phosphate indices
# ============================================================

Al_results <- calculate_al_fe_index(
  al_sheet,
  "Al"
)

Ca_results <- calculate_ca_index(
  ca_sheet
)

Fe_results <- calculate_al_fe_index(
  fe_sheet,
  "Fe"
)


# ============================================================
# 7. Combine individual phosphate results
# ============================================================

all_summary <- bind_rows(
  Al_results$summary,
  Ca_results$summary,
  Fe_results$summary
)


# ============================================================
# 8. Rank strains within each phosphate type
# ============================================================

all_summary <- all_summary %>%
  group_by(Phosphate) %>%
  arrange(desc(Mean_Index), .by_group = TRUE) %>%
  mutate(
    Phosphate_Rank = row_number(),
    Total_Strains = n(),
    Classification = case_when(
      Phosphate_Rank <= ceiling(Total_Strains / 3) ~ "High",
      Phosphate_Rank <= ceiling(2 * Total_Strains / 3) ~ "Medium",
      TRUE ~ "Low"
    )
  ) %>%
  ungroup()


# ============================================================
# 9. Create wide-format phosphate results
# ============================================================
#
# This gives one row per strain with:
#
# Al Mean Index
# Ca Mean Index
# Fe Mean Index
#
# ============================================================

overall_data <- all_summary %>%
  select(
    BW_Strain,
    Phosphate,
    Mean_Index
  ) %>%
  pivot_wider(
    names_from = Phosphate,
    values_from = Mean_Index,
    names_prefix = "Mean_Index_"
  )


# ============================================================
# 10. Calculate OVERALL INDEX
# ============================================================
#
# Overall Index =
#
# (Al + Ca + Fe) / 3
#
# na.rm = TRUE means that if one phosphate source is missing,
# the available phosphate sources can still contribute.
#
# ============================================================

overall_data <- overall_data %>%
  mutate(
    Overall_Index = rowMeans(
      select(
        .,
        Mean_Index_Al,
        Mean_Index_Ca,
        Mean_Index_Fe
      ),
      na.rm = TRUE
    ),
    
    Number_of_Phosphate_Types = rowSums(
      !is.na(
        select(
          .,
          Mean_Index_Al,
          Mean_Index_Ca,
          Mean_Index_Fe
        )
      )
    )
  )


# ============================================================
# 11. Overall ranking
# ============================================================

overall_data <- overall_data %>%
  arrange(desc(Overall_Index)) %>%
  mutate(
    Overall_Rank = row_number(),
    Total_Strains = n()
  )


# ============================================================
# 12. Overall High / Medium / Low classification
# ============================================================

overall_data <- overall_data %>%
  mutate(
    Overall_Classification = case_when(
      Overall_Rank <= ceiling(Total_Strains / 3) ~ "High",
      Overall_Rank <= ceiling(2 * Total_Strains / 3) ~ "Medium",
      TRUE ~ "Low"
    )
  )


# ============================================================
# 13. Add individual phosphate classifications
# ============================================================

classification_wide <- all_summary %>%
  select(
    BW_Strain,
    Phosphate,
    Phosphate_Rank,
    Classification
  ) %>%
  pivot_wider(
    names_from = Phosphate,
    values_from = c(Phosphate_Rank, Classification),
    names_sep = "_"
  )


# ============================================================
# 14. Final overall table
# ============================================================

overall_results <- overall_data %>%
  left_join(
    classification_wide,
    by = "BW_Strain"
  ) %>%
  select(
    BW_Strain,
    
    Mean_Index_Al,
    Phosphate_Rank_Al,
    Classification_Al,
    
    Mean_Index_Ca,
    Phosphate_Rank_Ca,
    Classification_Ca,
    
    Mean_Index_Fe,
    Phosphate_Rank_Fe,
    Classification_Fe,
    
    Overall_Index,
    Overall_Rank,
    Overall_Classification,
    
    Number_of_Phosphate_Types
  ) %>%
  arrange(Overall_Rank)


# ============================================================
# 15. Replicate-level results
# ============================================================

all_replicates <- bind_rows(
  Al_results$replicate_data,
  Ca_results$replicate_data,
  Fe_results$replicate_data
)


# ============================================================
# 16. Create separate overall classification tables
# ============================================================

high_overall <- overall_results %>%
  filter(Overall_Classification == "High")

medium_overall <- overall_results %>%
  filter(Overall_Classification == "Medium")

low_overall <- overall_results %>%
  filter(Overall_Classification == "Low")


# ============================================================
# 17. Save everything to Excel
# ============================================================

write.xlsx(
  list(
    "Overall Results" = overall_results,
    "High Overall" = high_overall,
    "Medium Overall" = medium_overall,
    "Low Overall" = low_overall,
    "Individual Phosphate" = all_summary,
    "Replicate Indices" = all_replicates
  ),
  output_file,
  overwrite = TRUE
)


# ============================================================
# 18. Print overall ranking
# ============================================================

cat("\n========================================\n")
cat("OVERALL PHOSPHATE SOLUBILISATION RANKING\n")
cat("========================================\n\n")

print(
  overall_results %>%
    select(
      Overall_Rank,
      BW_Strain,
      Mean_Index_Al,
      Mean_Index_Ca,
      Mean_Index_Fe,
      Overall_Index,
      Overall_Classification
    )
)


# ============================================================
# 19. Print classification counts
# ============================================================

cat("\n========================================\n")
cat("OVERALL CLASSIFICATION COUNTS\n")
cat("========================================\n\n")

print(
  overall_results %>%
    count(Overall_Classification)
)


cat("\n========================================\n")
cat("Output saved to:\n")
cat(output_file)
cat("\n========================================\n")
