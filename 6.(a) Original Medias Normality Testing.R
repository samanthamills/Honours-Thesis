# ============================================================
# NORMALITY TESTING
# RAW PHOSPHATE RESULTS
#
# Tests:
#   - Shapiro-Wilk test
#   - Normal Q-Q plots
#
# Measurements:
#   Al-ph: Colony diameter
#   Al-ph: Organic acid production
#   Fe-ph: Colony diameter
#   Fe-ph: Organic acid production
#   Ca-ph: Colony diameter
#   Ca-ph: Zone of clearing
#
# Uses individual replicate measurements
# ============================================================


# ============================================================
# 1. LOAD PACKAGES
# ============================================================

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(writexl)


# ============================================================
# 2. FILE LOCATION
# ============================================================

setwd("/Users/samanthamills/Honours Project (R Studio)/Lab Result Graphing")

file_path <- "/Users/samanthamills/Honours Project (R Studio)/Lab Result Graphing/Data/Raw_Phosphate_Results.xlsx"


# ============================================================
# 3. READ DATA
# ============================================================

Al_ph <- read_excel(
  file_path,
  sheet = "Al-ph"
)

Fe_ph <- read_excel(
  file_path,
  sheet = "Fe-ph"
)

Ca_ph <- read_excel(
  file_path,
  sheet = "Ca-ph"
)


# ============================================================
# 4. FUNCTION TO EXTRACT REPLICATE MEASUREMENTS
# ============================================================

extract_measurements <- function(data, sheet_name) {
  
  # Colony diameter
  colony_cols <- grep(
    "^Colony size[1-6]$",
    names(data),
    value = TRUE
  )
  
  # Organic acid production
  acid_cols <- grep(
    "^Organic Acid Production[1-6]$",
    names(data),
    value = TRUE
  )
  
  # Zone of clearing
  zone_cols <- grep(
    "^Zone of clearing[1-6]$",
    names(data),
    value = TRUE
  )
  
  
  # Combine relevant columns
  measurement_cols <- c(
    colony_cols,
    acid_cols,
    zone_cols
  )
  
  
  # Convert to long format
  data %>%
    
    select(
      BW_Strain,
      all_of(measurement_cols)
    ) %>%
    
    pivot_longer(
      cols = all_of(measurement_cols),
      names_to = "Measurement",
      values_to = "Value"
    ) %>%
    
    mutate(
      Value = suppressWarnings(as.numeric(Value)),
      Media = sheet_name
    ) %>%
    
    filter(
      !is.na(Value)
    )
}


# ============================================================
# 5. EXTRACT DATA FROM EACH MEDIA
# ============================================================

Al_long <- extract_measurements(
  Al_ph,
  "Al-ph"
)

Fe_long <- extract_measurements(
  Fe_ph,
  "Fe-ph"
)

Ca_long <- extract_measurements(
  Ca_ph,
  "Ca-ph"
)


# ============================================================
# 6. COMBINE ALL DATA
# ============================================================

normality_data <- bind_rows(
  Al_long,
  Fe_long,
  Ca_long
)


# ============================================================
# 7. CLEAN VARIABLE NAMES
# ============================================================

normality_data <- normality_data %>%
  
  mutate(
    
    Variable = case_when(
      
      grepl(
        "^Colony size",
        Measurement,
        ignore.case = TRUE
      ) ~ "Colony diameter",
      
      grepl(
        "^Organic Acid Production",
        Measurement,
        ignore.case = TRUE
      ) ~ "Organic acid production",
      
      grepl(
        "^Zone of clearing",
        Measurement,
        ignore.case = TRUE
      ) ~ "Zone of clearing",
      
      TRUE ~ Measurement
    )
    
  )


# ============================================================
# 8. VIEW NUMBER OF OBSERVATIONS
# ============================================================

observation_counts <- normality_data %>%
  
  group_by(
    Media,
    Variable
  ) %>%
  
  summarise(
    N = sum(!is.na(Value)),
    .groups = "drop"
  )

print(observation_counts)


# ============================================================
# 9. SHAPIRO-WILK TEST
# ============================================================

# Shapiro-Wilk tests whether the data are consistent
# with a normal distribution.
#
# H0 = data are normally distributed
# H1 = data are not normally distributed
#
# p > 0.05:
#   insufficient evidence to reject normality
#
# p < 0.05:
#   evidence that the data deviate from normality


shapiro_results <- normality_data %>%
  
  group_by(
    Media,
    Variable
  ) %>%
  
  summarise(
    
    N = sum(!is.na(Value)),
    
    Shapiro_W = unname(
      shapiro.test(Value)$statistic
    ),
    
    P_value = shapiro.test(Value)$p.value,
    
    Normality = case_when(
      
      P_value >= 0.05 ~
        "Approximately normal",
      
      P_value < 0.05 ~
        "Not normally distributed"
      
    ),
    
    .groups = "drop"
  )


# ============================================================
# 10. PRINT SHAPIRO-WILK RESULTS
# ============================================================

print(shapiro_results)


# ============================================================
# 11. CREATE Q-Q PLOTS
# ============================================================

qq_plot <- ggplot(
  normality_data,
  aes(
    sample = Value
  )
) +
  
  stat_qq(
    size = 1.5,
    alpha = 0.7
  ) +
  
  stat_qq_line(
    linewidth = 0.8
  ) +
  
  facet_grid(
    Variable ~ Media,
    scales = "free"
  ) +
  
  labs(
    title = "Normal Q-Q plots of phosphate solubilisation measurements",
    x = "Theoretical quantiles",
    y = "Sample quantiles"
  ) +
  
  theme_classic() +
  
  theme(
    strip.text = element_text(
      face = "bold"
    ),
    plot.title = element_text(
      face = "bold"
    )
  )


# Display plot
print(qq_plot)


# ============================================================
# 12. SAVE Q-Q PLOT
# ============================================================

ggsave(
  "Normality_QQ_Plots.png",
  qq_plot,
  width = 10,
  height = 7,
  dpi = 300
)


# ============================================================
# 13. SAVE RESULTS TO EXCEL
# ============================================================

write_xlsx(
  
  list(
    
    Shapiro_Wilk = shapiro_results,
    
    Observation_Counts = observation_counts,
    
    Raw_Normality_Data = normality_data
    
  ),
  
  "Normality_Test_Results.xlsx"
)


# ============================================================
# 14. FINISHED
# ============================================================

cat("\n")
cat("=============================================\n")
cat("NORMALITY ANALYSIS COMPLETE\n")
cat("=============================================\n")
cat("\n")
cat("Shapiro-Wilk results saved as:\n")
cat("Normality_Test_Results.xlsx\n")
cat("\n")
cat("Q-Q plots saved as:\n")
cat("Normality_QQ_Plots.png\n")
cat("=============================================\n")
