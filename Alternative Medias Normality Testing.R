# ============================================================
# NORMALITY TESTING
# ALTERNATIVE PHOSPHATE MEDIA
#
# Tests:
#   - Shapiro-Wilk test
#   - Normal Q-Q plots
#
# Measurements:
#   Alt-Al: Colony diameter
#   Alt-Al: Organic acid production
#   Alt-Fe: Colony diameter
#   Alt-Fe: Organic acid production
#   Alt-Ca: Colony diameter
#
# IMPORTANT:
#   Alt-Ca organic acid production is NOT tested because
#   the Alt-Ca medium does not contain a pH indicator and
#   these values are all zero.
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
# 3. READ ALTERNATIVE MEDIA DATA
# ============================================================

Alt_Al <- read_excel(
  file_path,
  sheet = "Alt-Al"
)

Alt_Fe <- read_excel(
  file_path,
  sheet = "Alt-Fe"
)

Alt_Ca <- read_excel(
  file_path,
  sheet = "Alt-Ca"
)


# ============================================================
# 4. CHECK COLUMN NAMES
# ============================================================

print(names(Alt_Al))
print(names(Alt_Fe))
print(names(Alt_Ca))


# ============================================================
# 5. FUNCTION TO EXTRACT MEASUREMENTS
# ============================================================

extract_alt_measurements <- function(
    data,
    media_name) {
  
  # ----------------------------------------------------------
  # Colony diameter
  # ----------------------------------------------------------
  
  colony_cols <- grep(
    "^Colony size[1-6]$",
    names(data),
    value = TRUE
  )
  
  
  # ----------------------------------------------------------
  # Organic acid production
  # ----------------------------------------------------------
  
  acid_cols <- grep(
    "^Organic Acid Production[1-6]$",
    names(data),
    value = TRUE
  )
  
  
  # ----------------------------------------------------------
  # Zone of clearing
  # ----------------------------------------------------------
  
  zone_cols <- grep(
    "^Zone of clearing[1-6]$",
    names(data),
    value = TRUE
  )
  
  
  # ----------------------------------------------------------
  # Combine relevant measurement columns
  # ----------------------------------------------------------
  
  measurement_cols <- c(
    colony_cols,
    acid_cols,
    zone_cols
  )
  
  
  # ----------------------------------------------------------
  # Convert to long format
  # ----------------------------------------------------------
  
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
      
      Value = suppressWarnings(
        as.numeric(Value)
      ),
      
      Media = media_name
      
    ) %>%
    
    filter(
      !is.na(Value)
    )
}


# ============================================================
# 6. EXTRACT DATA
# ============================================================

Alt_Al_long <- extract_alt_measurements(
  Alt_Al,
  "Alt-Al"
)

Alt_Fe_long <- extract_alt_measurements(
  Alt_Fe,
  "Alt-Fe"
)

Alt_Ca_long <- extract_alt_measurements(
  Alt_Ca,
  "Alt-Ca"
)


# ============================================================
# 7. COMBINE DATA
# ============================================================

alternative_normality_data <- bind_rows(
  Alt_Al_long,
  Alt_Fe_long,
  Alt_Ca_long
)


# ============================================================
# 8. IDENTIFY VARIABLE
# ============================================================

alternative_normality_data <- alternative_normality_data %>%
  
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
# 9. REMOVE ALT-CA ORGANIC ACID DATA
# ============================================================

# This is included as a safeguard in case the Alt-Ca sheet
# contains an Organic Acid Production column.

alternative_normality_data <- alternative_normality_data %>%
  
  filter(
    !(Media == "Alt-Ca" &
        Variable == "Organic acid production")
  )


# ============================================================
# 10. CHECK NUMBER OF OBSERVATIONS
# ============================================================

observation_counts <- alternative_normality_data %>%
  
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
# 11. SHAPIRO-WILK TEST
# ============================================================

# H0 = data are normally distributed
#
# H1 = data are not normally distributed
#
# p >= 0.05:
#   No significant evidence against normality
#
# p < 0.05:
#   Evidence that the data deviate from normality


shapiro_results <- alternative_normality_data %>%
  
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
# 12. PRINT SHAPIRO-WILK RESULTS
# ============================================================

print(shapiro_results)


# ============================================================
# 13. CREATE Q-Q PLOTS
# ============================================================

qq_plot_alternative <- ggplot(
  
  alternative_normality_data,
  
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
    
    title =
      "Normal Q-Q plots of alternative phosphate media",
    
    x =
      "Theoretical quantiles",
    
    y =
      "Sample quantiles"
    
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


# ============================================================
# 14. DISPLAY Q-Q PLOTS
# ============================================================

print(qq_plot_alternative)


# ============================================================
# 15. SAVE Q-Q PLOTS
# ============================================================

ggsave(
  
  "Alternative_Normality_QQ_Plots.png",
  
  qq_plot_alternative,
  
  width = 10,
  
  height = 7,
  
  dpi = 300
  
)


# ============================================================
# 16. SAVE RESULTS TO EXCEL
# ============================================================

write_xlsx(
  
  list(
    
    Shapiro_Wilk =
      shapiro_results,
    
    Observation_Counts =
      observation_counts,
    
    Raw_Normality_Data =
      alternative_normality_data
    
  ),
  
  "Alternative_Normality_Test_Results.xlsx"
  
)


# ============================================================
# 17. FINISHED
# ============================================================

cat("\n")
cat("=============================================\n")
cat("ALTERNATIVE MEDIA NORMALITY ANALYSIS COMPLETE\n")
cat("=============================================\n")
cat("\n")

cat("Shapiro-Wilk results saved as:\n")
cat("Alternative_Normality_Test_Results.xlsx\n")

cat("\n")

cat("Q-Q plots saved as:\n")
cat("Alternative_Normality_QQ_Plots.png\n")

cat("\n")
cat("Variables tested:\n")
cat("Alt-Al - Colony diameter\n")
cat("Alt-Al - Organic acid production\n")
cat("Alt-Fe - Colony diameter\n")
cat("Alt-Fe - Organic acid production\n")
cat("Alt-Ca - Colony diameter\n")

cat("\n")
cat("Alt-Ca organic acid production was excluded because\n")
cat("the medium does not contain a pH indicator and the\n")
cat("recorded values are all zero.\n")

cat("=============================================\n")