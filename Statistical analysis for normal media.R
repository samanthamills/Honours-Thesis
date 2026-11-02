############################################################
# STATISTICAL ANALYSIS
# PHOSPHATE SOLUBILISATION EXPERIMENT
#
# 3 agar types:
#   Al-ph
#   Fe-ph
#   Ca-ph
#
# 50 strains
# 6 replicates per strain
#
# Statistical tests:
#   - One-way ANOVA if assumptions are reasonable
#   - Kruskal-Wallis if assumptions are substantially violated
#
# Post-hoc:
#   - Tukey HSD after significant ANOVA
#   - Dunn's test after significant Kruskal-Wallis
############################################################


############################################################
# 1. INSTALL PACKAGES IF NEEDED
############################################################

# Run these lines ONCE if packages are not installed:

# install.packages("readxl")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("ggplot2")
# install.packages("car")
# install.packages("rstatix")
# install.packages("writexl")


############################################################
# 2. LOAD PACKAGES
############################################################

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(car)
library(rstatix)
library(writexl)


############################################################
# 3. SET WORKING DIRECTORY
############################################################

setwd("/Users/samanthamills/Honours Project (R Studio)/Lab Result Graphing/Data")


############################################################
# 4. LOAD EXCEL WORKBOOK
############################################################

# CHANGE THIS TO YOUR ACTUAL FILE NAME

file_name <- "Raw_Phosphate_Results.xlsx"


############################################################
# 5. READ THE THREE SHEETS
############################################################

Al_ph <- read_excel(
  file_name,
  sheet = "Al-ph"
)

Fe_ph <- read_excel(
  file_name,
  sheet = "Fe-ph"
)

Ca_ph <- read_excel(
  file_name,
  sheet = "Ca-ph"
)


############################################################
# 6. CHECK COLUMN NAMES
############################################################

names(Al_ph)

names(Fe_ph)

names(Ca_ph)


############################################################
# 7. FUNCTION TO RESHAPE THE DATA
############################################################

# This converts:
#
# Colony size1
# Colony size2
# Colony size3
# ...
# Colony size6
#
# into one column called Colony_Size
# with a separate Replicate column.

reshape_phosphate_data <- function(data) {
  
  data %>%
    
    pivot_longer(
      
      # ONLY include the numerical measurements
      cols = matches(
        "^(Colony size|Organic Acid Production|Zone of clearing)[1-6]$"
      ),
      
      names_to = c("Variable", "Replicate"),
      
      names_pattern = "^(.*?)([1-6])$",
      
      values_to = "Value"
      
    ) %>%
    
    mutate(
      
      Replicate = as.numeric(Replicate)
      
    )
}

############################################################
# 8. RESHAPE EACH DATASET
############################################################

Al_long <- reshape_phosphate_data(Al_ph)

Fe_long <- reshape_phosphate_data(Fe_ph)

Ca_long <- reshape_phosphate_data(Ca_ph)


############################################################
# 9. LOOK AT THE LONG DATA
############################################################

head(Al_long)

head(Fe_long)

head(Ca_long)

############################################################
# 11. MAKE STRAIN A FACTOR
############################################################

Al_long$BW_Strain <- as.factor(Al_long$BW_Strain)

Fe_long$BW_Strain <- as.factor(Fe_long$BW_Strain)

Ca_long$BW_Strain <- as.factor(Ca_long$BW_Strain)


############################################################
# 12. CHECK THE VARIABLES
############################################################

unique(Al_long$Variable)

unique(Fe_long$Variable)

unique(Ca_long$Variable)


############################################################
# 13. CREATE ANALYSIS FUNCTION
############################################################

analyse_variable <- function(data, variable_name, agar_name) {
  
  
  cat("\n\n")
  cat("=============================================\n")
  cat("AGAR:", agar_name, "\n")
  cat("VARIABLE:", variable_name, "\n")
  cat("=============================================\n")
  
  
  ##########################################################
  # SELECT VARIABLE
  ##########################################################
  
  analysis_data <- data %>%
    
    filter(
      Variable == variable_name,
      !is.na(Value)
    )
  
  
  ##########################################################
  # CHECK NUMBER OF OBSERVATIONS
  ##########################################################
  
  cat("\nNumber of observations:", nrow(analysis_data), "\n")
  
  cat(
    "Number of strains:",
    length(unique(analysis_data$BW_Strain)),
    "\n"
  )
  
  
  ##########################################################
  # ONE-WAY ANOVA
  ##########################################################
  
  model <- aov(
    Value ~ BW_Strain,
    data = analysis_data
  )
  
  
  ##########################################################
  # ANOVA RESULTS
  ##########################################################
  
  anova_table <- summary(model)
  
  print(anova_table)
  
  
  ##########################################################
  # RESIDUALS
  ##########################################################
  
  model_residuals <- residuals(model)
  
  
  ##########################################################
  # SHAPIRO-WILK TEST
  ##########################################################
  
  shapiro <- shapiro.test(model_residuals)
  
  cat("\nShapiro-Wilk test:\n")
  
  print(shapiro)
  
  
  ##########################################################
  # LEVENE'S TEST
  ##########################################################
  
  levene <- leveneTest(
    Value ~ BW_Strain,
    data = analysis_data
  )
  
  cat("\nLevene's test:\n")
  
  print(levene)
  
  
  ##########################################################
  # DETERMINE WHETHER ASSUMPTIONS ARE REASONABLE
  ##########################################################
  
  normality_ok <-
    shapiro$p.value > 0.05
  
  variance_ok <-
    levene$`Pr(>F)`[1] > 0.05
  
  
  ##########################################################
  # CHOOSE TEST
  ##########################################################
  
  if (normality_ok && variance_ok) {
    
    
    ########################################################
    # ANOVA
    ########################################################
    
    test_used <- "One-way ANOVA"
    
    p_value <-
      summary(model)[[1]][["Pr(>F)"]][1]
    
    
    cat("\n")
    cat("TEST USED: ONE-WAY ANOVA\n")
    
    
    ########################################################
    # TUKEY HSD
    ########################################################
    
    if (p_value < 0.05) {
      
      
      tukey <- TukeyHSD(model)
      
      tukey_table <-
        as.data.frame(tukey$BW_Strain)
      
      tukey_table$Comparison <-
        rownames(tukey_table)
      
      rownames(tukey_table) <- NULL
      
      
    } else {
      
      tukey_table <- NULL
      
    }
    
    
    dunn_table <- NULL
    
    
  } else {
    
    
    ########################################################
    # KRUSKAL-WALLIS
    ########################################################
    
    test_used <- "Kruskal-Wallis"
    
    
    kw <- kruskal.test(
      Value ~ BW_Strain,
      data = analysis_data
    )
    
    
    p_value <- kw$p.value
    
    
    cat("\n")
    cat("TEST USED: KRUSKAL-WALLIS\n")
    
    print(kw)
    
    
    ########################################################
    # DUNN'S TEST
    ########################################################
    
    if (p_value < 0.05) {
      
      
      dunn_table <- analysis_data %>%
        
        dunn_test(
          Value ~ BW_Strain,
          p.adjust.method = "BH"
        )
      
      
    } else {
      
      dunn_table <- NULL
      
    }
    
    
    tukey_table <- NULL
    
  }
  
  
  ##########################################################
  # RETURN RESULTS
  ##########################################################
  
  return(
    
    list(
      
      model = model,
      
      test_used = test_used,
      
      p_value = p_value,
      
      shapiro_p = shapiro$p.value,
      
      levene_p = levene$`Pr(>F)`[1],
      
      tukey = tukey_table,
      
      dunn = dunn_table
      
    )
    
  )
  
}


############################################################
# 14. RUN AL-PH ANALYSES
############################################################

Al_colony <- analyse_variable(
  Al_long,
  "Colony size",
  "Al-ph"
)


Al_organic <- analyse_variable(
  Al_long,
  "Organic Acid Production",
  "Al-ph"
)


############################################################
# 15. RUN FE-PH ANALYSES
############################################################

Fe_colony <- analyse_variable(
  Fe_long,
  "Colony size",
  "Fe-ph"
)


Fe_organic <- analyse_variable(
  Fe_long,
  "Organic Acid Production",
  "Fe-ph"
)


############################################################
# 16. RUN CA-PH ANALYSES
############################################################

Ca_colony <- analyse_variable(
  Ca_long,
  "Colony size",
  "Ca-ph"
)


Ca_zone <- analyse_variable(
  Ca_long,
  "Zone of clearing",
  "Ca-ph"
)


############################################################
# 17. SOLUBILISATION INDEX / DIFFUSION INDEX
############################################################

# IMPORTANT:
#
# This section assumes you ALREADY have columns containing
# the calculated indices.
#
# If the indices are not currently in your Excel file,
# we can calculate them from the replicate measurements.
#
# See the section below.


############################################################
# 18. CREATE SUMMARY TABLE
############################################################

make_summary <- function(result, agar, variable) {
  
  data.frame(
    
    Agar = agar,
    
    Variable = variable,
    
    Test = result$test_used,
    
    Shapiro_p = result$shapiro_p,
    
    Levene_p = result$levene_p,
    
    Overall_p = result$p_value,
    
    Significant = ifelse(
      result$p_value < 0.05,
      "Yes",
      "No"
    )
    
  )
}


############################################################
# 19. COMBINE RESULTS
############################################################

overall_results <- bind_rows(
  
  make_summary(
    Al_colony,
    "Al-ph",
    "Colony size"
  ),
  
  make_summary(
    Al_organic,
    "Al-ph",
    "Organic Acid Production"
  ),
  
  make_summary(
    Fe_colony,
    "Fe-ph",
    "Colony size"
  ),
  
  make_summary(
    Fe_organic,
    "Fe-ph",
    "Organic Acid Production"
  ),
  
  make_summary(
    Ca_colony,
    "Ca-ph",
    "Colony size"
  ),
  
  make_summary(
    Ca_zone,
    "Ca-ph",
    "Zone of clearing"
  )
)


############################################################
# 20. VIEW OVERALL RESULTS
############################################################

print(overall_results)


############################################################
# 21. CALCULATE STRAIN MEANS
############################################################

Al_means <- Al_long %>%
  
  group_by(BW_Strain, Variable) %>%
  
  summarise(
    
    Mean = mean(
      Value,
      na.rm = TRUE
    ),
    
    SD = sd(
      Value,
      na.rm = TRUE
    ),
    
    N = sum(
      !is.na(Value)
    ),
    
    .groups = "drop"
    
  )


Fe_means <- Fe_long %>%
  
  group_by(BW_Strain, Variable) %>%
  
  summarise(
    
    Mean = mean(
      Value,
      na.rm = TRUE
    ),
    
    SD = sd(
      Value,
      na.rm = TRUE
    ),
    
    N = sum(
      !is.na(Value)
    ),
    
    .groups = "drop"
    
  )


Ca_means <- Ca_long %>%
  
  group_by(BW_Strain, Variable) %>%
  
  summarise(
    
    Mean = mean(
      Value,
      na.rm = TRUE
    ),
    
    SD = sd(
      Value,
      na.rm = TRUE
    ),
    
    N = sum(
      !is.na(Value)
    ),
    
    .groups = "drop"
    
  )


############################################################
# 22. CREATE DIAGNOSTIC PLOTS
############################################################

if (!dir.exists("Statistical_Diagnostic_Plots")) {
  
  dir.create(
    "Statistical_Diagnostic_Plots"
  )
}


create_diagnostic_plot <- function(
    result,
    agar,
    variable) {
  
  
  png(
    
    filename = paste0(
      "Statistical_Diagnostic_Plots/",
      agar,
      "_",
      gsub(" ", "_", variable),
      "_diagnostics.png"
    ),
    
    width = 1600,
    height = 800,
    res = 150
    
  )
  
  
  par(
    mfrow = c(1, 2)
  )
  
  
  # Residuals vs fitted
  
  plot(
    
    result$model,
    
    which = 1,
    
    main = paste(
      agar,
      variable,
      "\nResiduals vs Fitted"
    )
    
  )
  
  
  # Normal Q-Q plot
  
  plot(
    
    result$model,
    
    which = 2,
    
    main = paste(
      agar,
      variable,
      "\nNormal Q-Q"
    )
    
  )
  
  
  dev.off()
  
}


############################################################
# 23. MAKE DIAGNOSTIC PLOTS
############################################################

create_diagnostic_plot(
  Al_colony,
  "Al-ph",
  "Colony size"
)

create_diagnostic_plot(
  Al_organic,
  "Al-ph",
  "Organic Acid Production"
)

create_diagnostic_plot(
  Fe_colony,
  "Fe-ph",
  "Colony size"
)

create_diagnostic_plot(
  Fe_organic,
  "Fe-ph",
  "Organic Acid Production"
)

create_diagnostic_plot(
  Ca_colony,
  "Ca-ph",
  "Colony size"
)

create_diagnostic_plot(
  Ca_zone,
  "Ca-ph",
  "Zone of clearing"
)

############################################################
# 24. SAVE RESULTS TO EXCEL
############################################################

write_xlsx(
  
  list(
    
    Overall_Results =
      overall_results,
    
    Al_ph_Means =
      Al_means,
    
    Fe_ph_Means =
      Fe_means,
    
    Ca_ph_Means =
      Ca_means,
    
    Al_ph_Posthoc =
      if (Al_colony$test_used == "One-way ANOVA")
        Al_colony$tukey
    else
      Al_colony$dunn,
    
    Al_organic_Posthoc =
      if (Al_organic$test_used == "One-way ANOVA")
        Al_organic$tukey
    else
      Al_organic$dunn,
    
    Fe_ph_Posthoc =
      if (Fe_colony$test_used == "One-way ANOVA")
        Fe_colony$tukey
    else
      Fe_colony$dunn,
    
    Fe_organic_Posthoc =
      if (Fe_organic$test_used == "One-way ANOVA")
        Fe_organic$tukey
    else
      Fe_organic$dunn,
    
    Ca_ph_Posthoc =
      if (Ca_colony$test_used == "One-way ANOVA")
        Ca_colony$tukey
    else
      Ca_colony$dunn,
    
    Ca_zone_Posthoc =
      if (Ca_zone$test_used == "One-way ANOVA")
        Ca_zone$tukey
    else
      Ca_zone$dunn
    
  ),
  
  "Phosphate_Statistical_Analysis.xlsx"
)


############################################################
# 25. FINISHED
############################################################

cat("\n")
cat("=============================================\n")
cat("ANALYSIS COMPLETE\n")
cat("=============================================\n")
cat("\n")
cat("Excel results saved as:\n")
cat("Phosphate_Statistical_Analysis.xlsx\n")
cat("\n")
cat("Diagnostic plots saved in:\n")
cat("Statistical_Diagnostic_Plots/\n")
cat("=============================================\n")

#=============================================
# Matrix for presenting significant results
#=============================================

#--------------------------------------------------
# Create tables of significant Dunn's test results
# Only pairwise comparisons with adjusted p < 0.05
#--------------------------------------------------

# Install packages if needed
# install.packages(c("dplyr", "writexl"))

library(dplyr)
library(writexl)

#--------------------------------------------------
# 1. Function to extract significant comparisons
#--------------------------------------------------

get_significant_dunn <- function(dunn_result, trait_name) {
  
  # If there are no Dunn results
  if (is.null(dunn_result) || nrow(dunn_result) == 0) {
    return(
      data.frame(
        Trait = character(),
        Strain_1 = character(),
        Strain_2 = character(),
        Statistic = numeric(),
        P_value = numeric(),
        Adjusted_P = numeric(),
        Significance = character()
      )
    )
  }
  
  dunn_result %>%
    # Keep only significant comparisons
    filter(p.adj < 0.05) %>%
    
    # Select and rename columns
    transmute(
      Trait = trait_name,
      Strain_1 = group1,
      Strain_2 = group2,
      Statistic = statistic,
      P_value = p,
      Adjusted_P = p.adj,
      
      # Add significance symbols
      Significance = case_when(
        p.adj < 0.0001 ~ "****",
        p.adj < 0.001  ~ "***",
        p.adj < 0.01   ~ "**",
        p.adj < 0.05   ~ "*"
      )
    ) %>%
    
    # Order from smallest to largest adjusted p-value
    arrange(Adjusted_P)
}


#--------------------------------------------------
# 2. Extract significant results for each trait
#--------------------------------------------------

Al_colony_sig <- get_significant_dunn(
  Al_colony$dunn,
  "Al-ph: Colony size"
)

Al_organic_sig <- get_significant_dunn(
  Al_organic$dunn,
  "Al-ph: Organic acid production"
)

Fe_colony_sig <- get_significant_dunn(
  Fe_colony$dunn,
  "Fe-ph: Colony size"
)

Fe_organic_sig <- get_significant_dunn(
  Fe_organic$dunn,
  "Fe-ph: Organic acid production"
)

Ca_colony_sig <- get_significant_dunn(
  Ca_colony$dunn,
  "Ca-ph: Colony size"
)

Ca_zone_sig <- get_significant_dunn(
  Ca_zone$dunn,
  "Ca-ph: Zone of clearing"
)


#--------------------------------------------------
# 3. Combine all significant comparisons
#--------------------------------------------------

All_significant_Dunn <- bind_rows(
  Al_colony_sig,
  Al_organic_sig,
  Fe_colony_sig,
  Fe_organic_sig,
  Ca_colony_sig,
  Ca_zone_sig
)


#--------------------------------------------------
# 4. Print results in R
#--------------------------------------------------

print(All_significant_Dunn)


#--------------------------------------------------
# 5. Create separate sheets for each trait
#--------------------------------------------------

Dunn_results <- list(
  
  `Al Colony` = Al_colony_sig,
  
  `Al Organic Acid` = Al_organic_sig,
  
  `Fe Colony` = Fe_colony_sig,
  
  `Fe Organic Acid` = Fe_organic_sig,
  
  `Ca Colony` = Ca_colony_sig,
  
  `Ca Zone Clearing` = Ca_zone_sig,
  
  `All Significant` = All_significant_Dunn
)


#--------------------------------------------------
# 6. Export to Excel
#--------------------------------------------------

write_xlsx(
  Dunn_results,
  "Significant_Dunn_Comparisons.xlsx"
)


#--------------------------------------------------
# 7. Confirmation message
#--------------------------------------------------

cat(
  "\nSignificant Dunn's test results have been saved to:\n",
  file.path(getwd(), "Significant_Dunn_Comparisons.xlsx"),
  "\n"
)
