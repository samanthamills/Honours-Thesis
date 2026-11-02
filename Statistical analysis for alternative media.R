#==================================================
# STATISTICAL ANALYSIS OF ALTERNATIVE PHOSPHATE DATA
#==================================================
#
# Excel sheets:
#   Alt-Al
#   Alt-Ca
#   Alt-Fe
#
# Traits analysed:
#   Alt-Al: Colony size + Organic acid production
#   Alt-Ca: Colony size only
#   Alt-Fe: Colony size + Organic acid production
#
#==================================================


#==================================================
# 1. LOAD PACKAGES
#==================================================

# Install if needed:
# install.packages(c("readxl", "dplyr", "tidyr",
#                    "ggplot2", "car", "rstatix",
#                    "writexl"))

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(car)
library(rstatix)
library(writexl)


#==================================================
# 2. SET WORKING DIRECTORY
#==================================================

setwd(
  "/Users/samanthamills/Honours Project (R Studio)/Lab Result Graphing/Data"
)


#==================================================
# 3. READ EXCEL DATA
#==================================================

Alt_Al <- read_excel(
  "Raw_Phosphate_Results.xlsx",
  sheet = "Alt-Al"
)

Alt_Ca <- read_excel(
  "Raw_Phosphate_Results.xlsx",
  sheet = "Alt-Ca"
)

Alt_Fe <- read_excel(
  "Raw_Phosphate_Results.xlsx",
  sheet = "Alt-Fe"
)


#==================================================
# 4. RESHAPE DATA
#==================================================

reshape_phosphate_data <- function(data) {
  
  data %>%
    pivot_longer(
      cols = matches(
        "^(Colony size|Organic Acid Production)[1-6]$"
      ),
      names_to = c("Variable", "Replicate"),
      names_pattern = "^(.*?)([1-6])$",
      values_to = "Value"
    ) %>%
    mutate(
      Replicate = as.numeric(Replicate),
      Value = as.numeric(Value)
    )
}


Alt_Al_long <- reshape_phosphate_data(Alt_Al)

Alt_Ca_long <- reshape_phosphate_data(Alt_Ca)

Alt_Fe_long <- reshape_phosphate_data(Alt_Fe)


#==================================================
# 5. STATISTICAL ANALYSIS FUNCTION
#==================================================

analyse_variable <- function(data, variable_name) {
  
  # Select variable
  df <- data %>%
    filter(
      Variable == variable_name,
      !is.na(Value)
    ) %>%
    mutate(
      BW_Strain = as.factor(BW_Strain)
    )
  
  
  #------------------------------------------------
  # Check for no variation
  #------------------------------------------------
  
  if (n_distinct(df$Value) <= 1) {
    
    return(
      list(
        Test = "No variation",
        P_value = NA,
        Shapiro_P = NA,
        Levene_P = NA,
        Tukey = data.frame(),
        Dunn = data.frame()
      )
    )
  }
  
  
  #------------------------------------------------
  # ANOVA model
  #------------------------------------------------
  
  model <- aov(
    Value ~ BW_Strain,
    data = df
  )
  
  
  #------------------------------------------------
  # Shapiro-Wilk test
  #------------------------------------------------
  
  shapiro <- tryCatch(
    shapiro.test(
      residuals(model)
    ),
    error = function(e) NULL
  )
  
  
  if (!is.null(shapiro)) {
    shapiro_p <- shapiro$p.value
  } else {
    shapiro_p <- NA
  }
  
  
  #------------------------------------------------
  # Levene's test
  #------------------------------------------------
  
  levene <- car::leveneTest(
    Value ~ BW_Strain,
    data = df
  )
  
  levene_p <- levene$`Pr(>F)`[1]
  
  
  #------------------------------------------------
  # Check assumptions
  #------------------------------------------------
  
  normality_ok <- (
    !is.na(shapiro_p) &&
      shapiro_p > 0.05
  )
  
  variance_ok <- (
    !is.na(levene_p) &&
      levene_p > 0.05
  )
  
  
  #================================================
  # ANOVA
  #================================================
  
  if (normality_ok && variance_ok) {
    
    anova_result <- summary(model)
    
    p_value <- anova_result[[1]][["Pr(>F)"]][1]
    
    
    #----------------------------------------------
    # Tukey HSD if significant
    #----------------------------------------------
    
    if (p_value < 0.05) {
      
      tukey <- TukeyHSD(model)
      
      tukey_df <- as.data.frame(
        tukey$BW_Strain
      )
      
      tukey_df$Comparison <- rownames(tukey_df)
      
      rownames(tukey_df) <- NULL
      
    } else {
      
      tukey_df <- data.frame()
    }
    
    
    return(
      list(
        Test = "One-way ANOVA",
        P_value = p_value,
        Shapiro_P = shapiro_p,
        Levene_P = levene_p,
        Tukey = tukey_df,
        Dunn = data.frame()
      )
    )
  }
  
  
  #================================================
  # KRUSKAL-WALLIS
  #================================================
  
  kruskal <- kruskal.test(
    Value ~ BW_Strain,
    data = df
  )
  
  p_value <- kruskal$p.value
  
  
  #------------------------------------------------
  # Dunn test if significant
  #------------------------------------------------
  
  if (p_value < 0.05) {
    
    dunn <- df %>%
      dunn_test(
        Value ~ BW_Strain,
        p.adjust.method = "BH"
      )
    
  } else {
    
    dunn <- data.frame()
  }
  
  
  return(
    list(
      Test = "Kruskal-Wallis",
      P_value = p_value,
      Shapiro_P = shapiro_p,
      Levene_P = levene_p,
      Tukey = data.frame(),
      Dunn = dunn
    )
  )
}


#==================================================
# 6. RUN THE FIVE ANALYSES
#==================================================


#--------------------------------------------------
# Alt-Al
#--------------------------------------------------

Alt_Al_colony <- analyse_variable(
  Alt_Al_long,
  "Colony size"
)

Alt_Al_organic <- analyse_variable(
  Alt_Al_long,
  "Organic Acid Production"
)


#--------------------------------------------------
# Alt-Ca
#--------------------------------------------------
#
# ONLY colony size.
#
# Organic acid production is not analysed because
# the Alt-Ca agar had no pH indicator.
#--------------------------------------------------

Alt_Ca_colony <- analyse_variable(
  Alt_Ca_long,
  "Colony size"
)


#--------------------------------------------------
# Alt-Fe
#--------------------------------------------------

Alt_Fe_colony <- analyse_variable(
  Alt_Fe_long,
  "Colony size"
)

Alt_Fe_organic <- analyse_variable(
  Alt_Fe_long,
  "Organic Acid Production"
)


#==================================================
# 7. PRINT TESTS USED
#==================================================

cat("\n============================================\n")
cat("STATISTICAL TESTS USED\n")
cat("============================================\n\n")

cat(
  "Alt-Al | Colony size: ",
  Alt_Al_colony$Test,
  "\n"
)

cat(
  "Alt-Al | Organic acid production: ",
  Alt_Al_organic$Test,
  "\n"
)

cat(
  "Alt-Ca | Colony size: ",
  Alt_Ca_colony$Test,
  "\n"
)

cat(
  "Alt-Fe | Colony size: ",
  Alt_Fe_colony$Test,
  "\n"
)

cat(
  "Alt-Fe | Organic acid production: ",
  Alt_Fe_organic$Test,
  "\n"
)


#==================================================
# 8. OVERALL STATISTICS TABLE
#==================================================

Overall_Results <- data.frame(
  
  Medium = c(
    "Alt-Al",
    "Alt-Al",
    "Alt-Ca",
    "Alt-Fe",
    "Alt-Fe"
  ),
  
  Trait = c(
    "Colony size",
    "Organic Acid Production",
    "Colony size",
    "Colony size",
    "Organic Acid Production"
  ),
  
  Test = c(
    Alt_Al_colony$Test,
    Alt_Al_organic$Test,
    Alt_Ca_colony$Test,
    Alt_Fe_colony$Test,
    Alt_Fe_organic$Test
  ),
  
  P_value = c(
    Alt_Al_colony$P_value,
    Alt_Al_organic$P_value,
    Alt_Ca_colony$P_value,
    Alt_Fe_colony$P_value,
    Alt_Fe_organic$P_value
  ),
  
  Shapiro_P = c(
    Alt_Al_colony$Shapiro_P,
    Alt_Al_organic$Shapiro_P,
    Alt_Ca_colony$Shapiro_P,
    Alt_Fe_colony$Shapiro_P,
    Alt_Fe_organic$Shapiro_P
  ),
  
  Levene_P = c(
    Alt_Al_colony$Levene_P,
    Alt_Al_organic$Levene_P,
    Alt_Ca_colony$Levene_P,
    Alt_Fe_colony$Levene_P,
    Alt_Fe_organic$Levene_P
  )
)


print(Overall_Results)


#==================================================
# 9. EXPORT OVERALL RESULTS
#==================================================

write_xlsx(
  Overall_Results,
  "Alternative_Phosphate_Overall_Statistics.xlsx"
)


#==================================================
# 10. CREATE POST-HOC RESULTS
#==================================================

Posthoc_Results <- list()


#--------------------------------------------------
# Alt-Al Colony
#--------------------------------------------------

if (Alt_Al_colony$Test == "One-way ANOVA") {
  
  if (nrow(Alt_Al_colony$Tukey) > 0) {
    Posthoc_Results[["Alt-Al Colony Tukey"]] <-
      Alt_Al_colony$Tukey
  }
  
} else if (Alt_Al_colony$Test == "Kruskal-Wallis") {
  
  if (nrow(Alt_Al_colony$Dunn) > 0) {
    Posthoc_Results[["Alt-Al Colony Dunn"]] <-
      Alt_Al_colony$Dunn
  }
}

#--------------------------------------------------
# Alt-Al Organic Acid
#--------------------------------------------------

if (Alt_Al_organic$Test == "One-way ANOVA") {
  
  if (nrow(Alt_Al_organic$Tukey) > 0) {
    Posthoc_Results[["Alt-Al Organic Tukey"]] <-
      Alt_Al_organic$Tukey
  }
  
} else if (Alt_Al_organic$Test == "Kruskal-Wallis") {
  
  if (nrow(Alt_Al_organic$Dunn) > 0) {
    Posthoc_Results[["Alt-Al Organic Dunn"]] <-
      Alt_Al_organic$Dunn
  }
}

#--------------------------------------------------
# Alt-Ca Colony
#--------------------------------------------------

if (Alt_Ca_colony$Test == "One-way ANOVA") {
  
  if (nrow(Alt_Ca_colony$Tukey) > 0) {
    Posthoc_Results[["Alt-Ca Colony Tukey"]] <-
      Alt_Ca_colony$Tukey
  }
  
} else if (Alt_Ca_colony$Test == "Kruskal-Wallis") {
  
  if (nrow(Alt_Ca_colony$Dunn) > 0) {
    Posthoc_Results[["Alt-Ca Colony Dunn"]] <-
      Alt_Ca_colony$Dunn
  }
}


#--------------------------------------------------
# Alt-Fe Colony
#--------------------------------------------------

if (Alt_Fe_colony$Test == "One-way ANOVA") {
  
  if (nrow(Alt_Fe_colony$Tukey) > 0) {
    Posthoc_Results[["Alt-Fe Colony Tukey"]] <-
      Alt_Fe_colony$Tukey
  }
  
} else if (Alt_Fe_colony$Test == "Kruskal-Wallis") {
  
  if (nrow(Alt_Fe_colony$Dunn) > 0) {
    Posthoc_Results[["Alt-Fe Colony Dunn"]] <-
      Alt_Fe_colony$Dunn
  }
}


#--------------------------------------------------
# Alt-Fe Organic Acid
#--------------------------------------------------

if (Alt_Fe_organic$Test == "One-way ANOVA") {
  
  if (nrow(Alt_Fe_organic$Tukey) > 0) {
    Posthoc_Results[["Alt-Fe Organic Tukey"]] <-
      Alt_Fe_organic$Tukey
  }
  
} else if (Alt_Fe_organic$Test == "Kruskal-Wallis") {
  
  if (nrow(Alt_Fe_organic$Dunn) > 0) {
    Posthoc_Results[["Alt-Fe Organic Dunn"]] <-
      Alt_Fe_organic$Dunn
  }
}


#==================================================
# 11. EXPORT POST-HOC RESULTS
#==================================================

if (length(Posthoc_Results) > 0) {
  
  write_xlsx(
    Posthoc_Results,
    "Alternative_Phosphate_Posthoc_Results.xlsx"
  )
  
} else {
  
  cat(
    "\nNo significant post-hoc results to export.\n"
  )
}


#==================================================
# 12. FUNCTION TO EXTRACT SIGNIFICANT DUNN RESULTS
#==================================================

get_significant_dunn <- function(
    dunn_result,
    trait_name
) {
  
  if (
    is.null(dunn_result) ||
    nrow(dunn_result) == 0
  ) {
    
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
    
    filter(
      p.adj < 0.05
    ) %>%
    
    transmute(
      
      Trait = trait_name,
      
      Strain_1 = group1,
      
      Strain_2 = group2,
      
      Statistic = statistic,
      
      P_value = p,
      
      Adjusted_P = p.adj,
      
      Significance = case_when(
        p.adj < 0.0001 ~ "****",
        p.adj < 0.001 ~ "***",
        p.adj < 0.01 ~ "**",
        p.adj < 0.05 ~ "*"
      )
    ) %>%
    
    arrange(
      Adjusted_P
    )
}


#==================================================
# 13. EXTRACT SIGNIFICANT DUNN RESULTS
#==================================================

Alt_Al_colony_sig <- data.frame()

Alt_Al_organic_sig <- data.frame()

Alt_Ca_colony_sig <- data.frame()

Alt_Fe_colony_sig <- data.frame()

Alt_Fe_organic_sig <- data.frame()


#--------------------------------------------------
# Alt-Al Colony
#--------------------------------------------------

if (
  Alt_Al_colony$Test == "Kruskal-Wallis" &&
  nrow(Alt_Al_colony$Dunn) > 0
) {
  
  Alt_Al_colony_sig <- get_significant_dunn(
    Alt_Al_colony$Dunn,
    "Alt-Al: Colony size"
  )
}


#--------------------------------------------------
# Alt-Al Organic Acid
#--------------------------------------------------

if (
  Alt_Al_organic$Test == "Kruskal-Wallis" &&
  nrow(Alt_Al_organic$Dunn) > 0
) {
  
  Alt_Al_organic_sig <- get_significant_dunn(
    Alt_Al_organic$Dunn,
    "Alt-Al: Organic acid production"
  )
}


#--------------------------------------------------
# Alt-Ca Colony
#--------------------------------------------------

if (
  Alt_Ca_colony$Test == "Kruskal-Wallis" &&
  nrow(Alt_Ca_colony$Dunn) > 0
) {
  
  Alt_Ca_colony_sig <- get_significant_dunn(
    Alt_Ca_colony$Dunn,
    "Alt-Ca: Colony size"
  )
}


#--------------------------------------------------
# Alt-Fe Colony
#--------------------------------------------------

if (
  Alt_Fe_colony$Test == "Kruskal-Wallis" &&
  nrow(Alt_Fe_colony$Dunn) > 0
) {
  
  Alt_Fe_colony_sig <- get_significant_dunn(
    Alt_Fe_colony$Dunn,
    "Alt-Fe: Colony size"
  )
}


#--------------------------------------------------
# Alt-Fe Organic Acid
#--------------------------------------------------

if (
  Alt_Fe_organic$Test == "Kruskal-Wallis" &&
  nrow(Alt_Fe_organic$Dunn) > 0
) {
  
  Alt_Fe_organic_sig <- get_significant_dunn(
    Alt_Fe_organic$Dunn,
    "Alt-Fe: Organic acid production"
  )
}


#==================================================
# 14. COMBINE SIGNIFICANT DUNN RESULTS
#==================================================

All_significant_Dunn <- bind_rows(
  Alt_Al_colony_sig,
  Alt_Al_organic_sig,
  Alt_Ca_colony_sig,
  Alt_Fe_colony_sig,
  Alt_Fe_organic_sig
)


#--------------------------------------------------
# Print significant results
#--------------------------------------------------

print(All_significant_Dunn)


#==================================================
# 15. EXPORT SIGNIFICANT DUNN RESULTS
#==================================================

Significant_Dunn_Results <- list(
  
  `Alt-Al Colony` = Alt_Al_colony_sig,
  
  `Alt-Al Organic Acid` = Alt_Al_organic_sig,
  
  `Alt-Ca Colony` = Alt_Ca_colony_sig,
  
  `Alt-Fe Colony` = Alt_Fe_colony_sig,
  
  `Alt-Fe Organic Acid` = Alt_Fe_organic_sig,
  
  `All Significant` = All_significant_Dunn
)


write_xlsx(
  Significant_Dunn_Results,
  "Alternative_Significant_Dunn_Comparisons.xlsx"
)


#==================================================
# 16. FINAL MESSAGE
#==================================================

cat(
  "\n============================================\n",
  "ANALYSIS COMPLETE\n",
  "============================================\n\n",
  "Overall statistics:\n",
  "Alternative_Phosphate_Overall_Statistics.xlsx\n\n",
  "Post-hoc results:\n",
  "Alternative_Phosphate_Posthoc_Results.xlsx\n\n",
  "Significant Dunn comparisons:\n",
  "Alternative_Significant_Dunn_Comparisons.xlsx\n\n",
  "Files saved in:\n",
  getwd(),
  "\n"
)
