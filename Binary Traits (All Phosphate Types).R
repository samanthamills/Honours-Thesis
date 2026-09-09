#===========================================================
# CREATE BINARY PHENOTYPE TRAITS FOR SCOARY/PYSEER
#
# Input:
#   Raw_Phosphate_Results.xlsx
# Excel sheets:
#   Al-ph
#   Ca-ph
#   Fe-ph
# Output:
#   Binary_Traits.xlsx
#   Phenotype_Thresholds.xlsx
# Six binary traits:
#   1. Al-ph Colony Diameter
#   2. Al-ph Organic Acid Production
#   3. Ca-ph Colony Diameter
#   4. Ca-ph Zone of Clearing
#   5. Fe-ph Colony Diameter
#   6. Fe-ph Organic Acid Production
# Binary coding:
#   0 = below the mean
#   1 = at or above the mean
#   NA = no experimental data
# Each phenotype has its OWN threshold calculated from
# that phenotype and phosphate medium.
#===========================================================

#===========================================================
# 1. LOAD PACKAGES
#===========================================================

library(readxl)
library(dplyr)
library(stringr)
library(writexl)

#===========================================================
# 2. SET DIRECTORIES
#===========================================================

working_directory <-
  "/Users/samanthamills/Honours Project (R Studio)/Scoary/Output"
data_directory <-
  "/Users/samanthamills/Honours Project (R Studio)/Scoary/Data"

#-----------------------------------------------------------
# Set input file
#-----------------------------------------------------------

raw_file <- file.path(
  data_directory,
  "Raw_Phosphate_Results.xlsx"
)

#===========================================================
# 3. READ THE THREE EXCEL SHEETS
#===========================================================

al_raw <- read_excel(
  raw_file,
  sheet = "Al-ph"
)
ca_raw <- read_excel(
  raw_file,
  sheet = "Ca-ph"
)
fe_raw <- read_excel(
  raw_file,
  sheet = "Fe-ph"
)

#===========================================================
# 4. CONVERT MEASUREMENT COLUMNS TO NUMERIC
#===========================================================

al_raw <- al_raw %>%
  mutate(
    across(
      matches("^Colony size[1-6]$"),
      ~ as.numeric(.x)
    ),
    across(
      matches("^Organic Acid Production[1-6]$"),
      ~ as.numeric(.x)
    )
  )

ca_raw <- ca_raw %>%
  mutate(
    across(
      matches("^Colony size[1-6]$"),
      ~ as.numeric(.x)
    ),
    across(
      matches("^Zone of clearing[1-6]$"),
      ~ as.numeric(.x)
    )
  )

fe_raw <- fe_raw %>%
  mutate(
    across(
      matches("^Colony size[1-6]$"),
      ~ as.numeric(.x)
    ),
    across(
      matches("^Organic Acid Production[1-6]$"),
      ~ as.numeric(.x)
    )
  )

#===========================================================
# 5. CALCULATE MEAN PHENOTYPES FOR EACH STRAIN
#===========================================================

# The six replicate measurements for each phenotype are
# averaged for each strain.
# If all six replicates are missing, the result is converted
# from NaN to NA.

#-----------------------------------------------------------
# 5a. Al-ph summary
#-----------------------------------------------------------

al_summary <- al_raw %>%
  mutate(
    Al_ph_Colony_Diameter_Mean = rowMeans(
      across(
        matches("^Colony size[1-6]$")
      ),
      na.rm = TRUE
    ),
    Al_ph_Organic_Acid_Mean = rowMeans(
      across(
        matches("^Organic Acid Production[1-6]$")
      ),
      na.rm = TRUE
    )
  ) %>%
  
  mutate(
    Al_ph_Colony_Diameter_Mean =
      ifelse(
        is.nan(Al_ph_Colony_Diameter_Mean),
        NA,
        Al_ph_Colony_Diameter_Mean
      ),
    Al_ph_Organic_Acid_Mean =
      ifelse(
        is.nan(Al_ph_Organic_Acid_Mean),
        NA,
        Al_ph_Organic_Acid_Mean
      )
  ) %>%
  select(
    BW_Strain,
    Al_ph_Colony_Diameter_Mean,
    Al_ph_Organic_Acid_Mean
  )

#-----------------------------------------------------------
# 5b. Ca-ph summary
#-----------------------------------------------------------

ca_summary <- ca_raw %>%
  mutate(
    Ca_ph_Colony_Diameter_Mean = rowMeans(
      across(
        matches("^Colony size[1-6]$")
      ),
      na.rm = TRUE
    ),
    Ca_ph_Zone_Clearing_Mean = rowMeans(
      across(
        matches("^Zone of clearing[1-6]$")
      ),
      na.rm = TRUE
    )
  ) %>%
  mutate(
    Ca_ph_Colony_Diameter_Mean =
      ifelse(
        is.nan(Ca_ph_Colony_Diameter_Mean),
        NA,
        Ca_ph_Colony_Diameter_Mean
      ),
    Ca_ph_Zone_Clearing_Mean =
      ifelse(
        is.nan(Ca_ph_Zone_Clearing_Mean),
        NA,
        Ca_ph_Zone_Clearing_Mean
      )
  ) %>%
  select(
    BW_Strain,
    Ca_ph_Colony_Diameter_Mean,
    Ca_ph_Zone_Clearing_Mean
  )

#-----------------------------------------------------------
# 5c. Fe-ph summary
#-----------------------------------------------------------

fe_summary <- fe_raw %>%
  mutate(
    Fe_ph_Colony_Diameter_Mean = rowMeans(
      across(
        matches("^Colony size[1-6]$")
      ),
      na.rm = TRUE
    ),
    Fe_ph_Organic_Acid_Mean = rowMeans(
      across(
        matches("^Organic Acid Production[1-6]$")
      ),
      na.rm = TRUE
    )
  ) %>%
  mutate(
    Fe_ph_Colony_Diameter_Mean =
      ifelse(
        is.nan(Fe_ph_Colony_Diameter_Mean),
        NA,
        Fe_ph_Colony_Diameter_Mean
      ),
    Fe_ph_Organic_Acid_Mean =
      ifelse(
        is.nan(Fe_ph_Organic_Acid_Mean),
        NA,
        Fe_ph_Organic_Acid_Mean
      )
  ) %>%
  select(
    BW_Strain,
    Fe_ph_Colony_Diameter_Mean,
    Fe_ph_Organic_Acid_Mean
  )

#===========================================================
# 6. CALCULATE THE SIX PHENOTYPE THRESHOLDS
#===========================================================

# Each phenotype gets its own threshold.
# The threshold is the mean across ALL STRAINS with
# available measurements for that particular phenotype.

Al_ph_Colony_Diameter_Threshold <- mean(
  al_summary$Al_ph_Colony_Diameter_Mean,
  na.rm = TRUE
)
Al_ph_Organic_Acid_Threshold <- mean(
  al_summary$Al_ph_Organic_Acid_Mean,
  na.rm = TRUE
)


Ca_ph_Colony_Diameter_Threshold <- mean(
  ca_summary$Ca_ph_Colony_Diameter_Mean,
  na.rm = TRUE
)
Ca_ph_Zone_Clearing_Threshold <- mean(
  ca_summary$Ca_ph_Zone_Clearing_Mean,
  na.rm = TRUE
)


Fe_ph_Colony_Diameter_Threshold <- mean(
  fe_summary$Fe_ph_Colony_Diameter_Mean,
  na.rm = TRUE
)
Fe_ph_Organic_Acid_Threshold <- mean(
  fe_summary$Fe_ph_Organic_Acid_Mean,
  na.rm = TRUE
)

#-----------------------------------------------------------
# 6a. CREATE AL-PH BINARY TRAITS
#-----------------------------------------------------------

al_binary <- al_summary %>%
  mutate(
    Al_ph_Colony_Diameter = case_when(
      is.na(Al_ph_Colony_Diameter_Mean) ~ NA_real_,
      Al_ph_Colony_Diameter_Mean >=
        Al_ph_Colony_Diameter_Threshold ~ 1,
      Al_ph_Colony_Diameter_Mean <
        Al_ph_Colony_Diameter_Threshold ~ 0
    ),
    
    Al_ph_Organic_Acid = case_when(
      is.na(Al_ph_Organic_Acid_Mean) ~ NA_real_,
      Al_ph_Organic_Acid_Mean >=
        Al_ph_Organic_Acid_Threshold ~ 1,
      Al_ph_Organic_Acid_Mean <
        Al_ph_Organic_Acid_Threshold ~ 0
    )
  ) %>%
  
  select(
    BW_Strain,
    Al_ph_Colony_Diameter,
    Al_ph_Organic_Acid
  )

#-----------------------------------------------------------
# 6b. CREATE CA-PH BINARY TRAITS
#-----------------------------------------------------------

ca_binary <- ca_summary %>%
  mutate(
    Ca_ph_Colony_Diameter = case_when(
      is.na(Ca_ph_Colony_Diameter_Mean) ~ NA_real_,
      Ca_ph_Colony_Diameter_Mean >=
        Ca_ph_Colony_Diameter_Threshold ~ 1,
      Ca_ph_Colony_Diameter_Mean <
        Ca_ph_Colony_Diameter_Threshold ~ 0
    ),
    
    Ca_ph_Zone_of_Clearing = case_when(
      is.na(Ca_ph_Zone_Clearing_Mean) ~ NA_real_,
      Ca_ph_Zone_Clearing_Mean >=
        Ca_ph_Zone_Clearing_Threshold ~ 1,
      Ca_ph_Zone_Clearing_Mean <
        Ca_ph_Zone_Clearing_Threshold ~ 0
    )
  ) %>%
  
  select(
    BW_Strain,
    Ca_ph_Colony_Diameter,
    Ca_ph_Zone_of_Clearing
  )

#-----------------------------------------------------------
# 6c. CREATE FE-PH BINARY TRAITS
#-----------------------------------------------------------

fe_binary <- fe_summary %>%
  mutate(
    Fe_ph_Colony_Diameter = case_when(
      is.na(Fe_ph_Colony_Diameter_Mean) ~ NA_real_,
      Fe_ph_Colony_Diameter_Mean >=
        Fe_ph_Colony_Diameter_Threshold ~ 1,
      Fe_ph_Colony_Diameter_Mean <
        Fe_ph_Colony_Diameter_Threshold ~ 0
    ),
    
    Fe_ph_Organic_Acid = case_when(
      is.na(Fe_ph_Organic_Acid_Mean) ~ NA_real_,
      Fe_ph_Organic_Acid_Mean >=
        Fe_ph_Organic_Acid_Threshold ~ 1,
      Fe_ph_Organic_Acid_Mean <
        Fe_ph_Organic_Acid_Threshold ~ 0
    )
  ) %>%
  
  select(
    BW_Strain,
    Fe_ph_Colony_Diameter,
    Fe_ph_Organic_Acid
  )

#===========================================================
# 7. COMBINE ALL SIX BINARY TRAITS
#===========================================================

binary_traits <- full_join(
  al_binary,
  ca_binary,
  by = "BW_Strain"
) %>%
  full_join(
    fe_binary,
    by = "BW_Strain"
  )

#===========================================================
# 8. ARRANGE STRAINS NUMERICALLY
#===========================================================

binary_traits <- binary_traits %>%
  mutate(
    strain_number = as.numeric(
      str_extract(
        BW_Strain,
        "\\d+"
      )
    )
  ) %>%
  arrange(
    strain_number
  ) %>%
  select(
    -strain_number
  )

#===========================================================
# 9. PUT COLUMNS IN EXACT ORDER
#===========================================================

binary_traits <- binary_traits %>%
  select(
    BW_Strain,
    Al_ph_Colony_Diameter,
    Al_ph_Organic_Acid,
    Ca_ph_Colony_Diameter,
    Ca_ph_Zone_of_Clearing,
    Fe_ph_Colony_Diameter,
    Fe_ph_Organic_Acid
  )

#===========================================================
# 10. CREATE THRESHOLD TABLE
#===========================================================

thresholds <- data.frame(
  Trait = c(
    "Al-ph Colony Diameter",
    "Al-ph Organic Acid Production",
    "Ca-ph Colony Diameter",
    "Ca-ph Zone of Clearing",
    "Fe-ph Colony Diameter",
    "Fe-ph Organic Acid Production"
  ),
  
  Threshold = c(
    Al_ph_Colony_Diameter_Threshold,
    Al_ph_Organic_Acid_Threshold,
    Ca_ph_Colony_Diameter_Threshold,
    Ca_ph_Zone_Clearing_Threshold,
    Fe_ph_Colony_Diameter_Threshold,
    Fe_ph_Organic_Acid_Threshold
  )
)

#===========================================================
# 11. SAVE FINAL EXCEL FILE
#===========================================================

write_xlsx(
  binary_traits,
  file.path(
    working_directory,
    "Binary_Traits.xlsx"
  )
)

#===========================================================
# 12. SAVE THRESHOLDS AS EXCEL
#===========================================================

write_xlsx(
  thresholds,
  file.path(
    working_directory,
    "Phenotype_Thresholds.xlsx"
  )
)

#===========================================================
# !! 13. SAVE COMPLETE CHECKING WORKBOOK !!
#===========================================================

# This workbook contains:
#   Sheet 1 = Six binary traits
#   Sheet 2 = Thresholds
#   Sheet 3 = Binary counts
#   Sheet 4 = Al-ph means
#   Sheet 5 = Ca-ph means
#   Sheet 6 = Fe-ph means

write_xlsx(
  list(
    Binary_Traits = binary_traits,
    Thresholds = thresholds,
    Binary_Counts = binary_counts,
    Al_ph_Means = al_summary,
    Ca_ph_Means = ca_summary,
    Fe_ph_Means = fe_summary
  ),
  file.path(
    working_directory,
    "Binary_Traits_Check.xlsx"
  )
)

#===========================================================
# END OF SCRIPT
#===========================================================