#===========================================================
# RANKING STRAINS FOR EACH PHOSPHATE SOURCE
# AND OVERALL ACROSS ALL PHOSPHATE SOURCES
#===========================================================

#===========================================================
# 1. Install/load packages
#===========================================================

# Install if needed:
install.packages(c("readxl", "dplyr", "tidyr", "writexl", "stringr"))

library(readxl)
library(dplyr)
library(tidyr)
library(writexl)
library(stringr)


#===========================================================
# 2. Set working directory
#===========================================================

setwd("/Users/samanthamills/Honours Project (R Studio)/Lab Result Graphing")
file_directory <- "/Users/samanthamills/Honours Project (R Studio)/Lab Result Graphing/Data"

#===========================================================
# 3. READ EXCEL SHEETS
#===========================================================

file_path <- file.path(
  file_directory,
  "Raw_Phosphate_Results.xlsx"
)
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

#------------------
# Check column names
#------------------

print(names(al_ph))
print(names(ca_ph))
print(names(fe_ph))

#===========================================================
# 4. FUNCTION TO CONVERT GROWTH TO 0 / 1
#===========================================================

# Yes = 1 = growth
# No = 0 = no growth
# NA / blank = NA = no data

convert_growth <- function(x) {
  x <- str_to_lower(
    str_trim(
      as.character(x)
    )
  )
  case_when(
    x == "yes" ~ 1,
    x == "no" ~ 0,
    is.na(x) | x == "" ~ NA_real_,
    TRUE ~ NA_real_
  )
}

#===========================================================
# 5. AL-PH DATA
#===========================================================

#------------------
# Identify columns
#------------------

al_growth_cols <- grep(
  "^Growth",
  names(al_ph),
  value = TRUE,
  ignore.case = TRUE
)
al_colony_cols <- grep(
  "^Colony",
  names(al_ph),
  value = TRUE,
  ignore.case = TRUE
)
al_acid_cols <- grep(
  "Organic|Acid|Colour|Color",
  names(al_ph),
  value = TRUE,
  ignore.case = TRUE
)

print(al_growth_cols)
print(al_colony_cols)
print(al_acid_cols)

#------------------
# Calculate Al-ph means
#------------------

al_summary <- al_ph %>%
  mutate(
    `BW_Strain` = as.character(`BW_Strain`)
  ) %>%
  mutate(
    # Growth: Yes = 1, No = 0, NA = NA
    across(
      all_of(al_growth_cols),
      convert_growth
    ),
    # Colony diameter
    across(
      all_of(al_colony_cols),
      ~ suppressWarnings(as.numeric(.))
    ),
    # Organic acid / colour change
    across(
      all_of(al_acid_cols),
      ~ suppressWarnings(as.numeric(.))
    )
  ) %>%
  
  rowwise() %>%
  mutate(
    Al_ph_Growth_Mean = if(
      all(is.na(c_across(all_of(al_growth_cols))))
    ) {
      NA_real_
    } else {
      mean(
        c_across(all_of(al_growth_cols)),
        na.rm = TRUE
      )
    },
    Al_ph_Colony_Diameter_Mean = if(
      all(is.na(c_across(all_of(al_colony_cols))))
    ) {
      NA_real_
    } else {
      mean(
        c_across(all_of(al_colony_cols)),
        na.rm = TRUE
      )
    },
    Al_ph_Organic_Acid_Mean = if(
      all(is.na(c_across(all_of(al_acid_cols))))
    ) {
      NA_real_
    } else {
      mean(
        c_across(all_of(al_acid_cols)),
        na.rm = TRUE
      )
    }
  ) %>%
  
  ungroup() %>%
  select(
    `BW_Strain`,
    Al_ph_Growth_Mean,
    Al_ph_Colony_Diameter_Mean,
    Al_ph_Organic_Acid_Mean
  )

#------------------
# Remove duplicates
#------------------

al_summary <- al_summary %>%
  group_by(`BW_Strain`) %>%
  summarise(
    Al_ph_Growth_Mean = if(
      all(is.na(Al_ph_Growth_Mean))
    ) NA_real_
    else mean(
      Al_ph_Growth_Mean,
      na.rm = TRUE
    ),
    Al_ph_Colony_Diameter_Mean = if(
      all(is.na(Al_ph_Colony_Diameter_Mean))
    ) NA_real_
    else mean(
      Al_ph_Colony_Diameter_Mean,
      na.rm = TRUE
    ),
    Al_ph_Organic_Acid_Mean = if(
      all(is.na(Al_ph_Organic_Acid_Mean))
    ) NA_real_
    else mean(
      Al_ph_Organic_Acid_Mean,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  rename(
    BW_Strain = `BW_Strain`
  )

#===========================================================
# 6. CA-PH DATA
#===========================================================

#------------------
# Identify columns
#------------------

ca_growth_cols <- grep(
  "^Growth",
  names(ca_ph),
  value = TRUE,
  ignore.case = TRUE
)
ca_colony_cols <- grep(
  "^Colony",
  names(ca_ph),
  value = TRUE,
  ignore.case = TRUE
)
ca_zone_cols <- grep(
  "Zone",
  names(ca_ph),
  value = TRUE,
  ignore.case = TRUE
)

print(ca_growth_cols)
print(ca_colony_cols)
print(ca_zone_cols)

#------------------
# Calculate Ca-ph means
#------------------

ca_summary <- ca_ph %>%
  mutate(
    `BW Strain` = as.character(`BW_Strain`)
  ) %>%
  mutate(
    # Growth
    across(
      all_of(ca_growth_cols),
      convert_growth
    ),
    # Colony diameter
    across(
      all_of(ca_colony_cols),
      ~ suppressWarnings(as.numeric(.))
    ),
    # Zone of clearing
    across(
      all_of(ca_zone_cols),
      ~ suppressWarnings(as.numeric(.))
    )
  ) %>%
  
  rowwise() %>%
  mutate(
    Ca_ph_Growth_Mean = if(
      all(is.na(c_across(all_of(ca_growth_cols))))
    ) {
      NA_real_
    } else {
      mean(
        c_across(all_of(ca_growth_cols)),
        na.rm = TRUE
      )
    },
    Ca_ph_Colony_Diameter_Mean = if(
      all(is.na(c_across(all_of(ca_colony_cols))))
    ) {
      NA_real_
    } else {
      mean(
        c_across(all_of(ca_colony_cols)),
        na.rm = TRUE
      )
    },
    Ca_ph_Zone_Clearing_Mean = if(
      all(is.na(c_across(all_of(ca_zone_cols))))
    ) {
      NA_real_
    } else {
      mean(
        c_across(all_of(ca_zone_cols)),
        na.rm = TRUE
      )
    }
  ) %>%
  
  ungroup() %>%
  select(
    `BW_Strain`,
    Ca_ph_Growth_Mean,
    Ca_ph_Colony_Diameter_Mean,
    Ca_ph_Zone_Clearing_Mean
  )

#------------------
# Remove duplicates
#------------------

ca_summary <- ca_summary %>%
  group_by(`BW_Strain`) %>%
  summarise(
    Ca_ph_Growth_Mean = if(
      all(is.na(Ca_ph_Growth_Mean))
    ) NA_real_
    else mean(
      Ca_ph_Growth_Mean,
      na.rm = TRUE
    ),
    Ca_ph_Colony_Diameter_Mean = if(
      all(is.na(Ca_ph_Colony_Diameter_Mean))
    ) NA_real_
    else mean(
      Ca_ph_Colony_Diameter_Mean,
      na.rm = TRUE
    ),
    Ca_ph_Zone_Clearing_Mean = if(
      all(is.na(Ca_ph_Zone_Clearing_Mean))
    ) NA_real_
    else mean(
      Ca_ph_Zone_Clearing_Mean,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  rename(
    BW_Strain = `BW_Strain`
  )

#===========================================================
# 7. FE-PH DATA
#===========================================================

#------------------
# Identify columns
#------------------

fe_growth_cols <- grep(
  "^Growth",
  names(fe_ph),
  value = TRUE,
  ignore.case = TRUE
)
fe_colony_cols <- grep(
  "^Colony",
  names(fe_ph),
  value = TRUE,
  ignore.case = TRUE
)
fe_acid_cols <- grep(
  "Organic|Acid|Colour|Color",
  names(fe_ph),
  value = TRUE,
  ignore.case = TRUE
)

print(fe_growth_cols)
print(fe_colony_cols)
print(fe_acid_cols)

#------------------
# Calculate Fe-ph means
#------------------

fe_summary <- fe_ph %>%
  mutate(
    `BW Strain` = as.character(`BW_Strain`)
  ) %>%
  mutate(
    # Growth
    across(
      all_of(fe_growth_cols),
      convert_growth
    ),
    # Colony diameter
    across(
      all_of(fe_colony_cols),
      ~ suppressWarnings(as.numeric(.))
    ),
    # Organic acid / colour change
    across(
      all_of(fe_acid_cols),
      ~ suppressWarnings(as.numeric(.))
    )
  ) %>%
  
  rowwise() %>%
  mutate(
    Fe_ph_Growth_Mean = if(
      all(is.na(c_across(all_of(fe_growth_cols))))
    ) {
      NA_real_
    } else {
      mean(
        c_across(all_of(fe_growth_cols)),
        na.rm = TRUE
      )
    },
    Fe_ph_Colony_Diameter_Mean = if(
      all(is.na(c_across(all_of(fe_colony_cols))))
    ) {
      NA_real_
    } else {
      mean(
        c_across(all_of(fe_colony_cols)),
        na.rm = TRUE
      )
    },
    Fe_ph_Organic_Acid_Mean = if(
      all(is.na(c_across(all_of(fe_acid_cols))))
    ) {
      NA_real_
    } else {
      mean(
        c_across(all_of(fe_acid_cols)),
        na.rm = TRUE
      )
    }
  ) %>%
  
  ungroup() %>%
  select(
    `BW_Strain`,
    Fe_ph_Growth_Mean,
    Fe_ph_Colony_Diameter_Mean,
    Fe_ph_Organic_Acid_Mean
  )

#------------------
# Remove duplicates
#------------------

fe_summary <- fe_summary %>%
  group_by(`BW_Strain`) %>%
  summarise(
    Fe_ph_Growth_Mean = if(
      all(is.na(Fe_ph_Growth_Mean))
    ) NA_real_
    else mean(
      Fe_ph_Growth_Mean,
      na.rm = TRUE
    ),
    Fe_ph_Colony_Diameter_Mean = if(
      all(is.na(Fe_ph_Colony_Diameter_Mean))
    ) NA_real_
    else mean(
      Fe_ph_Colony_Diameter_Mean,
      na.rm = TRUE
    ),
    Fe_ph_Organic_Acid_Mean = if(
      all(is.na(Fe_ph_Organic_Acid_Mean))
    ) NA_real_
    else mean(
      Fe_ph_Organic_Acid_Mean,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  rename(
    BW_Strain = `BW_Strain`
  )

#===========================================================
# 8. COMBINE ALL PHOSPHATE SOURCES
#===========================================================

combined_summary <- al_summary %>%
  full_join(
    ca_summary,
    by = "BW_Strain"
  ) %>%
  full_join(
    fe_summary,
    by = "BW_Strain"
  )

#------------------
# Arrange strains numerically
#------------------

combined_summary <- combined_summary %>%
  mutate(
    strain_number = as.numeric(
      str_extract(
        BW_Strain,
        "\\d+"
      )
    )
  ) %>%
  arrange(strain_number) %>%
  select(-strain_number)

#===========================================================
# 9. AL-PH RANKING
#===========================================================

# The three Al-ph traits are:
# 1. Growth
# 2. Colony diameter
# 3. Organic acid production

al_ranked <- combined_summary %>%
  select(
    BW_Strain,
    Al_ph_Growth_Mean,
    Al_ph_Colony_Diameter_Mean,
    Al_ph_Organic_Acid_Mean
  ) %>%
  mutate(
    # Z-scores for each trait
    Al_Growth_Z = as.numeric(
      scale(Al_ph_Growth_Mean)
    ),
    Al_Colony_Z = as.numeric(
      scale(Al_ph_Colony_Diameter_Mean)
    ),
    Al_Organic_Acid_Z = as.numeric(
      scale(Al_ph_Organic_Acid_Mean)
    )
  ) %>%
  
  mutate(
    # Average the three trait z-scores
    Al_Overall_Score = rowMeans(
      cbind(
        Al_Growth_Z,
        Al_Colony_Z,
        Al_Organic_Acid_Z
      ),
      na.rm = TRUE
    )
  ) %>%
  arrange(
    desc(Al_Overall_Score)
  ) %>%
  mutate(
    Al_Rank = row_number()
  )

#===========================================================
# 10. CA-PH RANKING
#===========================================================

# The three Ca-ph traits are:
# 1. Growth
# 2. Colony diameter
# 3. Zone of clearing

ca_ranked <- combined_summary %>%
  select(
    BW_Strain,
    Ca_ph_Growth_Mean,
    Ca_ph_Colony_Diameter_Mean,
    Ca_ph_Zone_Clearing_Mean
  ) %>%
  mutate(
    Ca_Growth_Z = as.numeric(
      scale(Ca_ph_Growth_Mean)
    ),
    Ca_Colony_Z = as.numeric(
      scale(Ca_ph_Colony_Diameter_Mean)
    ),
    Ca_Zone_Z = as.numeric(
      scale(Ca_ph_Zone_Clearing_Mean)
    )
  ) %>%

  mutate(
    Ca_Overall_Score = rowMeans(
      cbind(
        Ca_Growth_Z,
        Ca_Colony_Z,
        Ca_Zone_Z
      ),
      na.rm = TRUE
    )
  ) %>%
  arrange(
    desc(Ca_Overall_Score)
  ) %>%
  mutate(
    Ca_Rank = row_number()
  )

#===========================================================
# 11. FE-PH RANKING
#===========================================================

# The three Fe-ph traits are:
# 1. Growth
# 2. Colony diameter
# 3. Organic acid production

fe_ranked <- combined_summary %>%
  select(
    BW_Strain,
    Fe_ph_Growth_Mean,
    Fe_ph_Colony_Diameter_Mean,
    Fe_ph_Organic_Acid_Mean
  ) %>%
  mutate(
    Fe_Growth_Z = as.numeric(
      scale(Fe_ph_Growth_Mean)
    ),
    Fe_Colony_Z = as.numeric(
      scale(Fe_ph_Colony_Diameter_Mean)
    ),
    Fe_Organic_Acid_Z = as.numeric(
      scale(Fe_ph_Organic_Acid_Mean)
    )
  ) %>%
  
  mutate(
    Fe_Overall_Score = rowMeans(
      cbind(
        Fe_Growth_Z,
        Fe_Colony_Z,
        Fe_Organic_Acid_Z
      ),
      na.rm = TRUE
    )
  ) %>%
  arrange(
    desc(Fe_Overall_Score)
  ) %>%
  mutate(
    Fe_Rank = row_number()
  )

#===========================================================
# 12. COMBINE PHOSPHATE-SPECIFIC RANKINGS
#===========================================================

phosphate_rankings <- al_ranked %>%
  select(
    BW_Strain,
    Al_Rank,
    Al_Overall_Score
  ) %>%
  full_join(
    ca_ranked %>%
      select(
        BW_Strain,
        Ca_Rank,
        Ca_Overall_Score
      ),
    by = "BW_Strain"
  ) %>%
  full_join(
    fe_ranked %>%
      select(
        BW_Strain,
        Fe_Rank,
        Fe_Overall_Score
      ),
    by = "BW_Strain"
  )

#===========================================================
# 13. OVERALL RANKING ACROSS ALL PHOSPHATE SOURCES
#===========================================================

# Each phosphate source contributes equally:
# Al-ph score
# Ca-ph score
# Fe-ph score
# These are averaged to create the final score.

overall_ranking <- phosphate_rankings %>%
  mutate(
    Overall_Score = rowMeans(
      cbind(
        Al_Overall_Score,
        Ca_Overall_Score,
        Fe_Overall_Score
      ),
      na.rm = TRUE
    )
  ) %>%
  arrange(
    desc(Overall_Score)
  ) %>%
  mutate(
    Overall_Rank = row_number()
  )

#===========================================================
# 14. BEST / MIDDLE / WORST FOR EACH PHOSPHATE SOURCE
#===========================================================

#------------------
# Al-ph
#------------------

Al_Best <- al_ranked %>%
  slice_head(n = 4)
Al_Worst <- al_ranked %>%
  slice_tail(n = 4)
Al_Median <- median(
  al_ranked$Al_Overall_Score,
  na.rm = TRUE
)
Al_Middle <- al_ranked %>%
  mutate(
    Distance_from_Median =
      abs(Al_Overall_Score - Al_Median)
  ) %>%
  arrange(
    Distance_from_Median
  ) %>%
  slice_head(n = 4)

#------------------
# Ca-ph
#------------------

Ca_Best <- ca_ranked %>%
  slice_head(n = 4)
Ca_Worst <- ca_ranked %>%
  slice_tail(n = 4)
Ca_Median <- median(
  ca_ranked$Ca_Overall_Score,
  na.rm = TRUE
)
Ca_Middle <- ca_ranked %>%
  mutate(
    Distance_from_Median =
      abs(Ca_Overall_Score - Ca_Median)
  ) %>%
  arrange(
    Distance_from_Median
  ) %>%
  slice_head(n = 4)

#------------------
# Fe-ph
#------------------

Fe_Best <- fe_ranked %>%
  slice_head(n = 4)
Fe_Worst <- fe_ranked %>%
  slice_tail(n = 4)
Fe_Median <- median(
  fe_ranked$Fe_Overall_Score,
  na.rm = TRUE
)
Fe_Middle <- fe_ranked %>%
  mutate(
    Distance_from_Median =
      abs(Fe_Overall_Score - Fe_Median)
  ) %>%
  arrange(
    Distance_from_Median
  ) %>%
  slice_head(n = 4)

#===========================================================
# 15. BEST / MIDDLE / WORST OVERALL
#===========================================================

Overall_Best <- overall_ranking %>%
  slice_head(n = 4)
Overall_Worst <- overall_ranking %>%
  slice_tail(n = 4)
Overall_Median <- median(
  overall_ranking$Overall_Score,
  na.rm = TRUE
)
Overall_Middle <- overall_ranking %>%
  mutate(
    Distance_from_Median =
      abs(Overall_Score - Overall_Median)
  ) %>%
  arrange(
    Distance_from_Median
  ) %>%
  slice_head(n = 4)

#===========================================================
# 16. PRINT RESULTS
#===========================================================

cat("AL-PH BEST STRAINS\n")
print(
  Al_Best %>%
    select(
      BW_Strain,
      Al_Rank,
      Al_Overall_Score
    )
)

cat("AL-PH MIDDLE STRAINS\n")
print(
  Al_Middle %>%
    select(
      BW_Strain,
      Al_Rank,
      Al_Overall_Score
    )
)

cat("AL-PH WORST STRAINS\n")
print(
  Al_Worst %>%
    select(
      BW_Strain,
      Al_Rank,
      Al_Overall_Score
    )
)

cat("CA-PH BEST STRAINS\n")
print(
  Ca_Best %>%
    select(
      BW_Strain,
      Ca_Rank,
      Ca_Overall_Score
    )
)

cat("CA-PH MIDDLE STRAINS\n")
print(
  Ca_Middle %>%
    select(
      BW_Strain,
      Ca_Rank,
      Ca_Overall_Score
    )
)

cat("CA-PH WORST STRAINS\n")
print(
  Ca_Worst %>%
    select(
      BW_Strain,
      Ca_Rank,
      Ca_Overall_Score
    )
)

cat("FE-PH BEST STRAINS\n")
print(
  Fe_Best %>%
    select(
      BW_Strain,
      Fe_Rank,
      Fe_Overall_Score
    )
)

cat("FE-PH MIDDLE STRAINS\n")
print(
  Fe_Middle %>%
    select(
      BW_Strain,
      Fe_Rank,
      Fe_Overall_Score
    )
)

cat("FE-PH WORST STRAINS\n")
print(
  Fe_Worst %>%
    select(
      BW_Strain,
      Fe_Rank,
      Fe_Overall_Score
    )
)

cat("OVERALL BEST STRAINS\n")
print(
  Overall_Best %>%
    select(
      BW_Strain,
      Overall_Rank,
      Overall_Score
    )
)

cat("OVERALL MIDDLE STRAINS\n")
print(
  Overall_Middle %>%
    select(
      BW_Strain,
      Overall_Rank,
      Overall_Score
    )
)

cat("OVERALL WORST STRAINS\n")
print(
  Overall_Worst %>%
    select(
      BW_Strain,
      Overall_Rank,
      Overall_Score
    )
)

#===========================================================
# 17. EXPORT EVERYTHING TO EXCEL
#===========================================================

write_xlsx(
  list(
    # Raw phenotype summaries
    Combined_Summary = combined_summary,
    # Complete rankings for each phosphate source
    Al_ph_Ranking = al_ranked,
    Ca_ph_Ranking = ca_ranked,
    Fe_ph_Ranking = fe_ranked,
    # Ranking comparison between phosphate sources
    Phosphate_Rankings = phosphate_rankings,
    # Final overall ranking
    Overall_Ranking = overall_ranking,
    # Al-ph groups
    Al_Best = Al_Best,
    Al_Middle = Al_Middle,
    Al_Worst = Al_Worst,
    # Ca-ph groups
    Ca_Best = Ca_Best,
    Ca_Middle = Ca_Middle,
    Ca_Worst = Ca_Worst,
    # Fe-ph groups
    Fe_Best = Fe_Best,
    Fe_Middle = Fe_Middle,
    Fe_Worst = Fe_Worst,
    # Overall groups
    Overall_Best = Overall_Best,
    Overall_Middle = Overall_Middle,
    Overall_Worst = Overall_Worst
  ),
  
  file.path(
    "/Users/samanthamills/Honours Project (R Studio)/Lab Result Graphing/Output",
    "Phosphate_Strain_Rankings.xlsx"
  )
)

cat("RANKING COMPLETE\n")
cat("Excel file saved as:\n")
cat(
  file.path(
    file_directory,
    "Phosphate_Strain_Rankings.xlsx"
  )
)
cat("\n====================================\n")