# Set working directory
setwd("/Users/samanthamills/Honours Project (R Studio)/PLaBAse R")

# Load libraries
library(dplyr)
library(tidyr)
library(writexl)
library(withr)

# Directory containing the .txt files
file_directory <- "~/Honours Project (R Studio)/PLaBAse R/Data"

# List all .txt files
file_list <- list.files(path = file_directory, pattern = "*Krona-blhm.txt", full.names = TRUE)

# Read all files into a list of data frames
data_list <- list()
for (file_path in file_list) {
  file_name_full <- tools::file_path_sans_ext(basename(file_path))
  file_name <- strsplit(file_name_full, "_Krona")[[1]][1]
  df <- read.delim(file_path, header = TRUE, sep = "\t")
  data_list[[file_name]] <- df
}

# ---------------------------
# 1. Filter PHOSPHATE_SOLUBILIZATION
# ---------------------------

process_df <- function(df) {
  filtered_df <- subset(df,
                        level3 == "PHOSPHATE_SOLUBILIZATION" &
                          !grepl("K-solubilisation", level5, ignore.case = TRUE))
  
  unique_level6 <- length(unique(filtered_df$level6))
  sum_freq <- sum(filtered_df$freq, na.rm = TRUE)
  
  return(list(filtered_df = filtered_df, unique_level6 = unique_level6, sum_freq = sum_freq))
}

results <- lapply(data_list, process_df)

new_summary_df <- data.frame(
  df_name = names(results),
  unique_level6 = sapply(results, function(x) x$unique_level6),
  sum_freq = sapply(results, function(x) x$sum_freq)
)

filtered_data_list <- lapply(results, function(x) x$filtered_df)

# ---------------------------
# 2. Clean and summarize level5 (merge typos)
# ---------------------------

# Mapping for known typos
level5_corrections <- c(
  "P-SOLUBILISATION-MALIC_ACID_TRASNPORT" = "P-SOLUBILISATION-MALIC_ACID_TRANSPORT"
  # Add more known corrections here if needed
)

process_df_1 <- function(df) {
  # Standardize case and trim whitespace
  df$level5 <- toupper(trimws(df$level5))
  
  # Apply known typo corrections
  df$level5 <- ifelse(df$level5 %in% names(level5_corrections),
                      level5_corrections[df$level5],
                      df$level5)
  
  # Summarise frequency by level5
  df_summary <- df %>%
    group_by(level5) %>%
    summarise(freq_sum = sum(freq, na.rm = TRUE), .groups = "drop")
  
  return(df_summary)
}

# Apply to all filtered data frames
results_1 <- lapply(filtered_data_list, process_df_1)

# ---------------------------
# 3. Combine all results and pivot
# ---------------------------

combined_df <- bind_rows(results_1, .id = "df_name") %>%
  group_by(df_name, level5) %>%
  summarise(freq_sum = sum(freq_sum, na.rm = TRUE), .groups = "drop")  # merges any remaining duplicates

new_summary_df_1 <- combined_df %>%
  pivot_wider(names_from = level5, values_from = freq_sum, values_fill = 0)

# Merge with original summary
p_sol_level5_frequency_by_sample <- merge(
  new_summary_df,
  new_summary_df_1,
  by = "df_name",
  all.x = TRUE
)

# ---------------------------
# 4. Genetically identical strains (cleaned names)
# ---------------------------

# Clean level5 and level6, summarize per strain
process_gene_lvl5_clean <- function(df) {
  df <- df %>%
    mutate(
      # Standardize case and trim whitespace
      level5 = toupper(trimws(level5)),
      level6 = toupper(trimws(level6)),
      
      # Clean level5 by removing P-SOLUBILISATION- prefix
      level5 = gsub("^P-SOLUBILISATION-", "", level5),
      
      # Clean level6 by removing anything after "->"
      level6 = gsub("->.*", "", level6)
    ) %>%
    group_by(level6) %>%
    summarise(freq_sum = sum(freq, na.rm = TRUE), .groups = "drop")
  
  return(df)
}

# Apply to all strains
gene_list <- lapply(filtered_data_list, process_gene_lvl5_clean)

# Add strain/sample names
for(i in seq_along(gene_list)) {
  gene_list[[i]]$strain <- names(gene_list)[i]
}

# Combine all strains
gene_long <- bind_rows(gene_list)

# Pivot to make strains columns, genes rows
gene_matrix <- gene_long %>%
  pivot_wider(names_from = strain, values_from = freq_sum, values_fill = 0)

# ---------------------------
# 5. Create a signature per strain for comparison
# ---------------------------
# We need genes as rows and strains as columns
# First, transpose to have strains as rows for creating signature

gene_matrix_long <- gene_matrix %>%
  pivot_longer(cols = -level6, names_to = "strain", values_to = "freq")

strain_profiles <- gene_matrix_long %>%
  pivot_wider(names_from = level6, values_from = freq) %>%
  mutate(
    profile_signature = apply(select(., -strain), 1, function(x) paste(x, collapse = "-"))
  )

# ---------------------------
# 6. Identify groups of genetically identical strains
# ---------------------------
identical_groups <- strain_profiles %>%
  group_by(profile_signature) %>%
  filter(n() > 1) %>%  # keep only groups with duplicates
  summarise(strains = paste(strain, collapse = ", "), .groups = "drop")

# Keep gene frequencies for each group (take one representative row per signature)
profile_details <- strain_profiles %>%
  filter(profile_signature %in% identical_groups$profile_signature) %>%
  select(-strain) %>%
  distinct(profile_signature, .keep_all = TRUE)

# Merge strains with their gene frequencies
genetically_identical_strains <- identical_groups %>%
  left_join(profile_details, by = "profile_signature") %>%
  select(strains, everything(), -profile_signature)

# ---------------------------
# 7. Save to Excel
# ---------------------------
write_xlsx(genetically_identical_strains, "Output/genetically_identical_strains.xlsx")

