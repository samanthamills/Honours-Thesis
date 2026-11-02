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
# 4. Save to Excel
# ---------------------------

write_xlsx(p_sol_level5_frequency_by_sample,
           "Output/phosphate_solubilisation_level5_frequency_by_sample.xlsx")

# ---------------------------
# 5. Gene frequency per level5 trait (cleaned names)
# ---------------------------

# Function to clean and summarize by level5 and level6
process_gene_lvl5 <- function(df) {
  df <- df %>%
    mutate(
      # Standardize case and trim whitespace
      level5 = toupper(trimws(level5)),
      level6 = toupper(trimws(level6)),
      
      # Remove "P-SOLUBILISATION-" prefix from level5
      level5 = gsub("^P-SOLUBILISATION-", "", level5),
      
      # Remove "->PGPT..." or anything after "->" from level6
      level6 = gsub("->.*", "", level6)
    ) %>%
    group_by(level5, level6) %>%
    summarise(freq_sum = sum(freq, na.rm = TRUE), .groups = "drop")
  
  return(df)
}

# Apply to all filtered data frames
gene_lvl5_list <- lapply(filtered_data_list, process_gene_lvl5)

# Add sample name to each data frame
for(i in seq_along(gene_lvl5_list)) {
  gene_lvl5_list[[i]]$df_name <- names(gene_lvl5_list)[i]
}

# Combine all samples
combined_gene_lvl5 <- bind_rows(gene_lvl5_list)

# ---------------------------
# 6. Total unique phosphate solubilisation genes
# ---------------------------

# Extract all PHOSPHATE_SOLUBILIZATION genes
all_phosphate_genes <- bind_rows(
  lapply(filtered_data_list, function(df) {
    df %>%
      mutate(
        # Standardize gene names
        level6 = toupper(trimws(level6)),
        
        # Remove anything after "->"
        level6 = gsub("->.*", "", level6)
      ) %>%
      select(level6)
  })
)

# Remove blank / NA gene names
all_phosphate_genes <- all_phosphate_genes %>%
  filter(
    !is.na(level6),
    level6 != ""
  )

# Get the complete list of unique genes
unique_phosphate_genes <- all_phosphate_genes %>%
  distinct(level6) %>%
  arrange(level6)

# Count the total number of unique genes
total_unique_phosphate_genes <- nrow(unique_phosphate_genes)

# Print result
cat(
  "Total number of unique PHOSPHATE_SOLUBILIZATION genes:",
  total_unique_phosphate_genes,
  "\n"
)

# Display the complete gene list
print(unique_phosphate_genes)

write_xlsx(
  unique_phosphate_genes,
  "Output/unique_phosphate_solubilisation_genes.xlsx"
)

# ---------------------------
# 7.. Calculate total frequency across all strains
# ---------------------------

total_freq <- combined_gene_lvl5 %>%
  group_by(level5, level6) %>%
  summarise(total_freq = sum(freq_sum, na.rm = TRUE), .groups = "drop")

# ---------------------------
# 8. Pivot to show individual frequencies per strain
# ---------------------------

individual_freq <- combined_gene_lvl5 %>%
  select(df_name, level5, level6, freq_sum) %>%
  pivot_wider(
    names_from = df_name,
    values_from = freq_sum,
    values_fill = 0
  )

# ---------------------------
# 9. Combine total and individual frequencies
# ---------------------------

gene_lvl5_matrix <- total_freq %>%
  left_join(individual_freq, by = c("level5", "level6"))

#----------------------------
# 10. Save results to excel
#----------------------------

write_xlsx(gene_lvl5_matrix, "Output/p_sol_gene_per_level5.xlsx")
