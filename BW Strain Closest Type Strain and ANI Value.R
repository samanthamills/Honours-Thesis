#---------------------------------
# 0. Setting up
#---------------------------------

setwd("/Users/samanthamills/Honours Project (R Studio)/BW Genetics")
file_directory <- "/Users/samanthamills/Honours Project (R Studio)/BW Genetics/Data"

#---------------------------------
# 1. Load packages
#---------------------------------

library(ape)
library(dplyr)
library(stringr)
library(writexl)

#---------------------------------
# 2. Read IQ-TREE Newick file
#---------------------------------

tree <- read.tree(
  file.path(
    file_directory,
    "BW_Strains_Type_Strains_IQTree.txt"
  )
)

#---------------------------------
# 3. Calculate evolutionary
#    distance between all tips
#---------------------------------

dist_matrix <- cophenetic(tree)
# Prevent a strain from being selected
# as its own closest relative
diag(dist_matrix) <- Inf

#---------------------------------
# 4. Identify BW and type strains
#---------------------------------

all_strains <- rownames(dist_matrix)
bw_isolates <- all_strains[
  grepl("^BW", all_strains, ignore.case = TRUE)
]
type_strains <- all_strains[
  !grepl("^BW", all_strains, ignore.case = TRUE)
]
# Check what has been identified
cat("\nNumber of BW isolates:", length(bw_isolates), "\n")
cat("Number of type strains:", length(type_strains), "\n")

#---------------------------------
# 5. Compare BW isolates only
#    against type strains
#---------------------------------

sub_matrix <- dist_matrix[
  bw_isolates,
  type_strains,
  drop = FALSE
]

#---------------------------------
# 6. Find closest type strain
#    for every BW isolate
#---------------------------------

closest_type_strains <- data.frame(
  BW_Strain = rownames(sub_matrix),
  Closest_Type_Strain =
    colnames(sub_matrix)[
      apply(sub_matrix, 1, which.min)
    ],
  IQTree_Distance =
    apply(sub_matrix, 1, min),
  stringsAsFactors = FALSE
)

#---------------------------------
# 7. View IQ-TREE results
#---------------------------------

print(closest_type_strains)

#---------------------------------
# 8. Read FastANI results
#---------------------------------

fastani_file <- file.path(
  file_directory,
  "FastANI_Strains.txt"
)

# FastANI output is tab-delimited with:
# Column 1 = query genome
# Column 2 = reference genome
# Column 3 = ANI
# Column 4 = number of matching fragments
# Column 5 = total fragments

fastani <- read.delim(
  fastani_file,
  header = FALSE,
  sep = "\t",
  stringsAsFactors = FALSE
)

#---------------------------------
# 9. Name FastANI columns
#---------------------------------

colnames(fastani)[1:5] <- c(
  "Query",
  "Reference",
  "ANI",
  "Matching_Fragments",
  "Total_Fragments"
)

#---------------------------------
# 10. Clean strain names
#---------------------------------
# This removes directory paths from genome names
# if they are present in the FastANI file.

fastani$Query <- basename(fastani$Query)
fastani$Reference <- basename(fastani$Reference)
# Remove common genome file extensions
fastani$Query <- str_remove(
  fastani$Query,
  "\\.(fasta|fa|fna|fna\\.gz|fasta\\.gz|fa\\.gz)$"
)
fastani$Reference <- str_remove(
  fastani$Reference,
  "\\.(fasta|fa|fna|fna\\.gz|fasta\\.gz|fa\\.gz)$"
)

#---------------------------------
# 11. Create both directions of
#     each ANI comparison
#---------------------------------

# This section makes a standardised
# comparison table so either direction
# can be matched.

ani_1 <- fastani %>%
  select(
    Query,
    Reference,
    ANI
  ) %>%
  rename(
    Strain_1 = Query,
    Strain_2 = Reference
  )
ani_2 <- fastani %>%
  select(
    Query,
    Reference,
    ANI
  ) %>%
  rename(
    Strain_1 = Reference,
    Strain_2 = Query
  )
ani_all <- bind_rows(
  ani_1,
  ani_2
)

#---------------------------------
# 12. Match IQ-TREE closest relatives
#     with FastANI values
#---------------------------------

final_results <- closest_type_strains %>%
  left_join(
    ani_all,
    by = c(
      "BW_Strain" = "Strain_1",
      "Closest_Type_Strain" = "Strain_2"
    )
  ) %>%
  select(
    BW_Strain,
    Closest_Type_Strain,
    ANI
  ) %>%
  distinct(BW_Strain, .keep_all = TRUE)

#---------------------------------
# 13. Check for missing ANI values
#---------------------------------

cat("\n---------------------------------\n")
cat("ANI matching results\n")
cat("---------------------------------\n\n")
print(final_results)
missing_ani <- final_results %>%
  filter(is.na(ANI))
if (nrow(missing_ani) > 0) {
  cat("\nWARNING:\n")
  cat(
    nrow(missing_ani),
    "BW strains did not have a matching ANI value.\n\n"
  )
  print(missing_ani)
} else {
  cat(
    "\nAll BW strains have a matching ANI value.\n"
  )
}

#---------------------------------
# 14. Save final Excel file
#---------------------------------

output_file <- file.path(
  file_directory,
  "BW_Closest_Type_Strain_ANI.xlsx"
)
write_xlsx(
  final_results,
  output_file
)