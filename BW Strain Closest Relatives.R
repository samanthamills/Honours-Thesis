#---------------------------------
#0. Setting up
#---------------------------------

setwd("/Users/samanthamills/Honours Project (R Studio)/BW Genetics")

file_directory <- "/Users/samanthamills/Honours Project (R Studio)/BW Genetics/Data"

#---------------------------------
# Closest relatives and iqtree distance for all strains
#---------------------------------

# 1. Load the required package
library(ape)

# 2. Read your iqtree output file
# Replace "your_file.tree" with your actual file name
tree <- read.tree("/Users/samanthamills/Honours Project (R Studio)/BW Genetics/Data/BW_Strains_Type_Strains_IQTree.txt")

# 3. Calculate the evolutionary distance matrix between all tips
dist_matrix <- cophenetic(tree)

# 4. Fill the diagonal with Infinity so a species isn't its own closest relative
diag(dist_matrix) <- Inf

# 5. Find the closest relative for every species in the tree
closest_relatives <- data.frame(
  Species = rownames(dist_matrix),
  Closest_Relative = colnames(dist_matrix)[apply(dist_matrix, 1, which.min)],
  Mash_Distance = apply(dist_matrix, 1, min)
)

# 6. View the results
print(closest_relatives)

#---------------------------------
# Closest relative for BW strains, compared to type strains
#---------------------------------

# 1. Load the package and read your file
library(ape)
library(writexl)
tree <- read.tree("/Users/samanthamills/Honours Project (R Studio)/BW Genetics/Data/BW_Strains_Type_Strains_Mashtree.tree")

# 2. Calculate the branch length distance matrix
dist_matrix <- cophenetic(tree)

# 3. Identify which strains are your isolates vs type strains
all_strains <- rownames(dist_matrix)
bw_isolates  <- all_strains[grepl("^BW", all_strains)]
type_strains <- all_strains[!grepl("^BW", all_strains)]

# 4. Subset the matrix: Rows = BW isolates, Columns = Type strains only
sub_matrix <- dist_matrix[bw_isolates, type_strains, drop = FALSE]

# 5. Find the closest type strain for each BW isolate
closest_type_strains <- data.frame(
  Isolate = rownames(sub_matrix),
  Closest_Type_Strain = colnames(sub_matrix)[apply(sub_matrix, 1, which.min)],
  Mash_Distance = apply(sub_matrix, 1, min)
)

# 6. View and save your results
print(closest_type_strains)
write_xlsx(closest_type_strains, "closest_type_strains_results.xlsx")
