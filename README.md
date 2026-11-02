# Honours-Thesis
R codes used for my honours thesis:
_This excludes the codes required to make figures._

1. PGPT-Pred Lvl 5 and 6 Gene Comparison (Section 2.5.1.2).
Matrix to compare all strains with all genes (level 6) within the level 5 (phosphate solubilisation) PLaBAse output

3. PGPT-Pred Genetically Identical Strains (Section 2.5.1.2)
Used to determine what strains can be removed from phosphate source assays as they have the same composition of genes across the phosphate solubilisation genes (level 5/6) in PLaBAse

4. Binary Traits (Scoary and Pyseer use) (Section 2.5.7.1)
Creation of binary traits based on the results from aluminium, calcium and iron phosphate source assays
Used on Scoary and Pyseer

5. BW Strain Closest Type Strain and ANI Value (Section 2.5.4.2)
Determine what is the closest known type strain to each of the BW strain based on the IQTree3.0 Newick format of those strains (experimental BW and Azospirillum type strains)
ANI values from FastANI, used to compare the genetic similarity of the two strains (BW and the best matched type strain)

6.  Strain Ranking (Phosphate Solubilisation Index) (Section 2.5.2.1)
Used to determine what a "high," "medium," and "low" overall phosphate solubilizer was, based on the indices for the insoluble phosphate medias.
Data came from the "Raw_Phosphate_Results" file, ranked individually between traits and combined traits to create an overall ranking.  

7. Normality Testing (Original and alternative medias) (Section 2.5.2.2)
Determined what type of statistical analysis is required.
Used "Raw_Phosphate_Results" file and carried out Shapiro-Wilk tests and normal Q-Q plots.

8. Statistical Analysis (Original and alternative medias) (Section 2.5.2.2)
Carries out normality testing within itself to determine what statistical analysis and post-hoc tests are required.
Used "Raw_Phosphate_Results" file to read data for all media types.
