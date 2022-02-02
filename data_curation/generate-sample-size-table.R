# Title     : Generate Sample Sizes Table
# Objective : This script performs some data checks to get some basic statistics
#             on the final phenotype table
# Created by: tayabsoomro
# Created on: 2021-05-31

############################
## IMPORTS & DATA LOADING ##
############################

library(dplyr)
library(readxl)
library(xlsx)

# load the data
final.df <- utils::read.table(
  'data/processed/final_phenotype_table.tsv',
  header = TRUE
)

all_phenotype_names <- head(colnames(final.df)[-1],n=-1)

sample_size_table <- as.data.frame(matrix(NA, nrow = 10, ncol = 3))
colnames(sample_size_table) <- c("Dessert", "English", "French")
rownames(sample_size_table) <- all_phenotype_names[2:length(all_phenotype_names)]

for(i in seq_along(all_phenotype_names)){
  phenotype <- all_phenotype_names[i+1]
  
  ph_dat <- cbind(
    sum(!is.na(final.df[which(final.df$AppleType == 'Dessert'),phenotype])),
    sum(!is.na(final.df[which(final.df$AppleType == 'England'),phenotype])),
    sum(!is.na(final.df[which(final.df$AppleType == 'France'),phenotype]))
  )
  
  sample_size_table[i,] <- ph_dat
}

# write sample sizes table
write.xlsx(
  sample_size_table,
  '../data/processed/tbl-sample-sizes.xlsx'
)
