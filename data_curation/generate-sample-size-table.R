# Title     : Generate Sample Sizes Table
# Objective : This script performs some data checks to get some basic statistics
#             on the final phenotype table

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
dim(final.df)
# [1] 54 13

all_phenotype_names <- head(tail(colnames(final.df),-3),-1)
all_phenotype_names
# [1] "Acidity"         "DeltaAcidity"    "SSC"             "Firmness"        "Weight"         
# [6] "Juiciness"       "PhenolicContent" "HarvestDate"     "FloweringDate"   "Softening" 

sample_size_table <- as.data.frame(matrix(NA, nrow = 10, ncol = 3))
colnames(sample_size_table) <- c("Dessert", "English", "French")
rownames(sample_size_table) <- all_phenotype_names

for(i in seq_along(all_phenotype_names)){
  print(i)
  phenotype <- all_phenotype_names[i]
  
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
  'data/processed/tbl-sample-sizes.xlsx'
)
