# Title     : Generate Final Apples Table
# Objective : This script generates a list file which contains the phenotype
#             data for all the apples that are used in our analyses


source("data_curation/constants.R")

###############################
## CIDER APPLES LIST CLEANUP ##
###############################

NEW_COLNAMES <- c(
  "PIID",
  "Name",
  "Acidity",
  "DeltaAcidity", # change in acidity during storage
  "SSC",
  "Firmness",
  "Weight",
  "Juiciness",
  "PhenolicContent",
  "HarvestDate",
  "FloweringDate",
  "Softening" # % Change in firmness during storage
)


# get the final cider apple list
final_cider.df <- read.table(
  'data/processed/final_cider_apple_phenotype_data.tsv',
  header = TRUE
)
dim(final_cider.df)
# [1] 82 59

# only keep certain columns
final_cider.df <- final_cider.df[, c(
  "PI..no..",
  "PLANTID",
  phenotype_cols,
  "Region.of.origin"
)]

# cleanup the column names
colnames(final_cider.df) <- c(NEW_COLNAMES, "AppleType")

dim(final_cider.df)
# [1] 82 13

#################################
## DESSERT APPLES LIST CLEANUP ##
#################################

# get the final dessert apple list
final_dessert.df <- read.table(
  'data/processed/final_dessert_apple_phenotype_data.tsv',
  header = TRUE
)
dim(final_dessert.df)
# [1] 16 49

# only keep certain columns
final_dessert.df <- final_dessert.df[,c(
  "ACNO",
  "PLANTID",
  phenotype_cols
)]

# cleanup the column names
colnames(final_dessert.df) <- NEW_COLNAMES

# add AppleType column
final_dessert.df$AppleType <- "Dessert"

dim(final_dessert.df)
# [1] 16 13

######################
## COMBINED DATASET ##
######################

final.df <- rbind(final_cider.df, final_dessert.df)
dim(final.df)
# [1] 98 13

#######################
## SAMPLE FILTRATION ##
#######################

# get the rows (accessions) with 50% or less missingness in terms of the phenotypes
phenos_idx <- 3:(ncol(final.df) - 1)
only_phenos_final.df <- final.df[, phenos_idx]
filtered_accessions <- which(rowSums(is.na(only_phenos_final.df)) <= 5) # 50% of 10 phenotypes (i.e., 5 phenotypes)
# should be present for the particular sample to be added

length(filtered_accessions)
# [1] 54 

# there are 54 samples that have more than 50% (i.e., 5) phenotypes missing and
# they should therefore be removed from the final set of samples.

# filter the final data frame
final.df <- final.df[filtered_accessions,]
dim(final.df)
# [1] 54 13

##################
## UPDATE NAMES ##
##################

final.df[grep("Barkley Rome",final.df$Name),'Name'] <- "Rome"
final.df[grep("Fuji",final.df$Name),'Name'] <- "Fuji"

####################
## EXPORTING DATA ##
####################

# save the final table
write.table(
  final.df,
  'data/processed/final_phenotype_table.tsv',
  row.names = FALSE
)

