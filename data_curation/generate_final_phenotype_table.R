# Title     : Generate Final Apples Table
# Objective : This script generates a list file which contains the phenotype
#             data for all the apples that are used in our analyses

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

phenotype_cols <- c(
  "acidity_17_harv",
  "percent_acidity_17",
  "brix_17_harv",
  "firmness_avg_17_harv",
  "weight_avg_17_harv",
  "juiciness_16_harv",
  "tpc",
  "date_jul_17_harv",
  "flowering_jul_16_harv",
  "percent_firmness_avg_17"
)

# get the final cider apple list
final_cider.df <- read.table(
  'data/processed/final_cider_apple_phenotype_data.tsv',
  header = TRUE
)
dim(final_cider.df)
# [1] 83 61

# only keep certain columns
final_cider.df <- final_cider.df[, c(
  "PI..no..",
  "Accession.name",
  phenotype_cols,
  "Region.of.origin"
)]

# cleanup the column names
colnames(final_cider.df) <- c(NEW_COLNAMES, "AppleType")

dim(final_cider.df)
# [1] 83 13

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
# [1] 99 13

#######################
## SAMPLE FILTRATION ##
#######################

# get the rows (accessions) with 50% or less missingness in terms of the phenotypes
phenos_idx <- 3:(ncol(final.df) - 1)
only_phenos_final.df <- final.df[, phenos_idx]
filtered_accessions <- which(rowSums(is.na(only_phenos_final.df)) <= 5) # 50% of 10 phenotypes (i.e., 5 phenotypes)
# should be present for the particular sample to be added

# filter the final dataframe
final.df <- final.df[filtered_accessions,]
dim(final.df)
# [1] 55 13

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

