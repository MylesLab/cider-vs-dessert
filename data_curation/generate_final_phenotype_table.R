# Title     : Generate Final Apples Table
# Objective : This script generates a list file which contains the phenotype
#             data for all the apples that are used in our analyses

library(ggplot2)

###############################
## CIDER APPLES LIST CLEANUP ##
###############################

# get the final cider apple list
final_cider.df <- read.table(
  'data/processed/final_cider_apple_phenotype_data.tsv',
  header = TRUE
)

# only keep certain columns
final_cider.df <- final_cider.df[,c(
  "PI..no..",
  "Accession.name",
  "acidity_17_harv",
  "percent_acidity_17",
  "brix_17_harv",
  "firmness_avg_17_harv",
  "weight_avg_17_harv",
  "juiciness_16_harv",
  "tpc",
  "date_jul_17_harv",
  "flowering_jul_16_harv",
  "percent_firmness_avg_17",
  "Region.of.origin"
)]

# cleanup the column names
colnames(final_cider.df) <- c(
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
  "Softening", # % Change in firmness during storage,
  "AppleType"
)

nrow(final_cider.df)
# [1] 83

#################################
## DESSERT APPLES LIST CLEANUP ##
#################################

# get the final dessert apple list
final_dessert.df <- read.table(
  'data/processed/final_dessert_apple_phenotype_data.tsv',
  header = TRUE
)

# only keep certain columns
final_dessert.df <- final_dessert.df[,c(
  "ACNO",
  "PLANTID",
  "acidity_17_harv",
  "percent_acidity_17",
  "brix_17_harv",
  "firmness_avg_17_harv",
  "weight_avg_17_harv",
  "juiciness_16_harv",
  "tpc",
  "date_jul_16_harv",
  "flowering_jul_16_harv",
  "percent_firmness_avg_17"
)]

# cleanup the column names
colnames(final_dessert.df) <- c(
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
  "Softening" # % Change in firmness during storage,
)

# add AppleType column
final_dessert.df$AppleType = "Dessert"

nrow(final_dessert.df)
# [1] 16

######################
## COMBINED DATASET ##
######################

final.df <- rbind(final_cider.df, final_dessert.df)

# save the final table
write.table(
  final.df,
  'data/processed/final_phenotype_table.tsv',
  row.names = FALSE
)
