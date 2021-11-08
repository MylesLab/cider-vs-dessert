# Title     : Distance Anlalysis
# Objective : This script calculates Euclidean distance between and among the apple types to answer the question of
#             whether there is a difference between cider and dessert apples.
# Created by: tayabsoomro
# Created on: 2021-08-13

############################
## IMPORTS & DATA LOADING ##
############################

library(ggplot2)
library(usedist)

source("themes/theme_avenir.R")

# laod the data
final.df <- utils::read.table(
  'data/processed/final_phenotype_table.tsv',
  header = TRUE
)

# change the apple types for the purposes of this analysis
final.df[which(final.df$AppleType == "England" | final.df$AppleType == "France"), 'AppleType'] <- "Cider"

# only keep the columns for the 10 traits
data_idxs <- tail(head(seq_along(final.df), -1), -2)
trait_names <- colnames(final.df)[data_idxs]
dat.df <- final.df[, data_idxs]

colnames(dat.df)
# [1] "Acidity"         "DeltaAcidity"    "SSC"             "Firmness"        "Weight"
# [6] "Juiciness"       "PhenolicContent" "HarvestDate"     "FloweringDate"   "Softening"


# calculate the Euclidean distance
dist_all <- stats::dist(dat.df, method = "euclidean")

grouped_dist <- dist_groups(dist_all, final.df$AppleType)

# add another column to clasify the comparison
grouped_dist[which(grouped_dist$Group1 == "Cider" & grouped_dist$Group2 == "Dessert"), 'Comparison'] <- "Cider:Dessert"
grouped_dist[which(grouped_dist$Group1 == "Cider" & grouped_dist$Group2 == "Cider"), 'Comparison'] <- "Cider:Cider"
grouped_dist[which(grouped_dist$Group1 == "Dessert" & grouped_dist$Group2 == "Dessert"), 'Comparison'] <- "Dessert:Dessert"

ggplot(grouped_dist) +
  geom_density(aes(x = Distance, fill = Comparison), alpha = 0.5) +
  theme_avenir()
