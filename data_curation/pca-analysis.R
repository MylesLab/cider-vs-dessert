# Title     : PCA anlaysis
# Objective : This script performs PCA analysis on the phenotype data of cider
#             and dessert apples
# Created by: tayabsoomro
# Created on: 2021-05-31

################################
## LIBRARY IMPORT & LOAD DATA ##
################################

library(ggbiplot)

# load cider data
cider_data <- read.table(
  'data/processed/final_cider_apple_phenotype_data.tsv',
  header = TRUE
)

# only get the columns that can be used for PCA
cider_pca_data <- cider_data[,c(23:ncol(cider_data))]

# add use category
cider_pca_data$use <- "C"

nrow(cider_pca_data)
# [1] 219

# load dessert data
dessert_data <- read.table(
  'data/processed/final_dessert_apple_phenotype_data.tsv',
  header = TRUE
)

# only get the columns that can be used for PCA
dessert_pca_data <- dessert_data[,c(10:ncol(dessert_data))]

# add use category
dessert_pca_data$use <- "D"

nrow(dessert_pca_data)
# [1] 27

# gather both cider and desset apple PCA data together
all_pca_data <- rbind(cider_pca_data, dessert_pca_data)

nrow(all_pca_data)
# [1] 246

colnames(all_pca_data)
# [1] "acidity_16_stor"         "brix_16_stor"            "weight_avg_16_stor"     
# [4] "firmness_avg_16_stor"    "brix_acid_16_stor"       "acidity_16_harv"        
# [7] "juiciness_16_harv"       "date_jul_16_harv"        "weight_avg_16_harv"     
# [10] "brix_avg_16_harv"        "firmness_avg_16_harv"    "flowering_jul_16_harv"  
# [13] "precocity_16_harv"       "brix_acid_16_harv"       "time_ripen_16_harv"     
# [16] "percent_acidity_16"      "percent_brix_16"         "percent_weight_16"      
# [19] "percent_firmness_avg_16" "percent_brix_acid_16"    "acidity_17_stor"        
# [22] "brix_17_stor"            "weight_avg_17_stor"      "firmness_avg_17_stor"   
# [25] "brix_acid_17_stor"       "brix_17_harv"            "acidity_17_harv"        
# [28] "date_jul_17_harv"        "firmness_avg_17_harv"    "weight_avg_17_harv"     
# [31] "brix_acid_17_harv"       "percent_acidity_17"      "percent_brix_17"        
# [34] "percent_weight_avg_17"   "percent_firmness_avg_17" "percent_brix_acid_17"   
# [37] "tpc"                     "frap"                    "time_ripen_2017"        
# [40] "use"

##################
## PCA ANALYSIS ##
##################

colnames(all_pca_data) <- c(seq("1","39"),'use')

pca_data <- scale(all_pca_data[,1:length(all_pca_data)-1], center = TRUE)
pca_data[is.na(pca_data)] = 0
pca <- prcomp(pca_data)

summary(pca)

ggbiplot(pca, labels = all_pca_data$use, groups = all_pca_data$use, choices = c(1,3))
