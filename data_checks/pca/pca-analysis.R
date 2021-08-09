# Title     : PCA anlaysis
# Objective : This script performs PCA analysis on the phenotype data of cider
#             and dessert apples
# Created by: tayabsoomro
# Created on: 2021-05-31

################################
## LIBRARY IMPORT & LOAD DATA ##
################################

library(ggbiplot)
library(reshape2)
library(ggpubr)
library(dplyr)
library(RColorBrewer)

# load the fancy ggplot theme
source("themes/theme_avenir.R")
source("data_checks/pca/utils.R")

# load cider data
cider_data <- read.table(
  "data/processed/final_cider_apple_phenotype_data.tsv",
  header = TRUE
)


#######################
## GENERATE PCA DATA ##
#######################

# see the distribution of apples
count(cider_data$Region.of.origin)
# x freq
# 1 England   35
# 2  France   48

# only get the columns that can be used for PCA
cider_pca_data <- cider_data[, c(23:ncol(cider_data), 11)]

# add use category
cider_pca_data$use <- "Cider"

nrow(cider_pca_data)
# [1] 83

# load dessert data
dessert_data <- read.table(
  "data/processed/final_dessert_apple_phenotype_data.tsv",
  header = TRUE
)

# only get the columns that can be used for PCA
dessert_data_cols <- 10:(ncol(dessert_data)-1)
dessert_pca_data <- dessert_data[, dessert_data_cols]

# add region of origin category
dessert_pca_data$Region.of.origin <- "Dessert"


nrow(dessert_pca_data)
# [1] 16

# gather both cider and desset apple PCA data together
all_pca_data <- rbind(cider_pca_data, dessert_pca_data)

# get the total distribution of apples by origin
count(all_pca_data$Region.of.origin)
# x freq
# 1 Dessert   16
# 2 England   35
# 3  France   48

nrow(all_pca_data)
# [1] 99

# only keep certain columns
all_pca_data <- all_pca_data[
  ,
  c(
    "acidity_17_harv",
    "percent_acidity_17",
    "brix_17_harv",
    "firmness_avg_17_harv",
    "weight_avg_17_harv",
    "juiciness_16_harv",
    "tpc",
    "date_jul_16_harv",
    "flowering_jul_16_harv",
    "percent_firmness_avg_17",
    "Region.of.origin"
  )
]

colnames(all_pca_data) <- c(
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
  "RegionOfOrigin"
)

##################
## PCA ANALYSIS ##
##################

pca_cols_idx <- head(seq_along(all_pca_data), -1)
pca_data <- scale(all_pca_data[, pca_cols_idx]) # scale and center the data
pca_data[is.na(pca_data)] <- 0
pca <- prcomp(pca_data)

# scree plot
vars_transformed <- apply(pca$x, 2, var)
pov <- (vars_transformed / sum(vars_transformed)) * 100
pov.df <- data.frame(pc = seq(1, 10), pov = pov)
scree_plot <- ggplot(pov.df, aes(x = pc, y = pov)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = paste0(round(pov, 2), "%")), hjust = -0.4, vjust = 0, size = 3) +
  theme_avenir() +
  xlab("Principal Components (PC)") +
  ylab("Cumulative Variance (%)") +
  ggtitle("PCA Scree Plot") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),
    axis.title.x = element_text(hjust = 0.5, size = 12),
    axis.title.y = element_text(hjust = 0.5, size = 12)
  )
ggsave(filename = "figures/pca/scree_plot.png", plot = scree_plot)

#########################################
## GENERATE PC BIPLOT AND VIOLIN PLOTS ##
#########################################

# TODO: WHY ARE PCA PLOTS WEIRD

pc1_pc2 <- generate_pca_biplot(as.data.frame(pca$x), all_pca_data, c("PC1","PC2"), pov)
pc1_pc3 <- generate_pca_biplot(as.data.frame(pca$x), all_pca_data, c("PC1","PC3"), pov)

PCs <- data.frame(
  PC1 = as.numeric(pca$x[, 1]),
  PC2 = as.numeric(pca$x[, 2]),
  PC3 = as.numeric(pca$x[, 3]),
  AppleType = all_pca_data$RegionOfOrigin
)
PCs[which(PCs$AppleType == "England"),'AppleType'] <- "English"
PCs[which(PCs$AppleType == "France"),'AppleType'] <- "French"
PCs$AppleType <- as.factor(PCs$AppleType)

pc_fig1 <- ggarrange(
  ggarrange(pc1_pc2, pc1_pc3, common.legend = TRUE, labels = c("A","B")),
  generate_pca_violin_plots(PCs,pov),
  nrow = 2, ncol = 1, labels = c("","C")
)
print(pc_fig1)
ggsave(
  filename = "figures/pca/Figure-1_pca.png",
  plot = pc_fig1,
  dpi = 600
)

############################
## GENERATE DENSITY PLOTS ##
############################
dplots <- create_density_plots(all_pca_data)

ggsave(
  filename = 'figures/density/fig-2_density_plots.png',
  plot = dplots$all_plots,
  dpi = 600
)

  