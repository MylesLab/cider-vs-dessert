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

# load the data
final.df <- utils::read.table(
  "data/processed/final_phenotype_table.tsv",
  header = TRUE
)

#######################
## GENERATE PCA DATA ##
#######################

# see the distribution of apples
table(final.df$AppleType)
# Dessert England  France 
# 14      11      29 

# only get the columns that can be used for PCA
pca_data <- final.df[, 3:ncol(final.df)]

nrow(pca_data)
# [1] 54

table(pca_data$AppleType)
# Dessert England  France
# 14      11      29

##################
## PCA ANALYSIS ##
##################

pca_cols_idxs <- head(seq_len(ncol(pca_data)), -1)
pca_data_matrix <- scale(pca_data[, pca_cols_idxs]) # scale and center the data
pca_data_matrix[is.na(pca_data_matrix)] <- 0
pca <- prcomp(pca_data_matrix)

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

pc1_pc2 <- generate_pca_biplot(as.data.frame(pca$x), pca_data, c("PC1", "PC2"), pov)
pc1_pc3 <- generate_pca_biplot(as.data.frame(pca$x), pca_data, c("PC1", "PC3"), pov)

PCs <- data.frame(
  PC1 = as.numeric(pca$x[, 1]),
  PC2 = as.numeric(pca$x[, 2]),
  PC3 = as.numeric(pca$x[, 3]),
  AppleType = final.df$AppleType
)
PCs[which(PCs$AppleType == "England"), 'AppleType'] <- "English"
PCs[which(PCs$AppleType == "France"), 'AppleType'] <- "French"
PCs$AppleType <- as.factor(PCs$AppleType)

pc_fig1 <- ggarrange(
  ggarrange(pc1_pc2, pc1_pc3, common.legend = TRUE, labels = c("A", "B")),
  generate_pca_violin_plots(PCs, pov),
  nrow = 2, ncol = 1, labels = c("", "C")
)
ggsave(
  filename = "figures/pca/Figure-1_pca.png",
  plot = pc_fig1,
  dpi = 600
)

############################
## GENERATE DENSITY PLOTS ##
############################
dplots <- create_density_plots(pca_data)

ggsave(
  filename = 'figures/density/fig-2_density_plots.png',
  plot = dplots$all_plots,
  dpi = 600,
  width = 6.92,
  height = 7,
  limitsize = FALSE
)

  