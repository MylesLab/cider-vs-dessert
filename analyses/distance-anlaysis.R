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
library(tidyverse)
library(ggthemes)
library(ggpubr)

source("themes/theme_main.R")
source('analyses/utils.R')

# laod the data
final.df <- utils::read.table(
  'data/processed/final_phenotype_table.tsv',
  header = TRUE
)
dim(final.df)
# [1] 54 13

# only keep the columns for the 10 traits
data_idxs <- tail(head(seq_along(final.df), -1), -2)
trait_names <- colnames(final.df)[data_idxs]
dat.df <- final.df[, data_idxs]

colnames(dat.df)
# [1] "Acidity"         "DeltaAcidity"    "SSC"             "Firmness"        "Weight"
# [6] "Juiciness"       "PhenolicContent" "HarvestDate"     "FloweringDate"   "Softening"

rownames(dat.df) <- final.df$Name

# calculate the Euclidean distance
dist_all <- dist(scale(dat.df), method = "euclidean")

grouped_dist <- dist_groups(dist_all, final.df$AppleType)
dim(grouped_dist)
# [1] 1431 6

# add another column to clasify the comparison
grouped_dist[which(grouped_dist$Group1 == "England" & grouped_dist$Group2 == "Dessert"), 'Comparison'] <- "English vs. Dessert"
grouped_dist[which(grouped_dist$Group1 == "France" & grouped_dist$Group2 == "Dessert"), 'Comparison'] <- "French vs. Dessert"
grouped_dist[which(grouped_dist$Group1 == "England" & grouped_dist$Group2 == "France"), 'Comparison'] <- "English vs. French"
grouped_dist[which(grouped_dist$Group1 == "France" & grouped_dist$Group2 == "England"), 'Comparison'] <- "English vs. French"
grouped_dist[which(grouped_dist$Group1 == "Dessert" & grouped_dist$Group2 == "Dessert"), 'Comparison'] <- "Dessert vs. Dessert"

# remove the between England and France
grouped_dist <- grouped_dist[-which(grouped_dist$Label == "Between England and France"),]

# remove all the rows where the comparison is between same varieties
grouped_dist <- grouped_dist %>% drop_na(Comparison)

# calculate the significance of difference between the distances of English vs. dessert and French vs. dessert
eng_vs_des_dist <- grouped_dist[which(grouped_dist$Comparison == "English vs. Dessert"),'Distance']
fr_vs_des_dist <- grouped_dist[which(grouped_dist$Comparison == "French vs. Dessert"), 'Distance']

wilcox.test(eng_vs_des_dist,fr_vs_des_dist)
# 
# Wilcoxon rank sum test with continuity correction
# 
# data:  eng_vs_des_dist and fr_vs_des_dist
# W = 35345, p-value = 0.1276
# alternative hypothesis: true location shift is not equal to 0

dist_plot <- ggplot(grouped_dist) +
  geom_density(aes(x = Distance, fill = Comparison), alpha = 0.5) +
  GLOBAL_THEME +
  scale_x_continuous(expand = c(0,0), limits = c(0,NA)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA)) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) + ylab("Density") + xlab("Pairwise Euclidean distance")
ggsave(
  filename = "figures/distance/euclidean_dist_distribution.png",
  plot = dist_plot,
  dpi = 600,
  width = 12.5,
  height = 6,
  limitsize = FALSE,
  bg = "white"
)


sim_plot.df <- grouped_dist[grouped_dist$Group2 == "Dessert", c("Item2", "Distance", "Comparison")]
for_plot.df <- sim_plot.df %>%
  group_by(Comparison, Item2) %>%
  summarize(MeanDist = mean(Distance)) %>%
  pivot_wider(names_from = "Comparison", values_from = "MeanDist") %>%
  rename("Name" = "Item2", "EnglishMeanDist" = "English vs. Dessert", "FrenchMeanDist" = "French vs. Dessert")

sim_plot <- for_plot.df %>%
  ggplot(aes(x = EnglishMeanDist, y = FrenchMeanDist)) +
  geom_text(aes(label = Name), nudge_y = 0.06) +
  geom_point() +
  GLOBAL_THEME +
  xlim(3.7, 5.8) +
  ylim(3.7, 5.8) + 
  xlab("Avg. distance from English \ncider varieties") +
  ylab("Avg. distance from French \ncider varieties")


#####################
# GENERATE FIGURE 1 #
#####################

# load the relevant data for Figure 1A
fig_1a.pca.df <- read_delim(
  'data/processed/pca/pca.csv',
  delim = ","
)
fig_1a.pov.df <- read_delim(
  'data/processed/pca/pov.csv',
  delim = ","
)

PCs <- data.frame(
  PC1 = as.numeric(fig_1a.pca.df$PC1),
  PC2 = as.numeric(fig_1a.pca.df$PC2),
  AppleType = fig_1a.pca.df$AppleType
)
PCs$AppleType <- as.factor(PCs$AppleType)

pc1_pc2 <- generate_pca_biplot(fig_1a.pca.df,c("PC1","PC2"),fig_1a.pov.df)
violin_plots <- generate_pca_violin_plots(PCs, fig_1a.pov.df, labels = c("B","C"))

fig1.plot <- ggarrange(
  ggarrange(
    pc1_pc2,
    violin_plots,
    nrow = 2, ncol=1,labels = c("A",""),
    common.legend = TRUE
  ),
  ggarrange(
    dist_plot, sim_plot,
    nrow = 2, ncol = 1,
    labels = c("D","E")
  ),
  nrow = 1, ncol=2
)

ggsave(
  filename = "figures/final_figures/Figure1.png",
  plot = fig1.plot,
  dpi = 600,
  width = 12,
  height = 9.5,
  limitsize = FALSE,
  bg = "white"

)

