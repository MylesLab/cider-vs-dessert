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

source("themes/theme_main.R")

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
grouped_dist[which(grouped_dist$Group1 == "England" & grouped_dist$Group2 == "Dessert"), 'Comparison'] <- "English:Dessert"
grouped_dist[which(grouped_dist$Group1 == "France" & grouped_dist$Group2 == "Dessert"), 'Comparison'] <- "French:Dessert"
grouped_dist[which(grouped_dist$Group1 == "England" & grouped_dist$Group2 == "France"), 'Comparison'] <- "English:French"
grouped_dist[which(grouped_dist$Group1 == "France" & grouped_dist$Group2 == "England"), 'Comparison'] <- "English:French"

rows <- grep("Within Dessert", grouped_dist$Label)
rows <- c(rows, grep("Between Dessert", grouped_dist$Label))

grouped_dist <- grouped_dist[rows,]

# remove all the rows where the comparison is between same varieties
grouped_dist <- grouped_dist %>% drop_na(Comparison)

ed <- grouped_dist[which(grouped_dist$Comparison == "English:Dessert"), 'Distance']
fd <- grouped_dist[which(grouped_dist$Comparison == "French:Dessert"), 'Distance']

length(ed)
length(fd)

wilcox.test(ed, fd)

table(grouped_dist$Comparison)

# grouped_dist <- grouped_dist[grep("Within",grouped_dist$Label),]

dist_plot <- ggplot(grouped_dist) +
  geom_density(aes(x = Distance, fill = Label), alpha = 0.5) +
  GLOBAL_THEME +
  theme(
    legend.position = "bottom"
  )
ggsave(
  filename = "figures/distance/euclidean_dist_distribution.png",
  plot = dist_plot,
  dpi = 600,
  width = 12.5,
  height = 6,
  limitsize = FALSE,
  bg = "white"
)

sim_plot.df <- grouped_dist[grouped_dist$Group2 == "Dessert",]

sim_plot.df <- grouped_dist[grouped_dist$Group2 == "Dessert", c("Item2", "Distance", "Comparison")]

for_plot.df <- sim_plot.df %>%
  group_by(Comparison, Item2) %>%
  summarize(MeanDist = mean(Distance)) %>%
  pivot_wider(names_from = "Comparison", values_from = "MeanDist") %>%
  rename("Name" = "Item2", "EnglishMeanDist" = "English:Dessert", "FrenchMeanDist" = "French:Dessert")

for_plot.df %>%
  ggplot(aes(x = EnglishMeanDist, y = FrenchMeanDist)) +
  geom_text(aes(label = Name)) +
  GLOBAL_THEME +
  xlab("Avg. distance from English cider varieties") +
  ylab("Avg. distance from French cider varieties")