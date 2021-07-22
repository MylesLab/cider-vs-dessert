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
library(RColorBrewer)

# load cider data
cider_data <- read.table(
  'data/processed/final_cider_apple_phenotype_data.tsv',
  header = TRUE
)

# only keep the England and France varieties
cider_data <- cider_data[which(
  cider_data$Region.of.origin == 'England' |
  cider_data$Region.of.origin == 'France'
),]

# see the distribution of apples
count(cider_data$Region.of.origin)
# x freq
# 1 England   35
# 2  France   48

# only get the columns that can be used for PCA
cider_pca_data <- cider_data[,c(23:ncol(cider_data),11)]

# add use category
cider_pca_data$use <- "Cider"

nrow(cider_pca_data)
# [1] 83

# load dessert data
dessert_data <- read.table(
  'data/processed/final_dessert_apple_phenotype_data.tsv',
  header = TRUE
)

# only get the columns that can be used for PCA
dessert_pca_data <- dessert_data[,c(10:ncol(dessert_data))]

# add region of origin category
dessert_pca_data$Region.of.origin = "Dessert"
# add use category
dessert_pca_data$use <- "Dessert"


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
    "Region.of.origin",
    "use"
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
  "RegionOfOrigin",
  "Use"
)

##################
## PCA ANALYSIS ##
##################

# colnames(all_pca_data) <- c(seq("1","39"),'use')

pca_data <- scale(all_pca_data[,c(1:10)], center = TRUE)
pca_data[is.na(pca_data)] = 0
pca <- prcomp(pca_data)


# scree plot
pov <- pca$sdev^2
plot(seq(1:10),as.numeric(unlist(pov)), pch=4, type = "o", xlab = ("Principal Components (PCs)"), ylab = "Cumulative Variance (%)", main = "Scree Plot")

# PCA on cider vs. dessert apples
pc1_pc2 <- ggbiplot(pca, choices = c(1,2), var.axes = FALSE) +
  theme_classic() +
  theme(
    legend.title = element_blank(),
    plot.title =  element_text(size = 18, face = "bold", hjust=0.5)
  ) + 
  geom_point(
    aes(
      fill=all_pca_data$RegionOfOrigin,
      shape=all_pca_data$RegionOfOrigin,
      color=all_pca_data$RegionOfOrigin
    ),
    size = 3,
    stroke = 1,
  ) +
  scale_fill_manual(name="Apple Types", values = c("#538EC1","#CC2440","#5BA398"), labels=c("Dessert", "English", "French")) + 
  scale_color_manual(name="Apple Types", values = c("#538EC1","#CC2440","#5BA398"), labels=c("Dessert", "English", "French")) + 
  scale_shape_manual(name="Apple Types", values=c(16,23,24), labels=c("Dessert", "English", "French"))

pc1_pc3 <- ggbiplot(pca, choices = c(1,3), var.axes = FALSE) +
  theme_classic() +
  theme(
    legend.title = element_blank(),
    plot.title =  element_text(size = 18, face = "bold", hjust=0.5)
  ) + 
  geom_point(
    aes(
      fill=all_pca_data$RegionOfOrigin,
      shape=all_pca_data$RegionOfOrigin,
      color=all_pca_data$RegionOfOrigin
    ),
    size = 3,
    stroke = 1,
  ) +
  scale_fill_manual(name="Apple Types", values = c("#538EC1","#CC2440","#5BA398"), labels=c("Dessert", "English", "French")) + 
  scale_color_manual(name="Apple Types", values = c("#538EC1","#CC2440","#5BA398"), labels=c("Dessert", "English", "French")) + 
  scale_shape_manual(name="Apple Types", values=c(16,23,24), labels=c("Dessert", "English", "French"))


dessert_pc1 <- as.numeric(unlist(pca$x[which(all_pca_data$RegionOfOrigin == "Dessert"),1]))
england_pc1 <- as.numeric(unlist(pca$x[which(all_pca_data$RegionOfOrigin == "England"),1]))
france_pc1 <- as.numeric(unlist(pca$x[which(all_pca_data$RegionOfOrigin == "France"),1]))

wilcox.test(dessert_pc1,england_pc1, exact = FALSE)$p.value
wilcox.test(dessert_pc1,france_pc1, exact = FALSE)$p.value
wilcox.test(england_pc1,france_pc1, exact = FALSE)$p.value

boxplot(
  dessert_pc1, 
  england_pc1, 
  france_pc1, 
  main="Distribution of PC1 values across different apple types", 
  xlab="Apple Types"
)
png(filename='figures/pca/distribution_of_pc1_boxplot_2.png', width = 5, height = 8, res = 600, units = "in")


pca_plots <- annotate_figure(
  ggarrange(
    pc1_pc2,
    pc1_pc3,
    align = "h",
    labels = c("A","B"),
    common.legend = TRUE
  ),
  top = text_grob("Principal Component Analysis of GCMS data grouped by apple varieties",face = "bold", size = 14)
)
ggsave(
  filename = 'figures/pca/pca_biplot.png',
  plot = pca_plots,
  dpi = 600,
  width = 8,
  height = 6,
  units = "in"
)

