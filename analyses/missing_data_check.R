# Title     : Missing Data Check
# Objective : This script performs some data checks to get some basic statistics
#             on the final phenotype table
# Created by: tayabsoomro
# Created on: 2021-05-31

############################
## IMPORTS & DATA LOADING ##
############################

library(ggplot2)
library(ggpubr)
library(reshape2)
library(viridis)

source('themes/theme_main.R')

final.df <- utils::read.table(
  'data/processed/final_phenotype_table.tsv',
  header = TRUE
)
dim(final.df)
# [1] 54 13

#######################
## DATA DISTRIBUTION ##
#######################

# organize data for bar plot
bar_dat <- melt(data.frame(
  English = sum(final.df$AppleType == "England"),
  French = sum(final.df$AppleType == "France"),
  Dessert = sum(final.df$AppleType == "Dessert")
))

# generate the bar plot
data_distribution_barplot <- ggplot(bar_dat, aes(x = variable, y = value, fill = variable)) +
  geom_text(aes(label = value), vjust = -0.2) +
  geom_bar(stat = "identity", alpha = 0.8) +
  GLOBAL_THEME +
  xlab("Apple Types") +
  ylab("Count") +
  scale_fill_discrete(name = "Apple Types")
ggsave(
  filename = "figures/missing_data/data_distribution_barplot.png",
  plot = data_distribution_barplot,
  bg = "white"
)

###############################
## MISSINGNESS BY PHENOTYPES ##
###############################

phenotype_avail.df <- NULL
for(p_i in 4:ncol(final.df)-1){
  
  # get the phenotype name
  phenotype <- colnames(final.df)[p_i]
  
  # the current data frame
  curr_df <- as.data.frame(
    table(final.df[which(!is.na(final.df[,phenotype])),'AppleType'])
  )
  curr_df$Phenotype <- phenotype

  phenotype_avail.df <- rbind(phenotype_avail.df, curr_df)

}

# cleaning up the data frame for plotting
colnames(phenotype_avail.df) <- c("AppleType", "Count", "Phenotype")

phenotype_avail.df$AppleType <- as.character(phenotype_avail.df$AppleType)

# update the names for plotting
phenotype_avail.df[phenotype_avail.df$AppleType == "England", 'AppleType'] <- "English"
phenotype_avail.df[phenotype_avail.df$AppleType == "France", 'AppleType'] <- "French"

avail_by_pheno_plot <- ggplot(
  phenotype_avail.df, aes(fill = AppleType, y = reorder(Phenotype, -Count), x = Count)
) +
  geom_bar(position = "stack", stat = "identity", alpha = 0.9) +
  GLOBAL_THEME +
  theme(
    axis.text.y = element_text(size = 10, hjust = 1),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.line.x = element_line(colour = "black", size = 0.2),
    axis.line.y = element_line(colour = "black", size = 0.2)
  ) +
  ylab("") +
  xlab("Sample Count")
ggsave(
  filename = "figures/missing_data/data_availability_by_phenotypes.png",
  plot = avail_by_pheno_plot,
  bg = "white"
)

############################
## MISSINGNESS BY SAMPLES ##
############################

traits <- 4:ncol(final.df)-1
traits_missing <- NULL
p_traits_missing <- NULL
for(i in seq_len(nrow(final.df))){

  num_traits_miss <- sum(is.na(final.df[i,traits]))
  p_traits_miss <- (num_traits_miss / length(traits) ) * 100

  traits_missing[i] <- num_traits_miss
  p_traits_missing[i] <- p_traits_miss

}

table(traits_missing)
# 0  1  2  3  4  5 
# 18  6 23  3  3  1 

# the missingness threshold for samples is 50%. In other words, an accession needs
# to have the data for at least 50% of the traits.