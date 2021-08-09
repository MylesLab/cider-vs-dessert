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
library(RColorBrewer)

source('themes/theme_avenir.R')

final.df <- read.table(
  'data/processed/final_phenotype_table.tsv',
  header = TRUE
)

#######################
## DATA DISTRIBUTION ##
#######################

# organize data for bar plot
bar_dat <- melt(data.frame(
  England=sum(final.df$AppleType == "England"),
  France=sum(final.df$AppleType == "France"),
  Dessert=sum(final.df$AppleType == "Dessert")
))

# generate the bar plot
data_distribution_barplot <- ggplot(bar_dat, aes(x=variable, y=value, fill=variable)) + 
  geom_text(aes(label=value), vjust=-0.2) + 
  geom_bar(stat = "identity", alpha = 0.8) + 
  theme_avenir() + 
  theme(
    plot.title = element_text(
      face = "bold", 
      size=18, 
      hjust=0.5
    )
  ) + 
  xlab("Apple Types") + 
  ylab("Count") + 
  scale_fill_discrete(name = "Apple Types")
ggsave(
  filename = "figures/missing_data/data_distribution_barplot.png",
  plot = data_distribution_barplot
)

###############################
## MISSINGNESS BY PHENOTYPES ##
###############################

phenotype_missingness.df <- NULL
for(p_i in 3:ncol(final.df)-1){
  
  # get the phenotype name
  phenotype <- colnames(final.df)[p_i]
  
  # the current data frame
  curr_df <- as.data.frame(
    table(final.df[which(!is.na(final.df[,phenotype])),'AppleType'])
  )
  curr_df$Phenotype <- phenotype
  
  phenotype_missingness.df <-  rbind(phenotype_missingness.df, curr_df)
  
}

# cleaning up the data frame for plotting
colnames(phenotype_missingness.df) <- c("AppleType", "Count", "Phenotype")

missingness_by_phenotypes <- ggplot(
  phenotype_missingness.df, aes(fill=AppleType, y=reorder(Phenotype, -Count), x=Count)
) + 
  geom_bar(position="stack", stat="identity", alpha = 0.9) + 
  theme_avenir(grid =  F, panel_x = F, panel_y = F) + 
  scale_fill_brewer(palette = "Pastel1") + 
  theme(
    axis.text.y = element_text(size = 10), 
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.line.x = element_line(colour="black", size = 0.2),
    axis.line.y = element_line(colour="black", size = 0.2)
  ) + ylab("")
ggsave(
  filename = "figures/missing_data/data_missingness_by_phenotypes.png",
  plot = missingness_by_phenotypes
)

############################
## MISSINGNESS BY SAMPLES ##
############################

ptypes_missing <- NULL
for(i in 1:nrow(final.df)){
  ptypes <- final.df[i,c(2:11)]
  num_missing <- sum(is.na(unlist(ptypes)))
  pcent_missing <- ( num_missing / ncol(ptypes) ) * 100
  print(paste0(i," -> ", pcent_missing))
  ptypes_missing[i] <- pcent_missing
}

samples_dat <- as.data.frame(ptypes_missing)
samples_missing_plot <- ggplot(
  samples_dat,
  aes(x = ptypes_missing)) +
  geom_histogram(
    bins = 15,
    color = "black", fill="lightblue") +
  theme_pubclean() +
  xlab("Missingness (%)") + ylab("Number of samples") +
  labs(title="Distibution of missingness by samples") +
  theme(
    plot.title = element_text(
      hjust = 0.5, face="bold", family = "Helvetica", size =  17)
  )
ggsave(
  filename = "figures/missing_data/samples_missingness_plot.png",
  plot = samples_missing_plot
)
