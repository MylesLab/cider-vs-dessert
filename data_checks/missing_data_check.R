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
  geom_bar(stat = "identity") + 
  theme_classic() + 
  ggtitle("Distribution of apples within our dataset") + 
  theme(
    plot.title = element_text(
      face = "bold", 
      size=18, 
      hjust=0.5,
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

phenotype_missingness.df <- as.data.frame(matrix(nrow=0,ncol=2))
for(p_i in 3:ncol(final.df)-1){
  
  # get the phenotype name
  phenotype <- colnames(final.df)[p_i]
  
  # get the column as vector
  pheno_missingness <- round((sum(is.na(final.df[,p_i])) / nrow(final.df)) * 100,2)
  
  # add the data
  phenotype_missingness.df <- 
    rbind(
      phenotype_missingness.df,
      c(phenotype, pheno_missingness)
    )
}

# cleaning up the data frame for plotting
colnames(phenotype_missingness.df) <- c("Phenotype","PercentMissingness")
phenotype_missingness.df$PercentMissingness <-  as.numeric(phenotype_missingness.df$PercentMissingness)

# plot the missing ness distribution
missingness_by_phenotypes <- ggplot(
  phenotype_missingness.df, 
  aes(fill=Phenotype, y=reorder(Phenotype, -PercentMissingness), x=PercentMissingness)) + 
  geom_bar(position="dodge", stat="identity", alpha=0.8) +
  geom_vline(xintercept = 50, size = 1, color = "red") + 
  geom_text(aes(label=paste0(PercentMissingness, "%"),x=0), hjust = -0.5) + 
  theme_classic() + 
  ggtitle("Distribution of missing data for phenotypes") + 
  theme(
   axis.line = element_line(colour = "black"),
    plot.title = element_text(hjust = 0.5, size=18, face="bold"),
    axis.title.x=element_text(size=14,face="bold",hjust=0.5),
    axis.title.y=element_text(size=14,face="bold",hjust=0.5)
  ) + 
  scale_fill_manual(values=c(
    '#8ecae6','#219ebc','#023047','#ffb703','#fb8500','#e76f51','#ccd5ae',
    '#a5a58d','#bb3e03','#f28482'
  )) +
  ylab("Phenotypes") + 
  xlab("% Missing Samples")

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
