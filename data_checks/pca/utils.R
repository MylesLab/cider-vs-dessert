# Title     : PCA anlaysis
# Objective : This script contains helpful functions for PCA
# Created by: tayabsoomro
# Created on: 2021-07-23

library(ggplot2)
source("themes/theme_avenir.R")

generate_pca_violin_plots <- function(dat, pov) {
  components <- c("PC1", "PC2", "PC3")
  plots <- list()
  idx <- 0
  max_y <- 6
  min_y <- -6
  for (component in components) {
    idx <- idx + 1
    plots[[idx]] <- ggplot(dat, aes_string(x = "AppleType", y = get(component, dat))) +
      geom_violin(aes(fill = AppleType), trim = FALSE, alpha = 0.4) +
      geom_boxplot(width = 0.1, fill = "white") +
      stat_compare_means(
        method = "wilcox.test",
        label = sprintf("p=%s","p.format"),
        comparisons = list(c("Dessert", "England"), c("Dessert", "France"), c("England", "France")),
        label.y = c(3.5,5,6)
      ) +
      theme_classic() +
      theme(
        legend.position = "none"
      ) +
      xlab("") +
      ylim(c(min_y, max_y)) +
      ylab(sprintf("%s (%.1f%%)", component, pov[component]))
  }
  ggarrange(plotlist = plots, nrow = 1, ncol = 3)
}

generate_pca_biplot <- function(pca, data, choices, pov) {
  plot <- ggbiplot(pca, choices = choices, var.axes = FALSE) +
    theme_avenir(axis = TRUE, grid = FALSE) +
    theme(
      axis.title.x = element_text(hjust = 0.5, size = 12),
      axis.title.y = element_text(hjust = 0.5, size = 12),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.line.x = element_line(colour = "black"),
      axis.line.y = element_line(colour = "black"),
      legend.background = element_blank(),
      legend.spacing.y = unit(5, "mm"),
      legend.spacing.x = unit(0, "mm"),
      legend.box.spacing = unit(5, "mm"),
      legend.box.background = element_rect(colour = "black", size = 0.3),
      aspect.ratio = 1, axis.text = element_text(colour = 1, size = 12)
    ) +
    geom_point(
      aes(
        fill = data$RegionOfOrigin,
        shape = data$RegionOfOrigin,
        color = data$RegionOfOrigin
      ),
      size = 3,
      stroke = 1,
    ) +
    xlab(sprintf("PC%d (%0.1f%%)",choices[1],pov[choices[1]])) + 
    ylab(sprintf("PC%d (%0.1f%%)",choices[2],pov[choices[2]])) + 
    scale_fill_manual(name = "Apple Types", values = c("#538EC1", "#CC2440", "#5BA398"), labels = c("Dessert", "English", "French")) +
    scale_color_manual(name = "Apple Types", values = c("#538EC1", "#CC2440", "#5BA398"), labels = c("Dessert", "English", "French")) +
    scale_shape_manual(name = "Apple Types", values = c(16, 23, 24), labels = c("Dessert", "English", "French")) +
    theme(
      plot.title = element_blank()
    )
     
    
  return(plot)
}

create_density_plots <- function(df) {
  pretty_labels = list(
    Acidity="Acidity (g/mL)",
    DeltaAcidity="Δ Acidity (%)",
    SSC="Sweetness (%)",
    Firmness="Firmness (km / cm²)",
    Weight="Weight (g)",
    Juiciness="Juciness (%)",
    PhenolicContent = "Phenolic Content (µmol/g)",
    HarvestDate="Harvest Date (julian days)",
    FloweringDate="Flowering Date (julian days)",
    Softening = "Softening (%)"
  )
  
  plots = list()
  return_plots = list()
  stats = list()
  idx = 1
  all_pheno_names <- names(all_pca_data[head(seq_along(all_pca_data),-2)])
  pval_text_xaxis <- c(12,25,15,12,270,20,20,260,160,0)
  for ( name in all_pheno_names) {
    
    # perform wilcoxon test
    test = pairwise.wilcox.test(get(name,df), df$RegionOfOrigin, p.adjust.method = "bonferroni", exact = FALSE)
    p_des_vs_eng <- round(test$p.value[1,1] / 10,2)
    p_des_vs_fr <- round(test$p.value[2,1] / 10,2)
    p_eng_vs_fr <- round(test$p.value[2,2] / 10,2)
    
    end=max(get(name,df), na.rm = TRUE)
    start=min(get(name,df), na.rm = TRUE)
    text_x = median(get(name,df),na.rm = TRUE)
    plt = ggplot(data=df, aes_string(x=get(name,df))) +
      geom_density(alpha=0.4, aes(fill=RegionOfOrigin)) +
      theme_avenir() +
      theme(
        legend.title = element_blank(),
        axis.title.x = element_text(hjust=0.5, size = 12),
        axis.title.y = element_text(hjust = 0.5, size = 10, margin = margin(t = 0, r = 0.1, b = 0, l = 0, unit = "in"))
      ) +
      xlab(pretty_labels[[ name ]]) 
    
    return_plots[[ name ]] = plt
    stats [[ name ]] = list(p_des_vs_eng=p_des_vs_eng, p_des_vs_fr=p_des_vs_fr, p_eng_vs_fr=p_eng_vs_fr)
    plots[[idx]] = plt
    idx = idx + 1
  }
  
  den_plots = ggarrange(plotlist = plots, ncol = 2,nrow=5,common.legend = TRUE)
  
  return(list(all_plots=den_plots,single_plot=return_plots,stats=stats))
}
