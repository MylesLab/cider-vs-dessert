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
      ylab(sprintf("%s (%.2f%%)", component, pov[component]))
  }
  ggarrange(plotlist = plots, nrow = 1, ncol = 3)
}

generate_pca_biplot <- function(pca, data, c1, c2) {
  plot <- ggbiplot(pca, choices = c(c1, c2), var.axes = FALSE) +
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
    scale_fill_manual(name = "Apple Types", values = c("#538EC1", "#CC2440", "#5BA398"), labels = c("Dessert", "English", "French")) +
    scale_color_manual(name = "Apple Types", values = c("#538EC1", "#CC2440", "#5BA398"), labels = c("Dessert", "English", "French")) +
    scale_shape_manual(name = "Apple Types", values = c(16, 23, 24), labels = c("Dessert", "English", "French")) +
    theme(
      plot.title = element_blank()
    )
  return(plot)
}
