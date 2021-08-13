# Title     : PCA anlaysis
# Objective : This script contains helpful functions for PCA
# Created by: tayabsoomro
# Created on: 2021-07-23

library(ggplot2)
source("themes/theme_avenir.R")

GLOBAL_PALETTE <- "PuOr"
GLOBAL_DIRECTION <- -1
GLOBAL_ALPHA <- 0.6
GLOBAL_THEME <- theme_avenir(
  panel_x = F,
  panel_y = F,
  grid = F,
  axis = T,
  axis_col = "black",
  ticks = T
) + theme(
  axis.text.x = element_text(size = 13, hjust = 0.5, vjust = -0.5),
  axis.text.y = element_text(size = 13, hjust = -0.5, vjust = 0.5),
  axis.title.x = element_text(size = 12, margin = margin(t = 10, r = 0, b = 0, l = 0)),
  axis.title.y = element_text(size = 12, margin = margin(t = 0, r = 10, b = 0, l = 0)),
  legend.text = element_text(size = 12)
)

generate_pca_violin_plots <- function(dat, pov) {
  components <- c("PC1", "PC2", "PC3")
  plots <- list()
  idx <- 0
  max_y <- 7
  min_y <- -7
  for (component in components) {
    idx <- idx + 1

    plots[[idx]] <- ggplot(dat, aes_string(x = "AppleType", y = get(component, dat))) +
      geom_violin(aes(fill = AppleType, color = AppleType), trim = FALSE,
                  color = "black", alpha = GLOBAL_ALPHA) +
      geom_boxplot(width = 0.1, fill = "white") +
      stat_compare_means(
        method = "wilcox.test",
        label = sprintf("p=%s", "p.format"),
        comparisons = list(c("Dessert", "English"), c("Dessert", "French"), c("English", "French")),
        label.y = c(4.8, 5.8, 6.8)
      ) +
      GLOBAL_THEME +
      theme(legend.position = "none") +
      xlab("") +
      ylim(c(min_y, max_y)) +
      scale_color_brewer(palette = GLOBAL_PALETTE, direction = GLOBAL_DIRECTION) +
      scale_fill_brewer(palette = GLOBAL_PALETTE, direction = GLOBAL_DIRECTION) +
      ylab(sprintf("%s (%.1f%%)", component, pov[component]))
  }
  ggarrange(plotlist = plots, nrow = 1, ncol = 3)
}

generate_pca_biplot <- function(pca, data, choices, pov) {
  GLOBAL_LABELS <- c(
    paste0("Dessert (N=", as.numeric(table(data$AppleType)["Dessert"]), ")"),
    paste0("English (N=", as.numeric(table(data$AppleType)["England"]), ")"),
    paste0("French (N=", as.numeric(table(data$AppleType)["France"]), ")")
  )

  plot <- ggplot(
    pca,
    aes(
      x = PC1,
      y = PC2,
      fill = data$AppleType,
      shape = data$AppleType)
  ) +
    geom_point(size = 3, alpha = GLOBAL_ALPHA) +
    xlab(sprintf("%s (%0.1f%%)", choices[1], pov[choices[1]])) +
    ylab(sprintf("%s (%0.1f%%)", choices[2], pov[choices[2]])) +
    scale_fill_brewer(
      name = "Apple Type",
      palette = GLOBAL_PALETTE,
      labels = GLOBAL_LABELS,
      direction = GLOBAL_DIRECTION
    ) +
    scale_shape_manual(
      name = "Apple Type",
      values = c(21, 23, 24),
      labels = GLOBAL_LABELS
    ) +
    GLOBAL_THEME


  return(plot)
}

create_density_plots <- function(df) {

  df[which(df$AppleType == 'England'), 'AppleType'] <- 'English'
  df[which(df$AppleType == 'France'), 'AppleType'] <- 'French'

  GLOBAL_LABELS <- c(
    paste0("Dessert (N=", as.numeric(table(df$AppleType)["Dessert"]), ")"),
    paste0("English (N=", as.numeric(table(df$AppleType)["English"]), ")"),
    paste0("French (N=", as.numeric(table(df$AppleType)["French"]), ")")
  )


  pretty_labels <- list(
    Acidity = "Acidity (g/mL)",
    DeltaAcidity = "Δ Acidity (%)",
    SSC = "Sweetness (%)",
    Firmness = "Firmness (km / cm²)",
    Weight = "Weight (g)",
    Juiciness = "Juciness (%)",
    PhenolicContent = "Phenolic Content (µmol/g)",
    HarvestDate = "Harvest Date (julian days)",
    FloweringDate = "Flowering Date (julian days)",
    Softening = "Δ Firmness (%)"
  )

  plots <- list()
  return_plots <- list()
  stats <- list()
  idx <- 1
  all_pheno_names <- names(df[head(seq_along(df), -1)])
  for (name in all_pheno_names) {

    des_ph <- df[which(df[, 'AppleType'] == 'Dessert'), name]
    fr_ph <- df[which(df[, 'AppleType'] == 'French'), name]
    eng_ph <- df[which(df[, 'AppleType'] == 'English'), name]

    # p-values
    p_des_vs_eng <- (wilcox.test(des_ph, eng_ph)$p.value) * 30
    p_des_vs_fr <- (wilcox.test(des_ph, fr_ph)$p.value) * 30
    p_eng_vs_fr <- (wilcox.test(eng_ph, fr_ph)$p.value) * 30

    if (p_des_vs_eng > 1) p_des_vs_eng <- 1
    if (p_des_vs_fr > 1) p_des_vs_fr <- 1
    if (p_eng_vs_fr > 1) p_eng_vs_fr <- 1

    # test statistic
    w_des_vs_eng <- wilcox.test(des_ph, eng_ph)$statistic
    w_des_vs_fr <- wilcox.test(des_ph, fr_ph)$statistic
    w_eng_vs_fr <- wilcox.test(eng_ph, fr_ph)$statistic

    plt <- ggplot(data = df, aes_string(x = get(name, df))) +
      geom_density(alpha = GLOBAL_ALPHA, aes(fill = AppleType)) +
      GLOBAL_THEME +
      scale_fill_brewer(
        name = "Apple Type",
        palette = GLOBAL_PALETTE,
        labels = GLOBAL_LABELS,
        direction = GLOBAL_DIRECTION
      ) +
      xlab(pretty_labels[[ name ]]) +
      ylab("Density")

    return_plots[[ name ]] <- plt
    stats [[ name ]] <-
      list(
        pvals = c(p_des_vs_eng = p_des_vs_eng, p_des_vs_fr = p_des_vs_fr, p_eng_vs_fr = p_eng_vs_fr),
        statistic = c(w_des_vs_eng = w_des_vs_eng, w_des_vs_fr = w_des_vs_fr, w_eng_vs_fr = w_eng_vs_fr)
      )
    stats [[ name ]]
    plots[[idx]] <- plt
    idx <- idx + 1
  }

  den_plots <- ggarrange(plotlist = plots, ncol = 2, nrow = 5, common.legend = TRUE)

  return(list(all_plots = den_plots, single_plot = return_plots, stats = stats))
}
