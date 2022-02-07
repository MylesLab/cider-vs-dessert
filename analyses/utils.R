# Title     : PCA anlaysis
# Objective : This script contains helpful functions for PCA
# Created by: tayabsoomro
# Created on: 2021-07-23

library(ggplot2)

source('themes/theme_main.R')

generate_pca_violin_plots <- function(dat, pov, labels) {
  components <- colnames(dat)[grep("PC", colnames(dat))]
  plots <- list()
  idx <- 0
  max_y <- 7
  min_y <- -7
  for (component in components) {
    idx <- idx + 1

    des_dat <- get(component, dat[which(dat$AppleType == "Dessert"),])
    eng_dat <- get(component, dat[which(dat$AppleType == "English"),])
    frn_dat <- get(component, dat[which(dat$AppleType == "French"),])

    pov_val <- pov[pov$pc == component, 'pov']

    print(component)
    
    wilcox.test(eng_dat, des_dat)$p.value
    wilcox.test(frn_dat, des_dat)$p.value
    wilcox.test(eng_dat, frn_dat)$p.value
    
    print(paste0("W( Eng_vs_Des ) = ", wilcox.test(eng_dat, des_dat)$statistic))
    print(paste0("W( Frn_vs_Des ) = ", wilcox.test(frn_dat, des_dat)$statistic))
    print(paste0("W( Eng_vs_Frn ) = ", wilcox.test(eng_dat, frn_dat)$statistic))
    print('---')

    plots[[idx]] <- ggplot(dat, aes_string(x = "AppleType", y = get(component, dat))) +
      geom_violin(aes(fill = AppleType, color = AppleType), trim = FALSE,
                  color = "black", alpha = GLOBAL_ALPHA) +
      geom_boxplot(width = 0.1, fill = "white") +
      stat_compare_means(
        aes(label = "..p.adj.."),
        method = "wilcox.test",
        p.adjust.method = "bonferroni",
        hide.ns = TRUE,
        comparisons = list(c("Dessert", "English"), c("Dessert", "French"), c("English", "French")),
        label.y = c(4.8, 5.8, 6.8)
      ) +
      GLOBAL_THEME +
      theme(legend.position = "none") +
      xlab("") +
      ylim(c(min_y, max_y)) +
      scale_color_brewer(palette = GLOBAL_PALETTE, direction = GLOBAL_DIRECTION) +
      scale_fill_brewer(palette = GLOBAL_PALETTE, direction = GLOBAL_DIRECTION) +
      ylab(sprintf("%s (%.1f%%)", component, pov_val))
  }
  ggarrange(plotlist = plots, nrow = 1, ncol = length(components), labels = labels)
}

generate_pca_biplot <- function(pca, choices, pov) {
  GLOBAL_LABELS <- c(
    paste0("Common Dessert (N=", as.numeric(table(pca$AppleType)["Dessert"]), ")"),
    paste0("English Cider (N=", as.numeric(table(pca$AppleType)["English"]), ")"),
    paste0("French Cider (N=", as.numeric(table(pca$AppleType)["French"]), ")")
  )

  pov1 <- pov[pov$pc == choices[1], 'pov']
  pov2 <- pov[pov$pc == choices[2], 'pov']

  plot <- ggplot(
    pca,
    aes_string(
      x = choices[1],
      y = choices[2],
      fill = "AppleType",
      shape = "AppleType"
    )
  ) +
    geom_point(size = 3, alpha = GLOBAL_ALPHA) +
    xlab(sprintf("%s (%0.1f%%)", choices[1], pov1)) +
    ylab(sprintf("%s (%0.1f%%)", choices[2], pov2)) +
    scale_fill_brewer(
      name = "Apple Type",
      palette = GLOBAL_PALETTE,
      labels = GLOBAL_LABELS,
      direction = GLOBAL_DIRECTION
    ) + coord_fixed() + 
    scale_shape_manual(
      name = "Apple Type",
      values = c(21, 23, 23),
      labels = GLOBAL_LABELS
    ) +
    GLOBAL_THEME


  return(plot)
}

create_density_plots <- function(dframe) {

  GLOBAL_LABELS <- c(
    paste0("Common Dessert (N=", as.numeric(table(dframe$AppleType)["Dessert"]), ")"),
    paste0("English Cider (N=", as.numeric(table(dframe$AppleType)["English"]), ")"),
    paste0("French Cider (N=", as.numeric(table(dframe$AppleType)["French"]), ")")
  )


  pretty_labels <- list(
    Acidity = "Acidity (g/mL)",
    DeltaAcidity = "Δ Acidity (%)",
    SSC = "Soluble Solids (Brix)",
    Firmness = "Firmness (kg / cm²)",
    Weight = "Weight (g)",
    Juiciness = "Juiciness (%)",
    PhenolicContent = "Phenolic Content (µmol/g)",
    HarvestDate = "Harvest Date (Julian day)",
    FloweringDate = "Flowering Date (Julian day)",
    Softening = "Δ Firmness (%)"
  )

  plots <- list()
  return_plots <- list()
  stats <- list()
  idx <- 1
  all_pheno_names <- names(dframe[head(seq_along(dframe), -1)])
  for (name in all_pheno_names) {

    des_ph <- dframe[which(dframe[, 'AppleType'] == 'Dessert'), name]
    fr_ph <- dframe[which(dframe[, 'AppleType'] == 'French'), name]
    eng_ph <- dframe[which(dframe[, 'AppleType'] == 'English'), name]

    # wilcoxon test for all the comparisons
    des_vs_eng_test <- wilcox.test(des_ph, eng_ph)
    des_vs_fr_test <- wilcox.test(des_ph, fr_ph)
    eng_vs_fr_test <- wilcox.test(eng_ph, fr_ph)

    # Bonferroni corrected p-values
    p_des_vs_eng <- des_vs_eng_test$p.value * 30
    p_des_vs_fr <- des_vs_fr_test$p.value * 30
    p_eng_vs_fr <- eng_vs_fr_test$p.value * 30

    if (p_des_vs_eng > 1) p_des_vs_eng <- 1
    if (p_des_vs_fr > 1) p_des_vs_fr <- 1
    if (p_eng_vs_fr > 1) p_eng_vs_fr <- 1

    # test statistic
    w_des_vs_eng <- des_vs_eng_test$statistic
    w_des_vs_fr <- des_vs_fr_test$statistic
    w_eng_vs_fr <- eng_vs_fr_test$statistic


    plt <- ggplot(data = dframe, aes_string(x = get(name, dframe))) +
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


