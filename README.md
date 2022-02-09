<div align="center">
    <h1>Quantifying the differences between cider and dessert apples</h1>
    <br />
    Tayab Soomro<sup>1</sup>, Sophie Watts<sup>1</sup>, ‪Zoë Migicovsky<sup>1</sup>, Sean Myles*<sup>1,2</sup>
    <br />
</div>
<b>Affiliations</b><br />
<sup>1</sup>Department of Plant, Food, and Environmental Sciences, Faculty of Agriculture, Dalhousie University
<br />
<sup>2</sup>Agriculture & Agri-Food Canada, Kentville Research and Development Centre, Kentville, Nova Scotia

---

## Table of contents

1. [About](#1-about) </br>
2. [Abstract](#2-abstract) <br />
3. [Figures](#3-figures) <br />
3a. [Figure 1](#3a-figure-1) <br />
3b. [Figure 2](#3b-figure-2) <br />
4. [Analysis](#4-analysis) <br />
4a. [File Architecture](#4a-file-architecture) <br />
4b. [Getting the code](#4b-getting-the-code) <br />
4c. [Dependencies](#4c-dependencies) <br />
4d. [Reproducing the results](#4d-reproducing-the-results) <br />

## 1. About
This repository contains data and scripts used to repoduce analyses in the manuscript "Quantifying the differences between cider and dessert apples".

## 2. Abstract
## 3. Figures
### 3a. Figure 1

| Principal Component Analysis and distance analysis of various phenotypes in commonly grown English and French cider varieties, and dessert varieties.  |
| ------------- |
| ![Figure1](https://user-images.githubusercontent.com/19979068/153270731-c35e0cfb-15c5-4dd3-8dbf-258d641364f8.png)  |
| <b>Principal Component Analysis and distance analysis of various phenotypes in commonly grown English and French cider varieties, and dessert varieties.</b> <b>A)</b> Biplot of PC1 vs PC2. <b>B,C)</b> The difference between Dessert and Cider (English & French) varieties along the PC1 and PC2 is shown in violin and boxplots. The p-values on top are the output of Wilcoxon Ranked Sum Test. <b>D)</b> Density plot for pairwise Euclidean distances for English cider and French cider vs. dessert apples, and within dessert apples. <b>E)</b> Scatter plot shows the average Euclidean distance of each of the dessert apples from English cider (x-axis) and French cider (y-axis) apple varieties.  |

### 3b. Figure 2

| Density plots for various phenotypes across English cider apples, French cider apples, and dessert apples. The x-axis shows the phenotype associated with each of the apple types. |
| ------------- |
| ![Figure2](https://user-images.githubusercontent.com/19979068/153272291-c055d4d3-334f-4653-a974-8d2a3415f83f.png)  |
| Density plots for various phenotypes across English cider apples, French cider apples, and dessert apples. The x-axis shows the phenotype associated with each of the apple types. |


## 4. Analysis
### 4a. File architecture

```
├── analyses
├── data
│   ├── processed
│   └── raw
├── data_curation
├── figures
│   ├── density
│   ├── distance
│   ├── final_figures
│   ├── missing_data
│   └── pca
└── themes
```

- The `data` directory contains all the data that was used for this analysis.
- The `data_curation` directory contains the scripts generated to generate the final curated dataset.
- The `analyses` directory contains the scripts generated for performing various analyses of this project
- The `figures` directory contains the intermediary and the final figures generated for this project. 


### 4b. Getting the code

You can download a copy of all the files in this repository by cloning the git repository:

```sh
$ git clone https://github.com/MylesLab/cider-vs-dessert.git
```

### 4c. Dependencies

All the code is written purely in R and the library dependencies for running the code are enumerated below:

```
ggpubr
ggthemes
readxl
reshape2
tidyverse
usedist
viridis
xlsx
```

### 4d. Reproducing the results

Given that you have above dependencies installed in your working machine, you should be able to run any of the code in this repository. However, in order to reproduce the analyses and obtain the same results, here is the list and order of the scripts you should run:

**1. Data Curation**
<br />
You first need to generate the dataset which is going to be used for running the analyses.<br />
**1a.** `data_curation/generate-final-dessert-apples-list.R` - Running this file will generate `data/processed/final_dessert_apple_phenotype_data.tsv`.<br />
**1b.** `data_curation/generate-final-cider-apples-list.R` - Running this file will generate `data/processed/final_cider_apple_phenotype_data.tsv`. <br />
**1c.** `data_curation/generate-final-phenotype-table.R` - Running this file will generate `data/processed/final_phenotype_table.tsv` which is the main file used for all the analyses. <br />

**2. Analyses** 
<br />
Once the `data/processed/final_phenotype_table.tsv` file is generated, we can start to run the anlyses. <br />
**2a.** `analyses/pca-density-analysis.R` - Running this file will generate Figure-2_density plot, as well as some of the intermediary data files which are required for generating final Figure 1 plot. <br />
**2b.** `analyses/distance-analysis.R` - Running this file will generate the final Figure 1 plot <br />
