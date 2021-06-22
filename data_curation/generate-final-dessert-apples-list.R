# Title     : Generate Final Dessert Apples List
# Objective : This script generates a list file which contains the phenotype
#             data for the dessert apple varieties that are commonly grown in 
#             US and Canada that are available in ABC
# Created by: tayabsoomro
# Created on: 2021-05-30

####################################
## LIBRARY IMPORTS & DATA LOADING ##
####################################

library(dplyr)
library(readxl)

abc_pop_info <- read_excel(
  'data/raw/pheno_meta_data_abc.xlsx'
)

####################################
## CANADIAN COMMON DESSERT APPLES ##
####################################

# Source: 
# https://www.agr.gc.ca/eng/canadas-agriculture-sectors/horticulture/market-information-infohort/apple-reports/?id=1605706730291

canada_dessert_apples = c(
  "Ambrosia",
  "Cortland",
  "Crispin",
  "Empire",
  "Fuji", 
  "Gala",
  "Golden Delicious",
  "Honeycrisp",
  "Idared",
  "Jonagold",
  "Lobo",
  "McIntosh",
  "Red Delicious",
  "Royal Gala",
  "Spartan",
  "Spy"
)

length(canada_dessert_apples)
# [1] 16

###############################
## U.S COMMON DESSERT APPLES ##
###############################

# Source: 
# https://www.agmrc.org/commodities-products/fruits/apples#:~:text=The%20United%20States%20grows%20approximately,U.S.%20Apple%20Association%2C%202018).

us_dessert_apples = c(
  "Red Delicious",
  "Gala",
  "Granny Smith",
  "Fuji",
  "Golden Delicious",
  "Honeycrisp",
  "McIntosh",
  "Rome",
  "Cripps",
  "Pink Lady",
  "Empire"
)

length(us_dessert_apples)
# [1] 11

###########################
## COMMON DESSERT APPLES ##
###########################

# combine both lists and get unique values
common_dessert_apples <- unique(
  c(
    canada_dessert_apples,
    us_dessert_apples
  )
)

length(common_dessert_apples)
# [1] 20

# There are 20 common dessert apples in US and Canada

###############################################
## COMMON DESSERT APPLES WITH PHENOTYPE DATA ##
###############################################

common_dessert_pheno_dat <- data.frame()
for(name in common_dessert_apples){
  print(name)
  common_dessert_pheno_dat <- rbind(
    common_dessert_pheno_dat,
    abc_pop_info[grep(name, abc_pop_info$PLANTID),]
  )
}

nrow(common_dessert_pheno_dat)
# [1] 30 varieties

# removing some of the varieties that are not cider apples
common_dessert_pheno_dat <- common_dessert_pheno_dat[-grep("Buckeye Gala",common_dessert_pheno_dat$PLANTID),]
common_dessert_pheno_dat <- common_dessert_pheno_dat[-grep("Gala Must Regal Prince",common_dessert_pheno_dat$PLANTID),]
common_dessert_pheno_dat <- common_dessert_pheno_dat[-grep("Galarina",common_dessert_pheno_dat),]
common_dessert_pheno_dat <- common_dessert_pheno_dat[-grep("Marshall McIntosh",common_dessert_pheno_dat$PLANTID),]
common_dessert_pheno_dat <- common_dessert_pheno_dat[-grep("Kimball McIntosh 2-4-4-4",common_dessert_pheno_dat$PLANTID),]
common_dessert_pheno_dat <- common_dessert_pheno_dat[-grep("Field Spy",common_dessert_pheno_dat$PLANTID),]
common_dessert_pheno_dat <- common_dessert_pheno_dat[-grep("Loop Giant Spy",common_dessert_pheno_dat$PLANTID),]
common_dessert_pheno_dat <- common_dessert_pheno_dat[-grep("Manitoba Spy",common_dessert_pheno_dat$PLANTID),]
common_dessert_pheno_dat <- common_dessert_pheno_dat[-grep("Red Spy",common_dessert_pheno_dat$PLANTID),]
common_dessert_pheno_dat <- common_dessert_pheno_dat[-grep("Prairie Spy",common_dessert_pheno_dat$PLANTID),]
common_dessert_pheno_dat <- common_dessert_pheno_dat[-grep("Hotle Rome",common_dessert_pheno_dat$PLANTID),]
common_dessert_pheno_dat <- common_dessert_pheno_dat[-grep("Rome Beauty Law",common_dessert_pheno_dat$PLANTID),]
common_dessert_pheno_dat <- common_dessert_pheno_dat[-grep("Red Australian Rome Beauty",common_dessert_pheno_dat$PLANTID),]
common_dessert_pheno_dat <- common_dessert_pheno_dat[-grep("Pink Lady", common_dessert_pheno_dat$PLANTID),]

# how many varieties that we end up with
nrow(common_dessert_pheno_dat)
# [1] 16

# writing the dessert apple phenotype table to file
write.table(
  common_dessert_pheno_dat,
  'data/processed/final_dessert_apple_phenotype_data.tsv',
  sep = "\t",
  row.names = FALSE
)

