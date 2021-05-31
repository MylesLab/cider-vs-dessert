# Title     : Generate Final Dessert Apples List
# Objective : This script generates a list file which contains the phenotype
#             data for the dessert apple varieties that are commonly grown in 
#             US and Canada that are available in ABC
# Created by: tayabsoomro
# Created on: 2021-05-30

library(dplyr)

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
  "Golden Russet",
  "Honeycrisp",
  "Idared",
  "Jonagold",
  "Lobo",
  "Mcintosh",
  "Red Delicious",
  "Royal Gala",
  "Spartan",
  "Spy"
)

length(canada_dessert_apples)
# [1] 17

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
  "Honey Crisp",
  "Mcintosh",
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
# [1] 22

# There are 22 common dessert apples in US and Canada


