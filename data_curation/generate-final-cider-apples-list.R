# Title     : Generate Final Cider Apples List
# Objective : This script generates a list file which contains the phenotype
#             data for only the cider apples that overlap between ABC and the ones
#             that are used in Greg Peck's recent paper titled:
#             "Classifying Cider Apple Germplasm Using Genetic Markers for Fruit Acidity"
# Created by: tayabsoomro
# Created on: 2021-05-20

library(dplyr)
library(readxl)

##################
## DATA LOADING ##
##################

# load the gpeck data
gpeck_data <- read_excel(
  'data/raw/DOI:10.21273-JASHS05056/cider_apple_data_pgreg.xlsx',
)

nrow(gpeck_data)
# [1] 217
# This number matches to the one in the paper and therefore I am confident that
# the data correctly loaded.

# load the ABC data with PI ids 
abc_pop_info <- read_excel('data/raw/20200204_abc_pop_info.xlsx')

# load the ABC phenotype table
abc_pheno_tbl <- read_excel('data/raw/pheno_meta_data_abc.xlsx')


# remove nursery id column because it does not have one-to-one mapping to apple id
cols_to_keep <- c("PLANTID","ACP","ACNO","apple_id")
abc_pop_info <- abc_pop_info[, names(abc_pop_info) %in% cols_to_keep]

# only keep the rows where the ACP is PI
abc_pop_info <- abc_pop_info[which(abc_pop_info$ACP == "PI"),]

# only keep the unique rows
abc_pop_info <- unique.data.frame(abc_pop_info)

# matching the ACCNO with the PI (no). in the gpeck data
final.cider.df <- left_join(gpeck_data,abc_pop_info,by = c("PI (no.)" = "ACNO"))

nrow(final.cider.df)
# [1] 219

# The reason why we find 219 rows after left join instead of the 217 that we
# expect is because there are a few rows where a multiple apple ids corresponded
# to the same variety. We are going to keep these.

# join the phenotype table
final.cider.pheno.df <- left_join(final.cider.df,abc_pheno_tbl, by = "apple_id")

length(final.cider.pheno.df$`Accession name`)
