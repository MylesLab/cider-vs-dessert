# Title     : Generate Final Cider Apples List
# Objective : This script generates a list file which contains the phenotype
#             data for only the cider apples that overlap between ABC and the ones
#             that are used in Greg Peck's recent paper titled:
#             "Classifying Cider Apple Germplasm Using Genetic Markers for Fruit Acidity"
# Created by: tayabsoomro
# Created on: 2021-05-20

# TODO: PCA with English, France and ABC to see if they are all separated nicely.
# TODO: Add supplementary table should contain:
# - pI ids
# - apple ids
# - traits
# - country of origin
# - ...anything that I end up using
#
# TODO: Materials & Methods
# TODO: Introduction
#  Great amount of cider being consumed, lots of demand for cider apples,
#  Common cultivars of cider compared to traditional english / french varieties
#  to see what the cider industry could have if they used these english adn french
#  cider and dessert apple varieties.
# TODO: Discussion: Things we didn't measure but should be considered:
#     - Yield
#     - Precocity (how long does it take to bear fruit)
#     - Bineal Bearing
# TODO: Mention that there are uneven sample sizes (17 dessert apples, 48 france apples and 35 england)
#       https://www.researchgate.net/figure/The-effect-of-uneven-sampling-on-PCA-projection-PCA-projection-of-samples-taken-from-a_fig5_38015799
#       The degree of difference between the two points might be skewed by uneven sampling
# TODO: COPY WHAT TOMMY DID
# TODO: Tasks
#   - Are english cider apples different from french cider apples
#   - Are english cider apple different from dessert apples
#   - Are french cider apple different from dessert apples
#   - Are cider apples different from dessert apples.
# TODO: Table1 from Tommy's Paper
# TODO: Density plots for the phenotypes
#     - three different tests (english, french, dessert)
#     - report a p-value after Bonferroni correction ( divide by 30)
# TODO: Read a brief report for PPP

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

###################
## DATA CURATION ##
###################

# only keep the columns required
cols_to_keep <- c("PLANTID","ACP","ACNO","apple_id")
abc_pop_info <- abc_pop_info[, names(abc_pop_info) %in% cols_to_keep]

# only keep the rows where the ACP is PI
abc_pop_info <- abc_pop_info[which(abc_pop_info$ACP == "PI"),]

# only keep the unique rows
abc_pop_info <- unique.data.frame(abc_pop_info)

#############
## JOINING ##
#############

# matching the ACCNO with the PI (no). in the gpeck data
final.cider.df <- inner_join(gpeck_data,abc_pop_info,by = c("PI (no.)" = "ACNO"))

nrow(final.cider.df)
# [1] 141

table(final.cider.df$`Region of origin`)

all_na_varieties <- final.cider.df[final.cider.df$`Region of origin` == 'North America','PLANTID']


# The reason why we find 219 rows after left join instead of the 217 that we
# expect is because there are a few rows where a multiple apple ids corresponded
# to the same variety. We are going to keep these.

# join the phenotype table
final.cider.pheno.df <- left_join(final.cider.df,abc_pheno_tbl, by = "apple_id")

# write this final cider apples table
write.table(
  final.cider.pheno.df,
  'data/processed/final_cider_apple_phenotype_data.tsv',
  sep = "\t",
  row.names = FALSE
)
