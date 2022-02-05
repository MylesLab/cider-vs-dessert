# Objective : This script generates a list file which contains the phenotype
#             data for only the cider apples that overlap between ABC and the ones
#             that are used in Greg Peck's recent paper titled:
#             "Classifying Cider Apple Germplasm Using Genetic Markers for Fruit Acidity"

library(dplyr)
library(readxl)

source("data_curation/utils.R")

##################
## DATA LOADING ##
##################

# load the data from the following paper
# Kumar, Shanthanu K, Nathan W, Laura D, Kenong X, and Gregory P 2021. 
# “Classifying Cider Apple Germplasm Using Genetic Markers for Fruit Acidity.” 
# J. Amer. Soc. Hort. Sci. 1 (aop): 1–16.
gpeck_data <- read_excel(
  'data/raw/DOI:10.21273-JASHS05056/cider_apple_data_pgreg.xlsx',
)

dim(gpeck_data)
# [1] 217 11
# This number of rows matches to the one in the paper and therefore 
# I am confident that the data correctly loaded.

# load the ABC data with PI ids 
abc_pop_info <- load_abc_pop_info()
dim(abc_pop_info)
# [1] 910 3

# load the ABC phenotype table
abc_pheno_tbl <- read_excel('data/raw/pheno_meta_data_abc.xlsx')
dim(abc_pheno_tbl)
# [1] 1119 48


#############
## JOINING ##
#############

abc_pop_info$ACNO <- as.double(abc_pop_info$ACNO)

# matching the ACCNO with the PI (no). in the gpeck data
gpeck_data_pivot <- inner_join(gpeck_data, abc_pop_info, by = c("PI (no.)" = "ACNO"))
nrow(gpeck_data_pivot)
# [1] 139

# Of the 217 total cider apple varieties in the paper, there are 139 that are
# present in ABC.

# inspecting to see if the "Accession name" and the "PLANTID" columns have the same
# values. There were 8 varieties that had differing values but after visual
# inspection, it is confirmed that they are in fact same names, except for 
# minor formatting changes.
subset(
  gpeck_data_pivot,
  gpeck_data_pivot$`Accession name` != gpeck_data_pivot$PLANTID
)[, c("Accession name", "PLANTID")]
# # A tibble: 8 x 2
# `Accession name`                 PLANTID               
# <chr>                            <chr>                 
#   1 Manch Rouge                      Manche Rouge          
# 2 Foxwhelp (misidentiﬁed)          Foxwhelp              
# 3 Yellow Bellﬂower                 Yellow Bellflower     
# 4 Dufﬂin                           Dufflin               
# 5 Tremlett's Bitter (misidentiﬁed) Trembletts Bitter     
# 6 Peau De Vache                    Peau de Vache         
# 7 Rouge Belle De Boskoop           Rouge Belle de Boskoop
# 8 Reinette D'Anjou                 Reinette d' Anjou 

# checking to see what the different regions of origins are
table(gpeck_data_pivot$`Region of origin`)
#
# Australia  Central Europe         England     Former USSR          France 
# 1              16              34               1              48 
# Japan             n/a     New Zealand   North America Northern Africa 
# 1               2               1              25               1 
# Northern Europe Southern Europe           Spain 
# 5               1               3 

gpeck_data_pivot$apple_id <- as.double(gpeck_data_pivot$apple_id)

# join the phenotype table
final.cider.df <- left_join(gpeck_data_pivot, abc_pheno_tbl)

nrow(final.cider.df)
# [1] 139

# only retain the English and French cider apples
final.cider.df <-
  final.cider.df[
    which(final.cider.df$`Region of origin` == "England" | 
            final.cider.df$`Region of origin` == "France"
    ),
]

nrow(final.cider.df)
# [1] 82

table(final.cider.df$`Region of origin`)
# England  France 
# 34      48

# We retain 82 cider apple varieties, of which 34 are English and 48 are French

#################
## EXPORT DATA ##
#################

# write this final cider apples table
write.table(
  final.cider.df,
  'data/processed/final_cider_apple_phenotype_data.tsv',
  sep = "\t",
  row.names = FALSE
)

