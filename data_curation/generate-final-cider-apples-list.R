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

###################
## DATA CURATION ##
###################

## CURATING THE ABC_POP_INFO DATAFRAME
# only keep the columns required from the abc_pop_info
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
gpeck_data_pivot <- inner_join(gpeck_data,abc_pop_info,by = c("PI (no.)" = "ACNO"))

nrow(gpeck_data_pivot)
# [1] 141

# Of the 217 total cider apple varieties in the paper, there are 141 that are
# present in ABC.

# inspecting to see if the "Accession name" and the "PLANTID" columns have the same
# values. There were 8 varieties that had differeing values but after visual
# inspection, it is confirmed that they are in fact same names, except for 
# minor formatting changes.
subset(gpeck_data_pivot, gpeck_data_pivot$`Accession name` != gpeck_data_pivot$PLANTID)[,c("Accession name", "PLANTID")]
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

table(gpeck_data_pivot$`Region of origin`)
# 
# Australia  Central Europe         England     Former USSR          France 
# 1              16              35               1              48 
# Japan             n/a     New Zealand   North America Northern Africa 
# 1               2               1              26               1 
# Northern Europe Southern Europe           Spain 
# 5               1               3 

# join the phenotype table
final.cider.df <- left_join(gpeck_data_pivot,abc_pheno_tbl, by = "apple_id")

nrow(final.cider.df)
# [1] 141

# only retain the English and French cider apples
final.cider.df <- 
  final.cider.df[
    which(final.cider.df$`Region of origin` == "England" | 
            final.cider.df$`Region of origin` == "France"
    ),
]

nrow(final.cider.df)
# [1] 83

table(final.cider.df$`Region of origin`)
# England  France 
# 35      48

# We retain 83 cider apple varieties, of which 35 are English and 48 are French

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

