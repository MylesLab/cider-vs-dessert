# Objective : This script implements a function for loading ABC Population Info
#             dataset into the environment.


load_abc_pop_info <- function(){
  #' This function loads the ABC population info dataframe by only keeping the
  #' apple accessions that have the least missing data for phenotypes wherever 
  #' there are duplicates
  
  library(readxl)
  source('data_curation/constants.R')
  
  # load the ABC phenotype table
  abc_pheno_tbl <- read_excel('data/raw/pheno_meta_data_abc.xlsx')
  dim(abc_pheno_tbl)
  # [1] 1119 48
  
  abc_pop_info <- read_excel(
    'data/raw/20200204_abc_pop_info.xlsx',
    col_types = "text"
  )
  dim(abc_pop_info)
  # [1] 3988 12
  
  abc_pop_info <- unique(abc_pop_info[which(abc_pop_info$ACP == "PI"),c("PLANTID","ACNO","apple_id")])
  dim(abc_pop_info)
  
  # get the names of apples for which there are more than one apple ids.
  duplicates <- unique(abc_pop_info[which(duplicated(abc_pop_info$PLANTID)),]$PLANTID)
  
  length(duplicates)
  # [1] 34
  
  rows_to_remove <- c()
  for(name in duplicates){
    aids <- abc_pop_info[which(abc_pop_info$PLANTID == name),]$apple_id
    
    num_missing <- 10
    final_aid <- NULL
    for(aid in aids){
      current_missing <- 
        sum(abc_pheno_tbl[which(abc_pheno_tbl$apple_id == aid), phenotype_cols] == "NA")
      if(current_missing < num_missing) {
        num_missing <- current_missing
        final_aid <- aid
      }
      
    }
    print(paste0("Name: ", name, " IDs: ", toString(aids)))
    print(final_aid)
    
    # remove all the rows that do not have this name and this apple id
    rows_to_remove <- c(
      rows_to_remove,
      which(abc_pop_info$PLANTID == name & abc_pop_info$apple_id != final_aid)
    )
    
  }
  abc_pop_info <- abc_pop_info[-rows_to_remove,]
  
  abc_pop_info$apple_id <- as.numeric(abc_pop_info$apple_id)
  nrow(abc_pop_info)
  # [1] 910
  
  return(abc_pop_info)

}

