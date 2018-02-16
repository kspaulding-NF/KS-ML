# Clear the enivornment
write('Clearing Environment', stderr())
rm(list = ls())
options(stringsAsFactors = FALSE)

# Load required librarys
library(yhatr)
library(sqldf)
library(plyr)
library(dplyr)
library(tidyr)
library(RCurl)
library(RODBC)
library(QuantPsyc)
library(ggplot2)
library(pROC)
library(RODBCext)
library(ggplot2)
library(xgboost)
library(RJDBC)
library(xml2)



# Source all functions 
loadFunctions <- function() {
  write('- Sourcing Files ... ', stderr())
  directories = c("dependencies/" , "dependencies/Equifax_Consumer_Credit_Data/" , "dependencies/Equifax_Consumer_Credit_Data/CUA/")
  for (d in directories) {
    files = list.files(d, ".R")
    files = files[!grepl(".RData", files) & !grepl(".Rdata", files)] # not RData files
    for (file in files) {
      filename = paste0(d, file)
      source(paste0(d, file))
    }
    write(sprintf('\t%-30s %s', d,  sprintf('[%2s files]', length(files))), stderr())
  }
}





# Call Function Loader
loadFunctions()

# Set the location where large datasets will be saved and loaded
# This is so the data is accessible but not in source control
# Instead data will be timestamped
DATA_DIR = "\\\\sdw-mainfs-p01\\BusinessIntelligence\\KS\\R\\xgboost\\data"
