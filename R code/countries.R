## Get coverage DB countries ##############################

if(!file.exists("Data/countries.rda")) {

  # Packages
  library(tidyverse)
  library(httr)
  
  # Source
  web <- 'https://osf.io/7tnfh/download'
  
  # Where to save
  filename <- 'Data/Output_5.zip'
  
  # Download data
  if(!file.exists(filename)) {
    GET(web, write_disk(filename, overwrite = TRUE))
    unzip(filename)
  }
  
  # Load data
  filename <- 'Data/Output_5.csv'
  Dat <- read_csv(filename,skip=3)
  
  # Get countries
  countrylist <- unique(Dat$Country)
  
  # Save
  save(countrylist,file="Data/countries.rda")
  
}