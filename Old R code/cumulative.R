### Load packages ###################################################

  library(tidyverse)
  library(httr)


### Load data #######################################################

  # Source
  url <- 'https://osf.io/7tnfh/download'
  
  # Create Data folder if necessary
  if (!"Data" %in% dir()){
    dir.create("Data")
  }
  
  # Where to save
  filename <- 'Data/Output_5.zip'
  
  # Download data
  GET(url, write_disk(filename, overwrite = TRUE))
  
  # Unzip 
  unzip(filename)
  
  # Load data 
  Dat <- read_csv(filename,skip=3)


### Edit/restrict data ##############################################

  # Countries of interest
  countrylist <- c("Germany","Belgium")
  region <- c("All")
  
  # Restrict country
  dat <- Dat %>% filter(Country %in% countrylist & Region %in% region)
  
  # Restrict gender
  dat <- dat %>% filter(Sex=="b")
  
  # Only keep interesting variables
  dat <- dat %>% select(Country,Date,Sex,Age,Cases)


### Calculate #######################################################

  # IFR by age (mid-interval)
  # Source: https://www.medrxiv.org/content/10.1101/2020.07.23.20160895v5.full.pdf)
  dat <- dat %>% mutate(ICR=  exp(-7.53 + 0.119 * (Age+2.5))/100)
  
  # Deaths
  dat <- dat %>% group_by(Country,Date) %>% mutate(Deaths=Cases*ICR) 
  
  # Results
  results <- dat %>% group_by(Country,Date) %>% 
              summarise(res = sum(Deaths)/sum(Cases))


### Plot ############################################################

  # Use dates
  results$Date <- as.Date(results$Date,"%d.%m.%Y")
  
  results %>% ggplot(aes(x=Date,y=res,color=Country))+
    geom_line()
