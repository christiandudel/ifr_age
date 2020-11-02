### Load packages ###################################################

  library(zoo)
  library(tidyverse)
  library(readr)
  library(lubridate)
  library(cowplot)
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

  
### Restrict/edit data ##############################################
  
  # Countries of interest
  countrylist <- c("Germany","Belgium")
  region <- c("All")
  
  # Restrict country
  dat <- Dat %>% 
    filter(Country %in% countrylist,
           Region %in% region,
           Sex == "b") 
  
  # Select variables
  dat <- dat %>% select(Country,Date,Sex,Age,Cases,Deaths)  
  
  # Set Date
  dat <- dat %>% mutate(Date = dmy(Date)) 
  

### Calculate new cases #############################################
  
  # Set locale to English (required for weekdays to work properly)
  Sys.setlocale("LC_ALL","English")
  
  # Select Sundays
  dat <- dat %>% filter(weekdays(Date) == "Sunday") 
  
  # Calculate weekly new cases
  dat <- dat %>% 
         arrange(Country, Age, Date) %>% 
         group_by(Country, Age) %>% 
         mutate(NewCases = diff(c(0,Cases)),
         NewDeaths = diff(c(0,Deaths))) %>% 
         ungroup() %>% 
         arrange(Country, Date, Age) 
  
  
### Results #########################################################
  
  # Calculate IFR contributions by age
  dat <- dat %>% group_by(Country, Date) %>% 
  mutate(IFR = exp(-7.53 + 0.119 * (Age+2.5))/100,
         weight = NewCases / sum(NewCases),
         res = IFR * weight) 

  # Sum over all ages
  results <- dat %>% summarize(res=sum(res))
  
  
### Plot results ####################################################
  
  p1 <- 
  results %>% 
    ggplot(aes(x=Date,y=res,color=Country))+
    geom_line()
  
  p2 <-
  dat %>% 
    summarize(NewDeaths = sum(NewDeaths)) %>% 
    ggplot(aes(x=Date,y=NewDeaths,color=Country))+
    geom_line()
    
  # Compare w total deaths by week, just to see how the peak timing relates
  # plot_grid(p1, p2, labels = c('A', 'B'), ncol = 1)
  # interestingly the deaths peak before the IFR peaks.


