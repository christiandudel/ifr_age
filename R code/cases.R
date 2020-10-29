### Prevalence and case count scenarios #############################


### General #########################################################

  # Packages
  library(tidyverse)
  library(readxl)
  library(httr)

  # Some settings
  maxage <- 99

  # Countries to look at
  countrylist <- c("Germany","Spain","Italy","France","Sweden",
                   "China","Japan","Colombia","Brazil","USA")
  
  # Data frame for results
  Prevalence <- data.frame(Age=seq(0,maxage))
  Counts <- data.frame(Age=seq(0,maxage,by=5))
  
  
### Scenarios of Levin et al ########################################

  # Set scenarios
  last <- length(65:maxage)
  levin1 <- c(rep(0.23,50),rep(0.16,15),rep(0.14,last))
  levin2 <- c(rep(0.20,50),rep(0.20,15),rep(0.20,last))
  levin3 <- c(rep(0.26,50),rep(0.10,15),rep(0.06,last))
  
  # Put in data
  Prevalence <- Prevalence %>% mutate(Levin1 = levin1,
                                      Levin2 = levin2,
                                      Levin3 = levin3)
  
  
### Seroprevelance studies ##########################################
  
  # Italy
  # https://www.istat.it/it/files//2020/08/ReportPrimiRisultatiIndagineSiero.pdf
  last <- length(70:maxage)
  italy <- c(rep(0.022,18),rep(0.021,17),rep(0.024,15),rep(0.031,10),
             rep(0.026,10),rep(0.025,last))
  
  # Spain?
  
  # Some other country?
  
  # Put in data
  Prevalence <- Prevalence %>% mutate(PrItaly = italy)
  
  
### Get population data #############################################
  
  # Load UN population data from web
  web <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_Population/WPP2019_POP_F07_1_POPULATION_BY_AGE_BOTH_SEXES.xlsx" 
  filename <- "Data/UNpop.xlsx"
  if(!file.exists(filename)) {
    GET(web, write_disk(filename, overwrite = TRUE))
  }
  
  # Read pop data
  UNpop <- read_xlsx(filename, sheet = 1, skip = 16)

  # Rename
  UNpop <- UNpop %>% rename("Country"="Region, subregion, country or area *",
                            "Year"="Reference date (as of 1 July)")
  
  # Recode country
  UNpop$Country <- recode(UNpop$Country, 
                          "United States of America" = "USA",
                          "United Kingdom"="UK")
  
  # Select countries
  UNpop <- UNpop %>% filter(Country%in%c(countrylist))
  
  # Keep most recent year for each country
  UNpop <- UNpop %>% group_by(Country) %>% 
    filter(Year==max(Year)) 

  # Keep variables of interest
  UNpop <- UNpop %>% select(Country,"0-4":"100+")
  
  # Rename
  names(UNpop)[2:dim(UNpop)[2]] <- paste(seq(0,100,by=5))
  
  # Reshape
  UNpop <- UNpop %>% gather(Age,Pop,-Country) 
  
  # Change types
  UNpop <- UNpop %>% mutate(Age=as.numeric(Age),
                            Pop=as.numeric(Pop))
  
  # Aggregate age if needed
  UNpop <- UNpop %>%  mutate(Age=ifelse(Age>maxage,max(Counts$Age),Age))
  UNpop <- aggregate(Pop~Country+Age,data=UNpop,sum)
  
  # Reshape again
  UNpop <- UNpop %>% spread(Country,Pop)
  
  
### Combine population data with prevalence data ####################
  
  # Restrict prevalence
  Prevalence <- Prevalence %>% filter(Age%in%UNpop$Age)
  
  # Loop over countries
  for(country in countrylist) {
    
    # Apply scenarios
    tmp <- UNpop[,country]*Prevalence[,-1]
    
    # Name
    names(tmp) <- paste(country,names(tmp),sep="_")
    
    # Put into results object
    Counts <- cbind(Counts,tmp)
    
  }
  
  
### Get data for case and death counts ##############################
  
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
  Dat <- read_csv(filename,skip=3)
  
  # Restrict country
  region <- c("All")
  Dat <- Dat %>% filter(Country %in% countrylist & Region %in% region)
  
  # Restrict gender
  Dat <- Dat %>% filter(Sex=="b")
  
  # Set date
  Dat$Date <- as.Date(Dat$Date,"%d.%m.%Y")
  
  
### Death counts ####################################################

  # Restrict to most recent date
  dat <- Dat %>% group_by(Country) %>% filter(Date==max(Date))

  # Restrict age
  dat <- dat %>% mutate(Age=ifelse(Age>maxage,max(Counts$Age),Age))
  
  # Aggregate
  deaths <- aggregate(Deaths~Country+Age,data=dat,sum)
  
  # Reshape
  deaths <- deaths %>% spread(Country,Deaths)
  
  # Rename
  names(deaths)[2:dim(deaths)[2]] <- paste(names(deaths)[2:dim(deaths)[2]],
                                           "Deaths",
                                           sep="_")
    
  # Merge
  Counts <- inner_join(Counts,deaths)
  
  
### Cumulative cases ################################################

  # Aggregate
  cases <- aggregate(Cases~Country+Age,data=dat,sum)
  
  # Reshape
  cases <- cases %>% spread(Country,Cases)
  
  # Rename
  names(cases)[2:dim(cases)[2]] <- paste(names(cases)[2:dim(cases)[2]],
                                         "Cases",
                                         sep="_")
  
  # Merge
  Counts <- inner_join(Counts,cases)
  
  
### Recent cases ####################################################
  
  # Set locale to English (required for weekdays to work properly)
  Sys.setlocale("LC_ALL","English")
  
  # Select Sundays
  dat <- Dat %>% filter(weekdays(Date) == "Sunday") 
  
  # Restrict age
  dat <- dat %>% mutate(Age=ifelse(Age>maxage,max(Counts$Age),Age))
  
  # Calculate weekly new cases/deaths
  cases <- dat %>% 
           # Sort by date
           arrange(Country, Age, Date) %>% 
           group_by(Country, Age) %>%
           # Calculate new cases/deaths
           mutate(NewCases = diff(c(0,Cases)),
                  NewDeaths = diff(c(0,Deaths))) %>% 
           ungroup() %>% 
           # Keep most recent date
           group_by(Country) %>% filter(Date==max(Date))
  
  # Aggregate
  cases <- aggregate(NewCases~Country+Age,data=cases,sum)
  
  # Consistency check
  cases <- cases %>% mutate(NewCases=ifelse(NewCases<0,0,NewCases))
  
  # Reshape
  cases <- cases %>% spread(Country,NewCases)
  
  # Rename
  names(cases)[2:dim(cases)[2]] <- paste(names(cases)[2:dim(cases)[2]],
                                         "NewCases",
                                         sep="_")
  
  # Merge
  Counts <- inner_join(Counts,cases)
  
  
### Rescale and save ################################################
  
  # Scale to 1 
  Age <- Counts$Age
  Counts <- apply(Counts,2,function(x) x/sum(x))
  Counts <- as.data.frame(Counts)
  Counts$Age <- Age
  
  # Save
  write.csv(Counts,"Output/Counts.csv",row.names=F)
  