### IFR scenarios ###################################################


### General #########################################################

  # Packages
  library(tidyverse)
  library(readxl)
  library(httr)

  # Functions
  source("New Code/functions.R")

  # Some settings
  minage <- 0
  maxage <- 99
  resolution <- 0.5
  
  # Countries to look at
  countrylist <- c("Germany","Spain","Italy")
  
  # Data frame for results
  IFRs <- data.frame(Age=seq(minage,maxage,by=resolution))


### Levin et al #####################################################
  
  # Calculate according to formula
  IFRs <- IFRs %>% mutate(Levin=  exp(-7.53 + 0.119 * Age)/100)
  
  
### Verity et al ####################################################
  
  # Load
  verity <- read.table("Data/IFR-Verity.txt",
                       header=FALSE, stringsAsFactors = FALSE)
  
  # Ungroup
  Verity <- ungroupIFR(IFR=verity[,2],interval=verity[,1],midinterval=5,
                       minage=minage,maxage=maxage,resolution=resolution)
  
  # Put into data frame
  IFRs$Verity <- Verity
  

### Salje et al #####################################################

  # Load
  salje <- read.table("Data/IFR-Salje.txt",
                       header=FALSE, stringsAsFactors = FALSE)
  
  # Ungroup
  Salje <- ungroupIFR(IFR=salje[,2]/100,interval=salje[,1],midinterval=5,
                      minage=minage,maxage=maxage,resolution=resolution)
  
  # Put into data frame
  IFRs$Salje <- Salje
  
  
### +- 25% ################################################
  
  IFRs <- IFRs %>% mutate(Levin_p25 = Levin*1.25,
                          Levin_m25 = Levin*0.75,
                          Verity_p25 = Verity*1.25,
                          Verity_m25 = Verity*0.75,
                          Salje_p25 = Salje*1.25,
                          Salje_m25 = Salje*0.75)  
  
  
### Preparations for scaling ########################################

  # Load data from web
  web <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/3_Mortality/WPP2019_MORT_F17_1_ABRIDGED_LIFE_TABLE_BOTH_SEXES.xlsx" 
  filename <- "Data/UNe0.xlsx"
  if(!file.exists(filename)) {
    GET(web, write_disk(filename, overwrite = TRUE))
  }

  # Load spreadsheet
  UNdat <- read_excel(path=filename,sheet="ESTIMATES",
                      range="A17:T77381")
  
  # Makes a few things easier below
  UNdat <- as.data.frame(UNdat)
  
  # Rename
  UNdat <- UNdat %>% rename("Country"="Region, subregion, country or area *",
                            "ex"="Expectation of life e(x)",
                            "Age" = "Age (x)")
  
  # Edit (gives an expected [?] error message)
  UNdat <- UNdat %>% mutate(ex=as.numeric(ex))

  # Subset (always China & France for Verity and Salje)
  UNdat <- UNdat %>% filter(Period=="2015-2020") %>% 
                     filter(Country%in%c(countrylist,"France","China"))
  
  
### Apply scaling for all countries, Verity/China ###################

  # Reference data
  e_2 <- UNdat[UNdat$Country=="China","ex"]
  interval <- UNdat[UNdat$Country=="China","Age"]
  
  # Loop over all countries
  for(i in countrylist) {
  
    # Get data
    e_1 <- UNdat[UNdat$Country==i,"ex"]
  
    # Get ages
    scaling <- match_e_x(e_1,e_2,interval=interval,maxage=maxage,minage=minage,
                         outputresolution=resolution)
    
    # Predict
    scaled <- ungroupIFR(IFR=verity[,2],interval=verity[,1],midinterval=5,
                         age=scaling)

    # Assign
    IFRs[,paste0("Verity_",i)] <- scaled
    
  }
  
  
### Apply scaling for all countries, Salje/France ###################

  # Reference data
  e_2 <- UNdat[UNdat$Country=="France","ex"]
  interval <- UNdat[UNdat$Country=="France","Age"]
  
  # Loop over all countries  
  for(i in countrylist) {
    
    # Get data
    e_1 <- UNdat[UNdat$Country==i,"ex"]
    
    # Get ages
    scaling <- match_e_x(e_1,e_2,interval=interval,maxage=maxage,minage=minage,
                         outputresolution=resolution)
    
    # Predict
    scaled <- ungroupIFR(IFR=salje[,2]/100,interval=salje[,1],midinterval=5,
                         age=scaling)
    
    # Assign
    IFRs[,paste0("Salje_",i)] <- scaled
    
  }  
  
### Save ############################################################
  
  write.csv(IFRs,"Output/IFRs.csv",row.names=F)
  
