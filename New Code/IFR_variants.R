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
  countrylist <- c("Germany","Spain","Italy","France","Sweden",
                   "China","Japan","Colombia","Brazil","USA")
  
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
  
  
### Modi et al ######################################################  
  
  # Load
  modi <- read.table("Data/IFR-Modi.txt",
                     header=FALSE, stringsAsFactors = FALSE)
  
  # Ungroup
  Modi1 <- ungroupIFR(IFR=modi[,2]/100,interval=modi[,1],midinterval=5,
                      minage=minage,maxage=maxage,resolution=resolution)
  
  Modi2 <- ungroupIFR(IFR=modi[,3]/100,interval=modi[,1],midinterval=5,
                      minage=minage,maxage=maxage,resolution=resolution)
  
  Modi3 <- ungroupIFR(IFR=modi[,4]/100,interval=modi[,1],midinterval=5,
                      minage=minage,maxage=maxage,resolution=resolution)
  
  # Put into data frame
  IFRs$Modi1 <- Modi1
  IFRs$Modi2 <- Modi2
  IFRs$Modi3 <- Modi3  
  
  
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

  # Rename
  UNdat <- UNdat %>% rename("Country"="Region, subregion, country or area *",
                            "ex"="Expectation of life e(x)",
                            "Age" = "Age (x)")
  
  # Recode country
  UNdat$Country <- recode(UNdat$Country, 
                          "United States of America" = "USA",
                          "United Kingdom"="UK")
  
  # Edit (gives an expected [?] error message)
  UNdat <- UNdat %>% mutate(ex=as.numeric(ex))

  # Subset (always China & France & Italy for Verity and Salje and Modi)
  UNdat <- UNdat %>% filter(Period=="2015-2020") %>% 
                     filter(Country%in%c(countrylist,"France","China","Italy"))
  
  # Change to data.frame
  UNdat <- as.data.frame(UNdat)
  
  
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
  
  
### Apply scaling for all countries, Modi/Italy ###################
  
  # Reference data
  e_2 <- UNdat[UNdat$Country=="Italy","ex"]
  interval <- UNdat[UNdat$Country=="Italy","Age"]
  
  # Loop over all countries  
  for(i in countrylist) {
    
    # Get data
    e_1 <- UNdat[UNdat$Country==i,"ex"]
    
    # Get ages
    scaling <- match_e_x(e_1,e_2,interval=interval,maxage=maxage,minage=minage,
                         outputresolution=resolution)
    
    # Predict
    scaled1 <- ungroupIFR(IFR=modi[,2]/100,interval=modi[,1],midinterval=5,
                         age=scaling)
    
    scaled2 <- ungroupIFR(IFR=modi[,3]/100,interval=modi[,1],midinterval=5,
                          age=scaling)
    
    scaled3 <- ungroupIFR(IFR=modi[,4]/100,interval=modi[,1],midinterval=5,
                          age=scaling)
    
    # Assign
    IFRs[,paste0("Modi1_",i)] <- scaled1
    IFRs[,paste0("Modi2_",i)] <- scaled2
    IFRs[,paste0("Modi3_",i)] <- scaled3
    
  }    
  
  
### Save ############################################################
  
  write.csv(IFRs,"Output/IFRs.csv",row.names=F)
  
