### IFR scenarios ###################################################


### General #########################################################

  # Packages
  library(tidyverse)
  library(readxl)
  library(httr)

  # Functions
  source("R code/functions.R")

  # Some settings
  minage <- 0
  maxage <- 99
  resolution <- 0.5
  
  # Countries to look at
  load("Data/countries.rda")
  # 
  # countrylist <- c("Germany","Spain","Italy","France","Sweden",
  #                  "China","Japan","Colombia","Brazil","USA")
  
  # Data frame for results
  IFRs <- data.frame(Age=seq(minage,maxage,by=resolution))


### Levin et al #####################################################
  
  # Calculate according to formula
  IFRs <- IFRs %>% mutate(Levin=  exp(-7.53 + 0.119 * Age)/100)
  
  
### Levin et al predicition intervals ###############################
  
  # Paths
  filename <- 'Data/levin_prediction.xlsx'
  preddata <- "https://www.medrxiv.org/highwire/filestream/98266/field_highwire_adjunct_files/0/2020.07.23.20160895-1.xlsx"
  
  # Download data
  if(!file.exists(filename)) {
    GET(preddata, write_disk(filename, overwrite = TRUE))
  }
  
  # Load data
  pred <- read_excel(path=filename,
                     sheet="Metaregression Predictions",
                     range="A2:F193",
                     col_names=T)
  
  # Rename and extract
  names(pred) <- c("Age","Pred","CIl","CUi","LevinLow","LevinUp")
  pred <- pred %>% select(Age,LevinLow,LevinUp)
  
  # Divide by 100
  pred <- pred %>% mutate(LevinLow=LevinLow/100,
                          LevinUp=LevinUp/100) 
  
  # Extend to age 99
  LevinLow <- ungroupIFR(IFR=pred$LevinLow,interval=pred$Age,
                         midinterval=0.25,
                         minage=minage,maxage=maxage,
                         resolution=resolution)
  
  LevinUp  <- ungroupIFR(IFR=pred$LevinUp,interval=pred$Age,
                         midinterval=0.25,
                         minage=minage,maxage=maxage,
                         resolution=resolution)
  
  # Assign
  IFRs$LevinLow <- LevinLow
  IFRs$LevinUp  <- LevinUp
  
  
### Verity et al ####################################################
  
  # Load
  verity <- read.table("Data/IFR-Verity.txt",
                       header=FALSE, stringsAsFactors = FALSE)
  
  # Ungroup: Point estimate
  Verity <- ungroupIFR(IFR=verity[,2],interval=verity[,1],
                       midinterval=5,minage=minage,maxage=maxage,
                       resolution=resolution)
  
  # Ungroup: 95% lower bound
  VerityLow <- ungroupIFR(IFR=verity[,3],interval=verity[,1],
                       midinterval=5,minage=minage,maxage=maxage,
                       resolution=resolution)
  
  # Ungroup: 95% upper bound
  VerityUp <- ungroupIFR(IFR=verity[,4],interval=verity[,1],
                          midinterval=5,minage=minage,maxage=maxage,
                          resolution=resolution)
  
  # Put into data frame
  IFRs$Verity    <- Verity
  IFRs$VerityLow <- VerityLow
  IFRs$VerityUp  <- VerityUp
  

### Salje et al #####################################################

  # Load
  salje <- read.table("Data/IFR-Salje.txt",
                       header=FALSE, stringsAsFactors = FALSE)
  
  # Ungroup: Point estimate
  Salje <- ungroupIFR(IFR=salje[,2]/100,interval=salje[,1],
                      midinterval=5,minage=minage,maxage=maxage,
                      resolution=resolution)
  
  # Ungroup: 95% low
  SaljeLow <- ungroupIFR(IFR=salje[,3]/100,interval=salje[,1],
                      midinterval=5,minage=minage,maxage=maxage,
                      resolution=resolution)
  
  # Ungroup: 95% upper
  SaljeUp <- ungroupIFR(IFR=salje[,4]/100,interval=salje[,1],
                         midinterval=5,minage=minage,maxage=maxage,
                         resolution=resolution)
  
  # Put into data frame
  IFRs$Salje    <- Salje
  IFRs$SaljeLow <- SaljeLow
  IFRs$SaljeUp  <- SaljeUp
  
  
### O'Driscoll et al ################################################
  
  # Load
  odriscoll <- read.table("Data/IFR-ODriscoll.txt",
                           header=FALSE,stringsAsFactors = FALSE)
  
  # Ungroup: Point estimate
  ODriscoll <- ungroupIFR(IFR=odriscoll[,2]/100,interval=odriscoll[,1],
                      midinterval=c(rep(2.5,16),5),
                      minage=minage,maxage=maxage,
                      resolution=resolution)
  
  # Ungroup: 95% low
  ODriscollLow <- ungroupIFR(IFR=odriscoll[,3]/100,interval=odriscoll[,1],
                         midinterval=c(rep(2.5,16),5),
                         minage=minage,maxage=maxage,
                         resolution=resolution)
  
  # Ungroup: 95% upper
  ODriscollUp <- ungroupIFR(IFR=odriscoll[,4]/100,interval=odriscoll[,1],
                        midinterval=c(rep(2.5,16),5),
                        minage=minage,maxage=maxage,
                        resolution=resolution)
  
  # Put into data frame
  IFRs$ODriscoll    <- ODriscoll
  IFRs$ODriscollLow <- ODriscollLow
  IFRs$ODriscollUp  <- ODriscollUp
  
  
### Brazeau et al ###################################################
  
  # Load
  brazeau <- read.table("Data/IFR-Brazeau.txt",
                          header=FALSE,stringsAsFactors = FALSE)
  
  # Ungroup: Point estimate
  Brazeau <- ungroupIFR(IFR=brazeau[,2]/100,interval=brazeau[,1],
                        midinterval=c(rep(2.5,18),5),
                        minage=minage,maxage=maxage,
                        resolution=resolution)
  
  # Ungroup: 95% low
  BrazeauLow <- ungroupIFR(IFR=brazeau[,3]/100,interval=brazeau[,1],
                           midinterval=c(rep(2.5,18),5),
                           minage=minage,maxage=maxage,
                           resolution=resolution)
  
  # Ungroup: 95% upper
  BrazeauUp <- ungroupIFR(IFR=brazeau[,4]/100,interval=brazeau[,1],
                          midinterval=c(rep(2.5,18),5),
                          minage=minage,maxage=maxage,
                          resolution=resolution)
  
  # Put into data frame
  IFRs$Brazeau    <- Brazeau
  IFRs$BrazeauLow <- BrazeauLow
  IFRs$BrazeauUp  <- BrazeauUp
  
  
### Acosta (Spain) ###################################################
  
  # Load
  file <- "https://raw.githubusercontent.com/kikeacosta/ifr_age_spain/master/Output/spain_sex_age_ifr.csv"
  acosta <- read_csv(file)
  
  # Keep only both sexes
  acosta <- acosta %>% filter(Sex=="b")
  
  # Ungroup: Point estimate
  Acosta <- ungroupIFR(IFR=acosta$IFR,interval=acosta$Age,
                        midinterval=c(rep(2.5,18),5),
                        minage=minage,maxage=maxage,
                        resolution=resolution)
  
  # Ungroup: 95% low
  AcostaLow <- ungroupIFR(IFR=acosta$IFR_l,interval=acosta$Age,
                       midinterval=c(rep(2.5,18),5),
                       minage=minage,maxage=maxage,
                       resolution=resolution)
  
  # Ungroup: 95% upper
  AcostaUp <- ungroupIFR(IFR=acosta$IFR_u,interval=acosta$Age,
                          midinterval=c(rep(2.5,18),5),
                          minage=minage,maxage=maxage,
                          resolution=resolution)
  
  # Put into data frame
  IFRs$Acosta    <- Acosta
  IFRs$AcostaLow <- AcostaLow
  IFRs$AcostaUp  <- AcostaUp
  
  
### Modi et al (Italy) ##############################################
  
  # Load
  modi <- read.table("Data/IFR-Modi.txt",
                     header=FALSE, stringsAsFactors = FALSE)
  
  # Ungroup
  Modi1 <- ungroupIFR(IFR=modi[,2]/100,interval=modi[,1],
                      midinterval=5,minage=minage,maxage=maxage,
                      resolution=resolution)
  
  Modi2 <- ungroupIFR(IFR=modi[,3]/100,interval=modi[,1],
                      midinterval=5,minage=minage,maxage=maxage,
                      resolution=resolution)
  
  Modi3 <- ungroupIFR(IFR=modi[,4]/100,interval=modi[,1],
                      midinterval=5,minage=minage,maxage=maxage,
                      resolution=resolution)
  
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
                          "Bolivia (Plurinational State of)" = "Bolivia",
                          "Republic of Korea" = "South Korea",
                          "Venezuela (Bolivarian Republic of)"= "Venezuela",
                          "China, Taiwan Province of China" = "Taiwan",
                          "State of Palestine" = "Palestine")
  
  # Remove countries which are not in UN data
  countrylist <- countrylist[countrylist%in%unique(UNdat$Country)]
  
  # Edit (gives an expected [?] error message)
  UNdat <- UNdat %>% mutate(ex=as.numeric(ex))

  # Subset (always China & France & Italy for Verity and Salje and Modi)
  countrylist2 <- c(countrylist,"France","China","Italy")
  UNdat <- UNdat %>% filter(Period=="2015-2020") %>% 
                     filter(Country%in%countrylist2) %>% 
                     select(Country,Age,ex)
  
  # Change to data.frame
  UNdat <- as.data.frame(UNdat)
  

### Add e_x for Levin ###############################################
  
  # Load data
  ex_levin <- read_csv("Output/ex_levin.csv")
  
  # Edit
  ex_levin <- ex_levin %>% select(Age,ex_weighted) %>% 
              filter(Age%in%unique(UNdat$Age)) %>% 
              mutate(Country="Levin") %>% 
              rename("ex"="ex_weighted")
  
  # Combine
  UNdat <- rbind(UNdat,ex_levin)
  
  
### Apply scaling for all countries, Verity/China ###################

  # Reference data
  e_2 <- UNdat[UNdat$Country=="China","ex"]
  interval <- UNdat[UNdat$Country=="China","Age"]
  
  # Loop over all countries
  for(i in countrylist) {
  
    # Get data
    e_1 <- UNdat[UNdat$Country==i,"ex"]
  
    # Get ages
    scaling <- match_e_x(e_1,e_2,interval=interval,
                         maxage=maxage,minage=minage,
                         outputresolution=resolution)
    
    # Predict: Point estimate
    scaled <- ungroupIFR(IFR=verity[,2],interval=verity[,1],
                         midinterval=5,age=scaling)
    
    # Predict: 95% lower
    scaled_low <- ungroupIFR(IFR=verity[,3],interval=verity[,1],
                         midinterval=5,age=scaling)
    
    # Predict: 95% upper
    scaled_up <- ungroupIFR(IFR=verity[,4],interval=verity[,1],
                             midinterval=5,age=scaling)

    # Assign
    IFRs[,paste0("Verity_",i)]    <- scaled
    IFRs[,paste0("VerityLow_",i)] <- scaled_low
    IFRs[,paste0("VerityUp_",i)]  <- scaled_up
    
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
    scaling <- match_e_x(e_1,e_2,interval=interval,
                         maxage=maxage,minage=minage,
                         outputresolution=resolution)
    
    # Predict: Point estimate
    scaled <- ungroupIFR(IFR=salje[,2]/100,interval=salje[,1],
                         midinterval=5,age=scaling)
    
    # Predict: 95% lower
    scaled_low <- ungroupIFR(IFR=salje[,3]/100,interval=salje[,1],
                         midinterval=5,age=scaling)
    
    # Predict: 95% upper
    scaled_up <- ungroupIFR(IFR=salje[,4]/100,interval=salje[,1],
                         midinterval=5,age=scaling)
    
    # Assign
    IFRs[,paste0("Salje_",i)]    <- scaled
    IFRs[,paste0("SaljeLow_",i)] <- scaled_low
    IFRs[,paste0("SaljeUp_",i)]  <- scaled_up
    
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
    scaling <- match_e_x(e_1,e_2,interval=interval,
                         maxage=maxage,minage=minage,
                         outputresolution=resolution)
    
    # Predict
    scaled1 <- ungroupIFR(IFR=modi[,2]/100,interval=modi[,1],
                          midinterval=5,age=scaling)
    
    scaled2 <- ungroupIFR(IFR=modi[,3]/100,interval=modi[,1],
                          midinterval=5,age=scaling)
    
    scaled3 <- ungroupIFR(IFR=modi[,4]/100,interval=modi[,1],
                          midinterval=5,age=scaling)
    
    # Assign
    IFRs[,paste0("Modi1_",i)] <- scaled1
    IFRs[,paste0("Modi2_",i)] <- scaled2
    IFRs[,paste0("Modi3_",i)] <- scaled3
    
  }    
  
  
### Apply scaling for all countries, Acosta/Spain ###################
  
  # Reference data
  e_2 <- UNdat[UNdat$Country=="Spain","ex"]
  interval <- UNdat[UNdat$Country=="Spain","Age"]
  
  # Loop over all countries  
  for(i in countrylist) {
    
    # Get data
    e_1 <- UNdat[UNdat$Country==i,"ex"]
    
    # Get ages
    scaling <- match_e_x(e_1,e_2,interval=interval,
                         maxage=maxage,minage=minage,
                         outputresolution=resolution)
    
    # Predict: Point estimate
    scaled <- ungroupIFR(IFR=IFRs$Acosta,interval=IFRs$Age,
                         midinterval=0.25,age=scaling)
    
    # Predict: 95% lower
    scaled_low <- ungroupIFR(IFR=IFRs$AcostaLow,interval=IFRs$Age,
                             midinterval=0.25,age=scaling)
    
    # Predict: 95% upper
    scaled_up <- ungroupIFR(IFR=IFRs$AcostaUp,interval=IFRs$Age,
                            midinterval=0.25,age=scaling)
    
    # Assign
    IFRs[,paste0("Acosta_",i)]    <- scaled
    IFRs[,paste0("AcostaLow_",i)] <- scaled_low
    IFRs[,paste0("AcostaUp_",i)]  <- scaled_up
    
  }    
  
  
### Apply scaling for all countries, Levin ##########################
  
  # Reference data
  e_2 <- UNdat[UNdat$Country=="Levin","ex"]
  interval <- UNdat[UNdat$Country=="Levin","Age"]
  
  # Loop over all countries
  for(i in countrylist) {
    
    # Get data
    e_1 <- UNdat[UNdat$Country==i,"ex"]
    
    # Get ages
    scaling <- match_e_x(e_1,e_2,interval=interval,
                         maxage=maxage,minage=minage,
                         outputresolution=resolution)
    
    # Predict: Point estimate
    scaled <- ungroupIFR(IFR=IFRs$Levin,interval=IFRs$Age,
                         midinterval=0.25,age=scaling)
    
    # Predict: 95% lower
    scaled_low <- ungroupIFR(IFR=IFRs$LevinLow,interval=IFRs$Age,
                         midinterval=0.25,age=scaling)
    
    # Predict: 95% upper
    scaled_up <- ungroupIFR(IFR=IFRs$LevinUp,interval=IFRs$Age,
                         midinterval=0.25,age=scaling)
    
    # Assign
    IFRs[,paste0("Levin_",i)]    <- scaled
    IFRs[,paste0("LevinLow_",i)] <- scaled_low
    IFRs[,paste0("LevinUp_",i)]  <- scaled_up
    
  }
  
  
### Save ############################################################
  
  write.csv(IFRs,"Output/IFRs.csv",row.names=F)
  
