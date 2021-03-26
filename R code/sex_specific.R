### Sex-specific IFRs based on Spain ################################


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

  # Data frame for results
  IFRs <- data.frame(Age=seq(minage,maxage,by=resolution))


  
### Sex-specific IFRs for Spain ###################################################

  # Load
  file <- "https://raw.githubusercontent.com/kikeacosta/ifr_age_spain/master/Output/spain_sex_age_ifr.csv"
  acosta <- read_csv(file)
  
  # Reshape
  acosta <- pivot_wider(acosta,id_cols=c(Sex,Age),
                        names_from=Estimate,
                        values_from=IFR)
  
  # Split sexes
  acosta_f <- acosta %>% filter(Sex=="f")
  acosta_m <- acosta %>% filter(Sex=="m")
  
  # Ungroup: Point estimate women
  Acosta_f <- ungroupIFR(IFR=acosta_f$Central,interval=acosta_f$Age,
                       midinterval=c(rep(2.5,18),5),
                       minage=minage,maxage=maxage,
                       resolution=resolution)
  
  # Ungroup: 95% low women
  AcostaLow_f <- ungroupIFR(IFR=acosta_f$Lower,interval=acosta_f$Age,
                          midinterval=c(rep(2.5,18),5),
                          minage=minage,maxage=maxage,
                          resolution=resolution)
  
  # Ungroup: 95% upper women
  AcostaUp_f <- ungroupIFR(IFR=acosta_f$Upper,interval=acosta_f$Age,
                         midinterval=c(rep(2.5,18),5),
                         minage=minage,maxage=maxage,
                         resolution=resolution)
  
  # Ungroup: Point estimate men
  Acosta_m <- ungroupIFR(IFR=acosta_m$Central,interval=acosta_m$Age,
                         midinterval=c(rep(2.5,18),5),
                         minage=minage,maxage=maxage,
                         resolution=resolution)
  
  # Ungroup: 95% low men
  AcostaLow_m <- ungroupIFR(IFR=acosta_m$Lower,interval=acosta_m$Age,
                            midinterval=c(rep(2.5,18),5),
                            minage=minage,maxage=maxage,
                            resolution=resolution)
  
  # Ungroup: 95% upper men
  AcostaUp_m <- ungroupIFR(IFR=acosta_m$Upper,interval=acosta_m$Age,
                           midinterval=c(rep(2.5,18),5),
                           minage=minage,maxage=maxage,
                           resolution=resolution)

  # Put into data frame
  IFRs$Acosta_f    <- Acosta_f
  IFRs$AcostaLow_f <- AcostaLow_f
  IFRs$AcostaUp_f  <- AcostaUp_f

  IFRs$Acosta_m    <- Acosta_m
  IFRs$AcostaLow_m <- AcostaLow_m
  IFRs$AcostaUp_m  <- AcostaUp_m

  
  
### Preparations for scaling ########################################

  # Load data from web
  web_f <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/3_Mortality/WPP2019_MORT_F17_3_ABRIDGED_LIFE_TABLE_FEMALE.xlsx" 
  web_m <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/3_Mortality/WPP2019_MORT_F17_2_ABRIDGED_LIFE_TABLE_MALE.xlsx" 
  
  filename_f <- "Data/UNe0_f.xlsx"
  filename_m <- "Data/UNe0_m.xlsx"
  
  if(!file.exists(filename_f)) {
    GET(web_f, write_disk(filename_f, overwrite = TRUE))
  }
  
  if(!file.exists(filename_m)) {
    GET(web_m, write_disk(filename_m, overwrite = TRUE))
  }

  # Load spreadsheets
  UNdat_f <- read_excel(path=filename_f,sheet="ESTIMATES",
                      range="A17:T77381")
  
  UNdat_m <- read_excel(path=filename_m,sheet="ESTIMATES",
                        range="A17:T77381")

  # Rename
  UNdat_f <- UNdat_f %>% rename("Country"="Region, subregion, country or area *",
                            "ex"="Expectation of life e(x)",
                            "Age" = "Age (x)")
  
  UNdat_m <- UNdat_m %>% rename("Country"="Region, subregion, country or area *",
                                "ex"="Expectation of life e(x)",
                                "Age" = "Age (x)")

  # Recode country
  UNdat_f$Country <- recode(UNdat_f$Country, 
                            "Republic of Moldova" = "Moldova",
                          "United States of America" = "USA",
                          "Bolivia (Plurinational State of)" = "Bolivia",
                          "Republic of Korea" = "South Korea",
                          "Venezuela (Bolivarian Republic of)"= "Venezuela",
                          "China, Taiwan Province of China" = "Taiwan",
                          "State of Palestine" = "Palestine")
  
  UNdat_m$Country <- recode(UNdat_m$Country, 
                            "Republic of Moldova" = "Moldova",
                            "United States of America" = "USA",
                            "Bolivia (Plurinational State of)" = "Bolivia",
                            "Republic of Korea" = "South Korea",
                            "Venezuela (Bolivarian Republic of)"= "Venezuela",
                            "China, Taiwan Province of China" = "Taiwan",
                            "State of Palestine" = "Palestine")

  # Remove countries which are not in UN data
  countrylist <- countrylist[countrylist%in%unique(UNdat_f$Country)]
  countrylist <- countrylist[countrylist%in%unique(UNdat_m$Country)]
  
  # Edit (gives an expected [?] error message)
  UNdat_f <- UNdat_f %>% mutate(ex=as.numeric(ex))
  UNdat_m <- UNdat_m %>% mutate(ex=as.numeric(ex))
  
  # Subset (always Spain)
  countrylist2 <- c(countrylist,"Spain")
  
  UNdat_f <- UNdat_f %>% filter(Period=="2015-2020") %>% 
    filter(Country%in%countrylist2) %>% 
    select(Country,Age,ex)
  
  UNdat_m <- UNdat_m %>% filter(Period=="2015-2020") %>% 
    filter(Country%in%countrylist2) %>% 
    select(Country,Age,ex)

  # Change to data.frame
  UNdat_f <- as.data.frame(UNdat_f)
  UNdat_m <- as.data.frame(UNdat_m)



### Apply scaling for all countries #################################

  ### Women
  
  # Reference data
  e_2 <- UNdat_f[UNdat_f$Country=="Spain","ex"]
  interval <- UNdat_f[UNdat_f$Country=="Spain","Age"]
  
  # Loop over all countries  
  for(i in countrylist) {
    
    # Get data
    e_1 <- UNdat_f[UNdat_f$Country==i,"ex"]
    
    # Get ages
    scaling <- match_e_x(e_1,e_2,interval=interval,
                         maxage=maxage,minage=minage,
                         outputresolution=resolution)
    
    # Predict: Point estimate
    scaled <- ungroupIFR(IFR=IFRs$Acosta_f,interval=IFRs$Age,
                         midinterval=0.25,age=scaling)
    
    # Predict: 95% lower
    scaled_low <- ungroupIFR(IFR=IFRs$AcostaLow_f,interval=IFRs$Age,
                             midinterval=0.25,age=scaling)
    
    # Predict: 95% upper
    scaled_up <- ungroupIFR(IFR=IFRs$AcostaUp_f,interval=IFRs$Age,
                            midinterval=0.25,age=scaling)
    
    # Assign
    IFRs[,paste0("Acosta_",i,"_f")]    <- scaled
    IFRs[,paste0("AcostaLow_",i,"_f")] <- scaled_low
    IFRs[,paste0("AcostaUp_",i,"_f")]  <- scaled_up
    
  }    

  ### Men
  
  # Reference data
  e_2 <- UNdat_m[UNdat_m$Country=="Spain","ex"]
  interval <- UNdat_m[UNdat_m$Country=="Spain","Age"]
  
  # Loop over all countries  
  for(i in countrylist) {
    
    # Get data
    e_1 <- UNdat_m[UNdat_m$Country==i,"ex"]
    
    # Get ages
    scaling <- match_e_x(e_1,e_2,interval=interval,
                         maxage=maxage,minage=minage,
                         outputresolution=resolution)
    
    # Predict: Point estimate
    scaled <- ungroupIFR(IFR=IFRs$Acosta_m,interval=IFRs$Age,
                         midinterval=0.25,age=scaling)
    
    # Predict: 95% lower
    scaled_low <- ungroupIFR(IFR=IFRs$AcostaLow_m,interval=IFRs$Age,
                             midinterval=0.25,age=scaling)
    
    # Predict: 95% upper
    scaled_up <- ungroupIFR(IFR=IFRs$AcostaUp_m,interval=IFRs$Age,
                            midinterval=0.25,age=scaling)
    
    # Assign
    IFRs[,paste0("Acosta_",i,"_m")]    <- scaled
    IFRs[,paste0("AcostaLow_",i,"_m")] <- scaled_low
    IFRs[,paste0("AcostaUp_",i,"_m")]  <- scaled_up
    
  }    

  ### Replace any value larger than 1 with 1, 0
  tmp <- IFRs[,-1]
  tmp[tmp<0] <- 0
  tmp[tmp>1] <- 1
  IFRs[,-1] <- tmp
  
  ### Save
  write.csv(IFRs,"Output/IFRs_EA_by_sex.csv",row.names=F)
  
  
  
### Get counts ######################################################

  # Empty data frame
  Counts <- data.frame(Age=seq(0,maxage,by=5))
  
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
  
  # Restrict country
  region <- c("All")
  Dat <- Dat %>% filter(Country %in% countrylist & Region %in% region)
  
  # Restrict gender
  Dat <- Dat %>% filter(Sex!="b")
  
  # Set date
  Dat$Date <- as.Date(Dat$Date,"%d.%m.%Y")
  
  # Restrict to dates with at least one type of information
  Dat$Any <- !(is.na(Dat$Cases)&is.na(Dat$Deaths))
  Dat <- Dat %>% filter(Any==T) 
  
  ### Death counts 
  
  # Restrict to most recent date
  dat <- Dat %>% group_by(Country) %>% filter(Date==max(Date))
  
  # Restrict age
  dat <- dat %>% mutate(Age=ifelse(Age>maxage,max(Counts$Age),Age))
  
  # Aggregate
  deaths <- aggregate(Deaths~Country+Age+Sex,data=dat,sum)
  
  # Reshape
  deaths <- deaths %>% spread(Country,Deaths)
  
  # Rename
  names(deaths)[2:dim(deaths)[2]] <- paste(names(deaths)[2:dim(deaths)[2]],
                                           "Deaths",
                                           sep="_")
  
  # Rename sex variable
  deaths <- deaths %>% rename("Sex"="Sex_Deaths")
  
  # Merge
  Counts <- inner_join(Counts,deaths)

  ### Cumulative cases 
  
  # Aggregate
  cases <- aggregate(Cases~Country+Age+Sex,data=dat,sum)
  
  # Reshape
  cases <- cases %>% spread(Country,Cases)
  
  # Rename
  names(cases)[2:dim(cases)[2]] <- paste(names(cases)[2:dim(cases)[2]],
                                         "Cases",
                                         sep="_")
  
  # Rename sex variable
  cases <- cases %>% rename("Sex"="Sex_Cases")
  
  # Merge
  Counts <- inner_join(Counts,cases)
  
  # Reshape
  Counts <- Counts %>% pivot_wider(id_cols=Age,
                                   names_from=Sex,
                                   values_from=names(Counts)[-c(1,2)])
  

  
### Age #############################################################
  
  # Add midinterval information
  Counts <- Counts %>% mutate(Age=
                                ifelse(Age==max(Age),
                                       Age+2.5,
                                       Age+2.5))
  
  # Only keep corresponding IFRs
  IFRs <- IFRs %>% filter(Age%in%Counts$Age)
  
  
  
### Calculate IFRs ##################################################
  
   # Data frame for results
  Scenarios <- data.frame(IFR=character(),
                          Cases=character(),
                          Result=character())
  
  # Get scenarios
  IFR_scenarios <- names(IFRs)[-1]
  Count_scenarios <- names(Counts)[-1]
  
  # Loop over scenarios for cases
  for(i in Count_scenarios) {
    
    # Death counts? If so apply inverse method
    tmp     <- strsplit(i,split="_")
    country <- tmp[[1]][1]
    death   <- tmp[[1]][2]
    sex     <- tmp[[1]][3]
    death   <- death=="Deaths"
    
    # Get country and corresponding IFR scenarios
    scenarios <- strsplit(IFR_scenarios,split="_")
    scenarios <- lapply(scenarios, function(x) any(x==country) & any(x==sex) )
    scenarios <- unlist(scenarios)
    scenarios <- IFR_scenarios[scenarios]
    n_IFRs <- length(scenarios)

    # If death counts...
    if(death) {
      
      # Inverse method
      result <- apply(IFRs[,scenarios],2,
                      function(x) {
                        sum(Counts[,i])/sum(1/x*Counts[,i])
                      })
      
      # If no death counts...
    } else {
      
      # Normal calculation
      result <- apply(IFRs[,scenarios],2,
                      function(x) {
                        sum(x*Counts[,i]/sum(Counts[,i]))
                      })
      
    }
    
    # Put into data frame
    result <- data.frame(IFRs=names(result),
                         Cases=rep(i,n_IFRs),
                         Result=result)
    
    # Combine with other results
    Scenarios <- rbind(Scenarios,result)
    
  }
  
  # Edit row names
  rownames(Scenarios) <- paste(1:dim(Scenarios)[1])
  
  # Country, case, gender variable
  Scenarios$Gender <- unlist(lapply(strsplit(Scenarios$Cases,"_"),function(x) x[[3]]))
  Scenarios$Country <- unlist(lapply(strsplit(Scenarios$Cases,"_"),function(x) x[[1]]))
  Scenarios$Cases <- unlist(lapply(strsplit(Scenarios$Cases,"_"),function(x) x[[2]]))
  
  # Low or high or central
  Scenarios$Type <- unlist(lapply(strsplit(Scenarios$IFRs,"_"),function(x) x[[1]]))
  Scenarios$Type <- recode(Scenarios$Type,
                           "Acosta"="Central",
                           "AcostaLow"="Low",
                           "AcostaUp"="Up")
  
  # Subset
  Scenarios <- Scenarios %>% select(Country,Gender,Cases,Type,Result)
  
  # Save
  write.csv(Scenarios,"Output/Scenarios_mf.csv",row.names = F)
  
  
  
### Quick check #####################################################
  
  # Country list
  countries <- c("Afghanistan","South Korea","Japan",
                 "Germany","Spain","Norway","Italy",
                 "USA",
                 "Brazil","Colombia",
                 "Nigeria","Gambia")
  
  # Data
  Check <- Scenarios %>% filter(Type=="Central" & 
                                Cases=="Deaths" &
                                Country%in%countries)
  
  # Reshape
  Check <- pivot_wider(Check,
                       id_cols=c(Country,Cases,Type),
                       names_from="Gender",
                       values_from="Result")
  
  # Results
  Check <- Check %>% mutate(Ratio=m/f*100-100,Diff=(m-f)*100)
  
  # Quick plot
  dotchart(Check$Diff,labels=Check$Country,xlab="Abs. diff. in percentage points",
           panel.first=abline(v=0,col="grey50",lty=2))
