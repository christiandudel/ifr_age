### Construct life table for scaling Levin et al. ###################


### Load packages ###################################################

  library(tidyverse)
  library(readxl)
  library(httr)


### Functions #######################################################

  # Ungroup l_x
  ungroup_lx <- function(lx){
    
    # Ages (hard-coded five-year age groups)
    ages <- c(0,1,seq(5,100,5))
    
    # Smoothing spline
  	smooth_lx <- smooth.spline(x=ages,y=lx)
  	
  	# New ages: single years
   	new_age <- c(seq(0,1,1),seq(2,5,1),seq(6,100,1))
   	
   	# Predict from spline
  	predict_lx <- predict(smooth_lx,new_age,len=new_age)
  	
  	# Return
  	return(predict_lx$y)
  	
  }

  
  # Calculate life expectancy at age x
  lexp_age_specific <- function(lx,ax,mx){  
  
    # d_x: Deaths aged x
  	dx <- c(-diff(lx), lx[length(lx)])  
  
  	# L_x: Person years
    Lx1 <- lx[-1]+ax[1:(length(ax)-1)]*dx[1:(length(dx)-1)]  
    Lx2 <- if(mx[length(mx)] == 0) 0 else dx[length(dx)]/mx[length(mx)]  
    Lx <- c(Lx1, Lx2)  
  
    # Cumulative person years  
    Tx <- rev(cumsum(rev(Lx)))  
    
    # Remaining life expectancy at age x
    ex <- Tx/lx  
    
    # If dividion by zero
    ex[is.nan(ex)] <- 0  
    
    # Return result
    return(ex)    
    
  }  
  
  # Function for weighted row means
  rowMeansW <- function(data,weight) {
    
    # Calculate contributions
    tmp <- apply(data,1,function(x) x*weights)
    
    # Sum
    colSums(tmp)
    
  }
  
  
### List of countries studied by Levin et al ########################
  
  # List of countries
  countries_levin <- c("United Kingdom",
                       "Ireland",
                       "Italy",
                       "Netherlands",
                       "Portugal",
                       "Spain",
                       "Switzerland",
                       "United States of America",
                       "Belgium",
                       "France",
                       "Sweden",
                       "Australia",
                       "Iceland",
                       "Republic of Korea",
                       "Lithuania",
                       "New Zealand")
  
  # Number of countries
  ncountries <- length(countries_levin)
  
  
### Load UN data ####################################################
  
  # Load UN abridged life table from web
  web <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/3_Mortality/WPP2019_MORT_F17_1_ABRIDGED_LIFE_TABLE_BOTH_SEXES.xlsx" 
  filename <- "Data/UNabr.xlsx"
  if(!file.exists(filename)) {
    GET(web, write_disk(filename, overwrite = TRUE))
  }
  
  # Read
  UNabr <- read_xlsx(filename, sheet = 1, skip = 16)
  
  # Load population data
  web1 <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/5_Interpolated/WPP2019_INT_F03_2_POPULATION_BY_AGE_ANNUAL_MALE.xlsx"
  web2 <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/5_Interpolated/WPP2019_INT_F03_3_POPULATION_BY_AGE_ANNUAL_FEMALE.xlsx"

  # Download men
  filename <- "Data/UNmen.xlsx"
  if(!file.exists(filename)) {
    GET(web1, write_disk(filename, overwrite = TRUE))
  }
  
  # Read file men
  UNmen <- read_xlsx(filename, sheet = 1, skip = 16)
  
  # Download women
  filename <- "Data/UNwomen.xlsx"
  if(!file.exists(filename)) {
    GET(web2, write_disk(filename, overwrite = TRUE))
  }
  
  # Read file women
  UNwomen <- read_xlsx(filename, sheet = 1, skip = 16)
  
  
### Edit data #######################################################
  
  # Edit life table data
  UNabr <- UNabr %>% 
           # Rename variables
           rename("Country"="Region, subregion, country or area *",
                  "Year"="Period",
                  "Age"="Age (x)",
                  "lx"="Number of survivors l(x)",
                  "ax"="Average number of years lived a(x,n)",
                  "mx"="Central death rate m(x,n)",
                  "ex"="Expectation of life e(x)") %>% 
           # Restrict period
           filter(Year=="2015-2020") %>% 
           # Only countries of interest
           filter(Country%in%countries_levin) %>% 
           # Change format to numeric
           mutate(lx=as.numeric(lx),
                  ax=as.numeric(ax),
                  mx=as.numeric(mx),
                  ex=as.numeric(ex)) %>% 
           # Only keep variables of interest
           select(Country,Age,lx,ax,mx,ex)
  
  # Data for men
  UNmen <- UNmen %>% 
           # Rename variables
           rename("Year"="Reference date (as of 1 July)",
                  "Country"="Region, subregion, country or area *") %>% 
           # Most recent year
           filter(Year==2019) %>%
           # Only countries of interest
           filter(Country%in%countries_levin) %>%  
           # Drop unneeded variables
           select("Country",paste(0:100)) %>% 
           # Change format
           mutate_at(paste(0:100),as.numeric)
          
  # Data for women 
  UNwomen <- UNwomen %>% 
             # Rename variables
             rename("Year"="Reference date (as of 1 July)",
                    "Country"="Region, subregion, country or area *") %>% 
             # Most recent year
             filter(Year==2019) %>% 
             # Only countries of interest
             filter(Country%in%countries_levin) %>% 
             # Drop unneeded variables
             select("Country",paste(0:100))%>% 
             # Change format
             mutate_at(paste(0:100),as.numeric)
  
  # Combine men and women
  UNpop <- UNwomen
  UNpop[,paste(0:100)] <- UNwomen[,paste(0:100)] + UNmen[,paste(0:100)]
  
  
### Combine abridged life tables ####################################
  
  # Reshape life table data
  UNresh <- UNabr %>%  pivot_wider(names_from=Country,
                         id_cols=Age,
                         values_from=c(lx,ax,mx,ex))
  
  # Mean and ungroup of l_x
  lx_combined <- UNresh %>% 
                 # Get right columns
                 select(paste("lx",countries_levin,sep="_")) %>% 
                 # Row means
                 rowMeans %>%
                 # Ungroup
                 ungroup_lx
  
  # Mean and ungroup of m_x
  mx_combined <- UNresh %>% 
                 # Get right columns
                 select(paste("mx",countries_levin,sep="_")) %>% 
                 # Row means
                 rowMeans %>%
                 # Ungroup
                 ungroup_lx
  
  # Get a_x for age 0
  ax1 <- UNresh %>% 
         # Get right columns
         select(paste("ax",countries_levin,sep="_")) %>% 
         # Row means
         rowMeans 
  
  # Get a_x all 
  ax_combined <- c(ax1[1],rep(0.5,length(seq(2,101,1))))
  
  # Get e_x
  ex_combined <- lexp_age_specific(lx=lx_combined,
                                   mx=mx_combined,
                                   ax=ax_combined)
  
  # Names
  names(ex_combined) <- paste(0:100)
  
  
### Population weights ##############################################
  
  # Get population
  weights <- rowSums(UNpop[,paste(0:100)])
  
  # Rescale
  weights <- weights/sum(weights)
  
  # Match countries
  resort <- match(countries_levin,UNpop$Country)
  weights <- weights[resort]

    
### Combine abridged life tables with weights #######################
  
  # Mean and ungroup of l_x
  lx_weighted <- UNresh %>% 
    # Get right columns
    select(paste("lx",countries_levin,sep="_")) %>% 
    # Row means
    rowMeansW(weight=weights) %>%
    # Ungroup
    ungroup_lx
  
  # Mean and ungroup of m_x
  mx_weighted <- UNresh %>% 
    # Get right columns
    select(paste("mx",countries_levin,sep="_")) %>% 
    # Row means
    rowMeansW(weight=weights) %>%
    # Ungroup
    ungroup_lx
  
  # Get a_x for age 0
  ax1 <- UNresh %>% 
    # Get right columns
    select(paste("ax",countries_levin,sep="_")) %>% 
    # Row means
    rowMeansW(weight=weights) 
  
  # Get a_x all 
  ax_weighted <- c(ax1[1],rep(0.5,length(seq(2,101,1))))
  
  # Get e_x
  ex_weighted <- lexp_age_specific(lx=lx_weighted,
                                   mx=mx_weighted,
                                   ax=ax_weighted)
  
  # Names
  names(ex_weighted) <- paste(0:100)
  

### Save ############################################################
  
  # Data frame
  results <- data.frame(Age=0:100,
                        ex_combined=ex_combined,
                        ex_weighted=ex_weighted)
  
  # Write as csv
  write_csv(results,path="Output/ex_levin.csv")

  
### Quick plot ######################################################
  
  # Weighted result
  plot(x=0:100,y=ex_weighted,type="l",lwd=2,ylab="",xlab="")
  # Unweighted
  lines(x=0:100,y=ex_combined,col="lightblue",lwd=2)
  # Raw data
  points(x=UNabr$Age,y=UNabr$ex,col="red",lwd=2)