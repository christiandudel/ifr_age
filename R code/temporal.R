### Trends over time ################################################

### General #########################################################

  # Packages
  library(tidyverse)
  library(httr)

  # For handling of age
  midinterval <- 2.5
  lastinterval <- 2.5

  # Countries to look at
  countrylist <- c("Germany","Spain","Italy","France","Sweden",
                   "China","Japan","Colombia","Brazil","USA")
  

### Get data for cumulative cases ###################################

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


### Cumulative cases + IFRs #########################################

  # Aggregate
  Counts <- aggregate(Cases~Country+Age+Date,data=Dat,sum)
  
  # Handling of age
  Counts <- Counts %>% mutate(Age=
                                ifelse(Age==max(Age),
                                       Age+lastinterval,
                                       Age+midinterval))
  
  # Load IFRs
  IFRs <- read_csv("Output/IFRs.csv")
  
  # Only keep corresponding IFRs
  IFRs <- IFRs %>% filter(Age%in%Counts$Age)
  
  # Select a specific IFR and merge
  IFRs <- IFRs %>% select(Age,Verity)
  
  # Rename
  names(IFRs)[2] <- "IFRx"
  
  # Merge
  Counts <- inner_join(Counts,IFRs)
  

### Calculate #######################################################
  
  # Age-specific deaths
  Counts <- Counts %>% mutate(Deaths=Cases*IFRx)
  
  # Calculate IFR
  results <- Counts %>% 
             group_by(Country,Date) %>% 
             summarize(IFR=sum(Deaths)/sum(Cases))

  
### Quick plot ######################################################
  
  # Plot
  countrylist2 <- c("Germany","Italy","Colombia","USA","Sweden")
  results %>% 
    filter(Date>"2020-03-15") %>% 
    filter(Country%in%countrylist2) %>% 
    ggplot(aes(x=Date,y=IFR,col=Country))+
              geom_line()
  