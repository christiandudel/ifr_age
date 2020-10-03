### Load packages ###################################################

  library(HMDHFDplus)
  library(tidyverse)
  
  #us <- "username"
  #pw <- "password"


### Load data #######################################################

  GerPop <- readHMDweb(CNTRY="DEUTNP","Exposures_1x1",us,pw)#Exposures_1x1 
  USAPop <- readHMDweb(CNTRY="USA","Exposures_1x1",us,pw)

  
### Restrict data ###################################################

  GerPop <- GerPop %>% filter(Year==2017) %>% select(Year,Age,Total)
  USAPop <- USAPop %>% filter(Year==2018) %>% select(Year,Age,Total)

  
### Aggregate/open ended category ###################################

  open <- 110
  maxage <- 93
  if(maxage<open) open <- maxage
  
  GerPop <- GerPop %>% filter(Age<=maxage)
  USAPop <- USAPop %>% filter(Age<=maxage)
  
  GerPop <- GerPop %>% mutate(Age=ifelse(Age>=open,open,Age))
  USAPop <- USAPop %>% mutate(Age=ifelse(Age>=open,open,Age))
  
  GerPop <- aggregate(Total~Year+Age,data=GerPop,sum)
  USAPop <- aggregate(Total~Year+Age,data=USAPop,sum)
  

### Scenarios #######################################################
  
  nopen <- length(65:open)
  scenario1 <- c(rep(0.23,50),rep(0.16,15),rep(0.14,nopen))
  scenario2 <- c(rep(0.20,50),rep(0.20,15),rep(0.20,nopen))
  scenario3 <- c(rep(0.26,50),rep(0.10,15),rep(0.06,nopen))
  


### Calculate Germany ###############################################

  # Scenarios
  GerPop$scenario1 <- scenario1
  GerPop$scenario2 <- scenario2
  GerPop$scenario3 <- scenario3
  
  # ICRs
  GerPop <- GerPop %>% mutate(ICR=  exp(-7.53 + 0.119 * (Age+0.5))/100)
  
  # Cases
  GerPop <- GerPop %>% mutate(Cases1=Total*scenario1) 
  GerPop <- GerPop %>% mutate(Cases2=Total*scenario2) 
  GerPop <- GerPop %>% mutate(Cases3=Total*scenario3) 
  
  # Deaths
  GerPop <- GerPop %>% mutate(Deaths1=Cases1*ICR)
  GerPop <- GerPop %>% mutate(Deaths2=Cases2*ICR)
  GerPop <- GerPop %>% mutate(Deaths3=Cases3*ICR)
  
  # Results
  sum(GerPop$Deaths1)/sum(GerPop$Cases1)
  sum(GerPop$Deaths2)/sum(GerPop$Cases2)
  sum(GerPop$Deaths3)/sum(GerPop$Cases3)

  
### Calculate USA ###################################################
  
  # Scenarios
  USAPop$scenario1 <- scenario1
  USAPop$scenario2 <- scenario2
  USAPop$scenario3 <- scenario3
  
  # ICRs
  USAPop <- USAPop %>% mutate(ICR=  exp(-7.53 + 0.119 * (Age+0.5))/100)
  
  # Cases
  USAPop <- USAPop %>% mutate(Cases1=Total*scenario1) 
  USAPop <- USAPop %>% mutate(Cases2=Total*scenario2) 
  USAPop <- USAPop %>% mutate(Cases3=Total*scenario3) 
  
  # Deaths
  USAPop <- USAPop %>% mutate(Deaths1=Cases1*ICR)
  USAPop <- USAPop %>% mutate(Deaths2=Cases2*ICR)
  USAPop <- USAPop %>% mutate(Deaths3=Cases3*ICR)
  
  # Results
  sum(USAPop$Deaths1)/sum(USAPop$Cases1)
  sum(USAPop$Deaths2)/sum(USAPop$Cases2)
  sum(USAPop$Deaths3)/sum(USAPop$Cases3)
  