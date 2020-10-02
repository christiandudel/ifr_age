library(zoo)
library(tidyverse)
library(readr)
library(lubridate)
# Load data 
Dat <- read_csv(filename,skip=3)

# Countries of interest
countrylist <- c("Germany","Belgium")
region <- c("All")

# Restrict country
results <- Dat %>% 
  filter(Country %in% countrylist,
         Region %in% region,
         Sex == "b") %>% 
  select(Country,Date,Sex,Age,Cases) %>% 
  mutate(Date = dmy(Date)) %>% 
  filter(weekdays(Date) == "Sunday") %>% 
  arrange(Country, Age, Date) %>% 
  group_by(Country, Age) %>% 
  mutate(NewCases = diff(c(0,Cases))) %>% 
  ungroup() %>% 
  arrange(Country, Date, Age) %>% 
  group_by(Country, Date) %>% 
  mutate(ICR = exp(-7.53 + 0.119 * (Age+2.5))/100,
         weight=NewCases/sum(NewCases),
         res=ICR*weight) %>% 
  summarize(res=sum(res))
  
results %>% ggplot(aes(x=Date,y=res,color=Country))+
  geom_line()
