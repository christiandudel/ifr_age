library(zoo)
library(tidyverse)
library(readr)
library(lubridate)
library(cowplot)
# Load data 
Dat <- read_csv(filename,skip=3)

# Countries of interest
countrylist <- c("Germany","Belgium")
region <- c("All")

# Restrict country
dat <- Dat %>% 
  filter(Country %in% countrylist,
         Region %in% region,
         Sex == "b") %>% 
  select(Country,Date,Sex,Age,Cases,Deaths) %>% 
  mutate(Date = dmy(Date)) %>% 
  filter(weekdays(Date) == "Sunday") %>% 
  arrange(Country, Age, Date) %>% 
  group_by(Country, Age) %>% 
  mutate(NewCases = diff(c(0,Cases)),
         NewDeaths = diff(c(0,Deaths))) %>% 
  ungroup() %>% 
  arrange(Country, Date, Age) %>% 
  group_by(Country, Date) %>% 
  mutate(ICR = exp(-7.53 + 0.119 * (Age+2.5))/100,
         weight = NewCases / sum(NewCases),
         res = ICR * weight) 

results <-
  dat %>% 
  summarize(res=sum(res))
  
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
plot_grid(p1, p2, labels = c('A', 'B'))
# interestingly the deaths peak before the IFR peaks.

# dat %>% 
#   summarize(NewDeaths = sum(NewDeaths)) %>% 
#   View()
