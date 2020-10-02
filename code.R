### Load packages ###################################################

library(tidyverse)
library(httr)


### Load data #######################################################

# Source
url <- 'https://osf.io/7tnfh/download'

# Where to save
filename <- 'Data/Output_5.zip'

# Download data
GET(url, write_disk(filename, overwrite = TRUE))

# Unzip 
unzip(filename)

# Load data 
dat <- read_csv(filename,skip=3)


### Edit/restrict data ##############################################

# Countries of interest
countrylist <- c("Germany","USA")
region <- c("All")

# Restrict country
dat <- dat %>% filter(Country %in% countrylist & Region %in% region)

# Restrict gender
dat <- dat %>% filter(Sex=="b")

# Only keep interesting variables
dat <- dat %>% select(Country,Date,Sex,Age,Cases)


### Calculate #######################################################

# IFR by age (mid-interval)
# Source: https://www.medrxiv.org/content/10.1101/2020.07.23.20160895v5.full.pdf)
dat <- dat %>% mutate(ICR=  exp(-7.53 + 0.119 * (Age+2.5))/100)

# Proportions
dat <- dat %>% group_by(Country,Date) %>% mutate(weight=Cases/sum(Cases)) 

# IFR + proportions
dat <- dat %>% group_by(Country,Date) %>% mutate(res=ICR*weight) 

# Results
results <- dat %>% group_by(Country,Date) %>% summarise(res = sum(res))


### Plot ############################################################

# Use dates
results$Date <- as.Date(results$Date,"%d.%m.%Y")

results %>% ggplot(aes(x=Date,y=res,color=Country))+
  geom_line()
