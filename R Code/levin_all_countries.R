library(tidyverse)
library(readxl)

setwd("C:/Users/acosta/Desktop")

db <- read_xlsx("WPP2019_POP_F07_1_POPULATION_BY_AGE_BOTH_SEXES.xlsx",
                sheet = 1,
                skip = 16)

db2 <- db %>% 
  rename(Country = 3,
         year = 8) %>% 
  filter(Type == "Country/Area",
         year == 2020) %>% 
  select(Country, 9:29) %>% 
  gather(-Country, key = "Age", value = "pop") %>% 
  separate(Age, c("Age", "trash"), sep = "-") %>% 
  mutate(Age = ifelse(Age == "100+", "100", Age),
         Age = as.numeric(Age),
         pop = as.numeric(pop) * 1000) %>% 
  arrange(Country, Age) %>% 
  select(-trash)

db3 <- db2 %>% 
  mutate(ifr = exp(-7.53 + 0.119 * (Age + 2.5)) / 100,
         s1_ir = case_when(Age < 50 ~ 0.23,
                           Age >= 50 &   Age < 65 ~ 0.16,
                           Age >= 65 ~ 0.14),
         s2_ir = 0.2,
         s3_ir = case_when(Age < 50 ~ 0.26,
                           Age >= 50 &   Age < 65 ~ 0.1,
                           Age >= 65 ~ 0.06),
         s1_infec = pop * s1_ir,
         s2_infec = pop * s2_ir,
         s3_infec = pop * s3_ir,
         s1_deaths = s1_infec * ifr,
         s2_deaths = s2_infec * ifr,
         s3_deaths = s3_infec * ifr) %>% 
  group_by(Country) %>% 
  summarise(s1_infec = sum(s1_infec),
            s1_deaths = sum(s1_deaths),
            s2_infec = sum(s2_infec),
            s2_deaths = sum(s2_deaths),
            s3_infec = sum(s3_infec),
            s3_deaths = sum(s3_deaths),
            s1_ifr = 100 * s1_deaths / s1_infec,
            s2_ifr = 100 * s2_deaths / s2_infec,
            s3_ifr = 100 * s3_deaths / s3_infec) %>% 
  ungroup()%>% 
  gather(-Country, key = 'Measure', value = "Value") %>% 
  separate(Measure, c("Scenario", "Measure"), sep = "_")
        
db3 %>% 
  filter(Measure == "ifr")
  ggplot()+
  geom_point(aes(Measure, Country, col = Scenario))