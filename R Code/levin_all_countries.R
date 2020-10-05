library(tidyverse)
library(readxl)

# Importing population data for all countries from the 
# UN World Population Prospects - Population Division, 
# source: https://population.un.org/wpp/

db <- read_xlsx("Data/WPP2019_POP_F07_1_POPULATION_BY_AGE_BOTH_SEXES.xlsx",
                sheet = 1,
                skip = 16)


# defining maximum age, age interval, and open age interval 
open1 <- 85 # To reproduce Levin et al. set open1 > 80
maxage <- 100 # To reproduce Levin et al set maxage to 80
interval <- 0.5 # Set to zero to reproduce Levin et al.
openinterval <- 7

# Selecting only countries and population for year 2020
db2 <- db %>% 
  rename(Country = 3,
         year = 8) %>% 
  filter(Type == "Country/Area",
         year == 2020) %>% 
  select(Country, 9:29) %>% 
  gather(-Country, key = "Age", value = "pop") %>% 
  separate(Age, c("Age", "trash"), sep = "-") %>% 
  mutate(Age = ifelse(Age == "100+", "100", Age),
         Age = ifelse(as.numeric(Age) < open1, as.numeric(Age), open1),
         pop = as.numeric(pop) * 1000) %>% 
  arrange(Country, Age) %>% 
  select(-trash) %>% 
  group_by(Country, Age) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup()

# Estimating age-specific infection fatality rate (IFR) and infection rates, 
# according to the IFR formula and the three scenarios proposed by Levin et al. (2020),
db3 <- db2 %>% 
  mutate(interval = ifelse(Age < open1, interval, openinterval),
         ifr = exp(-7.53 + 0.119 * (Age + interval)) / 100,
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
         s3_deaths = s3_infec * ifr)
  
# Calculating the overall IFR by country for each scenario 
db4 <- db3 %>% 
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

unique(db4$Country)

# selecting countries to plot
countries <- c("Brazil", 
               "Mexico", 
               "United States of America", 
               "Italy", 
               "Germany", 
               "China", 
               "India", 
               "Nigeria",
               "Russia",
               "Iran",
               "Colombia",
               "Morocco",
               "Spain",
               "France",
               "United Kingdom",
               "Japan",
               "Ethiopia",
               "South Africa")

# Plotting the overall IFR each scenario for the selected countries 
db4 %>% 
  filter(Measure == "ifr",
         Country %in% countries) %>% 
  ggplot()+
  geom_point(aes(Value, reorder(Country, Value), col = Scenario), size = 0.7)+
  scale_x_continuous(breaks = seq(-2, 3, 0.5))+
  scale_color_manual(values = c('#e41a1c', '#377eb8', '#4daf4a'))+
  labs(x = "Overall Infection Fatality Rate", y = "Country")+
  theme_bw()+
  theme(legend.key.size = unit(0.3, "cm"),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8), 
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8))

ggsave("Figures/ifr_country.png", width = 5, height = 3, dpi = 600)

  
  
  
