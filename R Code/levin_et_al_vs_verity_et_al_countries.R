library(tidyverse)
library(readxl)
Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")

# selecting countries to plot
countries <- c("Brazil", 
               "Mexico", 
               "USA", 
               "Italy", 
               "Germany", 
               "China", 
               "India", 
               "Nigeria",
               "Iran",
               "Colombia",
               "Morocco",
               "Spain",
               "France",
               "United Kingdom",
               "Japan",
               "Ethiopia",
               "South Africa")


# Importing population data for all countries from the 
# UN World Population Prospects - Population Division, 
# source: https://population.un.org/wpp/

db <- read_xlsx("Data/WPP2019_POP_F07_1_POPULATION_BY_AGE_BOTH_SEXES.xlsx",
                sheet = 1,
                skip = 16)


# defining maximum age, age interval, and open age interval 
open1 <- 85 # To reproduce Levin et al. set open1 > 80
maxage <- 100 # To reproduce Levin et al set maxage to 80
interval <- 2.5 # Set to zero to reproduce Levin et al.
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
         pop = as.numeric(pop) * 1000,
         Country = ifelse(Country == "United States of America", "USA", Country)) %>% 
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

ggsave("Figures/levin_ifr_country.png", width = 5, height = 3, dpi = 600)


################################################################
# Verity IFRs and scalated IFR, using Bohk-Ewald et al.'s method
################################################################

verity <- read_rds("Data/verity_ifrs_mode.rds") %>% 
  rownames_to_column("age") %>% as_tibble() %>% 
  mutate(age = as.integer(age))

ver2 <- verity %>% 
  gather(-age, key = "Country", value = "ver_ifr")

ver3 <- ver2 %>% 
  mutate(Age = age - age %% 5) %>% 
  group_by(Country, Age) %>% 
  summarise(ifr_ver = mean(ver_ifr)) %>% 
  select(Country, Age, ifr_ver) %>% 
  ungroup()

unique(ver3$Country)

ver_crude <- ver3 %>% 
  filter(Country == "Verity") %>% 
  select(-Country)

ver_adjusted <- ver3 %>% 
  mutate(Country = case_when(Country == "US" ~ "USA", 
                             Country == "Verity" ~ "China",
                             TRUE ~ Country)) %>% 
  rename(ifr_ver_adj = ifr_ver)

ver_ests <- db3 %>% 
  filter(Country %in% countries) %>% 
  select(Country, Age, s1_infec, s2_infec, s3_infec) %>% 
  left_join(ver_crude) %>% 
  left_join(ver_adjusted) %>% 
  mutate(s1_deaths = s1_infec * ifr_ver,
         s2_deaths = s2_infec * ifr_ver,
         s3_deaths = s3_infec * ifr_ver,
         s1_adj_deaths = s1_infec * ifr_ver_adj,
         s2_adj_deaths = s2_infec * ifr_ver_adj,
         s3_adj_deaths = s3_infec * ifr_ver_adj)
  
ver4 <- ver_ests %>% 
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
  separate(Measure, c("Scenario", "Measure"), sep = "_") %>% 
  mutate(Source = "Verity et al.")

ver5 <- ver_ests %>% 
  group_by(Country) %>% 
  summarise(s1_infec = sum(s1_infec),
            s1_deaths = sum(s1_adj_deaths),
            s2_infec = sum(s2_infec),
            s2_deaths = sum(s2_adj_deaths),
            s3_infec = sum(s3_infec),
            s3_deaths = sum(s3_adj_deaths),
            s1_ifr = 100 * s1_deaths / s1_infec,
            s2_ifr = 100 * s2_deaths / s2_infec,
            s3_ifr = 100 * s3_deaths / s3_infec) %>% 
  ungroup()%>% 
  gather(-Country, key = 'Measure', value = "Value") %>% 
  separate(Measure, c("Scenario", "Measure"), sep = "_") %>% 
  mutate(Source = "Verity et al. (scaled)")

all_ifrs <- db4 %>% 
  filter(Country %in% countries) %>% 
  mutate(Source = "Levin et al.") %>% 
  bind_rows(ver4, ver5)

all_ifrs %>% 
  filter(Measure == "ifr",
         Scenario == "s2") %>% 
  ggplot()+
  geom_point(aes(Value, reorder(Country, Value), col = Source), size = 0.7, alpha = 0.7)+
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

ggsave("Figures/ifr_country_levin_verity.png", width = 5, height = 3, dpi = 600)

all_ifrs %>% 
  filter(Measure == "ifr",
         Scenario == "s2") %>%
  mutate(Value = ifelse(Source == "Levin et al.", Value, 10 * Value)) %>% 
  ggplot()+
  geom_point(aes(Value, reorder(Country, Value), col = Source), size = 0.7, alpha = 0.7)+
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

MeanAge <- 
db2 %>% 
  group_by(Country) %>% 
  summarize(MeanAge = sum(Age * pop) / sum(pop))

library(ggrepel)
ScatterData <-
all_ifrs %>% 
  filter(Measure == "ifr",
         Scenario == "s2",
         Country != "Morocco") %>%
  mutate(Value = ifelse(Source == "Levin et al.", Value, 10 * Value)) %>% 
  left_join(MeanAge)  

labelData <-
  ScatterData %>% 
 filter(Source == "Levin et al.")  %>% 
mutate(x = 0)
  
  
  
ScatterData %>% 
  ggplot(aes(x = Value, y = MeanAge, color = Source)) + 
  geom_point() +
  ylab("Mean age of population") + 
  xlab("IFR (percent)") + 
  geom_text(filter(labelData),
             mapping = aes(x = x, y = MeanAge, label = Country),
            color = "black",
            hjust = "right") + 
  theme_bw()
    
# um, I couild google forever to get this just right, or just do it in base...
# C65961
# 5D8A00
# 0094A8

ScatterData <-
ScatterData %>% 
  mutate(color = case_when(
    Source == "Levin et al." ~ "#C65961",
    Source == "Verity et al." ~ "#5D8A00",
    Source == "Verity et al. (scaled)" ~ "#0094A8"
  ))


LineData <- ScatterData %>% 
  group_by(Country) %>% 
  filter(Value == max(Value)) %>% 
  ungroup()
LineData <- 
  LineData %>% 
  mutate(labely = ifelse(Country == "Germany", MeanAge + .2, 
                         ifelse(Country == "Spain", MeanAge - .2,MeanAge)))

LineData<- LineData %>% filter(!duplicated(LineData$Country))

LegendData <-
  ScatterData %>% 
  filter(Country == "USA")

png("Figures/IFRMeanAgeScatter.png",600,500)
par(mai=c(.8,1.2,.2,1))
plot(NULL, type = 'n', xlim = c(0,17), ylim = c(20,45),
     axes = FALSE, xlab = "", ylab = "")
segments(rep(0,nrow(LineData)),LineData$MeanAge, LineData$Value,LineData$MeanAge, col = "#AAAAAA50")
segments(seq(0,15,by=2.5),19.5,seq(0,15,by=2.5),45.5, col = "#AAAAAA50")
points(ScatterData$Value, ScatterData$MeanAge, pch = 16, col = ScatterData$color,
       cex = 1.2)
text(0,y = LineData$labely, LineData$Country,pos = 2, xpd = TRUE)
axis(4, las = 1)
axis(1,pos=19.5,xpd=T)
mtext("Mean age of population",side = 4,3)
mtext("IFR (%)",side = 1,2)

legend(x=12,y=24,
       col = LegendData$color, 
       pch = 16, 
       legend = LegendData$Source, 
       bty = 'n')
dev.off()
