## Load packages ###################################################

library(tidyverse)
library(httr)
library(osfr)
library(HMDHFDplus)

### Load data #######################################################

osf_retrieve_file("7tnfh") %>%
  osf_download(conflicts = "overwrite") 

# This reads it in
dat <-  read_csv("Output_5.zip",
                 skip = 3,
                 col_types = "ccccciiddd")


# exposure
### capturing data from the Human Mortality database
# Username of HMD
hmd_name <- "kikepaila@gmail.com"
# Password of HMD
hmd_pass <- "secreto"

us <- readHMDweb(CNTRY = "USA",
                 item = "Exposures_1x1",
                 username = hmd_name,
                 password = hmd_pass,
                 fixup = TRUE) %>%
  as_tibble() %>%
  select(Year, Age, Total) %>%
  filter(Year == 2018) %>% 
  mutate(country = "USA")

de <- readHMDweb(CNTRY = "DEUTNP",
                 item = "Exposures_1x1",
                 username = hmd_name,
                 password = hmd_pass,
                 fixup = TRUE) %>%
  as_tibble() %>%
  select(Year, Age, Total) %>%
  filter(Year == 2017) %>% 
  mutate(country = "Germany")

all <- bind_rows(us, de) %>% 
  rename(exposure = Total) %>% 
  mutate(s1_ir = case_when(Age < 50 ~ 0.23,
                           Age >= 50 &   Age < 65 ~ 0.16,
                           Age >= 65 ~ 0.14),
         s2_ir = 0.2,
         s3_ir = case_when(Age < 50 ~ 0.26,
                           Age >= 50 &   Age < 65 ~ 0.1,
                           Age >= 65 ~ 0.06),
         cfr = exp(-7.53 + 0.119 * (Age + .5))/100,
         s1_infec = exposure * s1_ir,
         s2_infec = exposure * s2_ir,
         s3_infec = exposure * s3_ir,
         s1_deaths = s1_infec * cfr,
         s2_deaths = s2_infec * cfr,
         s3_deaths = s3_infec * cfr) %>% 
  group_by(country) %>% 
  summarise(s1_infec = sum(s1_infec),
            s1_deaths = sum(s1_deaths),
            s2_infec = sum(s2_infec),
            s2_deaths = sum(s2_deaths),
            s3_infec = sum(s3_infec),
            s3_deaths = sum(s3_deaths),
            ifr1 = s1_deaths / s1_infec,
            ifr2 = s2_deaths / s2_infec,
            ifr3 = s3_deaths / s3_infec) %>% 
  ungroup() %>% 
  gather(-country, key = 'Measure', value = "Value")
                        
