##################################
########### Background ###########
##################################

# This script generates tables used to make Figs. in:
# Burgess, Langendorf, Moyer, et al.:
# "Long-standing historical dynamics suggest a slow-growth, high-inequality
# economic future"
# This code produces csv files, which were the converted to JMP files
# to make the final figures.

############################
########### Code ###########
############################

### Clear environment with command below:
# rm(list = ls())

#####################################
########### LOAD PACKAGES ###########
#####################################

library(tidyverse)
library(tidyquant)
library(here)
library(readxl)
library(imputeTS)
library(lubridate)      # easily work with dates and times
library(fpp2)           # working with time series data
library(zoo)            # working with time series data
library(PearsonDS)
library(purrr)
library(reshape2)
# library(readr)
# library(magrittr) ## Needed for the self-assignment operator %<>%
# library(Hmisc)
# library(optimx)
# library(deSolve)

########################################
########### SSP Database ###############
########################################

### SSP database
sspdf <- read_csv(here("Data", "SSP_IAM_V2_201811.csv"))

#Isolate SSP markers (SSP1-19, SSP1-26, SSP2-34, SSP2-45, SSP6-60, SSP3-Baseline (i.e. 7.0), SSP5-Baseline (i.e. 8.5))
# Marker models: SSP1: IMAGE, SSP2: MESSAGE, SSP3: AIM, SSP4: GCAM, SSP5: REMIND, from Table 1 in Riahi et al. 2017

sspmarkers <- sspdf %>% 
  filter((SCENARIO == 'SSP1-19' & grepl("IMAGE", MODEL)) |
           (SCENARIO == 'SSP1-26' & grepl("IMAGE", MODEL)) |
           (SCENARIO == 'SSP2-34' & grepl("MESSAGE", MODEL)) |
           (SCENARIO == 'SSP2-45' & grepl("MESSAGE", MODEL)) |
           (SCENARIO == 'SSP4-60' & grepl("GCAM", MODEL)) |
           (SCENARIO == 'SSP3-Baseline' & grepl("AIM", MODEL)) |
           (SCENARIO == 'SSP5-Baseline' & grepl("REMIND", MODEL)))


#########################################
########### Region Lookup ###############
#########################################

# Note: World Bank income classifications can be found 
# here: https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups 

owidregionlookup <- read_csv(here("BurgLangMoyeretal-growth-futures", 
                                  "Region-lookup-growth.csv"))

#######################################################
########### GDP per capita observations ###############
#######################################################

# GDP currency value conversions
worldGDP2005in2017USD <- 80.91
worldGDP2005in2005USD <- 65.952

worldGDP2011in2017USD <- 99.861
worldGDP2011in2011USD <- 95.1

gdpconversion2017to2005USD <- worldGDP2005in2005USD/worldGDP2005in2017USD
gdpconversion2011to2005USD <- gdpconversion2017to2005USD * (worldGDP2011in2017USD/worldGDP2011in2011USD)
# Conversion calculated from World Bank US GDP data: https://data.worldbank.org/indicator/NY.GDP.MKTP.PP.KD 

worldGDP2005in2017USD <- 80.91
worldGDP2005in2005USD <- 65.952

worldGDP2011in2017USD <- 99.861
worldGDP2011in2011USD <- 95.1

gdpconversion2017to2005USD <- worldGDP2005in2005USD/worldGDP2005in2017USD
gdpconversion2011to2005USD <- gdpconversion2017to2005USD * (worldGDP2011in2017USD/worldGDP2011in2011USD)
# Conversion calculated from World Bank US GDP data: https://data.worldbank.org/indicator/NY.GDP.MKTP.PP.KD 

## Calculate 1960-2020 observations from OWID (1960-2018) 
## and World Bank (2019-2020) data

# population data
popobs <- read_csv(here("Data/2020-Kaya-observations-update", 
                        "population-since-1800.csv"))

# GDP per capita data
pcgdppppobs1818 <- read_csv(here("Data/2020-Kaya-observations-update", 
                                 "gdp-per-capita-maddison-2020.csv")) %>%
  select(-`145446-annotations`) %>%
  left_join(popobs) %>%
  left_join(owidregionlookup) %>%
  filter(Year > 1959) %>%
  rename(pcgdp = `GDP per capita`,
         pop = `Population (historical estimates)`) %>%
  mutate(gdp = pcgdp * pop)

pcgdppppobs9020 <- read_csv(here("Data/2020-Kaya-observations-update", 
                                 "gdp-per-capita-worldbank.csv")) %>%
  left_join(popobs) %>%
  left_join(owidregionlookup) %>%
  filter(Year > 2017) %>%
  rename(pcgdp = `GDP per capita, PPP (constant 2017 international $)`,
         pop = `Population (historical estimates)`) %>%
  mutate(gdp = pcgdp * pop)


## Aggregate GDP per capita by Tilman et al. 2011 economic groups
# Note: the following countries are not included due to lack of
# OWID coverage: Eritrea (F), North Korea (E), Cuba (D), Syria (C), Venezuela (B)
pcgdpOWIDTilman <- pcgdppppobs1818 %>%
  filter(!is.na(TilmanEconomicGroup)) %>%
  group_by(Year, TilmanEconomicGroup) %>%
  summarise(totalpop = sum(pop,na.rm = T),
            totalgdp = sum(gdp,na.rm = T)) %>%
  mutate(pcgdpthousand2005usd = (totalgdp/totalpop) * 
           (gdpconversion2011to2005USD/1000)) 

pcgdpWBTilman <- pcgdppppobs9020 %>%
  filter(!is.na(TilmanEconomicGroup)) %>%
  group_by(Year, TilmanEconomicGroup) %>%
  summarise(totalpop = sum(pop,na.rm = T),
            totalgdp = sum(gdp,na.rm = T)) %>%
  mutate(pcgdpthousand2005usd = (totalgdp/totalpop) * (gdpconversion2017to2005USD/1000)) 

# Calculate country-level normalizer linking OWID and WB datasets. 
# Normalize WB 2018 values to OWID 2018 values.
wbowidnormalizerTilman <- pcgdpOWIDTilman %>%
  select(Year, TilmanEconomicGroup, pcgdpthousand2005usd) %>%
  filter(Year == 2018) %>%
  rename(pcgdpowid = pcgdpthousand2005usd) %>%
  left_join(pcgdpWBTilman %>%
              select(Year, TilmanEconomicGroup, pcgdpthousand2005usd) %>%
              filter(Year == 2018) %>%
              rename(pcgdpwb = pcgdpthousand2005usd)) %>%
  mutate(wbnormalizer = pcgdpowid/pcgdpwb) %>%
  group_by(TilmanEconomicGroup) %>%
  select(TilmanEconomicGroup, wbnormalizer)

# Combine two tables
pcgdpTilman <- bind_rows(pcgdpOWIDTilman,
                         pcgdpWBTilman %>%
                        filter(Year > 2018) %>%
                        left_join(wbowidnormalizerTilman) %>%
                        mutate(pcgdpthousand2005usd = pcgdpthousand2005usd * wbnormalizer) %>%
                        select(-wbnormalizer)) %>%
  arrange(TilmanEconomicGroup, Year) %>%
  group_by(TilmanEconomicGroup) %>%
  mutate(pcgdpgrowth = 100*(log(pcgdpthousand2005usd) - 
                              log(lag(pcgdpthousand2005usd)))) # calculate growth

## Aggregate GDP per capita by World Bank economic groups
## Include only countries with full 1960-2020 data
pcgdpOWIDWB <- pcgdppppobs1818 %>%
  filter(!is.na(WBEconomicGroup)) %>%
  left_join(pcgdppppobs1818 %>%
              filter(!is.na(WBEconomicGroup)) %>%
              filter(Year == 1960) %>%
              select(Entity, Code) %>%
              mutate(completepanel = "T")) %>% # remove countries without full data
  filter(!is.na(completepanel)) %>%
  group_by(Year, WBEconomicGroup) %>%
  summarise(totalpop = sum(pop,na.rm = T),
            totalgdp = sum(gdp,na.rm = T)) %>%
  mutate(pcgdpthousand2005usd = (totalgdp/totalpop) * 
           (gdpconversion2011to2005USD/1000)) 

pcgdpWBWB <- pcgdppppobs9020 %>%
  filter(!is.na(WBEconomicGroup)) %>%
  left_join(pcgdppppobs1818 %>%
              filter(!is.na(WBEconomicGroup)) %>%
              filter(Year == 1960) %>%
              select(Entity, Code) %>%
              mutate(completepanel = "T")) %>% # remove countries without full data
  filter(!is.na(completepanel)) %>%
  group_by(Year, WBEconomicGroup) %>%
  summarise(totalpop = sum(pop,na.rm = T),
            totalgdp = sum(gdp,na.rm = T)) %>%
  mutate(pcgdpthousand2005usd = (totalgdp/totalpop) * (gdpconversion2017to2005USD/1000)) 

# Calculate country-level normalizer linking OWID and WB datasets. 
# Normalize WB 2018 values to OWID 2018 values.
wbowidnormalizerWB <- pcgdpOWIDWB %>%
  select(Year, WBEconomicGroup, pcgdpthousand2005usd) %>%
  filter(Year == 2018) %>%
  rename(pcgdpowid = pcgdpthousand2005usd) %>%
  left_join(pcgdpWBWB %>%
              select(Year, WBEconomicGroup, pcgdpthousand2005usd) %>%
              filter(Year == 2018) %>%
              rename(pcgdpwb = pcgdpthousand2005usd)) %>%
  mutate(wbnormalizer = pcgdpowid/pcgdpwb) %>%
  group_by(WBEconomicGroup) %>%
  select(WBEconomicGroup, wbnormalizer)

# Combine two tables
pcgdpWB <- bind_rows(pcgdpOWIDWB,
                         pcgdpWBWB %>%
                           filter(Year > 2018) %>%
                           left_join(wbowidnormalizerWB) %>%
                           mutate(pcgdpthousand2005usd = pcgdpthousand2005usd * wbnormalizer) %>%
                           select(-wbnormalizer)) %>%
  arrange(WBEconomicGroup, Year) %>%
  group_by(WBEconomicGroup) %>%
  mutate(pcgdpgrowth = 100*(log(pcgdpthousand2005usd) - 
                              log(lag(pcgdpthousand2005usd)))) # calculate growth


## Aggregate GDP per capita for individual countries, 
# from all with 144 countries full 1960-2020 data & a WB group:
pcgdpcountriesOWID <- pcgdppppobs1818 %>%
  filter(!is.na(WBEconomicGroup)) %>%
  left_join(pcgdppppobs1818 %>%
              filter(!is.na(WBEconomicGroup)) %>%
              filter(Year == 1960) %>%
              select(Entity, Code) %>%
              mutate(completepanel = "T")) %>% # remove countries without full data
  filter(!is.na(completepanel)) %>%
select(Entity, Code, TilmanEconomicGroup, WBEconomicGroup, Year, pop, pcgdp, gdp) %>%
  mutate(pcgdpthousand2005usd = pcgdp * 
           (gdpconversion2011to2005USD/1000))

pcgdpcountriesWB <- pcgdppppobs9020 %>%
  filter(!is.na(WBEconomicGroup)) %>%
  left_join(pcgdppppobs1818 %>%
              filter(!is.na(WBEconomicGroup)) %>%
              filter(Year == 1960) %>%
              select(Entity, Code) %>%
              mutate(completepanel = "T")) %>% # remove countries without full data
  filter(!is.na(completepanel)) %>%
  select(Entity, Code, TilmanEconomicGroup, WBEconomicGroup, Year, pop, pcgdp, gdp) %>%
  mutate(pcgdpthousand2005usd = pcgdp * (gdpconversion2017to2005USD/1000)) 


# Calculate country-level normalizer linking OWID and WB datasets. 
# Normalize WB 2018 values to OWID 2018 values.
wbowidnormalizercountries <- pcgdpcountriesOWID %>%
  select(Entity, Code, Year, TilmanEconomicGroup, WBEconomicGroup, pcgdpthousand2005usd) %>%
  filter(Year == 2018) %>%
  rename(pcgdpowid = pcgdpthousand2005usd) %>%
  left_join(pcgdpcountriesWB %>%
              select(Entity, Code, Year, TilmanEconomicGroup, WBEconomicGroup, pcgdpthousand2005usd) %>%
              filter(Year == 2018) %>%
              rename(pcgdpwb = pcgdpthousand2005usd)) %>%
  mutate(wbnormalizer = pcgdpowid/pcgdpwb) %>%
  group_by(Entity, Code) %>%
  select(Entity, Code, TilmanEconomicGroup, WBEconomicGroup, wbnormalizer)

# Combine two tables
pcgdpcountries <- bind_rows(pcgdpcountriesOWID,
                         pcgdpcountriesWB %>%
                           filter(Year > 2018) %>%
                           left_join(wbowidnormalizercountries) %>%
                           mutate(pcgdpthousand2005usd = pcgdpthousand2005usd * wbnormalizer) %>%
                           select(-wbnormalizer)) %>%
  arrange(Entity, Code, TilmanEconomicGroup, WBEconomicGroup, Year) %>%
  group_by(Entity, Code, TilmanEconomicGroup, WBEconomicGroup) %>%
  mutate(pcgdpgrowth = 100*(log(pcgdpthousand2005usd) - 
                              log(lag(pcgdpthousand2005usd)))) # calculate growth

#### GDP per capita for world
pcgdpWorldOWID <- pcgdppppobs1818 %>%
  filter(Entity == 'World',
         Year == 2018 |
           Year < 1991) %>%
  select(Entity, Code, TilmanEconomicGroup, WBEconomicGroup, Year, pop, pcgdp, gdp) %>%
  mutate(pcgdpthousand2005usd = pcgdp * 
           (gdpconversion2011to2005USD/1000))

pcgdpWorldWB <- read_csv(here("Data/2020-Kaya-observations-update", 
                              "gdp-per-capita-worldbank.csv")) %>%
  left_join(popobs) %>%
  left_join(owidregionlookup) %>%
  rename(pcgdp = `GDP per capita, PPP (constant 2017 international $)`,
         pop = `Population (historical estimates)`) %>%
  mutate(gdp = pcgdp * pop) %>%
  filter(Entity == 'World') %>%
  select(Entity, Code, TilmanEconomicGroup, WBEconomicGroup, Year, pop, pcgdp, gdp) %>%
  mutate(pcgdpthousand2005usd = pcgdp * (gdpconversion2017to2005USD/1000)) 


# Calculate country-level normalizer linking OWID and WB datasets. 
# Normalize WB 2018 values to OWID 2018 values.
wbowidnormalizerWorld <- pcgdpWorldOWID %>%
  select(Entity, Code, Year, TilmanEconomicGroup, WBEconomicGroup, pcgdpthousand2005usd) %>%
  filter(Year == 2018) %>%
  rename(pcgdpowid = pcgdpthousand2005usd) %>%
  left_join(pcgdpWorldWB %>%
              select(Entity, Code, Year, TilmanEconomicGroup, WBEconomicGroup, pcgdpthousand2005usd) %>%
              filter(Year == 2018) %>%
              rename(pcgdpwb = pcgdpthousand2005usd)) %>%
  mutate(wbnormalizer = pcgdpowid/pcgdpwb) %>%
  group_by(Entity, Code) %>%
  select(Entity, Code, TilmanEconomicGroup, WBEconomicGroup, wbnormalizer)

# Combine two tables
pcgdpworld <- bind_rows(pcgdpWorldWB %>%
                              left_join(wbowidnormalizerWorld) %>%
                              mutate(pcgdpthousand2005usd = pcgdpthousand2005usd * wbnormalizer) %>%
                              select(-wbnormalizer)) %>%
  arrange(Entity, Code, TilmanEconomicGroup, WBEconomicGroup, Year) %>%
  group_by(Entity, Code, TilmanEconomicGroup, WBEconomicGroup) %>%
  mutate(pcgdpgrowth = 100*(log(pcgdpthousand2005usd) - 
                              log(lag(pcgdpthousand2005usd)))) # calculate growth
 
# Add pre-1990 pcGDP using WB group weighted averages to get growth rates:
pcgdpWorldfromWBpre1990 <- pcgdpWB %>%
  mutate(total2005gdp = totalpop*pcgdpthousand2005usd) %>%
  group_by(Year) %>%
  summarise(gdp2005usd = sum(total2005gdp, na.rm = T),
            pop = sum(totalpop, na.rm = T)) %>%
  filter(Year < 1991) %>%
  mutate(pcgdpthousand2005usd = (7.552300/6.941839)*(gdp2005usd/pop), # factor normalizes to WB world calibration
         Entity = 'World',
         Code = 'OWID_WRL',
         TilmanEconomicGroup = NA,
         WBEconomicGroup = NA) %>%
  arrange(Entity, Code, TilmanEconomicGroup, WBEconomicGroup, Year) %>%
  group_by(Entity, Code, TilmanEconomicGroup, WBEconomicGroup) %>%
  mutate(pcgdpgrowth = 100*(log(pcgdpthousand2005usd) - 
                              log(lag(pcgdpthousand2005usd)))) # calculate growth
  
pcgdpWorld <- bind_rows(pcgdpWorldfromWBpre1990,
                        pcgdpworld %>%
                          filter(Year > 1990))


#### Combine GDP per capita tables
pcgdpcombined <- bind_rows(
  pcgdpcountries %>%
    select(-pcgdp,-gdp),
  pcgdpTilman %>%
    rename(Entity = TilmanEconomicGroup,
           pop = totalpop) %>%
    mutate(Code = "TilmanGroups") %>%
    select(Entity, Code, Year, pop, pcgdpthousand2005usd, pcgdpgrowth),
  pcgdpWB %>%
    rename(Entity = WBEconomicGroup,
           pop = totalpop) %>%
    mutate(Code = "WBGroups") %>%
    select(Entity, Code, Year, pop, pcgdpthousand2005usd, pcgdpgrowth),
  pcgdpWorld %>%
    select(Entity, Code, Year, pop, pcgdpthousand2005usd, pcgdpgrowth)
) %>%
  mutate(rollingmean5 = rollmean(pcgdpgrowth, 5, fill = NA, 
                                 align = "center"),
         rollingmean11 = rollmean(pcgdpgrowth, 11, fill = NA, 
                                  align = "center"),
         rollingmean21 = rollmean(pcgdpgrowth, 21, fill = NA, 
                                  align = "center")) # calculate 5-, 11-y, and 21-y rolling mean growth

# list countries with full panels
pcgdpcountrylist <- pcgdpcountries %>%
  filter(Year == 1960)

#write_csv(pcgdpcountrylist,
#          "BurgLangMoyeretal-growth-futures/countrylist.csv")

############################################
########### Covariates (MIT) ###############
############################################

# load data
WBagedep <- read_csv(here("BurgLangMoyeretal-growth-futures/Covariates", 
                                 "WB-age-dependency-ratio.csv")) %>%
  gather(key = 'Year', value = "AgeDependency", -`Country Name`, -`Country Code`, 
         -`Indicator Name`, -`Indicator Code`) %>%
  select(-`Indicator Name`, -`Indicator Code`) %>%
  rename(Entity = `Country Name`,
         Code = `Country Code`) %>%
  mutate_at(vars(Year),as.double)

WBindustry <- read_csv(here("BurgLangMoyeretal-growth-futures/Covariates", 
                            "WB-employment-in-industry.csv")) %>%
  gather(key = 'Year', value = "EmploymentIndustry", -`Country Name`, -`Country Code`, 
         -`Indicator Name`, -`Indicator Code`) %>%
  select(-`Indicator Name`, -`Indicator Code`) %>%
  rename(Entity = `Country Name`,
         Code = `Country Code`) %>%
  mutate_at(vars(Year),as.double)

WBservices <- read_csv(here("BurgLangMoyeretal-growth-futures/Covariates", 
                            "WB-employment-in-services.csv")) %>%
  gather(key = 'Year', value = "EmploymentServices", -`Country Name`, -`Country Code`, 
         -`Indicator Name`, -`Indicator Code`) %>%
  select(-`Indicator Name`, -`Indicator Code`) %>%
  rename(Entity = `Country Name`,
         Code = `Country Code`) %>%
  mutate_at(vars(Year),as.double)

WBmanufacturing <- read_csv(here("BurgLangMoyeretal-growth-futures/Covariates", 
                                 "WB-manufacturing-value-added.csv")) %>%
  gather(key = 'Year', value = "ManufacturingValue", -`Country Name`, -`Country Code`, 
         -`Indicator Name`, -`Indicator Code`) %>%
  select(-`Indicator Name`, -`Indicator Code`) %>%
  rename(Entity = `Country Name`,
         Code = `Country Code`) %>%
  mutate_at(vars(Year),as.double)

OWIDFertility <- read_csv(here("BurgLangMoyeretal-growth-futures/Covariates", 
                               "children-per-woman-UN.csv")) %>%
  mutate_at(vars(Year),as.double) %>%
  rename(fertility = `Estimates, 1950 - 2020: Annually interpolated demographic indicators - Total fertility (live births per woman)`)

OWIDLaborProductivity <- read_csv(here("BurgLangMoyeretal-growth-futures/Covariates", 
                                   "labor-productivity-per-hour-PennWorldTable.csv")) %>%
  left_join(read_csv(here("BurgLangMoyeretal-growth-futures/Covariates", 
                          "annual-working-hours-per-worker.csv"))) %>%
  rename(laborproductivityhr = `Productivity (PWT 9.1 (2019))`,
         hours = `Average annual working hours per worker`) %>%
  mutate(laborproductivityhr = laborproductivityhr*gdpconversion2011to2005USD) %>% # convert to 2005 USD units
  mutate(laborproductivity = laborproductivityhr*hours)

# Average annual hours worked by persons engaged (avh) (PWT 9.1 (2019))
  
covariates <- OWIDFertility %>%
  left_join(OWIDLaborProductivity) %>%
  left_join(WBagedep) %>%
  left_join(WBindustry) %>%
  left_join(WBservices) %>%
  left_join(WBmanufacturing)

covariatescountries <- pcgdpcountries %>%
  left_join(covariates) %>%
  select(-pcgdp,-gdp, -pcgdpthousand2005usd, -pcgdpgrowth)

# calculate covariates by groups, weighting by population, 
# using balanced panels for each covariate, over indicated years
# panel start years: 
# 1960 for fertility (143/144 countries)
# 1970 for labor productivity (44/144 countries, none from Tilman groups F,G)
# 1960 for age dependency (131/144 countries)
# 1991 for employment in services (130/144)
# 1991 for employment in industry (130/144)
# 1995 for manufacturing value added (108/144)
##code used for figuring out which years to use:
#test <- covariatescountries %>%
#  filter(!is.na(laborproductivity),
#         Year == 1970)

fertilityTilman <- covariatescountries %>%
  left_join(covariatescountries %>%
              filter(!is.na(TilmanEconomicGroup),
                     !is.na(fertility),
                     Year == 1960) %>%
              select(Entity, Code) %>%
              mutate(completepanel = "T")) %>% # remove countries without full data
  filter(!is.na(completepanel),
         Year > 1959) %>%
  group_by(TilmanEconomicGroup, Year) %>%
  mutate(popfertility = pop*fertility) %>%
  summarise(popfertility = sum(popfertility, na.rm = T),
            pop = sum(pop, na.rm = T)) %>%
  mutate(fertility = popfertility/pop) %>%
  select(-pop, -popfertility)

fertilityWB <- covariatescountries %>%
  left_join(covariatescountries %>%
              filter(!is.na(WBEconomicGroup),
                     !is.na(fertility),
                     Year == 1960) %>%
              select(Entity, Code) %>%
              mutate(completepanel = "T")) %>% # remove countries without full data
  filter(!is.na(completepanel),
         Year > 1959) %>%
  group_by(WBEconomicGroup, Year) %>%
  mutate(popfertility = pop*fertility) %>%
  summarise(popfertility = sum(popfertility, na.rm = T),
            pop = sum(pop, na.rm = T)) %>%
  mutate(fertility = popfertility/pop) %>%
  select(-pop, -popfertility)

laborprodTilman <- covariatescountries %>%
  left_join(covariatescountries %>%
              filter(!is.na(TilmanEconomicGroup),
                     !is.na(laborproductivity),
                     Year == 1970) %>%
              select(Entity, Code) %>%
              mutate(completepanel = "T")) %>% # remove countries without full data
  filter(!is.na(completepanel),
         Year > 1969) %>%
  group_by(TilmanEconomicGroup, Year) %>%
  mutate(popprod = pop*laborproductivity,
         popprodhr = pop*laborproductivityhr) %>%
  summarise(popprod = sum(popprod, na.rm = T),
            popprodhr = sum(popprodhr, na.rm = T),
            pop = sum(pop, na.rm = T)) %>%
  mutate(laborproductivity = popprod/pop,
         laborproductivityhr = popprodhr/pop) %>%
  select(-pop, -popprod, -popprodhr)

laborprodWB <- covariatescountries %>%
  left_join(covariatescountries %>%
              filter(!is.na(WBEconomicGroup),
                     !is.na(laborproductivity),
                     Year == 1970) %>%
              select(Entity, Code) %>%
              mutate(completepanel = "T")) %>% # remove countries without full data
  filter(!is.na(completepanel),
         Year > 1969) %>%
  group_by(WBEconomicGroup, Year) %>%
  mutate(popprod = pop*laborproductivity,
         popprodhr = pop*laborproductivityhr) %>%
  summarise(popprod = sum(popprod, na.rm = T),
            popprodhr = sum(popprodhr, na.rm = T),
            pop = sum(pop, na.rm = T)) %>%
  mutate(laborproductivity = popprod/pop,
         laborproductivityhr = popprodhr/pop) %>%
  select(-pop, -popprod, -popprodhr)

agedepTilman <- covariatescountries %>%
  left_join(covariatescountries %>%
              filter(!is.na(TilmanEconomicGroup),
                     !is.na(AgeDependency),
                     Year == 1960) %>%
              select(Entity, Code) %>%
              mutate(completepanel = "T")) %>% # remove countries without full data
  filter(!is.na(completepanel),
         Year > 1959) %>%
  group_by(TilmanEconomicGroup, Year) %>%
  mutate(popagedep = pop*AgeDependency) %>%
  summarise(popagedep = sum(popagedep, na.rm = T),
            pop = sum(pop, na.rm = T)) %>%
  mutate(AgeDependency = popagedep/pop) %>%
  select(-pop, -popagedep)

agedepWB <- covariatescountries %>%
  left_join(covariatescountries %>%
              filter(!is.na(WBEconomicGroup),
                     !is.na(AgeDependency),
                     Year == 1960) %>%
              select(Entity, Code) %>%
              mutate(completepanel = "T")) %>% # remove countries without full data
  filter(!is.na(completepanel),
         Year > 1959) %>%
  group_by(WBEconomicGroup, Year) %>%
  mutate(popagedep = pop*AgeDependency) %>%
  summarise(popagedep = sum(popagedep, na.rm = T),
            pop = sum(pop, na.rm = T)) %>%
  mutate(AgeDependency = popagedep/pop) %>%
  select(-pop, -popagedep)

empservTilman <- covariatescountries %>%
  left_join(covariatescountries %>%
              filter(!is.na(TilmanEconomicGroup),
                     !is.na(EmploymentServices),
                     Year == 1991) %>%
              select(Entity, Code) %>%
              mutate(completepanel = "T")) %>% # remove countries without full data
  filter(!is.na(completepanel),
         Year > 1990) %>%
  group_by(TilmanEconomicGroup, Year) %>%
  mutate(popempserv = pop*EmploymentServices) %>%
  summarise(popempserv = sum(popempserv, na.rm = T),
            pop = sum(pop, na.rm = T)) %>%
  mutate(EmploymentServices = popempserv/pop) %>%
  select(-pop, -popempserv)

empservWB <- covariatescountries %>%
  left_join(covariatescountries %>%
              filter(!is.na(WBEconomicGroup),
                     !is.na(EmploymentServices),
                     Year == 1991) %>%
              select(Entity, Code) %>%
              mutate(completepanel = "T")) %>% # remove countries without full data
  filter(!is.na(completepanel),
         Year > 1990) %>%
  group_by(WBEconomicGroup, Year) %>%
  mutate(popempserv = pop*EmploymentServices) %>%
  summarise(popempserv = sum(popempserv, na.rm = T),
            pop = sum(pop, na.rm = T)) %>%
  mutate(EmploymentServices = popempserv/pop) %>%
  select(-pop, -popempserv)

empindTilman <- covariatescountries %>%
  left_join(covariatescountries %>%
              filter(!is.na(TilmanEconomicGroup),
                     !is.na(EmploymentIndustry),
                     Year == 1991) %>%
              select(Entity, Code) %>%
              mutate(completepanel = "T")) %>% # remove countries without full data
  filter(!is.na(completepanel),
         Year > 1990) %>%
  group_by(TilmanEconomicGroup, Year) %>%
  mutate(popempind = pop*EmploymentIndustry) %>%
  summarise(popempind = sum(popempind, na.rm = T),
            pop = sum(pop, na.rm = T)) %>%
  mutate(EmploymentIndustry = popempind/pop) %>%
  select(-pop, -popempind)

empindWB <- covariatescountries %>%
  left_join(covariatescountries %>%
              filter(!is.na(WBEconomicGroup),
                     !is.na(EmploymentIndustry),
                     Year == 1991) %>%
              select(Entity, Code) %>%
              mutate(completepanel = "T")) %>% # remove countries without full data
  filter(!is.na(completepanel),
         Year > 1990) %>%
  group_by(WBEconomicGroup, Year) %>%
  mutate(popempind = pop*EmploymentIndustry) %>%
  summarise(popempind = sum(popempind, na.rm = T),
            pop = sum(pop, na.rm = T)) %>%
  mutate(EmploymentIndustry = popempind/pop) %>%
  select(-pop, -popempind)

manufacturingTilman <- covariatescountries %>%
  left_join(covariatescountries %>%
              filter(!is.na(TilmanEconomicGroup),
                     !is.na(ManufacturingValue),
                     Year == 1995) %>%
              select(Entity, Code) %>%
              mutate(completepanel = "T")) %>% # remove countries without full data
  filter(!is.na(completepanel),
         Year > 1994) %>%
  group_by(TilmanEconomicGroup, Year) %>%
  mutate(popmanufacturing = pop*ManufacturingValue) %>%
  summarise(popmanufacturing = sum(popmanufacturing, na.rm = T),
            pop = sum(pop, na.rm = T)) %>%
  mutate(ManufacturingValue = popmanufacturing/pop) %>%
  select(-pop, -popmanufacturing)

manufacturingWB <- covariatescountries %>%
  left_join(covariatescountries %>%
              filter(!is.na(WBEconomicGroup),
                     !is.na(ManufacturingValue),
                     Year == 1995) %>%
              select(Entity, Code) %>%
              mutate(completepanel = "T")) %>% # remove countries without full data
  filter(!is.na(completepanel),
         Year > 1994) %>%
  group_by(WBEconomicGroup, Year) %>%
  mutate(popmanufacturing = pop*ManufacturingValue) %>%
  summarise(popmanufacturing = sum(popmanufacturing, na.rm = T),
            pop = sum(pop, na.rm = T)) %>%
  mutate(ManufacturingValue = popmanufacturing/pop) %>%
  select(-pop, -popmanufacturing)

covariatesTilman <- fertilityTilman %>%
  left_join(agedepTilman) %>%
  left_join(laborprodTilman) %>%
  left_join(empservTilman) %>%
  left_join(empindTilman) %>%
  left_join(manufacturingTilman) %>%
  rename(Entity = TilmanEconomicGroup) %>%
  mutate(Code = "TilmanGroups")

covariatesWB <- fertilityWB %>%
  left_join(agedepWB) %>%
  left_join(laborprodWB) %>%
  left_join(empservWB) %>%
  left_join(empindWB) %>%
  left_join(manufacturingWB) %>%
  rename(Entity = WBEconomicGroup) %>%
  mutate(Code = "WBGroups")

covariatescombined <- bind_rows(covariatescountries,
                                          covariatesTilman,
                                          covariatesWB) %>%
  select(-pop)

CombinedTable <- pcgdpcombined %>%
  left_join(covariatescombined) %>%
  mutate(EmploymentIndustry = ifelse(Year > 2019, NA, EmploymentIndustry),
         EmploymentServices = ifelse(Year > 2019, NA, EmploymentServices),
         laborproductivity = ifelse(Year > 2017, NA, laborproductivity/1000),
         laborproductivityhr = ifelse(Year > 2017, NA, laborproductivityhr)) %>% # remove missing years
  group_by(Entity) %>%
  arrange(Entity, Year) %>%
  mutate(laborprodgrowth = 100*(log(laborproductivity) - log(lag(laborproductivity))),
         laborprodhrgrowth = 100*(log(laborproductivityhr) - log(lag(laborproductivityhr))))

#########################################################
########### Load Kuznets Model Projections ##############
#########################################################

# Kuznets model takes pcgdpcombined.csv, and returns files loaded below

# Load Kuznets fitted parameters
KuznetsParameters <- read_csv(here("BurgLangMoyeretal-growth-futures/Kuznets_Projections", 
                                   "Kuznets_Parameters_20220130212906.csv"))

# Create columns with fits
KuznetsFits <- read_csv(here("BurgLangMoyeretal-growth-futures/Kuznets_Projections", 
                             "Kuznets-equations-template.csv")) %>%
  left_join(KuznetsParameters) %>%
  mutate(pcgdpgrowth = ifelse(Model == 'Kuznets_Asymptoting',
         (`Parameter_1` + (`Parameter_2`*log(pcgdpthousand2005usd))
         + (`Parameter_3`*((log(pcgdpthousand2005usd))^2)))*(1/pcgdpthousand2005usd),
         (`Parameter_1` + (`Parameter_2`*log(pcgdpthousand2005usd))
         + (`Parameter_3`*((log(pcgdpthousand2005usd))^2))))) %>%
  select(-Grouping, -Model)

# Load Kuznets model projections for WB groups, normal and asymptoting
WBKuznetsProjections <- bind_rows(
  read_csv(here("BurgLangMoyeretal-growth-futures/Kuznets_Projections", 
                             "forecast_kuznets_WB_20220130212906.csv")) %>%
    gather(key = 'Entity', value = 'pcgdpthousand2005usd',-Year) %>%
    mutate(Scenario = 'Kuznets-WB-projection'),
  read_csv(here("BurgLangMoyeretal-growth-futures/Kuznets_Projections", 
                "forecast_kuznets_WB_World_20220130212906.csv")) %>%
    gather(key = 'Entity', value = 'pcgdpthousand2005usd',-Year) %>%
    mutate(Scenario = 'Kuznets-WB-projection'),
  read_csv(here("BurgLangMoyeretal-growth-futures/Kuznets_Projections", 
                "forecast_kuznets_asymptoting_WB_20220130212906.csv")) %>%
    gather(key = 'Entity', value = 'pcgdpthousand2005usd',-Year) %>%
    mutate(Scenario = 'Kuznets-WB-asymptoting-projection'),
  read_csv(here("BurgLangMoyeretal-growth-futures/Kuznets_Projections", 
                "forecast_kuznets_asymptoting_WB_World_20220130212906.csv")) %>%
    gather(key = 'Entity', value = 'pcgdpthousand2005usd',-Year) %>%
    mutate(Scenario = 'Kuznets-WB-asymptoting-projection')) %>%
  group_by(Entity, Scenario) %>%
  arrange(Entity, Scenario, Year) %>%
  mutate(pcgdpgrowth = 100*(log(pcgdpthousand2005usd) - log(lag(pcgdpthousand2005usd))))


#############################################################################
########### Compile and Compare Kuznets, IF, & SSP projections ##############
#############################################################################

### Load IF & SSP projections by WB region.

# GDP per capita
IFpcgdpWB <- read_csv(here("BurgLangMoyeretal-growth-futures/IF-Output", 
                                  "INCOME GROUPS AND WORLD GDP PER CAPITA AT PPP.csv")) %>%
  select(-Code) %>%
  gather(value = "pcgdp", key = "Year",
         -Entity, -Unit, -Method) %>%
  rename(Scenario = Method) %>%
  mutate(Entity = ifelse(Entity == "WB High Income", "High income",
                         ifelse(Entity == "WB Upper Middle Income", "Upper-middle income",
                                ifelse(Entity == "WB Low Middle Income", "Lower-middle income",
                                       ifelse(Entity == "WB Low Income", "Low income", Entity))))) %>%
  mutate(pcgdpthousand2005usd = pcgdp*gdpconversion2011to2005USD) %>%
  select(-pcgdp, -Unit) %>%
  group_by(Entity, Scenario) %>%
  arrange(Entity, Scenario, Year) %>%
  mutate(pcgdpgrowth = 100*(log(pcgdpthousand2005usd) - log(lag(pcgdpthousand2005usd))))
  

# fertility rate
IFbirthWB <- read_csv(here("BurgLangMoyeretal-growth-futures/IF-Output", 
                           "TFR BY SCENARIO AND GROUP.csv")) %>%
  select(-Code) %>%
  gather(value = "fertility", key = "Year",
         -Entity, -Unit, -Method) %>%
  rename(Scenario = Method) %>%
  mutate(Entity = ifelse(Entity == "WB High Income", "High income",
                         ifelse(Entity == "WB Upper Middle Income", "Upper-middle income",
                                ifelse(Entity == "WB Low Middle Income", "Lower-middle income",
                                       ifelse(Entity == "WB Low Income", "Low income", Entity))))) %>%
  group_by(Entity, Scenario) %>%
  arrange(Entity, Scenario, Year) %>%
  select(-Unit)
  

# age dependency
IFagedepWB <- read_csv(here("BurgLangMoyeretal-growth-futures/IF-Output", 
                           "DEPENDENCY RATIO.csv")) %>%
  #select(-Code) %>%
  gather(value = "AgeDependency", key = "Year",
         -Entity, -Unit, -Method) %>%
  rename(Scenario = Method) %>%
  mutate(Entity = ifelse(Entity == "WB High Income", "High income",
                         ifelse(Entity == "WB Upper Middle Income", "Upper-middle income",
                                ifelse(Entity == "WB Low Middle Income", "Lower-middle income",
                                       ifelse(Entity == "WB Low Income", "Low income", Entity))))) %>%
  group_by(Entity, Scenario) %>%
  arrange(Entity, Scenario, Year) %>%
  mutate(AgeDependency = 100*AgeDependency) %>%
  select(-Unit)


# labor in services and manufacturing
IFlaborWB <- bind_rows(read_csv(here("BurgLangMoyeretal-growth-futures/IF-Output", 
                           "LAB MILLION SECTOR SCENARIO.csv")) %>%
  select(-Code), 
  read_csv(here("BurgLangMoyeretal-growth-futures/IF-Output", 
                "LABSECTORBASE.csv")) %>%
    select(-Code)) %>%
  gather(value = "labor", key = "Year",
         -Entity, -Sector, -Unit, -Method) %>%
  rename(Scenario = Method) %>%
  mutate(Entity = ifelse(Entity == "WB High Income", "High income",
                         ifelse(Entity == "WB Upper Middle Income", "Upper-middle income",
                                ifelse(Entity == "WB Low Middle Income", "Lower-middle income",
                                       ifelse(Entity == "WB Low Income", "Low income", Entity))))) %>%
  spread(key = Sector, value = labor) %>%
  group_by(Entity, Scenario) %>%
  arrange(Entity, Scenario, Year) %>%
  mutate(EmploymentIndustry = 100*((Energy + ICTech + Manufactures + Materials)/
                                     (Agriculture + Energy + ICTech + Manufactures + Materials + Services)),
         EmploymentServices = 100*((Services)/
                                     (Agriculture + Energy + ICTech + Manufactures + Materials + Services))) %>%
  select(Entity, Scenario, Year, EmploymentIndustry, EmploymentServices)



# value added in manufacturing
IFmanufacturingWB <- read_csv(here("BurgLangMoyeretal-growth-futures/IF-Output", 
                           "VALUE ADD BY SECTOR PERCENT GDP.csv")) %>%
  #select(-Code) %>%
  gather(value = "valueadded", key = "Year",
         -Entity, -Sector, -Unit, -Method) %>%
  rename(Scenario = Method) %>%
  mutate(Entity = ifelse(Entity == "WB High Income", "High income",
                         ifelse(Entity == "WB Upper Middle Income", "Upper-middle income",
                                ifelse(Entity == "WB Low Middle Income", "Lower-middle income",
                                       ifelse(Entity == "WB Low Income", "Low income", Entity))))) %>%
  spread(key = Sector, value = valueadded) %>%
  group_by(Entity, Scenario) %>%
  arrange(Entity, Scenario, Year) %>%
  mutate(ManufacturingValue = Manufactures) %>%
  select(Entity, Scenario, Year, ManufacturingValue)

# labor productivity
IFlaborproductivityWB <- read_csv(here("BurgLangMoyeretal-growth-futures/IF-Output", 
                           "LAB DIV GDP WORLD AND INCOME GROUPS.csv")) %>%
  #select(-Code) %>%
  gather(value = "laborproductivity", key = "Year",
         -Entity, -Unit, -Method) %>%
  rename(Scenario = Method) %>%
  mutate(Entity = ifelse(Entity == "WB High Income", "High income",
                         ifelse(Entity == "WB Upper Middle Income", "Upper-middle income",
                                ifelse(Entity == "WB Low Middle Income", "Lower-middle income",
                                       ifelse(Entity == "WB Low Income", "Low income", Entity))))) %>%
  group_by(Entity, Scenario) %>%
  arrange(Entity, Scenario, Year) %>%
  mutate(laborproductivity = laborproductivity*gdpconversion2011to2005USD) %>%
  select(-Unit) %>%
  mutate(laborprodgrowth = 100*(log(laborproductivity) - log(lag(laborproductivity))))


IFSSPs <- IFpcgdpWB %>%
  left_join(IFbirthWB) %>%
  left_join(IFlaborproductivityWB) %>%
  left_join(IFagedepWB) %>%
  left_join(IFlaborWB) %>%
  left_join(IFmanufacturingWB) %>%
  mutate_at(vars(Year),as.double)


CombinedTableIF <- bind_rows(CombinedTable %>%
                               mutate(Scenario = 'Observations'),
                             IFSSPs %>%
                               mutate(Code = 'WBGroups'),
                             WBKuznetsProjections %>%
                               mutate(Code = 'WBGroups'),
                             KuznetsFits %>%
                               mutate(Code = 'WBGroups')) %>%
  mutate(EntityScenario = paste(Entity, Scenario))

pcgdpprodgrowthtable <- CombinedTableIF %>%
  select(Entity, Code, Scenario, EntityScenario, TilmanEconomicGroup,
         WBEconomicGroup, Year, pcgdpthousand2005usd, pcgdpgrowth, laborprodgrowth) %>% # make a table with 11-y moving averages for pcgdp & productivity growth
  group_by(EntityScenario) %>%
  arrange(EntityScenario, Year) %>%
  mutate(pcgdpgrowth = ifelse(Scenario == 'Observations' & Code != 'WBGroups'
                              & Code != 'TilmanGroups',
                              rollmean(pcgdpgrowth, 11, fill = NA, align = "center"),
                              pcgdpgrowth),
         laborprodgrowth = ifelse(Scenario == 'Observations' & Code != 'WBGroups'
                                  & Code != 'TilmanGroups',
                              rollmean(laborprodgrowth, 11, fill = NA, align = "center"),
                              laborprodgrowth)) %>%
  filter(Code != 'TilmanGroups',
         Scenario == 'Observations' |
         Scenario == 'Kuznets fit',
         !is.na(TilmanEconomicGroup) |
           Code == 'WBGroups')

##################################################
########### IMF hindcast comparison ##############
##################################################

# Load WB Kuznets IMF comparison
KuznetsIMFProjectionv2 <- read_csv(here("BurgLangMoyeretal-growth-futures/Kuznets_Projections/Files_2022-4-6", 
                                      "forecast_kuznets_WB_Robustness-ICLastYear_20220223131753.csv")) %>%
  filter(Year < 2021) %>% #get rid of post-2020, since we are just looking at hindcasts here. 
  spread(value = pcGDP, key = Income) %>%
  left_join(pcgdpWB %>%
              select(Year, WBEconomicGroup,totalpop) %>%
              left_join(pcgdpWB %>%
                          filter(Year == 2019) %>%
                          select(WBEconomicGroup, totalpop) %>%
                          rename(pop2019 = totalpop)
              ) %>%
              mutate(totalpop = ifelse(Year == 2020, pop2019, totalpop)) %>% #eliminate quirk caused by 2020 rich-country pop decline
              select(-pop2019) %>%
              spread(key = WBEconomicGroup, value = totalpop) %>%
              rename(HIpop = `High income`,
                     LIpop = `Low income`,
                     UMIpop = `Upper-middle income`,
                     LMIpop = `Lower-middle income`)
  ) %>%
  mutate(World = ((`High income`*HIpop) +
                    (`Low income`*LIpop) +
                    (`Lower-middle income`*LMIpop) +
                    (`Upper-middle income`*UMIpop))/
           (HIpop+LIpop+UMIpop+LMIpop)) %>% # Add World projection using population weights on WB groups
  select(-HIpop, -LIpop, -UMIpop, -LMIpop) %>%
  gather(value = 'pcgdpthousand2005usd', key = 'Entity',
         -Method, -Year_IC, -Year, -Last_Year) %>%
  select(-Last_Year) %>%
  mutate(Scenario = paste(Method, Year_IC)) %>%
  group_by(Method, Scenario, Year_IC, Entity) %>%
  arrange(Method, Scenario, Year_IC, Entity, Year) %>%
  mutate(pcgdpgrowth = 100*(log(pcgdpthousand2005usd) - log(lag(pcgdpthousand2005usd)))) %>%
  mutate(EntityScenario = paste(Entity, "Kuznets", Scenario),
         Code = 'WBGroups',
         database = 'Kuznets Hindcasts')

### Version for box plot
KuznetsIMFProjection <- KuznetsIMFProjectionv2 %>%
  filter(Year > 2002) %>%
  mutate(`1-year projections` = ifelse(Year - Year_IC == 1,pcgdpgrowth,NA),
         `2-year projections` = ifelse(Year - Year_IC == 2,pcgdpgrowth,NA),
         `3-year projections` = ifelse(Year - Year_IC == 3,pcgdpgrowth,NA),
         `4-year projections` = ifelse(Year - Year_IC == 4,pcgdpgrowth,NA),
         `5-year projections` = ifelse(Year - Year_IC == 5,pcgdpgrowth,NA)) %>%
  group_by(Method, Entity, Year) %>%
  summarise(`1-year projections` = sum(`1-year projections`,na.rm = T),
            `2-year projections` = sum(`2-year projections`,na.rm = T),
            `3-year projections` = sum(`3-year projections`,na.rm = T),
            `4-year projections` = sum(`4-year projections`,na.rm = T),
            `5-year projections` = sum(`5-year projections`,na.rm = T)) %>%
  mutate(`1-year projections` = ifelse(`1-year projections` == 0,NA,`1-year projections`),
         `2-year projections` = ifelse(`2-year projections` == 0,NA,`2-year projections`),
         `3-year projections` = ifelse(`3-year projections` == 0,NA,`3-year projections`),
         `4-year projections` = ifelse(`4-year projections` == 0,NA,`4-year projections`),
         `5-year projections` = ifelse(`5-year projections` == 0,NA,`5-year projections`)) %>%
  mutate(Scenario = 'Kuznets',
         EntityScenario = paste(Entity, Method),
                   Code = 'WBGroups')

  
# Old version  
# KuznetsIMFProjection <- read_csv(here("BurgLangMoyeretal-growth-futures/Kuznets_Projections",
#                                    "Forecasts_IMFcomparison_20220203165816.csv")) %>%
#   left_join(pcgdpWB %>%
#               select(Year, WBEconomicGroup,totalpop) %>%
#               left_join(pcgdpWB %>%
#                           filter(Year == 2019) %>%
#                           select(WBEconomicGroup, totalpop) %>%
#                           rename(pop2019 = totalpop)
#                         ) %>%
#               mutate(totalpop = ifelse(Year == 2020, pop2019, totalpop)) %>% #eliminate quirk caused by 2020 rich-country pop decline
#               select(-pop2019) %>%
#               spread(key = WBEconomicGroup, value = totalpop) %>%
#               rename(HIpop = `High income`,
#                      LIpop = `Low income`,
#                      UMIpop = `Upper-middle income`,
#                      LMIpop = `Lower-middle income`)
#             ) %>%
#   mutate(World = ((`High income`*HIpop) +
#                     (`Low income`*LIpop) +
#                     (`Lower-middle income`*LMIpop) +
#                     (`Upper-middle income`*UMIpop))/
#            (HIpop+LIpop+UMIpop+LMIpop)) %>% # Add World projection using population weights on WB groups
#   select(-HIpop, -LIpop, -UMIpop, -LMIpop) %>%
#   gather(value = 'pcgdpthousand2005usd', key = 'Entity',
#          -Grouping, -Model, -IC_Year, -Year) %>%
#   group_by(Grouping, Model, IC_Year, Entity) %>%
#   arrange(Grouping, Model, IC_Year, Entity, Year) %>%
#   mutate(pcgdpgrowth = 100*(log(pcgdpthousand2005usd) - log(lag(pcgdpthousand2005usd)))) %>%
#   mutate(`1-year projections` = ifelse(Year - IC_Year == 1,pcgdpgrowth,NA),
#          `2-year projections` = ifelse(Year - IC_Year == 2,pcgdpgrowth,NA),
#          `3-year projections` = ifelse(Year - IC_Year == 3,pcgdpgrowth,NA),
#          `4-year projections` = ifelse(Year - IC_Year == 4,pcgdpgrowth,NA),
#          `5-year projections` = ifelse(Year - IC_Year == 5,pcgdpgrowth,NA)) %>%
#   group_by(Grouping, Model, Entity, Year) %>%
#   summarise(`1-year projections` = sum(`1-year projections`,na.rm = T),
#             `2-year projections` = sum(`2-year projections`,na.rm = T),
#             `3-year projections` = sum(`3-year projections`,na.rm = T),
#             `4-year projections` = sum(`4-year projections`,na.rm = T),
#             `5-year projections` = sum(`5-year projections`,na.rm = T)) %>%
#   mutate(`1-year projections` = ifelse(`1-year projections` == 0,NA,`1-year projections`),
#          `2-year projections` = ifelse(`2-year projections` == 0,NA,`2-year projections`),
#          `3-year projections` = ifelse(`3-year projections` == 0,NA,`3-year projections`),
#          `4-year projections` = ifelse(`4-year projections` == 0,NA,`4-year projections`),
#          `5-year projections` = ifelse(`5-year projections` == 0,NA,`5-year projections`)) %>%
#   gather(key = 'Scenario', value = pcgdpgrowth, -Grouping, -Model, -Entity, -Year) %>%
#   mutate(EntityScenario = paste(Entity, "Kuznets", Scenario),
#          Code = 'WBGroups')

# Load IMF projections
IMFProjection <- read_csv(here("BurgLangMoyeretal-growth-futures/Kuznets_Projections", 
                               "imfwbregionscompiled-skewness.csv")) %>%
  filter(!is.na(WBEconomicGroup)) %>%
  rename(pcgdpgrowth = pcgdppppgrowthproj,
         Entity = WBEconomicGroup,
         Year = year) %>%
  mutate(database = "IMF",
         Code = "WBGroups",
         Scenario = ifelse(yearsaheadproj <= 0, "Observations",
                           ifelse(yearsaheadproj == 1, "1-year projections",
                                  ifelse(yearsaheadproj == 2, "2-year projections",
                                         ifelse(yearsaheadproj == 3, "3-year projections",
                                                ifelse(yearsaheadproj == 4, "4-year projections",
                                                       ifelse(yearsaheadproj == 5, "5-year projections",
                                                              NA))))))) %>%
  select(database, Entity, Code, Scenario, Year, pcgdpgrowth) %>%
  mutate(EntityScenario = paste(Entity, "IMF", Scenario))

CombinedTableAll <- bind_rows(CombinedTableIF,
                              KuznetsIMFProjection %>%
                                filter(Year > 2020),
                              IMFProjection  %>%
                                filter(Year > 2020)) %>%
  filter(Code != 'TilmanGroups',
         !is.na(TilmanEconomicGroup) |
           Code == 'WBGroups')

##########################################################
########### Range of global pcGDP forecasts ##############
##########################################################

# Make table with just global pcGDP and growth projections from SSPs, 
# plus historical observations
fig1tableScenarios <- CombinedTableIF %>%
  filter(Entity == 'World',
         Scenario != 'IF',
         grepl('Kuznets',EntityScenario) == F,
         grepl('IMF',EntityScenario) == F) %>%
  select(Entity, Code, EntityScenario, Year, pcgdpthousand2005usd, pcgdpgrowth)

# Add Christensen 10th, 25th, 50th, 75th, and 90th percentiles  
ChristensenTable10th <- fig1tableScenarios %>%
  filter(Year == 2010) %>%
  ungroup() %>%
  add_row(Entity = 'World',
          Code = 'OWID_WRL',
          EntityScenario = NA,
          Year = 2100,
          pcgdpthousand2005usd = NA, 
          pcgdpgrowth = NA) %>%
  complete(Year = full_seq(Year, 1),Entity, Code) %>% # expand table to annual
  arrange(Entity,Code, Year) %>%
  mutate(EntityScenario = ifelse(Year == 2010, EntityScenario, 
                                 'World Christensen 10th')) %>%
  mutate(pcgdpgrowth = ifelse(Year == 2010, pcgdpgrowth, 
                              0.6)) %>%
  mutate(pcgdpthousand2005usd = ifelse(Year == 2010, pcgdpthousand2005usd, 
                              10.871432*exp(0.01*(Year - 2010)*pcgdpgrowth)))

ChristensenTable25th <- fig1tableScenarios %>%
  filter(Year == 2010) %>%
  ungroup() %>%
  add_row(Entity = 'World',
          Code = 'OWID_WRL',
          EntityScenario = NA,
          Year = 2100,
          pcgdpthousand2005usd = NA, 
          pcgdpgrowth = NA) %>%
  complete(Year = full_seq(Year, 1),Entity, Code) %>% # expand table to annual
  arrange(Entity,Code, Year) %>%
  mutate(EntityScenario = ifelse(Year == 2010, EntityScenario, 
                                 'World Christensen 25th')) %>%
  mutate(pcgdpgrowth = ifelse(Year == 2010, pcgdpgrowth, 
                              1.36)) %>%
  mutate(pcgdpthousand2005usd = ifelse(Year == 2010, pcgdpthousand2005usd, 
                                       10.871432*exp(0.01*(Year - 2010)*pcgdpgrowth)))

ChristensenTable50th <- fig1tableScenarios %>%
  filter(Year == 2010) %>%
  ungroup() %>%
  add_row(Entity = 'World',
          Code = 'OWID_WRL',
          EntityScenario = NA,
          Year = 2100,
          pcgdpthousand2005usd = NA, 
          pcgdpgrowth = NA) %>%
  complete(Year = full_seq(Year, 1),Entity, Code) %>% # expand table to annual
  arrange(Entity,Code, Year) %>%
  mutate(EntityScenario = ifelse(Year == 2010, EntityScenario, 
                                 'World Christensen 50th')) %>%
  mutate(pcgdpgrowth = ifelse(Year == 2010, pcgdpgrowth, 
                              2.03)) %>%
  mutate(pcgdpthousand2005usd = ifelse(Year == 2010, pcgdpthousand2005usd, 
                                       10.871432*exp(0.01*(Year - 2010)*pcgdpgrowth)))

ChristensenTable75th <- fig1tableScenarios %>%
  filter(Year == 2010) %>%
  ungroup() %>%
  add_row(Entity = 'World',
          Code = 'OWID_WRL',
          EntityScenario = NA,
          Year = 2100,
          pcgdpthousand2005usd = NA, 
          pcgdpgrowth = NA) %>%
  complete(Year = full_seq(Year, 1),Entity, Code) %>% # expand table to annual
  arrange(Entity,Code, Year) %>%
  mutate(EntityScenario = ifelse(Year == 2010, EntityScenario, 
                                 'World Christensen 75th')) %>%
  mutate(pcgdpgrowth = ifelse(Year == 2010, pcgdpgrowth, 
                              2.85)) %>%
  mutate(pcgdpthousand2005usd = ifelse(Year == 2010, pcgdpthousand2005usd, 
                                       10.871432*exp(0.01*(Year - 2010)*pcgdpgrowth)))

ChristensenTable90th <- fig1tableScenarios %>%
  filter(Year == 2010) %>%
  ungroup() %>%
  add_row(Entity = 'World',
          Code = 'OWID_WRL',
          EntityScenario = NA,
          Year = 2100,
          pcgdpthousand2005usd = NA, 
          pcgdpgrowth = NA) %>%
  complete(Year = full_seq(Year, 1),Entity, Code) %>% # expand table to annual
  arrange(Entity,Code, Year) %>%
  mutate(EntityScenario = ifelse(Year == 2010, EntityScenario, 
                                 'World Christensen 90th')) %>%
  mutate(pcgdpgrowth = ifelse(Year == 2010, pcgdpgrowth, 
                              3.47)) %>%
  mutate(pcgdpthousand2005usd = ifelse(Year == 2010, pcgdpthousand2005usd, 
                                       10.871432*exp(0.01*(Year - 2010)*pcgdpgrowth)))

  
fig1table <- bind_rows(fig1tableScenarios,
                       ChristensenTable10th,
                       ChristensenTable25th,
                       ChristensenTable50th,
                       ChristensenTable75th,
                       ChristensenTable90th) 

########################################
############ Figure 4 ##################
########################################

sspmarkers <- sspdf %>% 
  filter((SCENARIO == 'SSP1-19' & grepl("IMAGE", MODEL)) |
           (SCENARIO == 'SSP1-26' & grepl("IMAGE", MODEL)) |
           (SCENARIO == 'SSP2-34' & grepl("MESSAGE", MODEL)) |
           (SCENARIO == 'SSP2-45' & grepl("MESSAGE", MODEL)) |
           (SCENARIO == 'SSP4-60' & grepl("GCAM", MODEL)) |
           (SCENARIO == 'SSP3-Baseline' & grepl("AIM", MODEL)) |
           (SCENARIO == 'SSP5-Baseline' & grepl("REMIND", MODEL))) %>%
  filter(REGION == 'World',
         VARIABLE == 'Primary Energy' | # EJ/yr
          VARIABLE == 'Emissions|CO2') %>% # Mt CO2/yr
  select(-UNIT) %>%
  gather(key = 'Year', value = 'value', -MODEL, 
         -SCENARIO, -REGION, -VARIABLE) %>%
  spread(key = VARIABLE, value = value) %>%
  rename(Entity = REGION,
         Scenario = SCENARIO,
         co2emissions = `Emissions|CO2`,
         energy = `Primary Energy`) %>%
  select(Entity, Scenario, Year, energy, co2emissions) %>%
  mutate_at(vars(Year),as.double)

fig4table <- read_csv(here("BurgLangMoyeretal-growth-futures/Fig-4-inputs", 
                           "fig4-IFs-input.csv")) %>%
  mutate(co2emissions = 3.664*`Carbon-emissions-Btons`*1000,
         energy = 6.11786*`Energy-BBOE`,
         population = `Population-M`/1000) %>%
  bind_rows(sspmarkers)

#########################################################################
############ IMF-Kuznets comparison using pcGDP levels ##################
#########################################################################
# imfprojtrajectories <- IMFProjection %>%
#   ungroup() %>%
#   bind_rows(pcgdpWB %>%
#               ungroup() %>%
#               rename(Entity = WBEconomicGroup) %>%
#               mutate(database = 'Observations',
#                      Scenario = 'Observations') %>%
#               select(database, Entity, Scenario,
#                      Year, pcgdpgrowth),
#             pcgdpWorld %>%
#               ungroup() %>%
#               mutate(database = 'Observations',
#                      Scenario = 'Observations') %>%
#               select(database, Entity, Code, Scenario,
#                      Year, pcgdpgrowth)) %>%
#   select(-Code, -EntityScenario, -database) %>%
#   spread(value = pcgdpgrowth, key = Scenario)


########################################
########### Export Tables ##############
########################################

# Table for fig1
write_csv(fig1table,
          "BurgLangMoyeretal-growth-futures/fig1table.csv")

# Table for fig2a, Extended Data fig-a,c,e
fig2aEDacetable <- pcgdpprodgrowthtable %>%
  left_join(covariatescombined %>%
              select(Entity, Code, WBEconomicGroup, TilmanEconomicGroup, Year,
                     fertility, EmploymentServices, EmploymentIndustry)) %>%
  filter(grepl("Tilman", EntityScenario) == F) %>%
  mutate(EmploymentIndustry = ifelse(Year > 2019, NA, EmploymentIndustry),
         EmploymentServices = ifelse(Year > 2019, NA, EmploymentServices)) # remove missing years


write_csv(fig2aEDacetable,
          "BurgLangMoyeretal-growth-futures/fig2aEDacetable.csv")

# V1 Table for fig2e
# fig2etable <- bind_rows(IMFProjection %>%
#                           filter(Entity == 'World'),
#                         KuznetsIMFProjection %>%
#                           filter(Entity == 'World'),
#                         fig1tableScenarios %>%
#                           filter(EntityScenario == 'World Observations') %>%
#                           select(-pcgdpthousand2005usd)) %>%
#   filter(Year < 2021) %>%
#   filter(grepl('Kuznets', EntityScenario) == F | 
#            Year != 2020) # remove Kuznets 2020 projections, which are affected by the population quirk
# 
# write_csv(fig2etable,
#           "BurgLangMoyeretal-growth-futures/fig2etable.csv")

  
# V1 Table for fig2f
# fig2ftable <- bind_rows(IMFProjection %>%
#                           filter(Entity != 'World'),
#                         KuznetsIMFProjection %>%
#                           filter(Entity != 'World'),
#                      pcgdpcombined %>%
#                        filter(Code == 'WBGroups') %>%
#                        mutate(Scenario = 'Observations') %>%
#                        mutate(EntityScenario = paste(Entity, Scenario))) %>%
#   select(Entity, Scenario, EntityScenario, Year, pcgdpgrowth)
# 
# write_csv(fig2ftable,
#             "BurgLangMoyeretal-growth-futures/fig2ftable.csv")
        

# Tables for v2 fig. 2 IMF vs Kuznets
fig2IMFWorldtable <- bind_rows(IMFProjection %>%
                            filter(Entity == 'World'),
                          KuznetsIMFProjectionv2 %>%
                            filter(Entity == 'World',
                                   Year_IC == 1980 |
                                     Year_IC == 1990 |
                                     Year_IC == 2000 |
                                     Year_IC == 2010),
                          fig1tableScenarios %>%
                            filter(EntityScenario == 'World Observations') %>%
                            select(-pcgdpthousand2005usd)) %>%
  filter(Year < 2021) %>%
  filter(grepl('Kuznets', EntityScenario) == F | 
           Year != 2020) %>% # remove Kuznets 2020 projections, which are affected by the population quirk 
  select(Entity, Scenario, EntityScenario, Year, pcgdpgrowth)

write_csv(fig2IMFWorldtable,
          "BurgLangMoyeretal-growth-futures/fig2IMFWorldtable.csv")

fig2IMFWBRegstable <-  bind_rows(IMFProjection %>%
                             filter(Entity != 'World'),
                           KuznetsIMFProjectionv2 %>%
                             filter(Entity != 'World',
                                    Year_IC == 1980 |
                                      Year_IC == 1990 |
                                      Year_IC == 2000 |
                                      Year_IC == 2010),
                           pcgdpcombined %>%
                             filter(Code == 'WBGroups') %>%
                             mutate(Scenario = 'Observations') %>%
                             mutate(EntityScenario = paste(Entity, Scenario))) %>%
  select(Entity, Scenario, EntityScenario, Year, pcgdpgrowth)

write_csv(fig2IMFWBRegstable,
          "BurgLangMoyeretal-growth-futures/fig2IMFWBRegstable.csv")

## Table for v2 of fig. 2b: different Kuznets projections vs. SSPs
# Add 2020-2100 for the world to Kuznets projections
Kuznetsforwardprojections <-  read_csv(here("BurgLangMoyeretal-growth-futures/Kuznets_Projections/Files_2022-4-6", 
                                            "forecast_kuznets_WB_Robustness-ICLastYear_20220223131753.csv")) %>%
  rename(pcgdpthousand2005usd = pcGDP,
         Entity = Income) %>%  
  spread(value = pcgdpthousand2005usd, key = Entity ) %>%
  rename(pcgdpHigh = `High income`,
         pcgdpUMI = `Upper-middle income`,
         pcgdpLMI = `Lower-middle income`,
         pcgdpLow = `Low income`) %>%
  filter(Year > 2019) %>%
  left_join(
    read_csv(here("BurgLangMoyeretal-growth-futures/Kuznets_Projections/Files_PopulationWeights_Projection", 
                  "PopulationWeights_WBgroups_Projection_UNmedium.csv")) %>%
      rename(popHigh = `High income`,
             popUMI = `Upper-middle income`,
             popLMI = `Lower-middle income`,
             popLow = `Low income`)
  ) %>%
  mutate(
    pcgdpthousand2005usd = 
      ((pcgdpHigh*popHigh)+(pcgdpUMI*popUMI)+(pcgdpLMI*popLMI)+(pcgdpLow*popLow))/
      (popHigh+popUMI+popLMI+popLow)
  ) %>%
  mutate(Entity = 'World',
         Scenario = paste(Method, Year_IC),
         EntityScenario = paste(Entity, Scenario)) %>%
  select(-pcgdpHigh, -pcgdpUMI, -pcgdpLMI, -pcgdpLow,
         -popHigh, -popUMI, -popLMI, -popLow) %>%
  bind_rows(
    read_csv(here("BurgLangMoyeretal-growth-futures/Kuznets_Projections/Files_2022-4-6", 
                  "forecast_kuznets_WB_Robustness-ICLastYear_20220223131753.csv")) %>%
      rename(pcgdpthousand2005usd = pcGDP,
             Entity = Income) %>%
      filter(Year > 2019) %>%
      mutate(Scenario = paste(Method, Year_IC),
             EntityScenario = paste(Entity, Scenario))
  )

fig2btablev2 <- bind_rows(fig1tableScenarios,
                          Kuznetsforwardprojections %>%
                            filter(Entity == 'World',
                                   Year_IC == 1980 |
                                     Year_IC == 1990 |
                                     Year_IC == 2000 |
                                     Year_IC == 2010 |
                                     Year_IC == 2020 ),
                          KuznetsIMFProjectionv2 %>%
                            mutate(EntityScenario = paste(Entity, Scenario)) %>%
                            select(-pcgdpgrowth) %>%
                            filter(Entity == 'World',
                                   Year_IC == 1980 |
                                     Year_IC == 1990 |
                                     Year_IC == 2000 |
                                     Year_IC == 2010 |
                                     Year_IC == 2020 ))

write_csv(fig2btablev2,
          "BurgLangMoyeretal-growth-futures/fig2btable-v2.csv")

# Table for Extended Data Fig. 1:
extendeddatafig6atable <- bind_rows(IMFProjection %>%
                            filter(Entity != 'World') %>%
                            rename(projectedpcgdpgrowth = pcgdpgrowth) %>%
                              mutate(Method = 'IMF'),
                          KuznetsIMFProjection %>%
                            filter(Entity != 'World') %>%
                            select(-Scenario) %>%
                            gather(key = 'Scenario', value = 'projectedpcgdpgrowth',
                                   -Method, -Entity, -Year, -Code, -EntityScenario) %>%
                            left_join(IMFProjection %>% # keep only or identify years with IMF projections
                                        filter(Entity == 'High income') %>%
                                        select(Scenario, Year) %>%
                                        mutate(hasIMF = 'Y')) %>%
                            filter(hasIMF == 'Y') %>% 
                            select(-hasIMF)
                          ) %>%
  left_join(
    pcgdpcombined %>%
      ungroup() %>%
      filter(Code == 'WBGroups') %>%
      select(Entity, Year, pcgdpgrowth)
  ) %>%
  mutate(growtherror = projectedpcgdpgrowth - pcgdpgrowth)
  
write_csv(extendeddatafig6atable,
          "BurgLangMoyeretal-growth-futures/extendeddatafig6atable.csv")

extendeddatafig7table <- KuznetsIMFProjectionv2 %>%
  filter(Entity != 'World',
         Year_IC == 1980 |
           Year_IC == 1990 |
           Year_IC == 2000 |
           Year_IC == 2010) %>%
  rename(projpcgdpgrowth = pcgdpgrowth,
         projpcgdpthousand2005usd = pcgdpthousand2005usd) %>%
  left_join(
    pcgdpcombined %>%
      ungroup() %>%
      filter(Code == 'WBGroups') %>%
      select(Entity, Year, pcgdpthousand2005usd, pcgdpgrowth)
  ) %>%
  mutate(pcgdpgrowtherror = projpcgdpgrowth - pcgdpgrowth,
         pcgdperror = projpcgdpthousand2005usd - pcgdpthousand2005usd,
         pcgdperrorpct = 100*(projpcgdpthousand2005usd - pcgdpthousand2005usd)/pcgdpthousand2005usd)
  

write_csv(extendeddatafig7table,
          "BurgLangMoyeretal-growth-futures/extendeddatafig7table.csv")

# Tables for fig3
fig3table <- CombinedTableIF %>%
  filter(Code == 'WBGroups' |
           Code == 'OWID_WRL') %>%
  filter(grepl("Tilman", EntityScenario) == F) %>%
  select(Entity, Code, Scenario, EntityScenario, 
         Year, pcgdpthousand2005usd, pcgdpgrowth, fertility, EmploymentServices, EmploymentIndustry)

fig3aEDbdftable <- fig3table %>%
  filter(grepl('Kuznets', Scenario) == F |
           grepl('projection', Scenario) == F ) %>%
  filter(Entity != 'World')

write_csv(fig3aEDbdftable,
          "BurgLangMoyeretal-growth-futures/fig3aEDbdftable.csv")

fig3btable <- fig3table %>% 
  filter(Entity == 'World')

write_csv(fig3btable,
          "BurgLangMoyeretal-growth-futures/fig3btable.csv")

fig3cftable <- fig3table %>% 
  filter(Entity != 'World') %>%
  filter(Scenario != 'Kuznets fit')

write_csv(fig3cftable,
          "BurgLangMoyeretal-growth-futures/fig3cftable.csv")

# Table for fig4
write_csv(fig4table,
          "BurgLangMoyeretal-growth-futures/fig4table.csv")
#####################################


## Write larger tables, if desired
#write_csv(pcgdpcombined,
#          "BurgLangMoyeretal-growth-futures/pcgdptable.csv")

#write_csv(CombinedTable,
 #         "BurgLangMoyeretal-growth-futures/combinedtable.csv")

#write_csv(CombinedTableIF,
#          "BurgLangMoyeretal-growth-futures/combinedtableIF.csv")

#write_csv(CombinedTableAll,
#          "BurgLangMoyeretal-growth-futures/combinedtableAll.csv")

#write_csv(pcgdpprodgrowthtable,
 #         "BurgLangMoyeretal-growth-futures/growthtable.csv")

# write_csv(covariatescombined, 
#          "BurgLangMoyeretal-growth-futures/covariates.csv")
