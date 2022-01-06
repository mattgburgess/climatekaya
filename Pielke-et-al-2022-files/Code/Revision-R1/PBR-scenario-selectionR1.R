##################################
########### Background ###########
##################################

# This script runs the scenario filtering analysis 
# and Kaya decomposition described in:
# Pielke Jr., Burgess, and Ritchie:
# "Plausible 2005-2050 emissions scenarios project between 2 and 3 degrees C of warming by 2100".
# The code generates lists of scenarios meeting each filter ("scenariolistR1.csv")
# and also stores csv tables used (via JMP) to make 
# Figs. S4, S5, S6 ("kayaplotR1", 
# for each of 2005-2019, 2005-2020, 2005-2050 STEPS, and 2005-2050 APS). 
# The jsl scripts used to make these figures are also stored in the github repo.
# The other figures were made by J.R., using an Excel file also in the github repo, 
# in the folder marked accordingly.

# This is the second version of this analysis, updated in Nov. 2021 following the publication
# of the IEA WEO 2021. The main difference from the previous version (published on SocAriXiv)
# is that IEA now projects fossil fuel and industry (FFI) emissions, instead of just fossil-fuel
# emissions. They also project to 2050 instead of 2040. Thus, our filters are updated to filter on
# FFI growth rates, and compare scenarios from 2005-2050, instead of 2005-2040. We also use
# different data sets for the observations, to include data to 2020, and also to include observed FFI emissions.
# The previous dataset from IEA only reported fossil-duel emissions as well.


############################
########### Code ###########
############################

### Clear environment with command below:
# rm(list = ls())

#####################################
########### LOAD PACKAGES ###########
#####################################

library(tidyverse)
library(forcats)
library(here)
library(readxl)
library(purrr)

#####################################
########### LOAD Data ###############
#####################################

### AR5 scenario database
ar5df <- read_csv(here("Data", "ar5_public_version102_compare_compare_20150629-130000.csv"))

### SSP database
sspdf <- read_csv(here("Data", "SSP_IAM_V2_201811.csv"))

### IPCC Regions classification: IEA countries -> IPCC Regions lookup table
ipccregionlookup <- read_csv(here("Data", "IEA-IPCC-region-lookup.csv"))

### AR5 baseline scenarios lookup table
ipccscenariolist <- read_csv(here("Data", "ipcc-scenario classification.dat")) %>%
  select(MODEL,SCENARIO,`Baseline Scenario`)

### Obervations
## FFI CO2 emissions
# Source: https://ourworldindata.org/co2-emissions
co2ffiobs <- read_csv(here("Data/2020-Kaya-observations-update", "annual-co2-emissions-per-country-ffi.csv")) %>%
  filter(Entity == 'World') %>% # filter to world 
  rename(ffico2 = `Annual CO2 emissions`) %>%
  filter(Year == 2005 | Year == 2019 | Year == 2020) %>% # calculate 2005-2020 growth rate
  spread(key = 'Year', value = ffico2) %>%
  mutate(growth0520 = 100*(log(`2020`)-log(`2005`))/15,
         growth0519 = 100*(log(`2019`)-log(`2005`))/14)

co2ffi0519growth <- co2ffiobs$growth0519[1] # store 2005-2020 global CO2 growth rate
co2ffi0520growth <- co2ffiobs$growth0520[1] # store 2005-2020 global CO2 growth rate
co2ffi2050growthieasteps <- -0.024772629 # See 'IEAWEO2021-KayaFactors-v3.xlsx'
co2ffi0550growthobsiea <- (15/45)*co2ffi0520growth + (30/45)*co2ffi2050growthieasteps

## Population:
# Source: https://ourworldindata.org/world-population-growth
popobs <- read_csv(here("Data/2020-Kaya-observations-update", 
                        "population-since-1800.csv")) %>%
  filter(Entity == 'World') %>% # filter to world 
  rename(pop = `Population (historical estimates)`) %>%
  filter(Year == 2005 | Year == 2019 | Year == 2020) %>% # calculate 2005-2020 growth rate
  spread(key = 'Year', value = pop) %>%
  mutate(growth0520 = 100*(log(`2020`)-log(`2005`))/15,
         growth0519 = 100*(log(`2019`)-log(`2005`))/14)

pop0519growth <- popobs$growth0519[1]
pop0520growth <- popobs$growth0520[1] # store 2005-2020 global population growth rate
pop2050growthieasteps <- 0.744069926 # See 'IEAWEO2021-KayaFactors-v3.xlsx'
pop0550growthobsiea <-(15/45)*pop0520growth + (30/45)*pop2050growthieasteps


## GDP per capita (PPP)
# Source: https://ourworldindata.org/grapher/gdp-per-capita-worldbank
pcgdppppobs <- read_csv(here("Data/2020-Kaya-observations-update", 
                        "gdp-per-capita-worldbank.csv")) %>%
  filter(Entity == 'World') %>% # filter to world 
  rename(pcgdpppp = `GDP per capita, PPP (constant 2017 international $)`) %>%
  filter(Year == 2005 | Year == 2019 | Year == 2020) %>% # calculate 2005-2020 growth rate
  spread(key = 'Year', value = pcgdpppp) %>%
  mutate(growth0520 = 100*(log(`2020`)-log(`2005`))/15,
         growth0519 = 100*(log(`2019`)-log(`2005`))/14)

pcgdpppp0519growth <- pcgdppppobs$growth0519[1]
pcgdpppp0520growth <- pcgdppppobs$growth0520[1] # store 2005-2020 global population growth rate

gdpppp2050ieasteps <- 2.955880224
pcgdpppp2050growthieasteps <- gdpppp2050ieasteps - pop2050growthieasteps # See 'IEAWEO2021-KayaFactors-v3.xlsx'
pcgdpppp0550growthobsiea <-(15/45)*pcgdpppp0520growth + (30/45)*pcgdpppp2050growthieasteps


## GDP per capita (MER)
# Source: https://data.worldbank.org/indicator/NY.GDP.PCAP.KD
pcgdpmerobs <- read_csv(here("Data/2020-Kaya-observations-update", 
                             "WB-gdppc-mer.csv")) %>%
  filter(`Country Name` == 'World') %>% # filter to world 
  mutate(growth0520 = 100*(log(`2020`)-log(`2005`))/15,
         growth0519 = 100*(log(`2019`)-log(`2005`))/14)

pcgdpmer0519growth <- pcgdpmerobs$growth0519[1]
pcgdpmer0520growth <- pcgdpmerobs$growth0520[1] # store 2005-2020 global population growth rate

pcgdpmer0550growthobsiea <-(15/45)*pcgdpmer0520growth + 
  (30/45)*pcgdpppp2050growthieasteps # note that this has the problem of mixing MER and PPP measures


## Primary energy
# Source: https://ourworldindata.org/energy-production-consumption
# Source for 2020 primary energy growth: https://www.bp.com/en/global/corporate/energy-economics/statistical-review-of-world-energy/primary-energy.html
energyobs <- read_csv(here("Data/2020-Kaya-observations-update", "global-energy-substitution.csv")) %>%
  gather(value = "primaryenergyTWh", key = "source", -Entity, -Code, -Year) %>%
  mutate(primaryenergyEJ = primaryenergyTWh/277.778) %>%
  group_by(Entity, Code, Year) %>%
  summarise(primaryenergyEJ = sum(primaryenergyEJ,na.rm = T)) %>%
  filter(Year == 2005 | Year == 2019) %>% # calculate 2005-2019 growth rate
  spread(key = 'Year', value = primaryenergyEJ) %>%
  mutate(growth0519 = 100*(log(`2019`)-log(`2005`))/15)

energy0519growth <- energyobs$growth0519[1]
energy0520growth <- (14/15)*energyobs$growth0519[1] + (1/15)*(100*(log(0.955)-log(1))) # 2020 growth rate from BP, converted to continuous units
energy2050growthieasteps <- 0.777702229 # See 'IEAWEO2021-KayaFactors-v3.xlsx'
energy0550growthobsiea <- (15/45)*energy0520growth + (30/45)*energy2050growthieasteps

## Energy intensity growth rates PPP
energyintensityppp0519growth <- energy0519growth - pcgdpppp0519growth - pop0519growth
energyintensityppp0520growth <- energy0520growth - pcgdpppp0520growth - pop0520growth
energyintensityppp2050growthieasteps <- energy2050growthieasteps - pcgdpppp2050growthieasteps - pop2050growthieasteps
energyintensityppp0550growthobsiea <- energy0550growthobsiea - pcgdpppp0550growthobsiea - pop0550growthobsiea

## Energy intensity growth rates MER
energyintensitymer0519growth <- energy0519growth - pcgdpmer0519growth - pop0519growth
energyintensitymer0520growth <- energy0520growth - pcgdpmer0520growth - pop0520growth
energyintensitymer2050growthieasteps <- energy2050growthieasteps - pcgdpppp2050growthieasteps - pop2050growthieasteps
energyintensitymer0550growthobsiea <- energy0550growthobsiea - pcgdpmer0550growthobsiea - pop0550growthobsiea

## Carbon intensity growth rates
carbonintensity0519growth <- co2ffi0519growth - energy0519growth
carbonintensity0520growth <- co2ffi0520growth - energy0520growth
carbonintensity2050growthieasteps <- co2ffi2050growthieasteps - energy2050growthieasteps
carbonintensity0550growthobsiea <- co2ffi0550growthobsiea - energy0550growthobsiea

### IEA APS scenario 
# IEA APS growth only differs from STEPS
# in energy and carbon (not GDP or population)
co2ffi2050growthieaaps <- -1.665174143
energy2050growthieaaps <- 0.450758194
co2ffi0550growthobsieaaps <- (15/45)*co2ffi0520growth + (30/45)*co2ffi2050growthieaaps

energyintensityppp2050growthieaaps <- energy2050growthieaaps - pcgdpppp2050growthieasteps - pop2050growthieasteps
energyintensityppp0550growthobsieaaps <- (15/45)*energyintensityppp0520growth + (30/45)*energyintensityppp2050growthieaaps

energyintensitymer2050growthieaaps <- energy2050growthieaaps - pcgdpppp2050growthieasteps - pop2050growthieasteps
energyintensitymer0550growthobsieaaps <- (15/45)*energyintensitymer0520growth + (30/45)*energyintensitymer2050growthieaaps

carbonintensity2050growthieaaps <- co2ffi2050growthieaaps - energy2050growthieasteps
carbonintensity0550growthobsieaaps <- (15/45)*carbonintensity0520growth + (30/45)*carbonintensity2050growthieaaps

###### Calculating growth 'errors' and 'divergences'
# Add in Baseline scenario column
ipccscenariolist <- read_csv(here("Data", "ipcc-scenario classification.dat")) %>%
  select(MODEL,SCENARIO,`Baseline Scenario`)

# AR5 Global FFI CO2 for error and divergence calculations
ar5ffico2errorsdivergences <- ar5df %>%
  filter(REGION == 'World',
         VARIABLE == 'Emissions|CO2|Fossil Fuels and Industry') %>%
  select(MODEL,SCENARIO,REGION,VARIABLE,UNIT,`2005`,`2020`,`2050`) %>%
  mutate(growth0520 = 100*(log(`2020`)-log(`2005`))/15,
         growth0550 = 100*(log(`2050`)-log(`2005`))/45,
         divergence0519 = growth0520 - co2ffi0519growth,
         divergence0520 = growth0520 - co2ffi0520growth,
         divergence0550steps = growth0550 - co2ffi0550growthobsiea,
         divergence0550aps = growth0550 - co2ffi0550growthobsieaaps) %>%
  left_join(ipccscenariolist) %>%
  rename(scenariotype = `Baseline Scenario`) %>%
  mutate(Database = 'AR5')

# SSP Global FFI CO2 for error and divergence calculations
sspffico2errorsdivergences <- sspdf %>%
  filter(REGION == 'World',
         VARIABLE == 'Emissions|CO2|Fossil Fuels and Industry') %>%
  select(MODEL,SCENARIO,REGION,VARIABLE,UNIT,`2005`,`2020`,`2050`) %>%
  mutate(growth0520 = 100*(log(`2020`)-log(`2005`))/15,
         growth0550 = 100*(log(`2050`)-log(`2005`))/45,
         divergence0519 = growth0520 - co2ffi0519growth,
         divergence0520 = growth0520 - co2ffi0520growth,
         divergence0550steps = growth0550 - co2ffi0550growthobsiea,
         divergence0550aps = growth0550 - co2ffi0550growthobsieaaps) %>%
  mutate(scenariotype = ifelse(grepl("Baseline", SCENARIO) == T,'Baseline','Policy')) %>%
  mutate(Database = 'SSP')

### Scenario filter lists
scenariolist <- bind_rows(ar5ffico2errorsdivergences,
                          sspffico2errorsdivergences) %>%
  mutate(absdivergence0519 = abs(divergence0519),
         absdivergence0520 = abs(divergence0520),
         absdivergence0550steps = abs(divergence0550steps),
         absdivergence0550aps = abs(divergence0550aps),
         tolerance01in2005to2019 = ifelse(absdivergence0519 < 0.1,'Y','N'),
         tolerance01in2005to2020 = ifelse(absdivergence0520 < 0.1,'Y','N'),
         tolerance01in2005to2050steps = ifelse(absdivergence0550steps < 0.1,'Y','N'),
         tolerance01in2005to2050aps = ifelse(absdivergence0550aps < 0.1,'Y','N'),
         tolerance03in2005to2019 = ifelse(absdivergence0519 < 0.3,'Y','N'),
         tolerance03in2005to2020 = ifelse(absdivergence0520 < 0.3,'Y','N'),
         tolerance03in2005to2050steps = ifelse(absdivergence0550steps < 0.3,'Y','N'),
         tolerance03in2005to2050aps =  ifelse(absdivergence0550aps < 0.3,'Y','N'))

write_csv(scenariolist,
          "Pielke-et-al-2021-files/Output-from-R-code/Revision-R1-files/scenariolistR1.csv")

# AR5 Global Kaya comparisons

# 2005-2020 and 2005-2050 comparisons
ar5kayadivergences <- ar5df %>%
  filter(REGION == 'World',
         VARIABLE == 'Emissions|CO2|Fossil Fuels and Industry' |
           VARIABLE == 'GDP|MER' |
           VARIABLE == 'Population' |
           VARIABLE == 'Primary Energy') %>%
  select(MODEL,SCENARIO,REGION,VARIABLE,UNIT,`2005`,`2020`) %>%
  mutate(growth0520 = 100*(log(`2020`)-log(`2005`))/15) %>%
  select(MODEL,SCENARIO,REGION,VARIABLE,growth0520) %>%
  spread(key = VARIABLE, value = growth0520) %>%
  rename(co2ffi = `Emissions|CO2|Fossil Fuels and Industry`,
         gdpmer = `GDP|MER`,
         population = `Population`,
         energy = `Primary Energy`) %>%
  mutate(pcgdpmer = gdpmer - population,
         energyintensitymer = energy - gdpmer,
         carbonintensity = co2ffi - energy) %>%
  select(MODEL,SCENARIO,REGION,co2ffi,population,
         pcgdpmer,energyintensitymer,carbonintensity) %>%
  gather(key = 'variable', value = 'growth0520', -MODEL,-SCENARIO,-REGION) %>%
  left_join(
    ar5df %>%
      filter(REGION == 'World',
             VARIABLE == 'Emissions|CO2|Fossil Fuels and Industry' |
               VARIABLE == 'GDP|MER' |
               VARIABLE == 'Population' |
               VARIABLE == 'Primary Energy') %>%
      select(MODEL,SCENARIO,REGION,VARIABLE,UNIT,`2005`,`2050`) %>%
      mutate(growth0550 = 100*(log(`2050`)-log(`2005`))/45) %>%
      select(MODEL,SCENARIO,REGION,VARIABLE,growth0550) %>%
      spread(key = VARIABLE, value = growth0550) %>%
      rename(co2ffi = `Emissions|CO2|Fossil Fuels and Industry`,
             gdpmer = `GDP|MER`,
             population = `Population`,
             energy = `Primary Energy`) %>%
      mutate(pcgdpmer = gdpmer - population,
             energyintensitymer = energy - gdpmer,
             carbonintensity = co2ffi - energy) %>%
      select(MODEL,SCENARIO,REGION,co2ffi,population,
             pcgdpmer,energyintensitymer,carbonintensity) %>%
      gather(key = 'variable', value = 'growth0550', -MODEL,-SCENARIO,-REGION)
  ) %>%
  mutate(
    divergence0519 = ifelse(variable == 'co2ffi',
                            growth0520 - co2ffi0519growth,
                            ifelse(variable == 'population',
                                   growth0520 - pop0519growth,
                                   ifelse(variable == 'pcgdpmer',
                                          growth0520 - pcgdpmer0519growth,
                                          ifelse(variable == 'energyintensitymer',
                                                 growth0520 - energyintensitymer0519growth,
                                                 ifelse(variable == 'carbonintensity',
                                                        growth0520 - carbonintensity0519growth,NA)
                                          )))),
    divergence0520 = ifelse(variable == 'co2ffi',
                            growth0520 - co2ffi0520growth,
                            ifelse(variable == 'population',
                                   growth0520 - pop0520growth,
                                   ifelse(variable == 'pcgdpmer',
                                          growth0520 - pcgdpmer0520growth,
                                          ifelse(variable == 'energyintensitymer',
                                                 growth0520 - energyintensitymer0520growth,
                                                 ifelse(variable == 'carbonintensity',
                                                        growth0520 - carbonintensity0520growth,NA)
                                          )))),
    divergence0550steps = ifelse(variable == 'co2ffi',
                            growth0550 - co2ffi0550growthobsiea,
                            ifelse(variable == 'population',
                                   growth0550 - pop0550growthobsiea,
                                   ifelse(variable == 'pcgdpmer',
                                          growth0550 - pcgdpmer0550growthobsiea,
                                          ifelse(variable == 'energyintensitymer',
                                                 growth0550 - energyintensitymer0550growthobsiea,
                                                 ifelse(variable == 'carbonintensity',
                                                        growth0550 - carbonintensity0550growthobsiea,NA)
                                          )))),
    divergence0550aps = ifelse(variable == 'co2ffi',
                            growth0550 - co2ffi0550growthobsieaaps,
                            ifelse(variable == 'population',
                                   growth0550 - pop0550growthobsiea,
                                   ifelse(variable == 'pcgdpmer',
                                          growth0550 - pcgdpmer0550growthobsiea,
                                          ifelse(variable == 'energyintensitymer',
                                                 growth0550 - energyintensitymer0550growthobsieaaps,
                                                 ifelse(variable == 'carbonintensity',
                                                        growth0550 - carbonintensity0550growthobsieaaps,NA)
                                          ))))
    ) 
  

# SSP Global Kaya Comparisons

# 2005-2020 and 2005-2050 comparisons
sspkayadivergences <- sspdf %>%
  filter(REGION == 'World',
         VARIABLE == 'Emissions|CO2|Fossil Fuels and Industry' |
           VARIABLE == 'GDP|PPP' |
           VARIABLE == 'Population' |
           VARIABLE == 'Primary Energy') %>%
  select(MODEL,SCENARIO,REGION,VARIABLE,UNIT,`2005`,`2020`) %>%
  mutate(growth0520 = 100*(log(`2020`)-log(`2005`))/15) %>%
  select(MODEL,SCENARIO,REGION,VARIABLE,growth0520) %>%
  spread(key = VARIABLE, value = growth0520) %>%
  rename(co2ffi = `Emissions|CO2|Fossil Fuels and Industry`,
         gdpppp = `GDP|PPP`,
         population = `Population`,
         energy = `Primary Energy`) %>%
  mutate(pcgdpppp = gdpppp - population,
         energyintensityppp = energy - gdpppp,
         carbonintensity = co2ffi - energy) %>%
  select(MODEL,SCENARIO,REGION,co2ffi,population,
         pcgdpppp,energyintensityppp,carbonintensity) %>%
  gather(key = 'variable', value = 'growth0520', -MODEL,-SCENARIO,-REGION) %>%
  left_join(
    sspdf %>%
      filter(REGION == 'World',
             VARIABLE == 'Emissions|CO2|Fossil Fuels and Industry' |
               VARIABLE == 'GDP|PPP' |
               VARIABLE == 'Population' |
               VARIABLE == 'Primary Energy') %>%
      select(MODEL,SCENARIO,REGION,VARIABLE,UNIT,`2005`,`2050`) %>%
      mutate(growth0550 = 100*(log(`2050`)-log(`2005`))/45) %>%
      select(MODEL,SCENARIO,REGION,VARIABLE,growth0550) %>%
      spread(key = VARIABLE, value = growth0550) %>%
      rename(co2ffi = `Emissions|CO2|Fossil Fuels and Industry`,
             gdpppp = `GDP|PPP`,
             population = `Population`,
             energy = `Primary Energy`) %>%
      mutate(pcgdpppp = gdpppp - population,
             energyintensityppp = energy - gdpppp,
             carbonintensity = co2ffi - energy) %>%
      select(MODEL,SCENARIO,REGION,co2ffi,population,
             pcgdpppp,energyintensityppp,carbonintensity) %>%
      gather(key = 'variable', value = 'growth0550', -MODEL,-SCENARIO,-REGION)
  ) %>%
  mutate(
    divergence0519 = ifelse(variable == 'co2ffi',
                            growth0520 - co2ffi0519growth,
                            ifelse(variable == 'population',
                                   growth0520 - pop0519growth,
                                   ifelse(variable == 'pcgdpppp',
                                          growth0520 - pcgdpppp0519growth,
                                          ifelse(variable == 'energyintensityppp',
                                                 growth0520 - energyintensityppp0519growth,
                                                 ifelse(variable == 'carbonintensity',
                                                        growth0520 - carbonintensity0519growth,NA)
                                          )))),
    divergence0520 = ifelse(variable == 'co2ffi',
                            growth0520 - co2ffi0520growth,
                            ifelse(variable == 'population',
                                   growth0520 - pop0520growth,
                                   ifelse(variable == 'pcgdpppp',
                                          growth0520 - pcgdpppp0520growth,
                                          ifelse(variable == 'energyintensityppp',
                                                 growth0520 - energyintensityppp0520growth,
                                                 ifelse(variable == 'carbonintensity',
                                                        growth0520 - carbonintensity0520growth,NA)
                                          )))),
    divergence0550steps = ifelse(variable == 'co2ffi',
                                 growth0550 - co2ffi0550growthobsiea,
                                 ifelse(variable == 'population',
                                        growth0550 - pop0550growthobsiea,
                                        ifelse(variable == 'pcgdpppp',
                                               growth0550 - pcgdpppp0550growthobsiea,
                                               ifelse(variable == 'energyintensityppp',
                                                      growth0550 - energyintensityppp0550growthobsiea,
                                                      ifelse(variable == 'carbonintensity',
                                                             growth0550 - carbonintensity0550growthobsiea,NA)
                                               )))),
    divergence0550aps = ifelse(variable == 'co2ffi',
                               growth0550 - co2ffi0550growthobsieaaps,
                               ifelse(variable == 'population',
                                      growth0550 - pop0550growthobsiea,
                                      ifelse(variable == 'pcgdpppp',
                                             growth0550 - pcgdpppp0550growthobsiea,
                                             ifelse(variable == 'energyintensityppp',
                                                    growth0550 - energyintensityppp0550growthobsieaaps,
                                                    ifelse(variable == 'carbonintensity',
                                                           growth0550 - carbonintensity0550growthobsieaaps,NA)
                                             ))))
  )

ar5filterskayaplotR1 <- ar5kayadivergences %>%
  left_join(scenariolist %>%
              select(-VARIABLE,-UNIT,-`2005`,-`2020`,-`2050`,-growth0520,-growth0550,
                     -divergence0519,-divergence0520,-divergence0550steps,-divergence0550aps))

sspfilterskayaplotR1 <- sspkayadivergences %>%
  left_join(scenariolist %>%
              select(-VARIABLE,-UNIT,-`2005`,-`2020`,-`2050`,-growth0520,-growth0550,
                     -divergence0519,-divergence0520,-divergence0550steps,-divergence0550aps))

kayaplotR10519 <- bind_rows(ar5filterskayaplotR1 %>%
                              filter(tolerance01in2005to2019 == 'Y') %>%
                              mutate(tolerance = '0.1%/y tolerance 2005-2019',
                                     variable = ifelse(variable == 'pcgdpmer','pcgdp',
                                                       ifelse(variable == 'energyintensitymer','energyintensity',
                                                              variable))) %>%
                              select(MODEL,SCENARIO,REGION,scenariotype,Database,tolerance,
                                     variable,growth0520,growth0550,divergence0519),
                            ar5filterskayaplotR1 %>%
                              filter(tolerance03in2005to2019 == 'Y') %>%
                              mutate(tolerance = '0.3%/y tolerance 2005-2019',
                                     variable = ifelse(variable == 'pcgdpmer','pcgdp',
                                                       ifelse(variable == 'energyintensitymer','energyintensity',
                                                              variable))) %>%
                              select(MODEL,SCENARIO,REGION,scenariotype,Database,tolerance,
                                     variable,growth0520,growth0550,divergence0519),
                            ar5filterskayaplotR1 %>%
                              filter((tolerance01in2005to2019 == 'N' &
                                       tolerance01in2005to2019 == 'N')) %>%
                              mutate(tolerance = 'Other scenarios',
                                     variable = ifelse(variable == 'pcgdpmer','pcgdp',
                                                       ifelse(variable == 'energyintensitymer','energyintensity',
                                                              variable))) %>%
                              select(MODEL,SCENARIO,REGION,scenariotype,Database,tolerance,
                                     variable,growth0520,growth0550,divergence0519),
                            sspfilterskayaplotR1 %>%
                              filter(tolerance01in2005to2019 == 'Y') %>%
                              mutate(tolerance = '0.1%/y tolerance 2005-2019',
                                     variable = ifelse(variable == 'pcgdpppp','pcgdp',
                                                       ifelse(variable == 'energyintensityppp','energyintensity',
                                                              variable))) %>%
                              select(MODEL,SCENARIO,REGION,scenariotype,Database,tolerance,
                                     variable,growth0520,growth0550,divergence0519),
                            sspfilterskayaplotR1 %>%
                              filter(tolerance03in2005to2019 == 'Y') %>%
                              mutate(tolerance = '0.3%/y tolerance 2005-2019',
                                     variable = ifelse(variable == 'pcgdpppp','pcgdp',
                                                       ifelse(variable == 'energyintensityppp','energyintensity',
                                                              variable))) %>%
                              select(MODEL,SCENARIO,REGION,scenariotype,Database,tolerance,
                                     variable,growth0520,growth0550,divergence0519),
                            sspfilterskayaplotR1 %>%
                              filter((tolerance01in2005to2019 == 'N' &
                                        tolerance01in2005to2019 == 'N')) %>%
                              mutate(tolerance = 'Other scenarios',
                                     variable = ifelse(variable == 'pcgdpppp','pcgdp',
                                                       ifelse(variable == 'energyintensityppp','energyintensity',
                                                              variable))) %>%
                              select(MODEL,SCENARIO,REGION,scenariotype,Database,tolerance,
                                     variable,growth0520,growth0550,divergence0519))

kayaplotR10520 <- bind_rows(ar5filterskayaplotR1 %>%
                              filter(tolerance01in2005to2020 == 'Y') %>%
                              mutate(tolerance = '0.1%/y tolerance 2005-2020',
                                     variable = ifelse(variable == 'pcgdpmer','pcgdp',
                                                       ifelse(variable == 'energyintensitymer','energyintensity',
                                                              variable))) %>%
                              select(MODEL,SCENARIO,REGION,scenariotype,Database,tolerance,
                                     variable,growth0520,growth0550,divergence0520),
                            ar5filterskayaplotR1 %>%
                              filter(tolerance03in2005to2020 == 'Y') %>%
                              mutate(tolerance = '0.3%/y tolerance 2005-2020',
                                     variable = ifelse(variable == 'pcgdpmer','pcgdp',
                                                       ifelse(variable == 'energyintensitymer','energyintensity',
                                                              variable))) %>%
                              select(MODEL,SCENARIO,REGION,scenariotype,Database,tolerance,
                                     variable,growth0520,growth0550,divergence0520),
                            ar5filterskayaplotR1 %>%
                              filter((tolerance01in2005to2020 == 'N' &
                                        tolerance01in2005to2020 == 'N')) %>%
                              mutate(tolerance = 'Other scenarios',
                                     variable = ifelse(variable == 'pcgdpmer','pcgdp',
                                                       ifelse(variable == 'energyintensitymer','energyintensity',
                                                              variable))) %>%
                              select(MODEL,SCENARIO,REGION,scenariotype,Database,tolerance,
                                     variable,growth0520,growth0550,divergence0520),
                            sspfilterskayaplotR1 %>%
                              filter(tolerance01in2005to2020 == 'Y') %>%
                              mutate(tolerance = '0.1%/y tolerance 2005-2020',
                                     variable = ifelse(variable == 'pcgdpppp','pcgdp',
                                                       ifelse(variable == 'energyintensityppp','energyintensity',
                                                              variable))) %>%
                              select(MODEL,SCENARIO,REGION,scenariotype,Database,tolerance,
                                     variable,growth0520,growth0550,divergence0520),
                            sspfilterskayaplotR1 %>%
                              filter(tolerance03in2005to2020 == 'Y') %>%
                              mutate(tolerance = '0.3%/y tolerance 2005-2020',
                                     variable = ifelse(variable == 'pcgdpppp','pcgdp',
                                                       ifelse(variable == 'energyintensityppp','energyintensity',
                                                              variable))) %>%
                              select(MODEL,SCENARIO,REGION,scenariotype,Database,tolerance,
                                     variable,growth0520,growth0550,divergence0520),
                            sspfilterskayaplotR1 %>%
                              filter((tolerance01in2005to2020 == 'N' &
                                        tolerance01in2005to2020 == 'N')) %>%
                              mutate(tolerance = 'Other scenarios',
                                     variable = ifelse(variable == 'pcgdpppp','pcgdp',
                                                       ifelse(variable == 'energyintensityppp','energyintensity',
                                                              variable))) %>%
                              select(MODEL,SCENARIO,REGION,scenariotype,Database,tolerance,
                                     variable,growth0520,growth0550,divergence0520))

kayaplotR10550steps <- bind_rows(ar5filterskayaplotR1 %>%
                              filter(tolerance01in2005to2050steps == 'Y') %>%
                              mutate(tolerance = '0.1%/y tolerance 2005-2050 STEPS',
                                     variable = ifelse(variable == 'pcgdpmer','pcgdp',
                                                       ifelse(variable == 'energyintensitymer','energyintensity',
                                                              variable))) %>%
                              select(MODEL,SCENARIO,REGION,scenariotype,Database,tolerance,
                                     variable,growth0520,growth0550,divergence0550steps),
                            ar5filterskayaplotR1 %>%
                              filter(tolerance03in2005to2050steps == 'Y') %>%
                              mutate(tolerance = '0.3%/y tolerance 2005-2050 STEPS',
                                     variable = ifelse(variable == 'pcgdpmer','pcgdp',
                                                       ifelse(variable == 'energyintensitymer','energyintensity',
                                                              variable))) %>%
                              select(MODEL,SCENARIO,REGION,scenariotype,Database,tolerance,
                                     variable,growth0520,growth0550,divergence0550steps),
                            ar5filterskayaplotR1 %>%
                              filter((tolerance01in2005to2050steps == 'N' &
                                        tolerance01in2005to2050steps == 'N')) %>%
                              mutate(tolerance = 'Other scenarios',
                                     variable = ifelse(variable == 'pcgdpmer','pcgdp',
                                                       ifelse(variable == 'energyintensitymer','energyintensity',
                                                              variable))) %>%
                              select(MODEL,SCENARIO,REGION,scenariotype,Database,tolerance,
                                     variable,growth0520,growth0550,divergence0550steps),
                            sspfilterskayaplotR1 %>%
                              filter(tolerance01in2005to2050steps == 'Y') %>%
                              mutate(tolerance = '0.1%/y tolerance 2005-2050 STEPS',
                                     variable = ifelse(variable == 'pcgdpppp','pcgdp',
                                                       ifelse(variable == 'energyintensityppp','energyintensity',
                                                              variable))) %>%
                              select(MODEL,SCENARIO,REGION,scenariotype,Database,tolerance,
                                     variable,growth0520,growth0550,divergence0550steps),
                            sspfilterskayaplotR1 %>%
                              filter(tolerance03in2005to2050steps == 'Y') %>%
                              mutate(tolerance = '0.3%/y tolerance 2005-2050 STEPS',
                                     variable = ifelse(variable == 'pcgdpppp','pcgdp',
                                                       ifelse(variable == 'energyintensityppp','energyintensity',
                                                              variable))) %>%
                              select(MODEL,SCENARIO,REGION,scenariotype,Database,tolerance,
                                     variable,growth0520,growth0550,divergence0550steps),
                            sspfilterskayaplotR1 %>%
                              filter((tolerance01in2005to2050steps == 'N' &
                                        tolerance01in2005to2050steps == 'N')) %>%
                              mutate(tolerance = 'Other scenarios',
                                     variable = ifelse(variable == 'pcgdpppp','pcgdp',
                                                       ifelse(variable == 'energyintensityppp','energyintensity',
                                                              variable))) %>%
                              select(MODEL,SCENARIO,REGION,scenariotype,Database,tolerance,
                                     variable,growth0520,growth0550,divergence0550steps))

kayaplotR10550aps <- bind_rows(ar5filterskayaplotR1 %>%
                                   filter(tolerance01in2005to2050aps == 'Y') %>%
                                   mutate(tolerance = '0.1%/y tolerance 2005-2050 APS',
                                          variable = ifelse(variable == 'pcgdpmer','pcgdp',
                                                            ifelse(variable == 'energyintensitymer','energyintensity',
                                                                   variable))) %>%
                                   select(MODEL,SCENARIO,REGION,scenariotype,Database,tolerance,
                                          variable,growth0520,growth0550,divergence0550aps),
                                 ar5filterskayaplotR1 %>%
                                   filter(tolerance03in2005to2050aps == 'Y') %>%
                                   mutate(tolerance = '0.3%/y tolerance 2005-2050 APS',
                                          variable = ifelse(variable == 'pcgdpmer','pcgdp',
                                                            ifelse(variable == 'energyintensitymer','energyintensity',
                                                                   variable))) %>%
                                   select(MODEL,SCENARIO,REGION,scenariotype,Database,tolerance,
                                          variable,growth0520,growth0550,divergence0550aps),
                                 ar5filterskayaplotR1 %>%
                                   filter((tolerance01in2005to2050aps == 'N' &
                                             tolerance01in2005to2050aps == 'N')) %>%
                                   mutate(tolerance = 'Other scenarios',
                                          variable = ifelse(variable == 'pcgdpmer','pcgdp',
                                                            ifelse(variable == 'energyintensitymer','energyintensity',
                                                                   variable))) %>%
                                   select(MODEL,SCENARIO,REGION,scenariotype,Database,tolerance,
                                          variable,growth0520,growth0550,divergence0550aps),
                                 sspfilterskayaplotR1 %>%
                                   filter(tolerance01in2005to2050aps == 'Y') %>%
                                   mutate(tolerance = '0.1%/y tolerance 2005-2050 APS',
                                          variable = ifelse(variable == 'pcgdpppp','pcgdp',
                                                            ifelse(variable == 'energyintensityppp','energyintensity',
                                                                   variable))) %>%
                                   select(MODEL,SCENARIO,REGION,scenariotype,Database,tolerance,
                                          variable,growth0520,growth0550,divergence0550aps),
                                 sspfilterskayaplotR1 %>%
                                   filter(tolerance03in2005to2050aps == 'Y') %>%
                                   mutate(tolerance = '0.3%/y tolerance 2005-2050 APS',
                                          variable = ifelse(variable == 'pcgdpppp','pcgdp',
                                                            ifelse(variable == 'energyintensityppp','energyintensity',
                                                                   variable))) %>%
                                   select(MODEL,SCENARIO,REGION,scenariotype,Database,tolerance,
                                          variable,growth0520,growth0550,divergence0550aps),
                                 sspfilterskayaplotR1 %>%
                                   filter((tolerance01in2005to2050aps == 'N' &
                                             tolerance01in2005to2050aps == 'N')) %>%
                                   mutate(tolerance = 'Other scenarios',
                                          variable = ifelse(variable == 'pcgdpppp','pcgdp',
                                                            ifelse(variable == 'energyintensityppp','energyintensity',
                                                                   variable))) %>%
                                   select(MODEL,SCENARIO,REGION,scenariotype,Database,tolerance,
                                          variable,growth0520,growth0550,divergence0550aps))

write_csv(kayaplotR10519,
          "Pielke-et-al-2021-files/Output-from-R-code/Revision-R1-files/kayaplotR10519.csv")

write_csv(kayaplotR10520,
          "Pielke-et-al-2021-files/Output-from-R-code/Revision-R1-files/kayaplotR10520.csv")

write_csv(kayaplotR10550steps,
          "Pielke-et-al-2021-files/Output-from-R-code/Revision-R1-files/kayaplotR10550steps.csv")

write_csv(kayaplotR10550aps,
          "Pielke-et-al-2021-files/Output-from-R-code/Revision-R1-files/kayaplotR10550aps.csv")

