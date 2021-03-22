##################################
########### Background ###########
##################################

# This script runs the scenario filtering analysis 
# and Kaya decomposition described in:
# Pielke Jr., Burgess, and Ritchie:
# "Most plausible 2005-2040 emissions scenarios project less than 2.5(degrees)C of warming by 2100".
# The code generates lists of scenarios meeting each filter ("nineerrorscenariolists.csv")
# and also stores csv tables used (via JMP) to make 
# Figs. S4 ("ar5filterskayaplot" and "ar5filterskayaplot"), 
# and Fig. S5 ("ar5filterskayaplot20052020" and "ar5filterskayaplot2052020"). 
# The jsl scripts used to make these figures are also stored in the github repo.
# The other figures were made by J.R., using an Excel file also in the github repo, 
# in the folder marked accordingly.


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
library(imputeTS)
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

### IEA Kaya decomposition: 
### https://webstore.iea.org/co2-emissions-from-fuel-combustion-2019-highlights

# CO2 emissions from fossil combustion (Mt of CO2)
ieaco2world <- read_excel(here("Data", "CO2Highlights2019-Excel file.XLS"), 
                          sheet = "CO2 FC", range = cell_rows(4:6))[-1,]

ieaco2regions <- read_excel(here("Data", "CO2Highlights2019-Excel file.XLS"), 
                            sheet = "CO2 FC", range = cell_rows(24:191))

ieaco2regions <- type_convert(ieaco2regions, na = c("..", "x"))

# Total Primary Energy Supply (PJ)

ieatpesworld <- read_excel(here("Data", "CO2Highlights2019-Excel file.XLS"), 
                           sheet = "TPES PJ", range = cell_rows(4:6))[-1,]

ieatpesregions <- read_excel(here("Data", "CO2Highlights2019-Excel file.XLS"), 
                             sheet = "TPES PJ", range = cell_rows(24:191))

ieatpesregions <- type_convert(ieatpesregions, na = c("..", "x"))

# GDP: PPP B 2010 USD

ieagdppppworld <- read_excel(here("Data", "CO2Highlights2019-Excel file.XLS"), 
                             sheet = "GDP PPP", range = cell_rows(4:6))[-1,]

ieagdppppregions <- read_excel(here("Data", "CO2Highlights2019-Excel file.XLS"), 
                               sheet = "GDP PPP", range = cell_rows(22:189))

ieagdppppregions <- type_convert(ieagdppppregions, na = c("..", "x"))

# GDP: MER B 2010 USD

ieagdpmerworld <- read_excel(here("Data", "CO2Highlights2019-Excel file.XLS"), 
                             sheet = "GDP", range = cell_rows(4:6))[-1,]

ieagdpmerregions <- read_excel(here("Data", "CO2Highlights2019-Excel file.XLS"), 
                               sheet = "GDP", range = cell_rows(22:189))

ieagdpmerregions <- type_convert(ieagdpmerregions, na = c("..", "x"))

# Population (millions)

ieapopworld <- read_excel(here("Data", "CO2Highlights2019-Excel file.XLS"), 
                          sheet = "POP", range = cell_rows(4:6))[-1,]

ieapopregions <- read_excel(here("Data", "CO2Highlights2019-Excel file.XLS"), 
                            sheet = "POP", range = cell_rows(22:189))

ieapopregions <- type_convert(ieapopregions, na = c("..", "x"))


########################################
########### Kaya Analysis ##############
######################################## 

### Consolidate observed Kaya factors by IPCC region by region and year
### Resulting df: region, variable, 1971,...,2017

# combine CO2 data, and add Variable column
ieaco2world <- ieaco2world %>%
  rename(`Region/Country/Economy` = `million tonnes of CO2`)

ieaco2 <- bind_rows(ieaco2world,ieaco2regions)

ieaco2$variable <- 'fossilco2'

# combine TPES data, and add Variable column
ieatpesworld <- ieatpesworld %>%
  rename(`Region/Country/Economy` = `petajoules`)

ieatpes <- bind_rows(ieatpesworld,ieatpesregions)

ieatpes$variable <- 'energytpes'

# combine GDP PPP data, and add Variable column
ieagdppppworld <- ieagdppppworld %>%
  rename(`Region/Country/Economy` = `billion 2010 US dollars`)

ieagdpppp <- bind_rows(ieagdppppworld,ieagdppppregions)

ieagdpppp$variable <- 'gdpppp'

# combine GDP MER data, and add Variable column
ieagdpmerworld <- ieagdpmerworld %>%
  rename(`Region/Country/Economy` = `billion 2010 US dollars`)

ieagdpmer <- bind_rows(ieagdpmerworld,ieagdpmerregions)

ieagdpmer$variable <- 'gdpmer'

# combine population data, and add Variable column
ieapopworld <- ieapopworld %>%
  rename(`Region/Country/Economy` = `millions`)

ieapop <- bind_rows(ieapopworld,ieapopregions)

ieapop$variable <- 'pop'

## Merge IEA data from all Kaya factors
ieadf <- bind_rows(ieaco2,ieatpes,ieagdpmer,ieagdpppp,ieapop) %>%
  rename(`IEA REGION` = `Region/Country/Economy`)

# Add IPCC region column to merged df
ieadf <- left_join(x = ieadf,y = ipccregionlookup) %>%
  rename(iearegion = `IEA REGION`,
         ipccregion = `IPCC REGION`)

## Add up variables by IPCC REGION, remove AG-NA groups
ieasummary <- ieadf %>%
  select(-iearegion) %>%
  filter(ipccregion != "AG-NA") %>%
  group_by(variable, ipccregion) %>%
  summarise_all(funs(sum(.,na.rm = T)))

### Calculate 2005-2017 growth rates in Kaya factors, by region
### growth = (ln(2017) - ln(2005))/12
obsgrowth <- ieasummary %>%
  mutate(growth0517 = 100*((log(`2017`) - log(`2005`))/12)) %>%
  select(ipccregion,variable,growth0517) 

# get observed pcGDP growth rates for Fig. 3
obsgrowth2 <- obsgrowth %>%
  spread(key = variable, value = growth0517) %>%
  mutate(pcgdpmer0517 = gdpmer - pop,
         pcgdpppp0517 = gdpppp - pop) %>%
  select(-gdpppp,-gdpmer,-pop,-fossilco2,-energytpes) 

## Load Christensen et al. 2018 table for comparison
christensentbl <- read_excel(here("Data", 
                                  "Christensen-2018-expert-projections.XLS"))

## Load region lookup table for comparing Christensen growth rates (see Methods section 4)
christensenregs <- read_excel(here("Data", "ipcc-christensen-comparison-regions.XLS"))

### Calculate Kaya factor growth errors for AR5 and SSP databases by region
### Resulting dfs: Region, MODEL, SCENARIO, Baseline Scenario,('Baseline' or 'Policy'), 
###                Kaya factor, 2005-2020 growth error (growtherror), 
###                2020-2040 catch-up growth rate (catchup40)

## Filter AR5 database for key variables:
## GDP|MER, Primary Energy, Population
ar5keyvars <- ar5df %>%
  select(MODEL,SCENARIO,REGION,VARIABLE,UNIT,`2005`,`2020`,`2040`) %>%
  filter(VARIABLE == "GDP|MER" 
         | VARIABLE == "Primary Energy"
         | VARIABLE == "Population") %>%
  mutate(variable = ifelse(VARIABLE == "GDP|MER", "gdpmer", 
                           ifelse(VARIABLE == "Primary Energy","energytpes", 
                                  "pop"))) %>%
  select(-VARIABLE)

## add in regional FF emissions, with conversions from 2005-2020 SSPs
# load conversion factors from J. Ritchie
regfficonversions <- read_csv(here("Data", "FFI-regional-harmonization.csv"))

# generate and add in regional FF emissions
ar5regff <- ar5df %>%
  select(MODEL,SCENARIO,REGION,VARIABLE,UNIT,`2005`,`2020`,`2040`) %>%
  filter(VARIABLE == "Emissions|CO2|Fossil Fuels and Industry", 
         REGION != "World") %>%
  left_join(regfficonversions) %>%
  mutate(adj2005 = `2005` / hf2005,
         adj2020 = `2020` / hf2020,
         adj2040 = `2040` / hf2040,
         variable = "fossilco2") %>%
  select(-`2005`,-`2020`,-`2040`,-hf2005,
         -hf2010,-hf2020,-hf2030,-hf2040,-hf2050) %>%
  rename(`2005` = adj2005,
         `2020` = adj2020,
         `2040` = adj2040) %>%
  select(-VARIABLE)

ar5keyvars <- bind_rows(ar5keyvars,ar5regff)

## add in global FF emissions, using RD18 conversions

# define conversion factors for coal, oil, and gas -> CO2 emissions
# following RD18
epsilon_k <- 94.6 #MtCO2/EJ
epsilon_g <- 56.1 #MtCO2/EJ
epsilon_o <- 73.3 #MtCO2/EJ

# generate AR5 emissions rows - world only
ar5fossilco2 <- ar5df %>%
  select(MODEL,SCENARIO,REGION,VARIABLE,UNIT,`2005`,`2020`,`2040`) %>%
  filter(VARIABLE == "Primary Energy|Coal"
         | VARIABLE == "Primary Energy|Oil" 
         | VARIABLE == "Primary Energy|Gas") %>%
  mutate(epsilon = ifelse(VARIABLE == "Primary Energy|Coal", epsilon_k, 
                          ifelse(VARIABLE == "Primary Energy|Oil", 
                                 epsilon_o, epsilon_g))) %>%
  mutate(co2005 = `2005`*epsilon,
         co2020 = `2020`*epsilon,
         co2040 = `2040`*epsilon) %>%
  group_by(MODEL,SCENARIO,REGION) %>%
  summarise(`2005` = sum(co2005),
            `2020` = sum(co2020),
            `2040` = sum(co2040)) %>%
  mutate(variable = "fossilco2",
         UNIT = "Mt CO2/yr")

# Combine AR5 variables tables
ar5keyvars <- bind_rows(ar5keyvars,ar5fossilco2)

# add 2005-2020 growth rate column, and baseline scenario column
# add observed growth rates
# calculate catch-up rate
# add IEA 2018-2040 growth columns
# calclate 'ieadivergence' columns
ar5growth <- ar5keyvars %>%
  mutate(growth0520 = 100*((log(`2020`) - log(`2005`))/15),
         growth2040 = 100*((log(`2040`) - log(`2020`))/20),
         ipccregion = REGION) %>%
  left_join(ipccscenariolist) %>%
  select(-`2005`,-`2020`,-`2040`) %>%
  left_join(obsgrowth) %>%
  select(-ipccregion) %>%
  # iea1840 values from "IEA-Kaya-details" Excel file in Data folder
  # Note that IEA's forward-projected GDP is in PPP units. 
  # Thus, this is an imperfect comparison for AR5.
  mutate(iea1840stps = ifelse(REGION == "World",
                              ifelse(variable == "fossilco2", 0.309965376,
                                     ifelse(variable == "energytpes", 0.971023417,
                                            ifelse(variable == "gdpmer", 3.343477609,
                                                   ifelse(variable == "pop", 0.853381793,
                                                          NA)))),  NA),
         iea1840cps = ifelse(REGION == "World",
                             ifelse(variable == "fossilco2", 0.986666847,
                                    ifelse(variable == "energytpes", 1.329425295,
                                           ifelse(variable == "gdpmer", 3.343477609,
                                                  ifelse(variable == "pop", 0.853381793,
                                                         NA)))), NA),
         iea1940stps2020 = ifelse(REGION == "World",
                                  ifelse(variable == "fossilco2", -0.0025753,
                                         ifelse(variable == "energytpes", 0.8121719,
                                                ifelse(variable == "gdpmer", 2.9588,
                                                       ifelse(variable == "pop", 0.841017,
                                                              NA)))), NA),
         growtherror = growth0520 - growth0517,
         catchup40 = growth2040 + (growtherror*0.75),
         ieadivergencestps = growth2040 - iea1840stps,
         ieadivergencecps = growth2040 - iea1840cps,
         ieadivergencestps2020 = growth2040 - iea1940stps2020)

# convert baseline growth errors and catchups into Kaya factors
ar5basekayaerrors <- ar5growth %>%
  # filter(`Baseline Scenario` == "Baseline") %>% # commented to keep non-baselines in. 
  select(MODEL,SCENARIO,REGION,variable,growtherror) %>%
  spread(key = variable, value = growtherror) %>%
  mutate(pcgdpmer = gdpmer - pop,
         energyintensity = energytpes - gdpmer,
         co2intensity = fossilco2 - energytpes) %>%
  select(-gdpmer,-energytpes) %>%
  gather(key = "variable", value = "percent", -MODEL,-SCENARIO,-REGION) %>%
  mutate(resulttype = "growtherror")

ar5basekayacatchups <- ar5growth %>%
  # filter(`Baseline Scenario` == "Baseline") %>% # commented to keep non-baselines in. 
  select(MODEL,SCENARIO,REGION,variable,catchup40) %>%
  spread(key = variable, value = catchup40) %>%
  mutate(pcgdpmer = gdpmer - pop,
         energyintensity = energytpes - gdpmer,
         co2intensity = fossilco2 - energytpes) %>%
  select(-gdpmer,-energytpes) %>%
  gather(key = "variable", value = "percent", -MODEL,-SCENARIO,-REGION) %>%
  mutate(resulttype = "catchuprate")

ar5basekayacpsdivergence <- ar5growth %>%
  # filter(`Baseline Scenario` == "Baseline") %>%
  select(MODEL,SCENARIO,REGION,variable,ieadivergencecps) %>% # commented to keep non-baselines in. 
  spread(key = variable, value = ieadivergencecps) %>%
  mutate(pcgdpmer = gdpmer - pop,
         energyintensity = energytpes - gdpmer,
         co2intensity = fossilco2 - energytpes) %>%
  select(-gdpmer,-energytpes) %>%
  gather(key = "variable", value = "percent", -MODEL,-SCENARIO,-REGION) %>%
  mutate(resulttype = "ieadivergencecps")

ar5basekayastpsdivergence <- ar5growth %>%
  # filter(`Baseline Scenario` == "Baseline") %>% # commented to keep non-baselines in. 
  select(MODEL,SCENARIO,REGION,variable,ieadivergencestps) %>%
  spread(key = variable, value = ieadivergencestps) %>%
  mutate(pcgdpmer = gdpmer - pop,
         energyintensity = energytpes - gdpmer,
         co2intensity = fossilco2 - energytpes) %>%
  select(-gdpmer,-energytpes) %>%
  gather(key = "variable", value = "percent", -MODEL,-SCENARIO,-REGION) %>%
  mutate(resulttype = "ieadivergencestps")

ar5basekayastpsdivergence2020 <- ar5growth %>%
  # filter(`Baseline Scenario` == "Baseline") %>% # commented to keep non-baselines in. 
  select(MODEL,SCENARIO,REGION,variable,ieadivergencestps2020) %>%
  spread(key = variable, value = ieadivergencestps2020) %>%
  mutate(pcgdpmer = gdpmer - pop,
         energyintensity = energytpes - gdpmer,
         co2intensity = fossilco2 - energytpes) %>%
  select(-gdpmer,-energytpes) %>%
  gather(key = "variable", value = "percent", -MODEL,-SCENARIO,-REGION) %>%
  mutate(resulttype = "ieadivergencestps2020")

ar5kaya <- bind_rows(ar5basekayaerrors,
                     ar5basekayacatchups,
                     ar5basekayacpsdivergence,
                     ar5basekayastpsdivergence,
                     ar5basekayastpsdivergence2020) 

### Filter SSP database for key variables:
### GDP|PPP, Primary Energy, Population, Coal, Oil, Gas

# Standardize SSP region names to AR5
sspdf <- sspdf %>%
  rename(oldreg = REGION) %>%
  mutate(REGION = ifelse(oldreg == "R5.2ASIA", "ASIA",
                         ifelse(oldreg == "R5.2LAM", "LAM",
                                ifelse(oldreg == "R5.2MAF", "MAF",
                                       ifelse(oldreg == "R5.2OECD", "OECD90",
                                              ifelse(oldreg == "R5.2REF", "REF",
                                                     "World")))))) %>%
  select(-oldreg)

# isolate baseline scenarios
sspbaselinedf <- sspdf # %>%
  # filter(grepl("Baseline", SCENARIO)) # commented to keep non-baselines in. 

# isolate key variables: GDP|PPP, Population, Primary Energy
sspbasekeyvars <- sspbaselinedf %>%
  select(MODEL,SCENARIO,REGION,VARIABLE,UNIT,`2005`,`2020`,`2040`) %>%
  filter(VARIABLE == "GDP|PPP" 
         | VARIABLE == "Primary Energy"
         | VARIABLE == "Population") %>%
  mutate(variable = ifelse(VARIABLE == "GDP|PPP", "gdpppp", 
                           ifelse(VARIABLE == "Primary Energy","energytpes", 
                                  "pop"))) %>%
  select(-VARIABLE)

## calculate fossil CO2 using RD18's oil, coal, and gas conversions 
sspfossilco2 <- sspbaselinedf %>%
  select(MODEL,SCENARIO,REGION,VARIABLE,UNIT,`2005`,`2020`,`2040`) %>%
  filter(VARIABLE == "Primary Energy|Coal"
         | VARIABLE == "Primary Energy|Oil" 
         | VARIABLE == "Primary Energy|Gas") %>%
  mutate(epsilon = ifelse(VARIABLE == "Primary Energy|Coal", epsilon_k, 
                          ifelse(VARIABLE == "Primary Energy|Oil", 
                                 epsilon_o, epsilon_g))) %>%
  mutate(co2005 = `2005`*epsilon,
         co2020 = `2020`*epsilon,
         co2040 = `2040`*epsilon) %>%
  group_by(MODEL,SCENARIO,REGION) %>%
  summarise(`2005` = sum(co2005),
            `2020` = sum(co2020),
            `2040` = sum(co2040)) %>%
  mutate(variable = "fossilco2",
         UNIT = "Mt CO2/yr")

# Combine SSP baseline variables tables
sspbasekeyvars <- bind_rows(sspbasekeyvars,sspfossilco2)

# add 2005-2020 growth rate column
# add observed growth rates
# calculate projected - observed
# calculate catch-up rate

# rename region variable in obsgrowth to match ssp table
obsgrowth3 <- obsgrowth %>%
  rename(REGION = ipccregion)

# calculate SSP growth errors and divergences
sspbasegrowth <- sspbasekeyvars %>%
  mutate(growth0520 = 100*((log(`2020`) - log(`2005`))/15),
         growth2040 = 100*((log(`2040`) - log(`2020`))/20)) %>%
  select(-`2005`,-`2020`,-`2040`) %>%
  left_join(obsgrowth3) %>%
  mutate(iea1840stps = ifelse(REGION == "World",
                              ifelse(variable == "fossilco2", 0.309965376,
                                     ifelse(variable == "energytpes", 0.971023417,
                                            ifelse(variable == "gdpppp", 3.343477609,
                                                   ifelse(variable == "pop", 0.853381793,
                                                          NA)))), NA),
         iea1840cps = ifelse(REGION == "World", 
                             ifelse(variable == "fossilco2", 0.986666847,
                                    ifelse(variable == "energytpes", 1.329425295,
                                           ifelse(variable == "gdpppp", 3.343477609,
                                                  ifelse(variable == "pop", 0.853381793,
                                                         NA)))), NA),
         iea1940stps2020 = ifelse(REGION == "World",
                              ifelse(variable == "fossilco2", -0.0025753,
                                     ifelse(variable == "energytpes", 0.8121719,
                                            ifelse(variable == "gdpppp", 2.9588,
                                                   ifelse(variable == "pop", 0.841017,
                                                          NA)))), NA),
         growtherror = growth0520 - growth0517,
         catchup40 = growth2040 + (growtherror*0.75),
         ieadivergencestps = growth2040 - iea1840stps,
         ieadivergencecps = growth2040 - iea1840cps,
         ieadivergencestps2020 = growth2040 - iea1940stps2020)

## convert baseline growth errors and catchups into Kaya factors
sspbasekayaerrors <- sspbasegrowth %>%
  select(MODEL,SCENARIO,REGION,variable,growtherror) %>%
  spread(key = variable, value = growtherror) %>%
  mutate(pcgdpppp = gdpppp - pop,
         energyintensity = energytpes - gdpppp,
         co2intensity = fossilco2 - energytpes) %>%
  select(-gdpppp,-energytpes) %>%
  gather(key = "variable", value = "percent", 
         -MODEL,-SCENARIO,-REGION) %>%
  mutate(resulttype = "growtherror")

sspbasekayacatchups <- sspbasegrowth %>%
  select(MODEL,SCENARIO,REGION,variable,catchup40) %>%
  spread(key = variable, value = catchup40) %>%
  mutate(pcgdpppp = gdpppp - pop,
         energyintensity = energytpes - gdpppp,
         co2intensity = fossilco2 - energytpes) %>%
  select(-gdpppp,-energytpes) %>%
  gather(key = "variable", value = "percent", 
         -MODEL,-SCENARIO,-REGION) %>%
  mutate(resulttype = "catchuprate")

sspbasekayacpsdivergence <- sspbasegrowth %>%
  select(MODEL,SCENARIO,REGION,variable,ieadivergencecps) %>%
  spread(key = variable, value = ieadivergencecps) %>%
  mutate(pcgdpppp = gdpppp - pop,
         energyintensity = energytpes - gdpppp,
         co2intensity = fossilco2 - energytpes) %>%
  select(-gdpppp,-energytpes) %>%
  gather(key = "variable", value = "percent", 
         -MODEL,-SCENARIO,-REGION) %>%
  mutate(resulttype = "ieadivergencecps")

sspbasekayastpsdivergence <- sspbasegrowth %>%
  select(MODEL,SCENARIO,REGION,variable,ieadivergencestps) %>%
  spread(key = variable, value = ieadivergencestps) %>%
  mutate(pcgdpppp = gdpppp - pop,
         energyintensity = energytpes - gdpppp,
         co2intensity = fossilco2 - energytpes) %>%
  select(-gdpppp,-energytpes) %>%
  gather(key = "variable", value = "percent", 
         -MODEL,-SCENARIO,-REGION) %>%
  mutate(resulttype = "ieadivergencestps")

sspbasekayastpsdivergence2020 <- sspbasegrowth %>%
  select(MODEL,SCENARIO,REGION,variable,ieadivergencestps2020) %>%
  spread(key = variable, value = ieadivergencestps2020) %>%
  mutate(pcgdpppp = gdpppp - pop,
         energyintensity = energytpes - gdpppp,
         co2intensity = fossilco2 - energytpes) %>%
  select(-gdpppp,-energytpes) %>%
  gather(key = "variable", value = "percent", 
         -MODEL,-SCENARIO,-REGION) %>%
  mutate(resulttype = "ieadivergencestps2020")

sspkaya <- bind_rows(sspbasekayaerrors,
                     sspbasekayacatchups,
                     sspbasekayacpsdivergence,
                     sspbasekayastpsdivergence,
                     sspbasekayastpsdivergence2020) 

ar5kaya2 <- ar5kaya %>%
  filter(REGION == "World",
         resulttype != "catchuprate") %>%
  spread(key = resulttype, value = percent) %>%
  mutate(errorplusdivergencestps = ((15/35)*growtherror)+
           ((20/35)*ieadivergencestps),
         errorplusdivergencestps2020 = ((15/35)*growtherror)+
           ((20/35)*ieadivergencestps2020)) %>%
  mutate(absgrowtherror = abs(growtherror),
       absieadivergencecps = abs(ieadivergencecps),
       absieadivergencestps = abs(ieadivergencestps),
       absieadivergencestps2020 = abs(ieadivergencestps2020),
       abserrorplusdivergencestps = abs(errorplusdivergencestps),
       abserrorplusdivergencestps2020 = abs(errorplusdivergencestps2020)) %>%
  group_by(variable) %>%
  mutate(errorpctile = cume_dist(absgrowtherror),
         ieadivergencecpspctile = cume_dist(absieadivergencecps),
         ieadivergencestpspctile = cume_dist(absieadivergencestps),
         ieadivergencestpspctile2020 = cume_dist(absieadivergencestps2020),
         errorplusdivergencestpspctile = cume_dist(abserrorplusdivergencestps),
         errorplusdivergencestpspctile2020 = cume_dist(abserrorplusdivergencestps2020))

sspkaya2 <- sspkaya %>%
  filter(REGION == "World",
         resulttype != "catchuprate") %>%
  spread(key = resulttype, value = percent) %>%
  mutate(errorplusdivergencestps = ((15/35)*growtherror)+
           ((20/35)*ieadivergencestps),
         errorplusdivergencestps2020 = ((15/35)*growtherror)+
           ((20/35)*ieadivergencestps2020)) %>%
  mutate(absgrowtherror = abs(growtherror),
         absieadivergencecps = abs(ieadivergencecps),
         absieadivergencestps = abs(ieadivergencestps),
         absieadivergencestps2020 = abs(ieadivergencestps2020),
         abserrorplusdivergencestps = abs(errorplusdivergencestps),
         abserrorplusdivergencestps2020 = abs(errorplusdivergencestps2020)) %>%
  group_by(variable) %>%
  mutate(errorpctile = cume_dist(absgrowtherror),
         ieadivergencecpspctile = cume_dist(absieadivergencecps),
         ieadivergencestpspctile = cume_dist(absieadivergencestps),
         ieadivergencestpspctile2020 = cume_dist(absieadivergencestps2020),
         errorplusdivergencestpspctile = cume_dist(abserrorplusdivergencestps),
         errorplusdivergencestpspctile2020 = cume_dist(abserrorplusdivergencestps2020))
  
ar5kaya3 <- ar5kaya2 %>%
  filter(variable == "fossilco2",
         absgrowtherror < 0.1,
         absieadivergencecps < 0.1)

sspkaya3 <- sspkaya2 %>%
  filter(variable == "fossilco2",
         absgrowtherror < 0.1,
         absieadivergencecps < 0.1)

ar5kaya4 <- ar5kaya2 %>%
  filter(variable == "fossilco2",
         absgrowtherror < 0.1,
         absieadivergencestps2020 < 0.1)

sspkaya4 <- sspkaya2 %>%
  filter(variable == "fossilco2",
         absgrowtherror < 0.1,
         absieadivergencestps2020 < 0.1)

### 9 lists of scenarios

# 0.1 error tolerance
ar5error0520at01 <- ar5kaya2 %>%
  filter(variable == "fossilco2",
         absgrowtherror < 0.1)
ssperror0520at01 <- sspkaya2 %>%
  filter(variable == "fossilco2",
         absgrowtherror < 0.1)
error0520at01 <- bind_rows(ar5error0520at01,ssperror0520at01) %>%
  mutate(errortolerance = 0.1,
         criterion = "2005 to ~2020 error",
         error = absgrowtherror) %>%
  select(MODEL, SCENARIO, REGION, variable, criterion, errortolerance, error)
  

ar5error2040at01 <- ar5kaya2 %>%
  filter(variable == "fossilco2",
         absieadivergencestps2020 < 0.1)
ssperror2040at01 <- sspkaya2 %>%
  filter(variable == "fossilco2",
         absieadivergencestps2020 < 0.1)
error2040at01 <- bind_rows(ar5error2040at01,ssperror2040at01) %>%
  mutate(errortolerance = 0.1,
         criterion = "~2020 to 2040 divergence from IEA STPS 2020",
         error =  absieadivergencestps2020) %>%
  select(MODEL, SCENARIO, REGION, variable, criterion, errortolerance, error)


ar5error0540at01 <- ar5kaya2 %>%
  filter(variable == "fossilco2",
         abserrorplusdivergencestps2020 < 0.1)
ssperror0540at01 <- sspkaya2 %>%
  filter(variable == "fossilco2",
         abserrorplusdivergencestps2020 < 0.1)
error0540at01 <- bind_rows(ar5error0540at01,ssperror0540at01) %>%
  mutate(errortolerance = 0.1,
         criterion = "2005 to 2040 error, observed and IEA STPS 2020",
         error = abserrorplusdivergencestps2020) %>%
  select(MODEL, SCENARIO, REGION, variable, criterion, errortolerance, error)

# 0.2 error tolerance
ar5error0520at02 <- ar5kaya2 %>%
  filter(variable == "fossilco2",
         absgrowtherror < 0.2)
ssperror0520at02 <- sspkaya2 %>%
  filter(variable == "fossilco2",
         absgrowtherror < 0.2)
error0520at02 <- bind_rows(ar5error0520at02,ssperror0520at02) %>%
  mutate(errortolerance = 0.2,
         criterion = "2005 to ~2020 error",
         error = absgrowtherror) %>%
  select(MODEL, SCENARIO, REGION, variable, criterion, errortolerance, error)


ar5error2040at02 <- ar5kaya2 %>%
  filter(variable == "fossilco2",
         absieadivergencestps2020 < 0.2)
ssperror2040at02 <- sspkaya2 %>%
  filter(variable == "fossilco2",
         absieadivergencestps2020 < 0.2)
error2040at02 <- bind_rows(ar5error2040at02,ssperror2040at02) %>%
  mutate(errortolerance = 0.2,
         criterion = "~2020 to 2040 divergence from IEA STPS 2020",
         error =  absieadivergencestps2020) %>%
  select(MODEL, SCENARIO, REGION, variable, criterion, errortolerance, error)


ar5error0540at02 <- ar5kaya2 %>%
  filter(variable == "fossilco2",
         abserrorplusdivergencestps2020 < 0.2)
ssperror0540at02 <- sspkaya2 %>%
  filter(variable == "fossilco2",
         abserrorplusdivergencestps2020 < 0.2)
error0540at02 <- bind_rows(ar5error0540at02,ssperror0540at02) %>%
  mutate(errortolerance = 0.2,
         criterion = "2005 to 2040 error, observed and IEA STPS 2020",
         error = abserrorplusdivergencestps2020) %>%
  select(MODEL, SCENARIO, REGION, variable, criterion, errortolerance, error)


# 0.3 error tolerance
ar5error0520at03 <- ar5kaya2 %>%
  filter(variable == "fossilco2",
         absgrowtherror < 0.3)
ssperror0520at03 <- sspkaya2 %>%
  filter(variable == "fossilco2",
         absgrowtherror < 0.3)
error0520at03 <- bind_rows(ar5error0520at03,ssperror0520at03) %>%
  mutate(errortolerance = 0.3,
         criterion = "2005 to ~2020 error",
         error = absgrowtherror) %>%
  select(MODEL, SCENARIO, REGION, variable, criterion, errortolerance, error)


ar5error2040at03 <- ar5kaya2 %>%
  filter(variable == "fossilco2",
         absieadivergencestps2020 < 0.3)
ssperror2040at03 <- sspkaya2 %>%
  filter(variable == "fossilco2",
         absieadivergencestps2020 < 0.3)
error2040at03 <- bind_rows(ar5error2040at03,ssperror2040at03) %>%
  mutate(errortolerance = 0.3,
         criterion = "~2020 to 2040 divergence from IEA STPS 2020",
         error =  absieadivergencestps2020) %>%
  select(MODEL, SCENARIO, REGION, variable, criterion, errortolerance, error)


ar5error0540at03 <- ar5kaya2 %>%
  filter(variable == "fossilco2",
         abserrorplusdivergencestps2020 < 0.3)
ssperror0540at03 <- sspkaya2 %>%
  filter(variable == "fossilco2",
         abserrorplusdivergencestps2020 < 0.3)
error0540at03 <- bind_rows(ar5error0540at03,ssperror0540at03) %>%
  mutate(errortolerance = 0.3,
         criterion = "2005 to 2040 error, observed and IEA STPS 2020",
         error = abserrorplusdivergencestps2020) %>%
  select(MODEL, SCENARIO, REGION, variable, criterion, errortolerance, error)

# combine the above
nineerrorscenariolists <- bind_rows(error0520at01,
                                    error0520at02,
                                    error0520at03,
                                    error2040at01,
                                    error2040at02,
                                    error2040at03,
                                    error0540at01,
                                    error0540at02,
                                    error0540at03)

write_csv(nineerrorscenariolists,
          "Pielke-et-al-2021-files/Output-from-R-code/nineerrorscenariolists.csv")

### Kaya tables for scenario lists above 

# Create columns for error scenarios
errorscenarios <- nineerrorscenariolists %>%
  mutate(errorscenario = paste(criterion,errortolerance),
         scenarioinerrorcategory = 1) %>%
  spread(key = errorscenario, value = scenarioinerrorcategory) %>%
  group_by(MODEL, SCENARIO, REGION) %>%  
  summarise(divergence01t0520 = sum(`2005 to ~2020 error 0.1`,na.rm = T),
            divergence02t0520 = sum(`2005 to ~2020 error 0.2`,na.rm = T),
            divergence03t0520 = sum(`2005 to ~2020 error 0.3`,na.rm = T),
            divergence01t2040 = sum(`~2020 to 2040 divergence from IEA STPS 2020 0.1`,na.rm = T),
            divergence02t2040 = sum(`~2020 to 2040 divergence from IEA STPS 2020 0.2`,na.rm = T),
            divergence03t2040 = sum(`~2020 to 2040 divergence from IEA STPS 2020 0.3`,na.rm = T),
            divergence01t0540 = sum(`2005 to 2040 error, observed and IEA STPS 2020 0.1`,na.rm = T),
            divergence02t0540 = sum(`2005 to 2040 error, observed and IEA STPS 2020 0.2`,na.rm = T),
            divergence03t0540 = sum(`2005 to 2040 error, observed and IEA STPS 2020 0.3`,na.rm = T))

ar5kayafactors <- ar5kaya2 %>%
  left_join(errorscenarios) %>%
  group_by(MODEL, SCENARIO, REGION, variable, growtherror, 
           ieadivergencecps, ieadivergencestps, ieadivergencestps2020, 
           errorplusdivergencestps, errorplusdivergencestps2020,
           absgrowtherror, absieadivergencecps, absieadivergencestps,
           absieadivergencestps2020, abserrorplusdivergencestps,
           abserrorplusdivergencestps2020, errorpctile, ieadivergencecpspctile,
           ieadivergencestpspctile, ieadivergencestpspctile2020,
           errorplusdivergencestpspctile, errorplusdivergencestpspctile2020) %>%  
  summarise(divergence01t0520 = sum(divergence01t0520,na.rm = T),
            divergence02t0520 = sum(divergence02t0520,na.rm = T),
            divergence03t0520 = sum(divergence03t0520,na.rm = T),
            divergence01t2040 = sum(divergence01t2040,na.rm = T),
            divergence02t2040 = sum(divergence02t2040,na.rm = T),
            divergence03t2040 = sum(divergence03t2040,na.rm = T),
            divergence01t0540 = sum(divergence01t0540,na.rm = T),
            divergence02t0540 = sum(divergence02t0540,na.rm = T),
            divergence03t0540 = sum(divergence03t0540,na.rm = T))
  

sspkayafactors <- sspkaya2 %>%
  left_join(errorscenarios) %>%
  group_by(MODEL, SCENARIO, REGION, variable, growtherror, 
           ieadivergencecps, ieadivergencestps, ieadivergencestps2020, 
           errorplusdivergencestps, errorplusdivergencestps2020,
           absgrowtherror, absieadivergencecps, absieadivergencestps,
           absieadivergencestps2020, abserrorplusdivergencestps,
           abserrorplusdivergencestps2020, errorpctile, ieadivergencecpspctile,
           ieadivergencestpspctile, ieadivergencestpspctile2020,
           errorplusdivergencestpspctile, errorplusdivergencestpspctile2020) %>%    
  summarise(divergence01t0520 = sum(divergence01t0520,na.rm = T),
            divergence02t0520 = sum(divergence02t0520,na.rm = T),
            divergence03t0520 = sum(divergence03t0520,na.rm = T),
            divergence01t2040 = sum(divergence01t2040,na.rm = T),
            divergence02t2040 = sum(divergence02t2040,na.rm = T),
            divergence03t2040 = sum(divergence03t2040,na.rm = T),
            divergence01t0540 = sum(divergence01t0540,na.rm = T),
            divergence02t0540 = sum(divergence02t0540,na.rm = T),
            divergence03t0540 = sum(divergence03t0540,na.rm = T))

ar5kayastackedlists2005to2040 <- bind_rows(
  ar5kayafactors %>%
  filter(divergence01t0540 == 1) %>%
  mutate(filter = "0.1%/y tolerance 2005-2040") %>%
  group_by(MODEL, SCENARIO, REGION, variable) %>%
  select(MODEL, SCENARIO, REGION, variable, 
         filter, growtherror, ieadivergencestps2020, errorplusdivergencestps2020) %>%
  rename(`2005-2040 divergence from IEA STPS` = errorplusdivergencestps2020),
  ar5kayafactors %>%
    filter(divergence02t0540 == 1) %>%
    mutate(filter = "0.2%/y tolerance 2005-2040") %>%
    group_by(MODEL, SCENARIO, REGION, variable) %>%
    select(MODEL, SCENARIO, REGION, variable, 
           filter, growtherror, ieadivergencestps2020, errorplusdivergencestps2020) %>%
    rename(`2005-2040 divergence from IEA STPS` = errorplusdivergencestps2020),
  ar5kayafactors %>%
    filter(divergence03t0540 == 1) %>%
    mutate(filter = "0.3%/y tolerance 2005-2040") %>%
    group_by(MODEL, SCENARIO, REGION, variable) %>%
    select(MODEL, SCENARIO, REGION, variable, 
           filter, growtherror, ieadivergencestps2020, errorplusdivergencestps2020) %>%
    rename(`2005-2040 divergence from IEA STPS` = errorplusdivergencestps2020),
  ar5kayafactors %>%
    filter(divergence03t0540 == 0) %>%
    mutate(filter = "Other Scenarios") %>%
    group_by(MODEL, SCENARIO, REGION, variable) %>%
    select(MODEL, SCENARIO, REGION, variable, 
           filter, growtherror, ieadivergencestps2020, errorplusdivergencestps2020) %>%
    rename(`2005-2040 divergence from IEA STPS` = errorplusdivergencestps2020))

sspkayastackedlists2005to2040 <- bind_rows(
  sspkayafactors %>%
    filter(divergence01t0540 == 1) %>%
    mutate(filter = "0.1%/y tolerance 2005-2040") %>%
    group_by(MODEL, SCENARIO, REGION, variable) %>%
    select(MODEL, SCENARIO, REGION, variable, 
           filter, growtherror, ieadivergencestps2020, errorplusdivergencestps2020) %>%
    rename(`2005-2040 divergence from IEA STPS` = errorplusdivergencestps2020),
  sspkayafactors %>%
    filter(divergence02t0540 == 1) %>%
    mutate(filter = "0.2%/y tolerance 2005-2040") %>%
    group_by(MODEL, SCENARIO, REGION, variable) %>%
    select(MODEL, SCENARIO, REGION, variable, 
           filter, growtherror, ieadivergencestps2020, errorplusdivergencestps2020) %>%
    rename(`2005-2040 divergence from IEA STPS` = errorplusdivergencestps2020),
  sspkayafactors %>%
    filter(divergence03t0540 == 1) %>%
    mutate(filter = "0.3%/y tolerance 2005-2040") %>%
    group_by(MODEL, SCENARIO, REGION, variable) %>%
    select(MODEL, SCENARIO, REGION, variable, 
           filter, growtherror, ieadivergencestps2020, errorplusdivergencestps2020) %>%
    rename(`2005-2040 divergence from IEA STPS` = errorplusdivergencestps2020),
  sspkayafactors %>%
    filter(divergence03t0540 == 0) %>%
    mutate(filter = "Other Scenarios") %>%
    group_by(MODEL, SCENARIO, REGION, variable) %>%
    select(MODEL, SCENARIO, REGION, variable, 
           filter, growtherror, ieadivergencestps2020, errorplusdivergencestps2020) %>%
    rename(`2005-2040 divergence from IEA STPS` = errorplusdivergencestps2020))

ar5kayastackedlists2005to2020 <- bind_rows(
  ar5kayafactors %>%
    filter(divergence01t0520 == 1) %>%
    mutate(filter = "0.1%/y tolerance 2005-2020") %>%
    group_by(MODEL, SCENARIO, REGION, variable) %>%
    select(MODEL, SCENARIO, REGION, variable, 
           filter, growtherror),
  ar5kayafactors %>%
    filter(divergence02t0520 == 1) %>%
    mutate(filter = "0.2%/y tolerance 2005-2020") %>%
    group_by(MODEL, SCENARIO, REGION, variable) %>%
    select(MODEL, SCENARIO, REGION, variable, 
           filter, growtherror),
  ar5kayafactors %>%
    filter(divergence03t0520 == 1) %>%
    mutate(filter = "0.3%/y tolerance 2005-2020") %>%
    group_by(MODEL, SCENARIO, REGION, variable) %>%
    select(MODEL, SCENARIO, REGION, variable, 
           filter, growtherror),
  ar5kayafactors %>%
    filter(divergence03t0520 == 0) %>%
    mutate(filter = "Other Scenarios") %>%
    group_by(MODEL, SCENARIO, REGION, variable) %>%
    select(MODEL, SCENARIO, REGION, variable, 
           filter, growtherror))

sspkayastackedlists2005to2020 <- bind_rows(
  sspkayafactors %>%
    filter(divergence01t0520 == 1) %>%
    mutate(filter = "0.1%/y tolerance 2005-2020") %>%
    group_by(MODEL, SCENARIO, REGION, variable) %>%
    select(MODEL, SCENARIO, REGION, variable, 
           filter, growtherror),
  sspkayafactors %>%
    filter(divergence02t0520 == 1) %>%
    mutate(filter = "0.2%/y tolerance 2005-2020") %>%
    group_by(MODEL, SCENARIO, REGION, variable) %>%
    select(MODEL, SCENARIO, REGION, variable, 
           filter, growtherror),
  sspkayafactors %>%
    filter(divergence03t0520 == 1) %>%
    mutate(filter = "0.3%/y tolerance 2005-2020") %>%
    group_by(MODEL, SCENARIO, REGION, variable) %>%
    select(MODEL, SCENARIO, REGION, variable, 
           filter, growtherror),
  sspkayafactors %>%
    filter(divergence03t0520 == 0) %>%
    mutate(filter = "Other Scenarios") %>%
    group_by(MODEL, SCENARIO, REGION, variable) %>%
    select(MODEL, SCENARIO, REGION, variable, 
           filter, growtherror))

write_csv(ar5kayafactors,"Pielke-et-al-2021-files/Output-from-R-code/ar5kayafactors.csv")
write_csv(sspkayafactors,"Pielke-et-al-2021-files/Output-from-R-code/sspkayafactors.csv")

write_csv(ar5kayastackedlists2005to2040,
          "Pielke-et-al-2021-files/Output-from-R-code/ar5filterskayaplot.csv")
write_csv(sspkayastackedlists2005to2040,
          "Pielke-et-al-2021-files/Output-from-R-code/sspfilterskayaplot.csv")

write_csv(ar5kayastackedlists2005to2020,
          "Pielke-et-al-2021-files/Output-from-R-code/ar5filterskayaplot20052020.csv")
write_csv(sspkayastackedlists2005to2020,
          "Pielke-et-al-2021-files/Output-from-R-code/sspfilterskayaplot20052020.csv")

