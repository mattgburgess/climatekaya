##################################
########### Background ###########
##################################

# This script runs the comparison of observed
# and projected Kaya factors described in:
# Burgess, Ritchie, Shapland, and Pielke Jr.:
# "Improving Climate Change Scenarios for Science and Policy".
# The code generates and stores csv tables used
# (via JMP) to make Figs. 2, S3, S4, S5.

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

## Add up variables by IPCC REGION
ieasummary <- ieadf %>%
  select(-iearegion) %>%
  group_by(variable, ipccregion) %>%
  summarise_all(funs(sum(.,na.rm = T)))

### Calculate 2005-2017 growth rates in Kaya factors, by region
### growth = (ln(2017) - ln(2005))/12
obsgrowth <- ieasummary %>%
  mutate(growth0517 = 100*((log(`2017`) - log(`2005`))/12)) %>%
  select(ipccregion,variable,growth0517) # %>%
  # spread(key = variable, value = growth0517) # optional for splitting table


### Calculate Kaya factor growth errors for AR5 and SSP databases by region
### Resulting dfs: Region, MODEL, SCENARIO, Baseline Scenario,('Baseline' or 'Policy'), 
###                Kaya factor, 2002-2020 growth error (growtherror), 
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
    select(-`2005`,-`2020`,-`2040`,-hf2005,-hf2020,-hf2040) %>%
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
# calculate projected - observed
# calculate catch-up rate
ar5growth <- ar5keyvars %>%
  mutate(growth0520 = 100*((log(`2020`) - log(`2005`))/15),
         growth2040 = 100*((log(`2040`) - log(`2020`))/20),
         ipccregion = REGION) %>%
  left_join(ipccscenariolist) %>%
  select(-`2005`,-`2020`,-`2040`) %>%
  left_join(obsgrowth) %>%
  select(-ipccregion) %>%
  mutate(growtherror = growth0520 - growth0517,
         catchup40 = growth2040 + (growtherror*0.75))

# convert baseline growth errors and catchups into Kaya factors
ar5basekayaerrors <- ar5growth %>%
  filter(`Baseline Scenario` == "Baseline") %>%
  select(MODEL,SCENARIO,REGION,variable,growtherror) %>%
  spread(key = variable, value = growtherror) %>%
  mutate(pcgdpmer = gdpmer - pop,
         energyintensity = energytpes - gdpmer,
         co2intensity = fossilco2 - energytpes) %>%
  select(-gdpmer,-energytpes) %>%
  gather(key = "variable", value = "percent", -MODEL,-SCENARIO,-REGION) %>%
  mutate(resulttype = "growtherror")


ar5basekayacatchups <- ar5growth %>%
  filter(`Baseline Scenario` == "Baseline") %>%
  select(MODEL,SCENARIO,REGION,variable,catchup40) %>%
  spread(key = variable, value = catchup40) %>%
  mutate(pcgdpmer = gdpmer - pop,
         energyintensity = energytpes - gdpmer,
         co2intensity = fossilco2 - energytpes) %>%
  select(-gdpmer,-energytpes) %>%
  gather(key = "variable", value = "percent", -MODEL,-SCENARIO,-REGION) %>%
  mutate(resulttype = "catchuperror")

ar5kaya <- bind_rows(ar5basekayaerrors,ar5basekayacatchups) 
  
##### EXPORT CSV FOR FIGS 2, S4 (CREATED IN JMP)
write_csv(ar5kaya,"ar5kaya.csv")

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
sspbaselinedf <- sspdf %>%
  filter(grepl("Baseline", SCENARIO))

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
sspbasegrowth <- sspbasekeyvars %>%
  mutate(growth0520 = 100*((log(`2020`) - log(`2005`))/15),
         growth2040 = 100*((log(`2040`) - log(`2020`))/20)) %>%
  select(-`2005`,-`2020`,-`2040`) %>%
  left_join(obsgrowth) %>%
  mutate(growtherror = growth0520 - growth0517,
         catchup40 = growth2040 + (growtherror*0.75))

## convert baseline growth errors and catchups into Kaya factors
sspbasekayaerrors <- sspbasegrowth %>%
  select(MODEL,SCENARIO,REGION,variable,growtherror) 
sspbasekayaerrors$index <- seq(1:7)
sspbasekayaerrors <- sspbasekayaerrors %>%
  mutate(scenindex = paste(MODEL,SCENARIO,index)) %>%
  spread(key = variable, value = growtherror) %>%
  mutate(pcgdpppp = gdpppp - pop,
         energyintensity = energytpes - gdpppp,
         co2intensity = fossilco2 - energytpes) %>%
  select(-gdpppp,-energytpes) %>%
  gather(key = "variable", value = "percent", 
         -MODEL,-SCENARIO,-REGION,-index,-scenindex) %>%
  mutate(resulttype = "growtherror")

sspbasekayacatchups <- sspbasegrowth %>%
  select(MODEL,SCENARIO,REGION,variable,catchup40)
sspbasekayacatchups$index <- seq(1:7)
sspbasekayacatchups <-sspbasekayacatchups %>%
  mutate(scenindex = paste(MODEL,SCENARIO,index)) %>%
  spread(key = variable, value = catchup40) %>%
  mutate(pcgdpppp = gdpppp - pop,
         energyintensity = energytpes - gdpppp,
         co2intensity = fossilco2 - energytpes) %>%
  select(-gdpppp,-energytpes) %>%
  gather(key = "variable", value = "percent", 
         -MODEL,-SCENARIO,-REGION,-index,-scenindex) %>%
  mutate(resulttype = "catchup")

sspkaya <- bind_rows(sspbasekayaerrors,sspbasekayacatchups) 

##### EXPORT CSV FOR FIGS S3, S5 (CREATED IN JMP)
write_csv(sspkaya,"sspkaya.csv")
