##################################
########### Background ###########
##################################

# This script runs the comparison of observed
# and projected Kaya factors described in:
# Burgess, Ritchie, Shapland, and Pielke Jr.:
# "IPCC baseline scenarios have over-projected CO2 emissions and economic growth".
# The code generates and stores csv tables used
# (via JMP) to make Figs. 2, 3, 4, S2, and S3. The jsl scripts used to 
# make these figures are also stored in the github repo.

# This version of the script updates the previous version, released on Feb 18, 2020.
# It adds analyses of catch-up rates for global economic growth, 
# in light of COVID-19 related developments.

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
         growtherror = growth0520 - growth0517,
         catchup40 = growth2040 + (growtherror*0.75),
         ieadivergencestps = growth2040 - iea1840stps,
         ieadivergencecps = growth2040 - iea1840cps)

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
  mutate(resulttype = "catchuprate")

ar5basekayacpsdivergence <- ar5growth %>%
  filter(`Baseline Scenario` == "Baseline") %>%
  select(MODEL,SCENARIO,REGION,variable,ieadivergencecps) %>%
  spread(key = variable, value = ieadivergencecps) %>%
  mutate(pcgdpmer = gdpmer - pop,
         energyintensity = energytpes - gdpmer,
         co2intensity = fossilco2 - energytpes) %>%
  select(-gdpmer,-energytpes) %>%
  gather(key = "variable", value = "percent", -MODEL,-SCENARIO,-REGION) %>%
  mutate(resulttype = "ieadivergencecps")

ar5basekayastpsdivergence <- ar5growth %>%
  filter(`Baseline Scenario` == "Baseline") %>%
  select(MODEL,SCENARIO,REGION,variable,ieadivergencestps) %>%
  spread(key = variable, value = ieadivergencestps) %>%
  mutate(pcgdpmer = gdpmer - pop,
         energyintensity = energytpes - gdpmer,
         co2intensity = fossilco2 - energytpes) %>%
  select(-gdpmer,-energytpes) %>%
  gather(key = "variable", value = "percent", -MODEL,-SCENARIO,-REGION) %>%
  mutate(resulttype = "ieadivergencestps")

ar5kaya <- bind_rows(ar5basekayaerrors,
                     ar5basekayacatchups,
                     ar5basekayacpsdivergence,
                     ar5basekayastpsdivergence) 

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
         growtherror = growth0520 - growth0517,
         catchup40 = growth2040 + (growtherror*0.75),
         ieadivergencestps = growth2040 - iea1840stps,
         ieadivergencecps = growth2040 - iea1840cps)

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

sspkaya <- bind_rows(sspbasekayaerrors,
                     sspbasekayacatchups,
                     sspbasekayacpsdivergence,
                     sspbasekayastpsdivergence) 

##### Combine observed and pcGDP catch-up for Fig. 4
# compare 2020-2040 catch-up rates to 
# 2005-2017 observations, 2020-2040 baseline projections,
# Christensen et al. (2018) (C18) expert range

## calculate growth errors for C18 expert projections 
## vs 2005-2017 observed (for C18 2010-2050)
## convert discrete growth (C18) to continuous

# convert ar5growth to table with pcgdp growth only
ar5pcgdponly <- ar5growth %>%
  filter(`Baseline Scenario` == "Baseline") %>%
  select(-`Baseline Scenario`,-UNIT,
         -ieadivergencecps, -ieadivergencestps,
         -iea1840cps, -iea1840stps) %>%
  filter(variable == "gdpmer"
         | variable == "pop") %>%
  gather(key = "timeframe", value = "percent", 
         -MODEL,-SCENARIO,-REGION,-variable) %>%
    spread(key = variable, value = percent) %>%
  mutate(pcgdpmer = gdpmer - pop) %>%
  select(-gdpmer,-pop) %>%
  spread(key = timeframe, value = pcgdpmer) %>%
  mutate(variable = "pcgdpmer")
ar5pcgdponly$DATABASE <- "AR5"

# convert sspbasegrowth to table with pcgdp growth only
ssppcgdponly <- sspbasegrowth %>%
  select(-UNIT,
         -ieadivergencecps, -ieadivergencestps,
         -iea1840cps, -iea1840stps) %>%
  filter(variable == "gdpppp"
         | variable == "pop") %>%
  gather(key = "timeframe", value = "percent", 
         -MODEL,-SCENARIO,-REGION,-variable) %>%
  spread(key = variable, value = percent) %>%
  mutate(pcgdpppp = gdpppp - pop) %>%
  select(-gdpppp,-pop) %>%
  spread(key = timeframe, value = pcgdpppp) %>%
  mutate(variable = "pcgdpppp")
ssppcgdponly$DATABASE <- "SSPs"

# merge ar5pcgdponly and ssppcgdponly
gdptbl <- bind_rows(ar5pcgdponly,ssppcgdponly) %>%
  gather(key = "growthmeasure", value = "percent", 
         -MODEL,-SCENARIO,-REGION,-variable,-DATABASE)

## add in Christensen growth rates
# for C18 comparison, pair: 
# MAF & Low Income
# LAM & Middle Income
# OECD90 & High Income
# Asia & China
# World & world

## make fig3tbcl
# rename cols for joining, & isolate expert 2010-2050 projections
# and make percentiles separate columns 
# make versions of the Christensen table that pair with each of SSPs and AR5
chrregs <- christensenregs %>%
  rename(REGION = ipccregion)
chrtbl <- christensentbl %>%
  mutate(pcgdpgrowth = 100*(log(1+(pcgdpgrowth/100)))) %>%
  rename(christensenregion = region) %>%
  filter(source == "expert") %>%
  select(-source)
chrtbl2 <- left_join(chrregs,chrtbl) %>%
  select(-christensenregion) %>%
  rename(SCENARIO = pctile,
         percent = pcgdpgrowth) %>%
  mutate_at(vars(SCENARIO),as.character) %>%
  mutate(DATABASE = "AR5",
         MODEL = ifelse(timeperiod == 1050, "Christensen2010to2050",
                        "Christensen2010to2100"),
         variable = "pcgdp",
         growthmeasure =  ifelse(timeperiod == 1050, "expert (2010-2050)",
                                 "expert (2010-2100)")) %>%
  select(-timeperiod)

chrtbl3 <- chrtbl2 %>%
  mutate(DATABASE = "SSPs")

# join gdptbl and chrtbl, and calculated chr pctiles as growth errors
# remove rows showing growth error and projected 2005-2020 growth
fig4pretbl <- bind_rows(gdptbl,chrtbl2,chrtbl3) 


################################################
##### Comparison of SSPs and IMF WEO ########### 
################# (Fig. 3) #####################
################################################

## Load IMF WEO data
# Oct 2019: 1980-2024
imfweo <- read_csv(here("Data", "WEOOct2019all.csv")) %>%
  type_convert(na = c("n/a")) 

# create list of IMF WEO countries, in order to create lookup table
# with IPCC regions
# imfcountrylist <- imfweo %>%
#   group_by(Country) %>%
#   summarise(mn05 = mean(`2005`)) %>%
#   select(-mn05)

# write_csv(imfcountrylist, here("Data", "imfcountries.csv")) 
# I did this once, and then added in lookups for IPCC regions by hand.
# I named the resulting table "imfregionlookup.csv"

# load IPCC-IMF region lookup table
imfregionlookup <- read_csv(here("Data", "imfregionlookup.csv"))

### US dollar converter (current -> constant 2011)

usdollarcurrentto2011 <- imfweo %>%
  filter(is.na(ISO) == F) %>%
  filter(ISO == "USA") %>%
  filter(`WEO Subject Code` == "NGDP_D") %>%    # US GDP deflator (current/constant $)
  select(-`WEO Country Code`,-`Subject Descriptor`,
         -`Subject Notes`,-`Country/Series-specific Notes`,
         -`Estimates Start After`) %>%
  gather(key = "year", value = "usgdpdeflator",
         -`WEO Subject Code`,-`ISO`,
         -Country,-Units,-Scale) %>%
  mutate_at(vars(year, usgdpdeflator),
            as.double) %>%
  mutate(usdcurrentto2011 = 98.118/usgdpdeflator) %>%
  select(year, usdcurrentto2011)

# calculate regional GDP per capita (PPP and MER) growth rates from IMF
imfweo <- imfweo %>%
  left_join(imfregionlookup) %>% # join region lookup table
  select(-`Estimates Start After`) %>%
  filter(is.na(ISO) == F) %>%
  filter(`WEO Subject Code` == "NGDPRPPPPC"    # select p.-c. GDP constant PPP
         | `WEO Subject Code` == "NGDP"        # select GDP current local currency
         | `WEO Subject Code` == "NGDPPC"      # select p.-c. GDP current local currency
         | `WEO Subject Code` == "NGDPD"       # select GDP current USD (MER)
  ) %>% 
  select(-`WEO Country Code`,-`ISO`,-`Subject Descriptor`,
         -`Subject Notes`,-`Country/Series-specific Notes`) %>%
  gather(key = "year", value = "value",
         -`WEO Subject Code`,-REGION,-Country,-Units,-Scale) %>%
  type_convert(na = c("n/a", "--")) %>%
  select(-Units,-Scale) %>%
  rename(series = `WEO Subject Code`) %>%
  spread(key = series, value = value) %>%
  left_join(usdollarcurrentto2011) %>% # add converter from current to constant USD
  mutate(population = NGDP/NGDPPC,                # calculate population (not given) from NGDP / NGDPPC,
         NGDPRPPP = NGDPRPPPPC * population,
         GDPMER = NGDPD * usdcurrentto2011)  # then multiply by NGDPRPPPPC to get contant PPP GDP (also not given)
# imfweo is now country-level pc-GDP to add 2020 projections to, and to consolidate regionally

imfweotblregion <- imfweo %>%
  group_by(REGION,year) %>%
  summarise(gdpmer = sum(GDPMER,na.rm = T),
            gdpppp = sum(NGDPRPPP,na.rm = T), # calculate sums of GDP and population by IPCC region
            population = sum(population,na.rm = T)) %>%
  group_by(REGION) %>%
  mutate(pcgdpppp = gdpppp/population,
         pcgdpmer = gdpmer/population) %>%
  mutate(pcgdpgrowthppp = 100*(log(pcgdpppp) - log(lag(pcgdpppp))),
         gdpgrowthppp = 100*(log(gdpppp) - log(lag(gdpppp))),
         pcgdpgrowthmer = 100*(log(pcgdpmer) - log(lag(pcgdpmer))),
         gdpgrowthmer = 100*(log(gdpmer) - log(lag(gdpmer))))

# calculate global GDP per capita growth rates from IMF  
imfweotblworld <- imfweotblregion %>%
  group_by(year) %>%
  summarise(gdpmer = sum(gdpmer,na.rm = T), # calculate sums of GDP and population by IPCC region
            gdpppp = sum(gdpppp,na.rm = T),
            population = sum(population,na.rm = T)) %>%
  mutate(pcgdpmer = gdpmer/population,
         pcgdpppp = gdpppp/population) %>%
  mutate(pcgdpgrowthppp = 100*(log(pcgdpppp) - log(lag(pcgdpppp))),
         gdpgrowthppp = 100*(log(gdpppp) - log(lag(gdpppp))),
         pcgdpgrowthmer = 100*(log(pcgdpmer) - log(lag(pcgdpmer))),
         gdpgrowthmer = 100*(log(gdpmer) - log(lag(gdpmer))),
         REGION = "World")

# combine global and regional numbers
imfweotbl <- bind_rows(imfweotblregion,imfweotblworld) %>%
  mutate_at(vars(year),as.double) %>%
  select(REGION, year, pcgdpppp, pcgdpmer, 
         pcgdpgrowthppp, pcgdpgrowthmer) %>%
  mutate(database = "IMF WEO Oct 2019",
         MODEL = "IMF WEO Oct 2019",
         SCENARIO = "IMF WEO Oct 2019")

##  Add new April 2020 projections
# April 2020
# Goes from 1980-2021, projection years indicated
# Use 2019-2021 from 2020 dataset; 
# compare updated projections to projections from Oct 2019 
# aggregates:
# imfweoapr2020aggs <- read_tsv(here("Data", "WEO_Data_April_2020_aggregates.xls"))
# countries:
imfweoapr2020countries <- read_tsv(here("Data", "WEOApr2020all.xls")) %>%
  type_convert(na = c("n/a")) %>%
  mutate(country = ifelse(ISO == "STP", "Sao Tome and Principe", # fixes a name loading error
                          ifelse(ISO == "CIV", "Cote d'Ivoire",
                                 Country))) %>%
  select(-Country) %>%
  rename(Country = country) %>%
  left_join(imfregionlookup) %>%
  #   filter(is.na(REGION == T)) #  optional, to check if countries are missing regions in the lookup table
  filter(is.na(ISO) == F) %>%
  filter(`WEO Subject Code` == "NGDP_RPCH"      # % change in GDP constant prices
         | `WEO Subject Code` == "NGDPRPPPPCPCH" # p.-c. GDP constant PPP % change
         | `WEO Subject Code` == "PPPGDP") %>%   # GDP PPP current prices
  select(-`WEO Country Code`,-`Subject Descriptor`,
         -`Subject Notes`,-`Country/Series-specific Notes`,
         -`Estimates Start After`) %>%
  gather(key = "year", value = "value",
         -`WEO Subject Code`,-`ISO`, -REGION,-Country,
         -Units,-Scale) %>%
  select(-Units,-Scale) %>%
  rename(series = `WEO Subject Code`) %>%
  spread(key = series, value = value) %>%
  rename(pctchangeGDPconstp = NGDP_RPCH,
         pctchangepcGDPconstp = NGDPRPPPPCPCH,
         GDPpppcurrentp = PPPGDP) %>% 
  mutate_at(vars(year, pctchangeGDPconstp, 
                 pctchangepcGDPconstp, GDPpppcurrentp),
            as.double) %>%
  # change to continuous units
  mutate(pcgdppppgrowthproj = 100 * log(1+(pctchangepcGDPconstp/100))) %>%
  select(Country, ISO, REGION, 
         year, pcgdppppgrowthproj) %>%
  filter(year > 2016) %>% # keep only post-2018 years
  left_join(imfweo) %>%
  rename(pcgdpppp2019data = NGDPRPPPPC,
         pop2019data = population) %>%
  mutate(pcgdpmer2019data = GDPMER/pop2019data) %>%
  select(Country, ISO, REGION, year,
         pcgdppppgrowthproj, pcgdpppp2019data,
         pcgdpmer2019data, pop2019data) %>%
  group_by(Country) %>%
  mutate(pcgdpppp2020dataimputed = ifelse(year == 2017, pcgdpppp2019data,
                                          ifelse(year == 2018, exp(log(lag(pcgdpppp2019data)) 
                                                                   + (pcgdppppgrowthproj/100)),
                                                 ifelse(year == 2019, exp(log(lag(pcgdpppp2019data, 2)) 
                                                                          + (lag(pcgdppppgrowthproj)/100)
                                                                          + (pcgdppppgrowthproj/100)),
                                                        ifelse(year == 2020, exp(log(lag(pcgdpppp2019data, 3)) 
                                                                                 + (lag(pcgdppppgrowthproj, 2)/100)
                                                                                 + (lag(pcgdppppgrowthproj)/100)
                                                                                 + (pcgdppppgrowthproj/100)),
                                                               ifelse(year == 2021, exp(log(lag(pcgdpppp2019data, 4)) 
                                                                                        + (lag(pcgdppppgrowthproj, 3)/100)
                                                                                        + (lag(pcgdppppgrowthproj, 2)/100)
                                                                                        + (lag(pcgdppppgrowthproj)/100)
                                                                                        + (pcgdppppgrowthproj/100)),
                                                               NA))))),
         gdpppp2020datatimputed = pcgdpppp2020dataimputed * pop2019data,
         pcgdpmer2020dataimputed = ifelse(year == 2017, pcgdpmer2019data,
                                          ifelse(year == 2018, exp(log(lag(pcgdpmer2019data)) 
                                                                   + (pcgdppppgrowthproj/100)),
                                                 ifelse(year == 2019, exp(log(lag(pcgdpmer2019data, 2)) 
                                                                          + (lag(pcgdppppgrowthproj)/100)
                                                                          + (pcgdppppgrowthproj/100)),
                                                        ifelse(year == 2020, exp(log(lag(pcgdpmer2019data, 3)) 
                                                                                 + (lag(pcgdppppgrowthproj, 2)/100)
                                                                                 + (lag(pcgdppppgrowthproj)/100)
                                                                                 + (pcgdppppgrowthproj/100)),
                                                               ifelse(year == 2021, exp(log(lag(pcgdpmer2019data, 4)) 
                                                                                        + (lag(pcgdppppgrowthproj, 3)/100)
                                                                                        + (lag(pcgdppppgrowthproj, 2)/100)
                                                                                        + (lag(pcgdppppgrowthproj)/100)
                                                                                        + (pcgdppppgrowthproj/100)),
                                                                      NA))))),
         gdpmer2020datatimputed = pcgdpmer2020dataimputed * pop2019data)


imfweo2020tblregion <- imfweoapr2020countries %>%
  group_by(REGION,year) %>%
  summarise(gdpppp = sum(gdpppp2020datatimputed,na.rm = T), # calculate sums of GDP and population by IPCC region
            gdpmer = sum(gdpmer2020datatimputed,na.rm = T),
            population = sum(pop2019data,na.rm = T)) %>%
  group_by(REGION) %>%
  mutate(pcgdpppp = gdpppp/population,
         pcgdpmer = gdpmer/population) %>%
  mutate(pcgdpgrowthppp = 100*(log(pcgdpppp) - log(lag(pcgdpppp))),
         gdpgrowthppp = 100*(log(gdpppp) - log(lag(gdpppp))),
         pcgdpgrowthmer = 100*(log(pcgdpmer) - log(lag(pcgdpmer))),
         gdpgrowthmer = 100*(log(gdpmer) - log(lag(gdpmer))))

# calculate global GDP per capita growth rates from IMF  
imfweo2020tblworld <- imfweo2020tblregion %>%
  group_by(year) %>%
  summarise(gdpppp = sum(gdpppp,na.rm = T), # calculate sums of GDP and population by IPCC region
            gdpmer = sum(gdpmer,na.rm = T),
            population = sum(population,na.rm = T)) %>%
  mutate(pcgdpppp = gdpppp/population,
         pcgdpmer = gdpmer/population) %>%
  mutate(pcgdpgrowthppp = 100*(log(pcgdpppp) - log(lag(pcgdpppp))),
         gdpgrowthppp = 100*(log(gdpppp) - log(lag(gdpppp))),
         pcgdpgrowthmer = 100*(log(pcgdpmer) - log(lag(pcgdpmer))),
         gdpgrowthmer = 100*(log(gdpmer) - log(lag(gdpmer))),
         REGION = "World")

imfweo2020tbl <- bind_rows(imfweo2020tblregion,imfweo2020tblworld) %>%
  mutate_at(vars(year),as.double) %>%
  select(REGION, year, pcgdpppp, pcgdpmer, 
         pcgdpgrowthppp, pcgdpgrowthmer) %>%
  mutate(database = "IMF WEO April 2020",
         MODEL = "IMF WEO April 2020",
         SCENARIO = "IMF WEO April 2020")


# check IMF numbers against IEA
ieapcdgpgrowth <- ieadf %>%
  rename(REGION = ipccregion) %>%
  filter(REGION != "AG-NA") %>%
  gather(key = "year", value = "value", -iearegion, -REGION,-variable) %>%
  spread(key = variable, value = value) %>%
  group_by(REGION, year) %>%
  summarise(gdpppp = sum(gdpppp, na.rm = T),
            pop = sum(pop, na.rm = T)) %>%
  mutate(pcgdpppp = gdpppp/pop,
         pcgdpgrowth = 100*(log(pcgdpppp) - log(lag(pcgdpppp))),
         gdpgrowth = 100*(log(gdpppp) - log(lag(gdpppp)))) %>%
  mutate_at(vars(year),as.double) %>%
  select(REGION, year, pcgdpppp, pcgdpgrowth) %>%
  mutate(database = "IEA",
         MODEL = "IEA",
         SCENARIO = "IEA")

# calculate growth rates in SSP baseline scenarios
ssppopgdp <- sspbaselinedf %>%
  filter(VARIABLE == "Population"
         | VARIABLE == "GDP|PPP") %>%
  gather(key = "year", value = "value",
         -MODEL,-SCENARIO,-VARIABLE,-UNIT,-REGION) %>%
  select(-UNIT) %>%
  spread(key = VARIABLE, value = value) %>%
  rename(gdpppp = `GDP|PPP`,
         population = Population) %>%
  mutate(pcgdpppp = gdpppp/population) %>%
  select(-gdpppp,-population) %>%
  group_by(MODEL,SCENARIO,REGION) %>%
  mutate_at(vars(year),as.double) %>%
  mutate(lnpcgdpppp = log(pcgdpppp)) %>% # convert pcgdp to log scale so linear interpolation preserves growth rate.
  complete(year = full_seq(year, 1),MODEL,SCENARIO,REGION) %>% # expand table to annual
  arrange(MODEL,SCENARIO,REGION, year) %>% 
  mutate(inlnpcgdp = na.interpolation(lnpcgdpppp)) %>% # linearly interpolate missing annual values
  mutate(pcgdpgrowth = 100*(inlnpcgdp - lag(inlnpcgdp))) %>% # calculate imputed annual growth rates
  mutate(database = "SSPs") 


# join IEA, IMF, and SSP tables
fig3tbl <- bind_rows(ssppopgdp, imfweotbl, imfweo2020tbl, ieapcdgpgrowth)



#######################################################
##### Updated catch-up-rate analysis (p.-c. GDP) ###### 
#################### (Fig. 4) #########################
#######################################################

## Catch-up analysis, focus on SSPs 
## (since results are highly similar to AR5, and SSPs are used in AR6)
# 1. old catch-up (already done above) (figure a,b)
# 2. catch up including COVID projections (figure a,b)
# 3. Relationship between how much cumulative growth is
#    lost to COVID and emissions? (figure a,b)
# 4. Repeat above with 2100, comparing to christensen (figure a,b)
# 5. What happens if bias is equal to average of past 5-y IMF bias?
# 6. What happens if global bias is equal to mean - median di,t?

## 2. catch up including COVID projections (figure a,b)
# isolate IMF April 2020 projections for 2018, 2019, 2020, 2021, 
# and use them to update the catchup40 calculation. Calculate 2018-2020 average 
# for each region (growth1820) and 2021 (growth21). Then:
# 'catchupcovid40' = catchup40 + 0.75*((growth0517 - growth1820)/(20/3))
#                    + ((growth2040 - growth21)/(20))
# 2nd term on RHS above corrects for the difference between 
# 2005-2017 average and 2018-2020 average
# 3rd term on RHS above corrects for the difference between 
# 2020-2040 projection and 2021 projection

# AR5 2100 GDP per capita
ar5v1gdppop <- read_excel(here("Data", "ar5_public_version1-gdp.xlsx"), 
                                        sheet = "ar5_public_version102_compare_c",
                          col_types = c("text", "text", "text", "text", "text",
                                        "numeric", "numeric", "numeric", "numeric",
                                        "numeric", "numeric", "numeric", "numeric",
                                        "numeric", "numeric", "numeric", "numeric",
                                        "numeric", "numeric", "numeric", "numeric",
                                        "numeric", "numeric", "numeric", "numeric"))  %>%
  filter(!is.na(`2100`)) %>%
  filter(VARIABLE == "Population" |
           VARIABLE == "GDP|MER") %>%
  left_join(ipccscenariolist) %>%
  filter(`Baseline Scenario` == "Baseline") %>%
  mutate(variable = ifelse(VARIABLE == "GDP|MER", "gdpmer", 
                           "pop")) %>%
  select(-VARIABLE) %>%
  mutate(growth20100 = (100/80)*(log(`2100`) - log(`2020`))) %>%
  select(MODEL, SCENARIO, REGION, variable, growth20100) %>%
  spread(key = variable, value = growth20100) %>%
  mutate(pcgdpmer = gdpmer - pop) %>%
  select(-gdpmer, -pop) %>%
  rename(percent = pcgdpmer) %>%
  mutate(variable = "pcgdpmer",
         DATABASE = "AR5",
         growthmeasure = "growth20100")


# SSP 2100 GDP per capita projections
sspbase20100 <- sspbaselinedf %>%
  select(MODEL,SCENARIO,REGION,VARIABLE,UNIT,`2020`,`2100`) %>%
  filter(VARIABLE == "GDP|PPP" 
         | VARIABLE == "Primary Energy"
         | VARIABLE == "Population") %>%
  mutate(variable = ifelse(VARIABLE == "GDP|PPP", "gdpppp", 
                           ifelse(VARIABLE == "Primary Energy","energytpes", 
                                  "pop"))) %>%
  filter(variable == "gdpppp" |
           variable == "pop") %>%
  select(-VARIABLE) %>%
  mutate(growth20100 = (100/80)*(log(`2100`) - log(`2020`))) %>%
  select(-UNIT, -`2100`,-`2020`) %>%
  spread(key = variable, value = growth20100) %>%
  mutate(pcgdpppp = gdpppp - pop) %>%
  select(-gdpppp, -pop) %>%
  rename(percent = pcgdpppp) %>%
  mutate(variable = "pcgdpppp",
         DATABASE = "SSPs",
         growthmeasure = "growth20100")

fig4tbl <- gdptbl %>%
  bind_rows(sspbase20100, ar5v1gdppop) %>%
  spread(value = percent, key = growthmeasure) %>%
  left_join(imfweo2020tbl %>%
              select(REGION, year, pcgdpgrowthmer, pcgdpgrowthppp) %>%
              rename(pcgdpmer = pcgdpgrowthmer,
                     pcgdpppp = pcgdpgrowthppp) %>%
              gather(key = "variable", value = "pcgdpgrowth",
                     -REGION, -year) %>%
              spread(key = year, value = pcgdpgrowth) %>%
              mutate(growth1820 = (`2018` + `2019` + `2020`)/3,
                     growth20 = `2020`,
                     growth21 = `2021`,
                     covideffect = ((`2018` + `2019` 
                                     - growth20 - growth21)/2)) %>%
              select(REGION, variable, growth1820, 
                     growth20, growth21, covideffect)
              ) %>%
  mutate(catchup100 = growth20100 + (growtherror*(15/80)),
         catchupcovid40 = catchup40 + 
           0.75*((growth0517 - growth1820)/(15/3)) + 
              ((growth2040 - growth21)/(20)),
         catchupcovid100 = catchup100 + 
           (15/80)*((growth0517 - growth1820)/(15/3)) + 
           ((growth20100 - growth21)/(80)),
         # calculate COVID effect on the growth rate to 2040 and 2100:
         covideffect40 = (0.75*(growth0520 - growth20)/(15)) + 
           ((growth2040 - growth21)/(20)),
         covideffect100 = ((15/80)*(growth0520 - growth20)/(15)) + 
           ((growth20100 - growth21)/(80)),
         catchupcovid402x = catchupcovid40 + (covideffect*(2/20)),
         catchupcovid1002x = catchupcovid100 + (covideffect*(2/80))
         ) %>%
  gather(key = "growthmeasure", value = "percent",
         -MODEL, -SCENARIO, -REGION, -variable, -DATABASE) %>%
  bind_rows(chrtbl2,chrtbl3) %>%
  filter(growthmeasure == "growth0517" |
           growthmeasure == "growth2040" |
           growthmeasure == "expert (2010-2050)" |
           growthmeasure == "catchup40" |
           growthmeasure == "catchupcovid40" |
           growthmeasure == "catchupcovid402x" |
           growthmeasure == "growth20100" |
           growthmeasure == "expert (2010-2100)" |
           growthmeasure == "catchup100" |
           growthmeasure == "catchupcovid100" |
           growthmeasure == "catchupcovid1002x")

##############################
##### Export files ########### 
##############################

##### EXPORT CSV FOR FIG 2a and S3 (CREATED IN JMP)
write_csv(ar5kaya,"ar5kaya.csv")

##### EXPORT CSV FOR FIG 2b and S3 (CREATED IN JMP)
write_csv(sspkaya,"sspkaya.csv")

##### EXPORT CSV FOR FIG 3 (CREATED IN JMP)
write_csv(fig3tbl,"fig3tbl.csv")

##### EXPORT CSV FOR FIG 4 (CREATED IN JMP)
write_csv(fig4tbl,"fig4tbl.csv")

#####################################################
##### Sensitivity analysis for economic ############# 
######### growth overprojection (Fig. S2) ###########
#####################################################

## Sensitivity test 1: eliminate 2008-2009 and average other 2005-2017 years
## call (this - unadjusted growth): sensadj1

obsgrowthsens1 <- ieasummary %>%
  filter(variable != "fossilco2",
         variable != "energytpes") %>%
  mutate(adjgrowth0517 = 100*
           ((
             (log(`2017`) - log(`2016`)) +
               (log(`2016`) - log(`2015`)) +
               (log(`2015`) - log(`2014`)) +
               (log(`2014`) - log(`2013`)) +
               (log(`2013`) - log(`2012`)) +
               (log(`2012`) - log(`2011`)) +
               (log(`2011`) - log(`2010`)) +
               (log(`2008`) - log(`2007`)) +
               (log(`2007`) - log(`2006`)) +
               (log(`2006`) - log(`2005`)))
            /10)) %>%
  select(ipccregion,variable,adjgrowth0517) %>%
  spread(key = variable, value = adjgrowth0517) %>%
  mutate(adjpcgdpmer0517 = gdpmer - pop,
         adjpcgdpppp0517 = gdpppp - pop) %>%
  select(-gdpppp,-gdpmer,-pop) %>%
  left_join(obsgrowth2) %>%
  mutate(pcgdpmer = adjpcgdpmer0517 - pcgdpmer0517,
         pcgdpppp = adjpcgdpppp0517 - pcgdpppp0517) %>%
  select(-pcgdpmer0517,-pcgdpppp0517,-adjpcgdpmer0517,-adjpcgdpppp0517) %>%
  gather(key = "variable", value = "sensadj1", 
         -ipccregion) %>%
  rename(REGION = ipccregion)
  

## Sensitivity test 2: add IMF 2017-2020 growth rate to 2005-2017 averaging, 
## call (this - unadjusted growth): sensadj2

obsgrowthsens2 <- ieasummary %>%
  filter(variable != "fossilco2",
         variable != "energytpes") %>%
  mutate(adjgrowth0517 = 100*
           ((log(`2017`) - log(`2005`))
            /12)) %>%
  select(ipccregion,variable,adjgrowth0517) %>%
  spread(key = variable, value = adjgrowth0517) %>%
  left_join(
    imfweo2020tbl %>%
      select(REGION, year, pcgdpgrowthmer, pcgdpgrowthppp) %>%
      rename(ipccregion = REGION,
             pcgdpmer = pcgdpgrowthmer,
             pcgdpppp = pcgdpgrowthppp) %>%
      gather(key = "variable", value = "pcgdpgrowth",
             -ipccregion, -year) %>%
      spread(key = year, value = pcgdpgrowth) %>%
      mutate(growth1820 = (`2018` + `2019` + `2020`)/3) %>%
      select(ipccregion, variable, growth1820) %>%
      spread(key = variable, value = growth1820) %>%
      rename(ppp1820 = pcgdpppp,
             mer1820 = pcgdpmer)
  ) %>%
  mutate(adjpcgdpmer0517 = ((12/15)*(gdpmer - pop)) +
           ((3/15)*(mer1820)),
         adjpcgdpppp0517 = ((12/15)*(gdpppp - pop)) +
           ((3/15)*(ppp1820))) %>%
  select(-gdpppp,-gdpmer,-pop, -mer1820, -ppp1820) %>%
  left_join(obsgrowth2) %>%
  mutate(pcgdpmer = adjpcgdpmer0517 - pcgdpmer0517,
         pcgdpppp = adjpcgdpppp0517 - pcgdpppp0517) %>%
  select(-pcgdpmer0517,-pcgdpppp0517,-adjpcgdpmer0517,-adjpcgdpppp0517) %>%
  gather(key = "variable", value = "sensadj2", 
         -ipccregion) %>%
  rename(REGION = ipccregion)

# calculate difference between these growth rates and unadjusted 2005-2017,
# and subtract this difference from errors shown in Fig. 2

obsgrowthsens <- left_join(obsgrowthsens1,obsgrowthsens2)

ar5sensadjpcgdp <- ar5basekayaerrors %>%
  filter(variable == "pcgdpmer") %>%
  left_join(obsgrowthsens) %>%
  mutate(DATABASE = "AR5",
         grerrsens1 = percent - sensadj1,
         grerrsens2 = percent - sensadj2)

sspsensadjpcgdp <- sspbasekayaerrors %>%
  filter(variable == "pcgdpppp") %>%
  left_join(obsgrowthsens) %>%
  mutate(DATABASE = "SSPs",
         grerrsens1 = percent - sensadj1,
         grerrsens2 = percent - sensadj2)

figs2tbl <- bind_rows(ar5sensadjpcgdp,sspsensadjpcgdp) %>%
  select(-sensadj1,-sensadj2,-resulttype) %>%
  gather(key = "sensitivity", value = "percent", 
         -REGION,-MODEL,-SCENARIO,-DATABASE,-variable)
  
##### EXPORT CSV FOR FIG S2 (CREATED IN JMP)
write_csv(figs2tbl,"figs2tbl.csv")

