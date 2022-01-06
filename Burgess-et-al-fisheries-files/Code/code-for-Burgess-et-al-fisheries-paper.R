##################################
########### Background ###########
##################################

# This script generates tables used to make Figs. 1B, 
# 2A,C, 3A,B,C,F (population), 
# and 5 (carbon capture and CO2/GDP) in:
# Burgess, Becker, Fredston, and Brooks:
# "Perspectives on most plausible climate futures, 
#  and recommendations for using scenarios in fisheries 
#  and aquatic conservation research"
# This code produces csv files, which were the converted to JMP files
# to make the final figures
# Fig. 3D is redrawn with permission from Mora et al. (2017).
# The other figures are drawn from data files mad directly in JMP.
# All JMP data files used to make all figures (exc. Fig. 3D) are in
# the folder "JMP-data-files-for-figures".
# The jsl journal for making the figures from these files is 
# "Burgess-fish-scenarios-figures.jrn" in the 'Code' folder

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
library(lubridate)      # easily work with dates and times
library(fpp2)           # working with time series data
library(zoo)            # working with time series data
library(PearsonDS)
library(purrr)

#####################################
########### LOAD Data ###############
#####################################

### SSP database
sspdf <- read_csv(here("Data", "SSP_IAM_V2_201811.csv"))

###############################################################################################
########### Fig. 1B: 2005-2100 GDP per capita in OECD and Middle East and Africa ##############
###############################################################################################

worldGDP2005in2017USD <- 80.91
worldGDP2005in2005USD <- 65.952

worldGDP2011in2017USD <- 99.861
worldGDP2011in2011USD <- 95.1

gdpconversion2017to2005USD <- worldGDP2005in2005USD/worldGDP2005in2017USD
gdpconversion2011to2005USD <- gdpconversion2017to2005USD * (worldGDP2011in2017USD/worldGDP2011in2011USD)
# Conversion calculated from World Bank US GDP data: https://data.worldbank.org/indicator/NY.GDP.MKTP.PP.KD 

# Calculate 1960-2020 observations from OWID (1960-2018) and World Bank (2019-2020) data
owidregionlookup <- read_csv(here("Data/2020-Kaya-observations-update", 
                                  "OWID-region-lookup.csv"))
popobs <- read_csv(here("Data/2020-Kaya-observations-update", 
                        "population-since-1800.csv"))

pcgdppppobs1818 <- read_csv(here("Data/2020-Kaya-observations-update", 
                                 "gdp-per-capita-maddison-2020.csv")) %>%
  select(-`145446-annotations`) %>%
  left_join(popobs) %>%
  left_join(owidregionlookup) %>%
  filter(REGION != 'AG-NA',
         Year > 1959) %>%
  rename(pcgdp = `GDP per capita`,
         pop = `Population (historical estimates)`) %>%
  mutate(gdp = pcgdp * pop) %>%
  group_by(Year, REGION) %>%
  summarise(totalpop = sum(pop,na.rm = T),
            totalgdp = sum(gdp,na.rm = T)) %>%
  mutate(pcgdpthousand2005usd = (totalgdp/totalpop) * (gdpconversion2011to2005USD/1000)) 

pcgdppppobs9020 <- read_csv(here("Data/2020-Kaya-observations-update", 
                             "gdp-per-capita-worldbank.csv")) %>%
  left_join(popobs) %>%
  left_join(owidregionlookup) %>%
  filter(REGION != 'AG-NA',
         Year > 2017) %>%
  rename(pcgdp = `GDP per capita, PPP (constant 2017 international $)`,
         pop = `Population (historical estimates)`) %>%
  mutate(gdp = pcgdp * pop) %>%
  group_by(Year, REGION) %>%
  summarise(totalpop = sum(pop,na.rm = T),
            totalgdp = sum(gdp,na.rm = T)) %>%
  mutate(pcgdpthousand2005usd = (totalgdp/totalpop) * (gdpconversion2017to2005USD/1000)) 

wbowidnormalizer <- pcgdppppobs1818 %>%
  select(Year, REGION, pcgdpthousand2005usd) %>%
  filter(Year == 2018) %>%
  rename(pcgdpowid = pcgdpthousand2005usd) %>%
  left_join(pcgdppppobs9020 %>%
              select(Year, REGION, pcgdpthousand2005usd) %>%
              filter(Year == 2018) %>%
              rename(pcgdpwb = pcgdpthousand2005usd)) %>%
  mutate(wbnormalizer = pcgdpowid/pcgdpwb) %>%
  group_by(REGION) %>%
  select(REGION, wbnormalizer)

pcgdpobs <- bind_rows(pcgdppppobs1818,
                      pcgdppppobs9020 %>%
                        filter(Year > 2018) %>%
                        left_join(wbowidnormalizer) %>%
                        mutate(pcgdpthousand2005usd = pcgdpthousand2005usd * wbnormalizer) %>%
                        select(-wbnormalizer)) %>%
  filter(REGION == 'OECD90' |
           REGION == 'MAF') %>%
  select(-totalpop, -totalgdp) %>%
  spread(key = REGION, value = pcgdpthousand2005usd) %>%
  mutate(MODEL = 'Observations',
         SCENARIO = 'Observations') %>%
  rename(OECD = OECD90) %>%
  mutate_at(vars(Year),as.double) %>%
  select(MODEL, SCENARIO, Year, MAF, OECD)
  
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

fig1bdata <- sspmarkers %>%
  filter(VARIABLE == 'GDP|PPP' |
           VARIABLE == 'Population') %>%
  gather(key = 'Year', value = 'value', 
         -MODEL, -SCENARIO, -REGION, -VARIABLE, -UNIT) %>%
  select(-UNIT) %>%
  spread(key = VARIABLE, value = value) %>%
  mutate(`p.-c. GDP (thousand 2005 USD PPP)` = `GDP|PPP`/Population) %>%
  filter(REGION == 'R5.2OECD' |
           REGION == 'R5.2MAF') %>%
  select(MODEL, SCENARIO, Year, REGION, `p.-c. GDP (thousand 2005 USD PPP)`) %>%
  spread(key = REGION, value = `p.-c. GDP (thousand 2005 USD PPP)`) %>%
  rename(OECD = `R5.2OECD`,
         MAF = `R5.2MAF`) %>%
  mutate_at(vars(Year),as.double) %>%
  bind_rows(pcgdpobs)

write_csv(fig1bdata,
          "Burgess-et-al-fisheries-files/R-output-files/burgess-fish-scenarios-fig1b.csv")

###########################################################################
########### Fig. 2C: scenarios comparison: emissions by 2100 ##############
###########################################################################
fig2cdata <- sspdf %>%
  filter(REGION == 'World',
         VARIABLE == 'Emissions|CO2') %>%
  select(-VARIABLE, ,-UNIT) %>%
  mutate(ModelScenario = paste(MODEL, SCENARIO, sep = " ")) %>%
  gather(key = 'Year', value = 'CO2emissions', -MODEL, -SCENARIO, -REGION, -ModelScenario) %>%
  select(-MODEL,-SCENARIO) %>%
  mutate_at(vars(Year),as.double) %>%
  complete(Year = full_seq(Year, 1), ModelScenario) %>%
  arrange(ModelScenario, Year) %>%
  mutate(SSPscenario = ifelse(grepl("SSP1",ModelScenario),'SSP1',
                              ifelse(grepl("SSP2",ModelScenario),'SSP2',
                                     ifelse(grepl("SSP3",ModelScenario),'SSP3',
                                            ifelse(grepl("SSP4",ModelScenario),'SSP4',
                                                   ifelse(grepl("SSP5",ModelScenario),'SSP5',NA))))),
         Marker = ifelse((grepl("SSP1-19", ModelScenario) & grepl("IMAGE", ModelScenario)) |
                           (grepl("SSP1-26", ModelScenario) & grepl("IMAGE", ModelScenario)) |
                           (grepl("SSP2-34", ModelScenario) & grepl("MESSAGE", ModelScenario)) |
                           (grepl("SSP2-45", ModelScenario) & grepl("MESSAGE", ModelScenario)) |
                           (grepl("SSP4-60", ModelScenario) & grepl("GCAM", ModelScenario)) |
                           (grepl("SSP3-Baseline", ModelScenario) & grepl("AIM", ModelScenario)) |
                           (grepl("SSP5-Baseline", ModelScenario) & grepl("REMIND", ModelScenario)),'Marker','Non-marker'),
         RCPScenario = ifelse(grepl("19",ModelScenario),'RCP1.9',
                              ifelse(grepl("26",ModelScenario),'RCP2.6',
                                     ifelse(grepl("34",ModelScenario),'RCP3.4',
                                            ifelse(grepl("45",ModelScenario),'RCP4.5',
                                                   ifelse(grepl("60",ModelScenario),'RCP6.0',
                                                          ifelse(grepl("Baseline",ModelScenario),'Baseline',NA))))))) %>%
  group_by(ModelScenario,SSPscenario,RCPScenario,Marker) %>%
  mutate(inCO2emissions = spline(x=Year, y=CO2emissions , xout=Year)$y) %>%
  filter(Year > 2009) %>%
  summarise(CumulativeCO2post2010 = sum(inCO2emissions,na.rm = T)) %>%
  mutate(`Cumulative CO2 emissions (2010-2100, thousand Gt)` = CumulativeCO2post2010/1000000)

## Ranges from Pielke et al. (2022)
pbr2022ffi <- read_csv(here("Burgess-et-al-fisheries-files/Data-files/Pielke-et-al-2022", 
                            "Pielke-et-al-2022-scenarios-FFI.csv")) %>%
  rename(ModelScenario = Scenario) %>%
  gather(key = 'Year', value = 'ffico2', -ModelScenario) %>%
  mutate_at(vars(Year),as.double) %>%
  complete(Year = full_seq(Year, 1), ModelScenario) %>%
  arrange(ModelScenario, Year) %>%
  group_by(ModelScenario) %>%
  mutate(inCO2emissions = spline(x=Year, y=ffico2 , xout=Year)$y) %>%
  filter(Year > 2009) %>%
  summarise(CumulativeCO2post2010 = sum(inCO2emissions,na.rm = T)) %>%
  mutate(`Cumulative CO2 emissions (2010-2100, thousand Gt)` = CumulativeCO2post2010/1000000) %>%
  left_join(
    read_csv(here("Burgess-et-al-fisheries-files/Data-files/Pielke-et-al-2022", 
                  "Pielke-2022-scenariolist.csv")) %>%
      mutate(ModelScenario = paste(MODEL, SCENARIO, sep = "")) %>%
      select(ModelScenario, divergence0550steps, divergence0550aps, 
             tolerance03in2005to2050steps, tolerance03in2005to2050aps)
  )

write_csv(fig2cdata,
          "Burgess-et-al-fisheries-files/R-output-files/burgess-fish-scenarios-fig2c.csv")

write_csv(pbr2022ffi,
          "Burgess-et-al-fisheries-files/R-output-files/burgess-fish-scenarios-fig2c-PBR-ranges.csv")

#######################################################
########### Fig. 2A: scenario comparison ##############
#######################################################

## Show: FFI for SSP markers, FFI for IEA WEO 2020 STEPS, constant 2019 emissions, 
## FFI for median PBR, FF for median PBR, FFI for SSP3-4.5 with SSP2 population growth 

sspmarkersworld <- sspmarkers %>%
  filter(REGION == 'World')

# Marker FFI CO2
sspmarkersworldffi <- sspmarkersworld %>%
  filter(VARIABLE == 'Emissions|CO2|Fossil Fuels and Industry') 

### Historical FFI emissions and IEA 2021 projections from Pielke et al. 2022.
### Pielke et al.'s (PBR) median (vis-a-vis 2100 emissions) 
### 0.3-tolerance consistent ('plausible') emissions FFI
pbr2022 <- read_csv(here("Burgess-et-al-fisheries-files/Data-files/Pielke-et-al-2022", "PBR-2022-summary-for-fisheries-paper.csv")) %>%
  select(MODEL, SCENARIO, UNIT, Year, 
         `Emissions|CO2|Fossil Fuels and Industry`,
         `Min (0.3)`, `Max (0.3)`)

# Optional: Make SSP3-4.5 with SSP2 population growth
# popratiossp2vssp3 <- sspdf %>%
#  filter(VARIABLE == 'Population',
#         MODEL == 'AIM/CGE') %>% 
#  filter(SCENARIO == 'SSP2-Baseline' |
#           SCENARIO == 'SSP3-Baseline') %>%
#   gather(key = 'Year', value = 'Population', 
#          -MODEL, -SCENARIO, -REGION, -VARIABLE, -UNIT) %>%
#   spread(key = SCENARIO, value = Population) %>%
#   mutate(Pop2div3 = `SSP2-Baseline`/`SSP3-Baseline`) %>%
#   filter(REGION != 'World') %>%
#   select(-VARIABLE, -UNIT, -`SSP2-Baseline`, -`SSP3-Baseline`)
# 
# ssp3growth2popffi <- sspdf %>%
#   filter(SCENARIO == 'SSP3-45',
#          VARIABLE == 'Emissions|CO2|Fossil Fuels and Industry',
#          MODEL == 'AIM/CGE') %>% 
#   filter(REGION != 'World') %>%
#   gather(key = 'Year', value = 'Emissions|CO2|Fossil Fuels and Industry',
#          -MODEL, -SCENARIO, -REGION, -VARIABLE, -UNIT) %>%
#   left_join(popratiossp2vssp3) %>%
#   rename(emissions = `Emissions|CO2|Fossil Fuels and Industry`) %>%
#   mutate(`Emissions|CO2|Fossil Fuels and Industry` = emissions * Pop2div3) %>%
#   select(-SCENARIO, -emissions) %>%
#   mutate(SCENARIO = 'SSP3-45-SSP2pop') %>%
#   group_by(MODEL, SCENARIO, VARIABLE, UNIT, Year) %>%
#   summarise(`Emissions|CO2|Fossil Fuels and Industry` = sum(`Emissions|CO2|Fossil Fuels and Industry`,na.rm = T)) %>%
#   spread(key = Year, value = `Emissions|CO2|Fossil Fuels and Industry`)


# Merge to Fig. 2a file
fig2adata <- bind_rows(pbr2022 %>%
                         mutate(Year = as.double(Year)), 
                       sspmarkersworldffi %>%
                         select(-REGION, -VARIABLE) %>%
                         gather(key = 'Year', value = 'Emissions|CO2|Fossil Fuels and Industry',
                                -MODEL, -SCENARIO, -UNIT) %>%
                         mutate(Year = as.double(Year))
                       ) %>%
  mutate(`FFI CO2 emissions (Gt/y)` = 
           `Emissions|CO2|Fossil Fuels and Industry`/1000,
         `Min (0.3)` = `Min (0.3)`/1000, 
         `Max (0.3)` = `Max (0.3)`/1000) %>%
  select(-`Emissions|CO2|Fossil Fuels and Industry`) # %>% optional: add ssp3growth2popffi

write_csv(fig2adata,
          "Burgess-et-al-fisheries-files/R-output-files/burgess-fish-scenarios-fig2a.csv")
  
###################################################
########### Fig. 3A: Coal per capita ##############
###################################################
sspmarkerscoalpc <- sspmarkersworld %>%
  filter(VARIABLE == 'Primary Energy|Coal' |
           VARIABLE == 'Population') %>%
  gather(key = 'Year', value = 'value',
         -MODEL, -SCENARIO, -REGION, -UNIT, -VARIABLE) %>%
  select(-UNIT) %>%
  spread(value = value, key = VARIABLE) %>%
  mutate(`Primary coal per capita (GJ/y)` = 1000 * `Primary Energy|Coal`/Population) %>%
  select(Year, SCENARIO, `Primary coal per capita (GJ/y)`)
  
write_csv(sspmarkerscoalpc,
          "Burgess-et-al-fisheries-files/R-output-files/burgess-fish-scenarios-fig3a-scenarios.csv")

#########################################################
########### Fig. 3B: Marker LULC emissions ##############
#########################################################
sspmarkerslulc <- sspmarkersworld %>%
  filter(VARIABLE == 'Emissions|CO2|Land Use') %>%
  gather(key = Year, value = 'lulcemissions', -MODEL, 
         -SCENARIO, -REGION, -UNIT, -VARIABLE) %>%
  mutate(`LULC emissions (GtC/y)` = lulcemissions/1000) %>%
  rename(Series = SCENARIO) %>%
  select(Year, Series, `LULC emissions (GtC/y)`) %>%
  mutate_at(vars(Year),as.double) %>%
  bind_rows(
    read_csv(here("Burgess-et-al-fisheries-files/Data-files/GCB-data-and-Schwalm-et-al-2020", "land-use-summary-data.csv")) %>%
      mutate_at(vars(Year),as.double)
  )

write_csv(sspmarkerslulc,
          "Burgess-et-al-fisheries-files/R-output-files/burgess-fish-scenarios-fig3b-lulc.csv")

#####################################################
########### Fig. 3C: SSP5 & SSP3 pcGDP ##############
#####################################################

ssp5and3pcgdp <- sspmarkers %>%
  filter(SCENARIO == 'SSP5-Baseline' |
           SCENARIO == 'SSP3-Baseline') %>%
  filter(REGION != 'World') %>%
  filter(VARIABLE == 'GDP|PPP' |
           VARIABLE == 'Population') %>%
  gather(key = 'Year', value = 'value', 
         -MODEL, -SCENARIO, -REGION, -VARIABLE, -UNIT) %>%
  select(-UNIT) %>%
  spread(key = VARIABLE, value = value) %>%
  mutate(`p.-c. GDP (thousand 2005 USD PPP)` = `GDP|PPP`/Population)

write_csv(ssp5and3pcgdp,
          "Burgess-et-al-fisheries-files/R-output-files/burgess-fish-scenarios-fig3c.csv")
  

#####################################################
########### Fig. 3F: Population growth ##############
#####################################################

# Load UN and Vollset numbers
unvollsetpopulation <- read_csv(here("Burgess-et-al-fisheries-files/Data-files/Vollset-UN-pop-files", "Vollset-pop-reference-GraphClick-and-UNMedium.csv")) %>%
  rename(`Vollset et al. 2020 (reference)` = `Population (Vollset reference, billions)`,
         `UN 2019 medium` = `Population (UN medium, 2019)`) %>%
  gather(key = SCENARIO, value = 'Population (billions)', -Year)

# Compile SSP marker populations
sspmarkerpops <- sspmarkersworld %>%
  filter(VARIABLE == 'Population') %>%
  gather(key = 'Year', value = 'Population (millions)', 
         -MODEL, -SCENARIO, -REGION, -VARIABLE, -UNIT) %>%
  mutate(`Population (billions)` = `Population (millions)`/1000) %>%
  select(SCENARIO, Year, `Population (billions)`) %>%
  mutate(Year = as.double(Year))

popfigdata <- bind_rows(unvollsetpopulation, sspmarkerpops)

write_csv(popfigdata,
          "Burgess-et-al-fisheries-files/R-output-files/burgess-fish-scenarios-fig3f.csv")


#########################################################################################
########### Fig. 5A: Carbon capture from markerscenarios compared to history ############
#########################################################################################

# historical carbon capture
ccshistorical <- read_csv(here("Burgess-et-al-fisheries-files/Data-files/CCS-Institute", "CCS-capacity-CCSInstitute2020.csv")) %>%
  rename(`CCS (Mt)` = `Operational CCS Capacity (MtCO2)`) %>%
  mutate(SCENARIO = 'Operational Capacity')

# marker scenarios carbon capture
ccsmarkers <- sspmarkersworld %>%
  filter(VARIABLE == 'Emissions|CO2|Carbon Capture and Storage') %>%
  gather(key = 'Year', value = 'CCS (Mt)', 
         -MODEL, -SCENARIO, -REGION, -VARIABLE, -UNIT) %>%
  mutate(Year = as.double(Year))
  
ccsdata <- bind_rows(ccshistorical, ccsmarkers)  

write_csv(ccsdata,
          "Burgess-et-al-fisheries-files/R-output-files/burgess-fish-scenarios-fig5a.csv")

##############################################################################################
########### Fig. 5B: CO2/GDP reductions to get to 1.5&2 without carbon capture, ############## 
########### with SSP3 GDP and SSP2 pop, compared to history& markers            ##############
##############################################################################################
# CO2 intensity of GDP for markers
sspco2intensity <- sspmarkersworld %>%
  filter(VARIABLE == 'Emissions|CO2' |
           VARIABLE == 'GDP|PPP') %>%
  gather(key = 'Year', value = 'value', 
         -MODEL, -SCENARIO, -REGION, -VARIABLE, -UNIT) %>%
  select(-UNIT) %>%
  spread(key = VARIABLE, value = value) %>%
  mutate(CO2intensity = `Emissions|CO2`/`GDP|PPP`) %>%
  mutate(Year = as.double(Year))
  

# CO2 intensity historical
co2historical <- read_csv(here("Burgess-et-al-fisheries-files/Data-files/Andrew-mitigation-curves", 
                               "co2-mitigation-2c.csv")) %>%
  filter(Entity == 'Historical') %>%
  rename(SCENARIO = Entity,
         `Emissions|CO2` = `CO2 mitigation curves for 2C (Andrews & GCP, 2019)`)

co2path2 <- read_csv(here("Burgess-et-al-fisheries-files/Data-files/Andrew-mitigation-curves", 
                               "co2-mitigation-2c.csv")) %>%
  filter(Entity == 'Start in 2020') %>%
  rename(SCENARIO = Entity,
         `Emissions|CO2` = `CO2 mitigation curves for 2C (Andrews & GCP, 2019)`) %>%
  mutate(SCENARIO = "SSP3-45-SSP2pop",
         Code = '2-degree path')

co2path15 <- read_csv(here("Burgess-et-al-fisheries-files/Data-files/Andrew-mitigation-curves", 
                                "co2-mitigation-15c.csv")) %>%
  filter(Entity == 'Start in 2020') %>%
  rename(SCENARIO = Entity,
         `Emissions|CO2` = `CO2 mitigation curves for 1.5C (Andrews & GCP, 2019)`) %>%
  mutate(SCENARIO = "SSP3-45-SSP2pop",
         Code = '1.5-degree path')

# normalize GDP to B 2005 PPP
gdphistorical <- read_csv(here("Burgess-et-al-fisheries-files/Data-files/OWID", 
                               "world-gdp-over-the-last-two-millennia.csv")) %>%
  mutate(`GDP|PPP` = (gdpconversion2011to2005USD/1000000000) * 
           `World GDP in 2011 Int.$ (OWID based on World Bank & Maddison (2017))`) %>%
  select(Year, `GDP|PPP`) %>%
  add_row(Year = 2016, `GDP|PPP` = 117.454 * 1000 * gdpconversion2017to2005USD) %>% # add 2016-2020 from World Bank: https://data.worldbank.org/indicator/NY.GDP.MKTP.PP.KD
  add_row(Year = 2017, `GDP|PPP` = 121.871 * 1000 * gdpconversion2017to2005USD) %>%
  add_row(Year = 2018, `GDP|PPP` = 126.23 * 1000 * gdpconversion2017to2005USD) %>%
  add_row(Year = 2019, `GDP|PPP` = 129.799 * 1000 * gdpconversion2017to2005USD) %>%
  add_row(Year = 2020, `GDP|PPP` = 125.565 * 1000 * gdpconversion2017to2005USD) %>%
  mutate(SCENARIO = 'Historical')

# add a SSP3 pcGDP, SSP2 population scenario, to extrapolate the Andrew trajectories
popratiossp2vssp3 <- sspdf %>%
  filter(VARIABLE == 'Population',
         MODEL == 'AIM/CGE') %>% 
  filter(SCENARIO == 'SSP2-Baseline' |
           SCENARIO == 'SSP3-Baseline') %>%
   gather(key = 'Year', value = 'Population', 
          -MODEL, -SCENARIO, -REGION, -VARIABLE, -UNIT) %>%
   spread(key = SCENARIO, value = Population) %>%
   mutate(Pop2div3 = `SSP2-Baseline`/`SSP3-Baseline`) %>%
   filter(REGION != 'World') %>%
   select(-VARIABLE, -UNIT, -`SSP2-Baseline`, -`SSP3-Baseline`)

ssp3growth2popGDP <- sspdf %>%
  filter(SCENARIO == 'SSP3-45',
         VARIABLE == 'GDP|PPP',
         MODEL == 'AIM/CGE') %>% 
  filter(REGION != 'World') %>%
  gather(key = 'Year', value = 'GDP|PPP',
         -MODEL, -SCENARIO, -REGION, -VARIABLE, -UNIT) %>%
  left_join(popratiossp2vssp3) %>%
  rename(GDP = `GDP|PPP`) %>%
  mutate(`GDP|PPP` = GDP * Pop2div3) %>%
  select(-SCENARIO, -GDP) %>%
  mutate(SCENARIO = 'SSP3-45-SSP2pop') %>%
  group_by(MODEL, SCENARIO, VARIABLE, UNIT, Year) %>%
  summarise(`GDP|PPP` = sum(`GDP|PPP`,na.rm = T)) %>%
  mutate(Year = as.double(Year)) %>%
  group_by(Year) %>%
  select(SCENARIO, Year, `GDP|PPP`)


co2historicalandpaths <- bind_rows(co2historical, co2path2, co2path15) %>%
  left_join(
    bind_rows(gdphistorical, ssp3growth2popGDP)) %>%
  mutate(CO2intensity = `Emissions|CO2`/(`GDP|PPP`*1000000))
  
fig5bdata <- bind_rows(co2historicalandpaths %>%
                         select(SCENARIO, Code, Year, CO2intensity) %>%
                         rename(Path = Code),
                       sspco2intensity %>%
                         select(SCENARIO, Year, CO2intensity))  

write_csv(fig5bdata,
          "Burgess-et-al-fisheries-files/R-output-files/burgess-fish-scenarios-fig5b.csv")

