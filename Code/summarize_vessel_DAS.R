# A script for summarizing DAS for nearshore surveys

library(tidyverse)
library(here)
library(tmaptools)
library(sf)

# Create data frames 
das.lm <- das.lbc <- das.fsv <- data.frame()

#2025
## Load nav data
load("C:/KLS/CODE/Github/estimATM/2506SH/Data/Nav/nav_data.Rdata")
nav.lm  <- read_GPX("C:/KLS/CODE/Github/estimATM/2506SH/Data/Nav/lm_nav.gpx")$track_points %>% st_set_geometry(NULL)
nav.lbc <- read_GPX("C:/KLS/CODE/Github/estimATM/2506SH/Data/Nav/lbc_nav.gpx")$track_points %>% st_set_geometry(NULL)

## Load nasc data
nasc.fsv  <- readRDS("C:/KLS/CODE/Github/estimATM/2506SH/Data/Backscatter/SH/nasc_vessel_SH.rds")
nasc.lbc <- readRDS("C:/KLS/CODE/Github/estimATM/2506SH/Data/Backscatter/LBC/nasc_vessel_LBC_nearshore.rds")
nasc.lm  <- readRDS("C:/KLS/CODE/Github/estimATM/2506SH/Data/Backscatter/LM/nasc_vessel_LM_nearshore.rds")

## Process FSV data
das.fsv.tmp <- nav %>% 
  mutate(date = date(time),
         year = year(date),
         vessel = "SH") %>% 
  group_by(vessel, year, date) %>% 
  tally(name = "DAS") %>% 
  group_by(vessel, year) %>% 
  tally(name = "DAS")

survey.fsv.tmp <- nasc.fsv %>% 
  mutate(date = date(datetime),
         year = year(date),
         vessel = "SH") %>% 
  group_by(vessel, year, date) %>% 
  tally(name = "DAS.nasc") %>% 
  group_by(vessel, year) %>% 
  tally(name = "DAS.nasc")

das.fsv.tmp <- left_join(das.fsv.tmp, survey.fsv.tmp)

## Process nearshore data
### LM
das.lm.tmp <- nav.lm %>% 
  mutate(date = date(time),
         year = year(date),
         vessel = "LM") %>% 
  group_by(vessel, year, date) %>% 
  tally(name = "DAS") %>% 
  group_by(vessel, year) %>% 
  tally(name = "DAS")

survey.lm.tmp <- nasc.lm %>% 
  mutate(date = date(datetime),
         year = year(date),
         vessel = "LM") %>% 
  group_by(vessel, year, date) %>% 
  tally(name = "DAS.nasc") %>% 
  group_by(vessel, year) %>% 
  tally(name = "DAS.nasc")

das.lm.tmp <- left_join(das.lm.tmp, survey.lm.tmp)

### LBC
das.lbc.tmp <- nav.lbc %>% 
  mutate(date = date(time),
         year = year(date),
         vessel = "LBC") %>% 
  group_by(vessel, year, date) %>% 
  tally(name = "DAS") %>% 
  group_by(vessel, year) %>% 
  tally(name = "DAS")

survey.lbc.tmp <- nasc.lbc %>% 
  mutate(date = date(datetime),
         year = year(date),
         vessel = "LBC") %>% 
  group_by(vessel, year, date) %>% 
  tally(name = "DAS.nasc") %>% 
  group_by(vessel, year) %>% 
  tally(name = "DAS.nasc")

das.lbc.tmp <- left_join(das.lbc.tmp, survey.lbc.tmp)

## Combine with other years
das.fsv <- bind_rows(das.fsv, das.fsv.tmp) 
das.lm  <- bind_rows(das.lm, das.lm.tmp) 
das.lbc <- bind_rows(das.lbc, das.lbc.tmp)

#2024
## Load nav data
load("C:/KLS/CODE/Github/estimATM/2407RL/Data/Nav/nav_data.Rdata")
load("C:/KLS/CODE/Github/estimATM/2407RL/Data/Nav/nav_data_nearshore.Rdata")
# nav.lm  <- read_GPX("C:/KLS/CODE/Github/estimATM/2407RL/Data/Nav/lm_nav.gpx")$track_points %>% st_set_geometry(NULL)
# nav.lbc <- read_GPX("C:/KLS/CODE/Github/estimATM/2407RL/Data/Nav/lbc_nav.gpx")$track_points %>% st_set_geometry(NULL)

## Load nasc data
nasc.fsv  <- readRDS("C:/KLS/CODE/Github/estimATM/2506SH/Data/Backscatter/SH/nasc_vessel_SH.rds")
nasc.lbc <- readRDS("C:/KLS/CODE/Github/estimATM/2506SH/Data/Backscatter/LBC/nasc_vessel_LBC_nearshore.rds")
nasc.lm  <- readRDS("C:/KLS/CODE/Github/estimATM/2506SH/Data/Backscatter/LM/nasc_vessel_LM_nearshore.rds")

## Process FSV data
das.fsv.tmp <- nav %>% 
  mutate(date = date(time),
         year = year(date),
         vessel = "SH") %>% 
  group_by(vessel, year, date) %>% 
  tally(name = "DAS") %>% 
  group_by(vessel, year) %>% 
  tally(name = "DAS")

survey.fsv.tmp <- nasc.fsv %>% 
  mutate(date = date(datetime),
         year = year(date),
         vessel = "SH") %>% 
  group_by(vessel, year, date) %>% 
  tally(name = "DAS.nasc") %>% 
  group_by(vessel, year) %>% 
  tally(name = "DAS.nasc")

das.fsv.tmp <- left_join(das.fsv.tmp, survey.fsv.tmp)

## Process nearshore data
### LM
das.lm.tmp <- nav.lm %>% 
  mutate(date = date(time),
         year = year(date),
         vessel = "LM") %>% 
  group_by(vessel, year, date) %>% 
  tally(name = "DAS") %>% 
  group_by(vessel, year) %>% 
  tally(name = "DAS")

survey.lm.tmp <- nasc.lm %>% 
  mutate(date = date(datetime),
         year = year(date),
         vessel = "LM") %>% 
  group_by(vessel, year, date) %>% 
  tally(name = "DAS.nasc") %>% 
  group_by(vessel, year) %>% 
  tally(name = "DAS.nasc")

das.lm.tmp <- left_join(das.lm.tmp, survey.lm.tmp)

### LBC
das.lbc.tmp <- nav.lbc %>% 
  mutate(date = date(time),
         year = year(date),
         vessel = "LBC") %>% 
  group_by(vessel, year, date) %>% 
  tally(name = "DAS") %>% 
  group_by(vessel, year) %>% 
  tally(name = "DAS")

survey.lbc.tmp <- nasc.lbc %>% 
  mutate(date = date(datetime),
         year = year(date),
         vessel = "LBC") %>% 
  group_by(vessel, year, date) %>% 
  tally(name = "DAS.nasc") %>% 
  group_by(vessel, year) %>% 
  tally(name = "DAS.nasc")

das.lbc.tmp <- left_join(das.lbc.tmp, survey.lbc.tmp)

## Combine with other years
das.fsv <- bind_rows(das.fsv, das.fsv.tmp) 
das.lm  <- bind_rows(das.lm, das.lm.tmp) 
das.lbc <- bind_rows(das.lbc, das.lbc.tmp)





# Combine all years
das.all <- das.fsv %>% 
  bind_rows(das.lm) %>% 
  bind_rows(das.lbc) %>% 
  arrange(vessel, year) %>% 
  select(year, everything())

# Write to file
write_csv(das.all, file = here("Output",paste0("das_summary_", min(das.all$year),"-",
                                               max(das.all$year), ".csv")))
