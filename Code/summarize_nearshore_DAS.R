# A script for summarizing DAS for nearshore surveys

library(tidyverse)
library(here)

# Create data frames 
das.lm <- das.lbc <- das.fsv <- data.frame()

#2019
load("C:/KLS/CODE/Github/estimATM/1907RL/Data/Nav/nav_data.Rdata")
nasc.fsv  <- readRDS("C:/KLS/CODE/Github/estimATM/1907RL/Data/Backscatter/RL/nasc_vessel_RL.rds")
nasc.lbc <- readRDS("C:/KLS/CODE/Github/estimATM/1907RL/Data/Backscatter/LBC/nasc_vessel_LBC_nearshore.rds")
nasc.lm  <- readRDS("C:/KLS/CODE/Github/estimATM/1907RL/Data/Backscatter/LM/nasc_vessel_LM_nearshore.rds")

das.fsv.tmp <- nav %>% 
  mutate(date = date(datetime),
         year = year(date)) %>% 
  group_by(year, date) %>% 
  tally(name = "DAS") %>% 
  group_by(year) %>% 
  tally(name = "DAS")

survey.fsv.tmp <- nasc.fsv %>% 
  mutate(date = date(datetime),
         year = year(date)) %>% 
  group_by(year, date) %>% 
  tally(name = "DAS.nasc") %>% 
  group_by(year) %>% 
  tally(name = "DAS.nasc")

das.fsv.tmp <- left_join(das.fsv.tmp, survey.fsv.tmp)

nav.lm  <- readRDS("C:/KLS/CODE/Github/estimATM/2506SH/Data/Nav/nav_vessel_LM.rds")

das.lm.tmp <- nav.lm %>% 
  mutate(date = date(datetime),
         year = year(date)) %>% 
  group_by(year, date) %>% 
  tally(name = "DAS") %>% 
  group_by(year) %>% 
  tally(name = "DAS")

survey.lm.tmp <- nasc.lm %>% 
  mutate(date = date(datetime),
         year = year(date)) %>% 
  group_by(year, date) %>% 
  tally(name = "DAS.nasc") %>% 
  group_by(year) %>% 
  tally(name = "DAS.nasc")

das.lm.tmp <- left_join(das.lm.tmp, survey.lm.tmp)

nav.lbc  <- readRDS("C:/KLS/CODE/Github/estimATM/1907RL/Data/Nav/nav_vessel_LBC.rds")

das.lbc.tmp <- nav.lbc %>% 
  mutate(date = date(datetime),
         year = year(date)) %>% 
  group_by(year,date) %>% 
  tally(name = "DAS") %>% 
  group_by(year) %>% 
  tally(name = "DAS")

survey.lbc.tmp <- nasc.lbc %>% 
  mutate(date = date(datetime),
         year = year(date)) %>% 
  group_by(year, date) %>% 
  tally(name = "DAS.nasc") %>% 
  group_by(year) %>% 
  tally(name = "DAS.nasc")

das.lbc.tmp <- left_join(das.lbc.tmp, survey.lbc.tmp)

das.fsv <- bind_rows(das.fsv, das.fsv.tmp) %>% mutate(vessel = "RL") %>% select(vessel, year, DAS, DAS.nasc)
das.lm <- bind_rows(das.lm, das.lm.tmp) %>% mutate(vessel = "LM") %>% select(vessel, year, DAS, DAS.nasc)
das.lbc <- bind_rows(das.lbc, das.lbc.tmp) %>% mutate(vessel = "LBC") %>% select(vessel, year, DAS, DAS.nasc)


das.lbc.tmp <- nasc.nearshore %>% 
  filter(vessel.orig == "LBC") %>% 
  mutate(date = date(datetime),
         year = year(date)) %>% 
  group_by(year,date) %>% 
  tally(name = "DAS") %>% 
  group_by(year) %>% 
  tally(name = "DAS")

das.fsv <- bind_rows(das.fsv, das.fsv.tmp)
das.lm <- bind_rows(das.lm, das.lm.tmp)
das.lbc <- bind_rows(das.lbc, das.lbc.tmp)

#2021
load("C:/KLS/CODE/Github/estimATM/2107RL/Data/Nav/nav_data.Rdata")
nasc.lbc <- readRDS("C:/KLS/CODE/Github/estimATM/2107RL/Data/Backscatter/LBC/nasc_vessel_LBC_nearshore.rds")
nasc.lm  <- readRDS("C:/KLS/CODE/Github/estimATM/2107RL/Data/Backscatter/LM/nasc_vessel_LM_nearshore.rds")

das.fsv.tmp <- nav %>% 
  mutate(date = date(time),
         year = year(date)) %>% 
  group_by(year, date) %>% 
  tally(name = "DAS") %>% 
  group_by(year) %>% 
  tally(name = "DAS")

nav.lm <- readRDS("C:/KLS/CODE/Github/estimATM/2107RL/Data/Nav/nav_vessel_LM.rds")

das.lm.tmp <- nav.lm %>% 
  mutate(date = date(datetime),
         year = year(date)) %>% 
  group_by(year,date) %>% 
  tally(name = "DAS") %>% 
  group_by(year) %>% 
  tally(name = "DAS")

nav.lbc <- readRDS("C:/KLS/CODE/Github/estimATM/2107RL/Data/Nav/nav_vessel_LBC.rds")

das.lbc.tmp <- nav.lbc %>% 
  mutate(date = date(datetime),
         year = year(date)) %>% 
  group_by(year,date) %>% 
  tally(name = "DAS") %>% 
  group_by(year) %>% 
  tally(name = "DAS")

das.fsv <- bind_rows(das.fsv, das.fsv.tmp)
das.lm <- bind_rows(das.lm, das.lm.tmp)
das.lbc <- bind_rows(das.lbc, das.lbc.tmp)

#2022
load("C:/KLS/CODE/Github/estimATM/2207RL/Data/Nav/nav_data.Rdata")
nasc.lbc <- readRDS("C:/KLS/CODE/Github/estimATM/2207RL/Data/Backscatter/LBC/nasc_vessel_LBC_nearshore.rds")
nasc.lm  <- readRDS("C:/KLS/CODE/Github/estimATM/2207RL/Data/Backscatter/LM/nasc_vessel_LM_nearshore.rds")

das.fsv.tmp <- nav %>% 
  mutate(date = date(time),
         year = year(date)) %>% 
  group_by(year, date) %>% 
  tally(name = "DAS") %>% 
  group_by(year) %>% 
  tally(name = "DAS")

nav.lm <- readRDS("C:/KLS/CODE/Github/estimATM/2207RL/Data/Nav/nav_vessel_LM.rds")

das.lm.tmp <- nav.lm %>% 
  mutate(date = date(datetime),
         year = year(date)) %>% 
  group_by(year,date) %>% 
  tally(name = "DAS") %>% 
  group_by(year) %>% 
  tally(name = "DAS")

nav.lbc <- readRDS("C:/KLS/CODE/Github/estimATM/2207RL/Data/Nav/nav_vessel_LBC.rds")

das.lbc.tmp <- nav.lbc %>% 
  mutate(date = date(datetime),
         year = year(date)) %>% 
  group_by(year,date) %>% 
  tally(name = "DAS") %>% 
  group_by(year) %>% 
  tally(name = "DAS")

das.fsv <- bind_rows(das.fsv, das.fsv.tmp)
das.lm <- bind_rows(das.lm, das.lm.tmp)
das.lbc <- bind_rows(das.lbc, das.lbc.tmp)

#2023
load("C:/KLS/CODE/Github/estimATM/2307RL/Data/Nav/nav_data.Rdata")
nasc.lbc <- readRDS("C:/KLS/CODE/Github/estimATM/2307RL/Data/Backscatter/LBC/nasc_vessel_LBC_nearshore.rds")
nasc.lm  <- readRDS("C:/KLS/CODE/Github/estimATM/2307RL/Data/Backscatter/LM/nasc_vessel_LM_nearshore.rds")

das.fsv.tmp <- nav %>% 
  mutate(date = date(time),
         year = year(date)) %>% 
  group_by(year, date) %>% 
  tally(name = "DAS") %>% 
  group_by(year) %>% 
  tally(name = "DAS")

nav.lm <- readRDS("C:/KLS/CODE/Github/estimATM/2307RL/Data/Nav/nav_vessel_LM.rds")

das.lm.tmp <- nav.lm %>% 
  mutate(date = date(datetime),
         year = year(date)) %>% 
  group_by(year,date) %>% 
  tally(name = "DAS") %>% 
  group_by(year) %>% 
  tally(name = "DAS")

nav.lbc <- readRDS("C:/KLS/CODE/Github/estimATM/2307RL/Data/Nav/nav_vessel_LBC.rds")

das.lbc.tmp <- nav.lbc %>% 
  mutate(date = date(datetime),
         year = year(date)) %>% 
  group_by(year,date) %>% 
  tally(name = "DAS") %>% 
  group_by(year) %>% 
  tally(name = "DAS")

das.fsv <- bind_rows(das.fsv, das.fsv.tmp)
das.lm <- bind_rows(das.lm, das.lm.tmp)
das.lbc <- bind_rows(das.lbc, das.lbc.tmp)

#2024
load("C:/KLS/CODE/Github/estimATM/2407RL/Data/Nav/nav_data.Rdata")
nasc.lbc <- readRDS("C:/KLS/CODE/Github/estimATM/2407RL/Data/Backscatter/LBC/nasc_vessel_LBC_nearshore.rds")
nasc.lm  <- readRDS("C:/KLS/CODE/Github/estimATM/2407RL/Data/Backscatter/LM/nasc_vessel_LM_nearshore.rds")

das.fsv.tmp <- nav %>% 
  mutate(date = date(time),
         year = year(date)) %>% 
  group_by(year, date) %>% 
  tally(name = "DAS") %>% 
  group_by(year) %>% 
  tally(name = "DAS")

nav.lm <- readRDS("C:/KLS/CODE/Github/estimATM/2407RL/Data/Nav/nav_vessel_LM.rds")

das.lm.tmp <- nav.lm %>% 
  mutate(date = date(datetime),
         year = year(date)) %>% 
  group_by(year,date) %>% 
  tally(name = "DAS") %>% 
  group_by(year) %>% 
  tally(name = "DAS")

nav.lbc <- readRDS("C:/KLS/CODE/Github/estimATM/2407RL/Data/Nav/nav_vessel_LBC.rds")

das.lbc.tmp <- nav.lbc %>% 
  mutate(date = date(datetime),
         year = year(date)) %>% 
  group_by(year,date) %>% 
  tally(name = "DAS") %>% 
  group_by(year) %>% 
  tally(name = "DAS")

das.fsv <- bind_rows(das.fsv, das.fsv.tmp) %>% mutate(vessel = "RL") %>% select(vessel, year, DAS)
das.lm <- bind_rows(das.lm, das.lm.tmp) %>% mutate(vessel = "LM") %>% select(vessel, year, DAS)
das.lbc <- bind_rows(das.lbc, das.lbc.tmp) %>% mutate(vessel = "LBC") %>% select(vessel, year, DAS)

#2025
load("C:/KLS/CODE/Github/estimATM/2506SH/Data/Nav/nav_data.Rdata")
nav.lm <- readRDS("C:/KLS/CODE/Github/estimATM/2506SH/Data/Nav/nav_vessel_LM.rds")
nav.lbc  <- readRDS("C:/KLS/CODE/Github/estimATM/2506SH/Data/Nav/nav_vessel_LBC.rds")

nasc.fsv  <- readRDS("C:/KLS/CODE/Github/estimATM/2506SH/Data/Backscatter/SH/nasc_vessel_SH.rds")
nasc.lbc <- readRDS("C:/KLS/CODE/Github/estimATM/2506SH/Data/Backscatter/LBC/nasc_vessel_LBC_nearshore.rds")
nasc.lm  <- readRDS("C:/KLS/CODE/Github/estimATM/2506SH/Data/Backscatter/LM/nasc_vessel_LM_nearshore.rds")

das.fsv.tmp <- nav %>% 
  mutate(date = date(time),
         year = year(date)) %>% 
  group_by(year, date) %>% 
  tally(name = "DAS") %>% 
  group_by(year) %>% 
  tally(name = "DAS")

survey.fsv.tmp <- nasc.fsv %>% 
  mutate(date = date(datetime),
         year = year(date)) %>% 
  group_by(year, date) %>% 
  tally(name = "DAS.nasc") %>% 
  group_by(year) %>% 
  tally(name = "DAS.nasc")

das.fsv.tmp <- left_join(das.fsv.tmp, survey.fsv.tmp)

das.lm.tmp <- nav.lm %>% 
  mutate(date = date(datetime),
         year = year(date)) %>% 
  group_by(year,date) %>% 
  tally(name = "DAS") %>% 
  group_by(year) %>% 
  tally(name = "DAS")

survey.lm.tmp <- nasc.lm %>% 
  mutate(date = date(datetime),
         year = year(date)) %>% 
  group_by(year, date) %>% 
  tally(name = "DAS.nasc") %>% 
  group_by(year) %>% 
  tally(name = "DAS.nasc")

das.lm.tmp <- left_join(das.lm.tmp, survey.lm.tmp)

das.lbc.tmp <- nav.lbc %>% 
  mutate(date = date(datetime),
         year = year(date),
         vessel = "LBC") %>% 
  group_by(vessel, year, date) %>% 
  tally(name = "DAS") %>% 
  group_by(year) %>% 
  tally(name = "DAS")

survey.lbc.tmp <- nasc.lbc %>% 
  mutate(date = date(datetime),
         year = year(date),
         vessel = "LBC") %>% 
  group_by(vessel, year, date) %>% 
  tally(name = "DAS.nasc") %>% 
  group_by(year) %>% 
  tally(name = "DAS.nasc")

das.lbc.tmp <- left_join(das.lbc.tmp, survey.lbc.tmp)

das.fsv <- bind_rows(das.fsv, das.fsv.tmp) #%>% select(vessel, year, DAS, DAS.nasc)
das.lm  <- bind_rows(das.lm, das.lm.tmp) #%>% select(vessel, year, DAS, DAS.nasc)
das.lbc <- bind_rows(das.lbc, das.lbc.tmp) #%>% select(vessel, year, DAS, DAS.nasc)
  
# #2026
# load("C:/KLS/CODE/Github/estimATM/2606RL/Data/Nav/nav_data.Rdata")
# 
# das.fsv.tmp <- nav %>% 
#   mutate(date = date(time),
#          year = year(date)) %>% 
#   group_by(year, date) %>% 
#   tally(name = "DAS") %>% 
#   group_by(year) %>% 
#   tally(name = "DAS")
# 
# load("C:/KLS/CODE/Github/estimATM/2606RL/Data/Backscatter/nasc_nearshore.Rdata")
# nasc.lbc <- readRDS("C:/KLS/CODE/Github/estimATM/1907RL/Data/Backscatter/LBC/nasc_vessel_LBC_nearshore.rds")
# nasc.lm  <- readRDS("C:/KLS/CODE/Github/estimATM/1907RL/Data/Backscatter/LM/nasc_vessel_LM_nearshore.rds")
# 
# das.lm.tmp <- nasc.nearshore %>% 
#   filter(vessel.orig == "LM") %>% 
#   mutate(date = date(datetime),
#          year = year(date)) %>% 
#   group_by(year,date) %>% 
#   tally(name = "DAS") %>% 
#   group_by(year) %>% 
#   tally(name = "DAS")
# 
# das.lbc.tmp <- nasc.nearshore %>% 
#   filter(vessel.orig == "LBC") %>% 
#   mutate(date = date(datetime),
#          year = year(date)) %>% 
#   group_by(year,date) %>% 
#   tally(name = "DAS") %>% 
#   group_by(year) %>% 
#   tally(name = "DAS")
# 
# das.fsv <- bind_rows(das.fsv, das.fsv.tmp)
# das.lm <- bind_rows(das.lm, das.lm.tmp)
# das.lbc <- bind_rows(das.lbc, das.lbc.tmp)
# 
# 
# das.fsv <- bind_rows(das.fsv, das.fsv.tmp) %>% mutate(vessel = "RL") %>% select(vessel, year, DAS)
# das.lm <- bind_rows(das.lm, das.lm.tmp) %>% mutate(vessel = "LM") %>% select(vessel, year, DAS)
# das.lbc <- bind_rows(das.lbc, das.lbc.tmp) %>% mutate(vessel = "LBC") %>% select(vessel, year, DAS)

das.all <- das.fsv %>% 
  bind_rows(das.lm) %>% 
  bind_rows(das.lbc) %>% 
  arrange(vessel, year)

# Write to file
write_csv(das.all, file = here("Output",paste0("das_summary_",min(das.all$year),"-",
                               max(das.all$year), ".csv")))


