# Load libraries
library(tidyverse)

# Load data
load("C:/KLS/CODE/Github/estimATM/2606RL/Data/Backscatter/nasc_all.Rdata")

# Summarize NASC by transect
nasc %>% 
  group_by(transect.orig, transect) %>% 
  summarise(transectStart = min(datetime),
            transectEnd = max(datetime)) %>% 
  arrange(transectStart) %>% 
  write_csv(here::here("Output/ek80_raw_summary.csv"))
