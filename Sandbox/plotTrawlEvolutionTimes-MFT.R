# Summarize nighttime trawl metrics
library(tidyverse)
library(atm)
library(here)
library(patchwork)

# Load all years data
load("C:/KLS/CODE/Github/estimATM/2506SH/Data/Trawl/trawl_data_raw.Rdata")
haul.old <- haul.all 

# Load 2026 data and combine with older data
load("C:/KLS/CODE/Github/estimATM/2606RL/Data/Trawl/trawl_data_raw.Rdata")
haul <- haul.all %>% 
  mutate(cruise = as.character(cruise)) %>%
  filter(cruise == "202606") %>% 
  bind_rows(haul.old) %>% 
  filter(year(equilibriumTime) >= 2024,
         month(equilibriumTime) >= 6,
         month(equilibriumTime) <= 11,
         trawlPerformance == "Good",
         !cruise == 202406) %>% 
  arrange(netInWaterTime) %>% 
  mutate(deploymentTime = difftime(equilibriumTime, netInWaterTime, units = "mins"),
         recoveryTime   = difftime(netOnDeckTime, haulBackTime, units = "mins"),
         evolutionTime  = difftime(netOnDeckTime, netInWaterTime, units = "mins"),
         duration       = round(difftime(haulBackTime, equilibriumTime, units = "mins"), 1)) %>% 
  filter(gearType == "MFT",
         between(as.numeric(duration), 25, 35))

# Plot deployment time
d.time <- ggplot(haul, aes(cruise, as.numeric(deploymentTime))) + 
  geom_boxplot() + 
  # facet_wrap(~trawlPerformance, nrow = 1) +
  labs(x = "Cruise", y = "Deployment time (mins)")

ggsave(d.time, file = here("Figs/fig_trawl_efficiency-deployment.png"),
       height =5, width = 5)

# Plot deployment time
r.time <- ggplot(haul, aes(cruise, as.numeric(recoveryTime))) + 
  geom_boxplot() + 
  # facet_wrap(~trawlPerformance, nrow = 1) +
  labs(x = "Cruise", y = "Recovery time (mins)")

ggsave(r.time, file = here("Figs/fig_trawl_efficiency-recovery.png"),
       height =5, width = 5)

# Plot deployment time
e.time <- ggplot(haul, aes(cruise, as.numeric(evolutionTime))) + 
  geom_boxplot() + 
  # facet_wrap(~trawlPerformance, nrow = 1) + 
  labs(x = "Cruise", y = "Evolution time (mins)")

ggsave(e.time, file = here("Figs/fig_trawl_efficiency-evolution.png"),
       height =5, width = 5)

all.times <- d.time / r.time / e.time
ggsave(all.times, file = here("Figs/fig_trawl_efficiency.png"),
       height =10, width = 5)
