# Combine haul and cluster length frequency objects for nearshore biomass estimation

library(tidyverse)
library(here)

# Process cluster-level data -------------------------------------------------------
# Load species-specific clf for core area
load("C:/KLS/CODE/Github/estimATM/2606RL/Output/cluster_length_frequency_all.Rdata")

# Load species-specific clf for seine data in the nearshore area
load("C:/KLS/CODE/Github/estimATM/2606RL/Output/cluster_length_frequency_all_seine.Rdata")

# Load species-specific clf for pseudo set data in the nearshore area
load("C:/KLS/CODE/Github/estimATM/2606RL/Output/cluster_length_frequency_all_pseudo.Rdata")

# Create list for storing all results
cluster.final.nearshore <- list()

for (i in names(cluster.final)) {
  # Combine cluster info from trawl clusters, purse seine sets, and pseudo sets
  clf.ns.tmp <- cluster.final[[i]] %>% 
    bind_rows(cluster.final.seine[[i]]) %>% 
    bind_rows(cluster.final.pseudo[[i]])
  
  # Combine results
  cluster.final.nearshore[[i]] <- clf.ns.tmp
  
  # ggplot(clf.ns.tmp, aes(long, lat, colour = sample.type)) + geom_point() + coord_map()
}

saveRDS(cluster.final.nearshore, 
        file = here("Output/cluster_length_frequency_all.rds"))

# Process haul-level data -------------------------------------------------------
# Load species-specific clf for core area
load("C:/KLS/CODE/Github/estimATM/2606RL/Output/haul_length_frequency_all.Rdata")

# Load species-specific clf for seine data in the nearshore area
load("C:/KLS/CODE/Github/estimATM/2606RL/Output/haul_length_frequency_all_seine.Rdata")

# Load species-specific clf for pseudo set data in the nearshore area
load("C:/KLS/CODE/Github/estimATM/2606RL/Output/haul_length_frequency_all_pseudo.Rdata")

# Create list for storing all results
haul.final.nearshore <- list()

for (i in names(cluster.final)) {
  # Combine cluster info from trawl hauls, purse seine sets, and pseudo sets
  hlf.ns.tmp <- haul.final[[i]] %>% 
    bind_rows(haul.final.seine[[i]]) %>% 
    bind_rows(haul.final.pseudo[[i]])
  
  # Combine results
  haul.final.nearshore[[i]] <- hlf.ns.tmp
  
  # ggplot(hlf.ns.tmp, aes(long, lat, colour = sample.type)) + geom_point() + coord_map()
}

saveRDS(haul.final.nearshore, 
        file = here("Output/haul_length_frequency_all.rds"))
