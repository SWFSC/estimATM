# Load packages
pacman::p_load(tidyverse, lubridate, here)

# Process TDR files?
process.tdr <- TRUE

# Set ggplot2 theme
theme_set(theme_bw())

if (process.tdr) {
  # Load TDR data
  load(here("Output/tdr_data_all.Rdata"))
  
  # Load haul info
  load(here("Output/tdr_haul_all.Rdata"))
  
  # Create df for storing TDR data
  tdr.fishing <- data.frame()
  
  # Extract fishing data
  for (i in unique(tbl.all$haul)) {
    haul.tmp <- haul %>% 
      filter(haul == i)
    
    tdr.tmp <- tbl.all %>% 
      filter(between(time, haul.tmp$equilibriumTime, haul.tmp$haulBackTime)) %>% 
      select(haul, time, loc, depth)
    
    if ("Kite" %in% tdr.tmp$loc & "Footrope" %in% tdr.tmp$loc) {
      tdr.tmp <- tdr.tmp %>% 
        pivot_wider(names_from = loc, values_from = depth) %>% 
        mutate(time.diff = c(0, diff(time)),
               time.cum  = cumsum(time.diff)/60,
               height = Kite - Footrope)
      
      # Combine results
      tdr.fishing <- bind_rows(tdr.tmp, tdr.fishing)
      
      # ggplot(tdr.tmp) + 
      #   geom_path(aes(time.cum, Kite), colour = "red") +
      #   geom_path(aes(time.cum, Footrope), colour = "blue") + 
      #   labs(x = "Cumulative time (mins)", y = "Depth (m)")
      # 
      # ggplot(tdr.tmp) + 
      #   geom_path(aes(time.cum, height), colour = "green") + 
      #   labs(x = "Cumulative time (mins)", y = "Opening height (m)")
    }
  }  
  
  # Save processed data
  save(tdr.fishing, file = here("Data/TDR/tdr_data_fishing.Rdata"))
  
} else {
  # Load processed data
  load(here("Data/TDR/tdr_data_fishing.Rdata"))
}

# tdr.plot <- tdr.fishing %>% 
#   pivot_longer()

ggplot(tdr.fishing) +
  geom_path(aes(time.cum, Kite, group = haul), colour = "red") +
  geom_path(aes(time.cum, Footrope, group = haul), colour = "blue") +
  labs(x = "Cumulative time (mins)", y = "Depth (m)")

ggplot(tdr.fishing) +
  geom_path(aes(time.cum, height, group = haul), colour = "green") +
  labs(x = "Cumulative time (mins)", y = "Opening height (m)")

