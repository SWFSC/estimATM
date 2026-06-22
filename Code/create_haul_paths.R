# Get haul nav data
## Create data frame to store results
haul.nav <- data.frame()

for (h in unique(haul$haul)) {
  # Get haul times
  haul.tmp <- filter(haul, haul == h)
  # Extract nav for each trawl period
  nav.fishing <- filter(nav, between(time, haul.tmp$equilibriumTime, haul.tmp$haulBackTime)) %>% 
    mutate(period = "Fishing")
  nav.deploy  <- filter(nav, between(time, haul.tmp$netInWaterTime, haul.tmp$equilibriumTime)) %>% 
    mutate(period = "Deployment")
  nav.recover <- filter(nav, between(time, haul.tmp$haulBackTime, haul.tmp$netOnDeckTime)) %>% 
    mutate(period = "Recovery")
  
  # Combine within haul
  haul.nav.tmp <- bind_rows(nav.deploy, nav.fishing, nav.recover) %>% 
    mutate(haul = h) 
  
  # Combine with other hauls
  haul.nav <- haul.nav %>% 
    bind_rows(haul.nav.tmp)
}

# Create haul paths from nav for each trawl period (Deployment, Fishing, and Recovery) if nav data exists
if (nrow(haul.nav) > 0) {
haul.paths <- haul.nav %>% 
  arrange(haul, time) %>% 
  st_as_sf(coords = c("long","lat"), crs = crs.geog) %>% 
  group_by(haul, period) %>% 
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING")

}
