# A script for matching trawl event data from CLAMS with nav data from ERDDAP/SCS
# K. Stierhoff (2025-06-17)

# Replace missing lat/long values for hauls (a CLAMS/SCS problem, usually)
# Create PDT time, for matching with trawl events
nav.match.position <- select(nav, time, lat, long) %>% 
  mutate(time_pdt = with_tz(time, tzone = "America/Los_Angeles")) %>% 
  arrange(time)

# Create data frames for match results
nav.match.haul.eq <- data.frame()
nav.match.haul.hb <- data.frame()

# Match haul times to nav data and extract positions  
for (i in seq_along(haul$equilibriumTime)) {
  min.diff.eq       <- which.min(abs(difftime(haul$equilibriumTime[i], nav.match.position$time_pdt)))
  nav.match.haul.eq <- bind_rows(nav.match.haul.eq, nav.match.position[min.diff.eq, ])
}

for (i in seq_along(haul$haulBackTime)) {
  min.diff.hb       <- which.min(abs(difftime(haul$haulBackTime[i], nav.match.position$time_pdt)))
  nav.match.haul.hb <- bind_rows(nav.match.haul.hb, nav.match.position[min.diff.hb, ])
}

# Combine haul and nav data
haul <- haul %>% 
  bind_cols(select(nav.match.haul.eq, startTimeEst = time, startLatEst = lat, startLongEst = long)) %>% 
  bind_cols(select(nav.match.haul.hb, stopTimeEst  = time, stopLatEst  = lat, stopLongEst  = long)) %>% 
  mutate(startLag = difftime(startTimeEst, equilibriumTime),
         stopLag  = difftime(stopTimeEst, haulBackTime)) %>% 
  mutate(
    positionsEst = case_when(
      is.na(startLatDecimal) ~ TRUE,
      TRUE ~ FALSE)) %>%
  mutate(
    startLatDecimal = case_when(
      is.na(startLatDecimal) ~ startLatEst,
      TRUE ~ startLatDecimal),
    stopLatDecimal = case_when(
      is.na(stopLatDecimal) ~ stopLatEst,
      TRUE ~ stopLatDecimal),
    startLongDecimal = case_when(
      is.na(startLongDecimal) ~ startLongEst,
      TRUE ~ startLongDecimal),
    stopLongDecimal = case_when(
      is.na(stopLongDecimal) ~ stopLongEst,
      TRUE ~ stopLongDecimal),
  ) %>% 
  arrange(haul)
