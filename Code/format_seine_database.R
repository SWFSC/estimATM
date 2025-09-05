# Process and format trawl data.

# Formats raw data from database for use in other analyses, depending on database source
# Trawl data are extracted using Code/collect_trawl_database.R
# Intended to be run following collect_trawl_database.R in scripts that also load settings from Doc/settings

if (seine.source == "SQL") {
  # Format set data
  sets.all <- sets.all %>% 
    mutate(datetime = datetime_UTC) %>% 
    arrange(datetime) %>% 
    mutate(season = case_when(
      month(datetime) < 6 ~ "spring",
      TRUE ~ "summer")) %>% 
    mutate(key = paste(cruise, ship, date, set))
  
  # Format catch data
  set.catch.all <- set.catch.all %>% 
    mutate(key = paste(cruise, ship, date, set))

} else if (seine.source == "Access") {
  sets.all <- sets.all %>% 
    mutate(datetime = with_tz(mdy_hm(paste(date, time), tz = seine.tz), 
                              tzone = "UTC")) %>% 
    arrange(datetime) %>% 
    mutate(season = case_when(
      month(datetime) < 6 ~ "spring",
      TRUE ~ "summer")) %>% 
    mutate(key = paste(cruise, ship, date, set))
  
  if (!"set_type" %in% names(sets.all))
    sets.all$set_type <- "research"
  
  # Format catch data
  set.catch.all <- set.catch.all %>% 
    left_join(select(sets.all, set, cruise, ship, date)) %>% 
    rename(totalNum = count) %>% 
    mutate(totalWeight = weight_lbs * 0.453592,
           key = paste(cruise, ship, date, set))
  
} else if (trawl.source == "Excel") {
  # Format haul data
  sets.all <- sets.all %>% 
    arrange(datetime) %>% 
    mutate(season = case_when(
      month(datetime) < 6 ~ "spring",
      TRUE ~ "summer")) %>% 
    mutate(key = paste(cruise, ship, date, set))
  
  # Format catch data
  set.catch.all <- set.catch.all %>% 
    mutate(key = paste(cruise, ship, date, set))
}
