# Import FSV nav data from .log files from new SCS version (e.g., "GPGGA.RAW.log")
if (get.nav) {
  # List .log files from the appropriate locations
  log.files <- dir_ls(scs.nav.path, regexp = scs.nav.pattern, recurse = scs.nav.recurse) %>% 
    # Get file info
    file_info() %>%
    # Select only files modified during the survey
    filter(between(modification_time, ymd(erddap.survey.start) - days(1), ymd(erddap.survey.end) + days(1))) %>% 
    pull(path)
  
  # Get only log files form this survey
  log.files.sub <- 

  # Read and format SCS data
  nav <- log.files.sub[1:3] %>% 
    purrr::map_df(~read_csv(.x, skip = 1,
                            col_names = FALSE,
                            col_select = c(X1,X5:X8)), 
                  .id = "id") %>% 
    # Remove rows with any NA values
    na.omit() %>% 
    rename(time = X1, 
           lat = X5, lat.dir = X6, 
           long = X7, long.dir = X8) %>% 
    mutate(lat = scs2dd(paste0(lat, lat.dir)),
           long = scs2dd(paste0(long, long.dir))) %>% 
    select(time, lat, long, id) %>% 
    # Get position every 30s
    slice(seq(1, nrow(.), 30)) %>% 
    mutate(leg      = paste("Leg", 
                            cut(as.numeric(date(time)), 
                                leg.breaks, 
                                labels = FALSE))) %>% 
   arrange(time)
  
  # Save unfiltered nav data
  saveRDS(nav, here("Data/Nav/nav_data_scs_raw.rds"))
  
  # Convert nav to spatial
  nav.sf <- st_as_sf(nav, coords = c("long","lat"), crs = crs.geog) 
  
  # Cast nav to transects
  nav.paths.sf <- nav.sf %>% 
    group_by(leg) %>% 
    summarise(do_union = FALSE) %>% 
    st_cast("LINESTRING") %>% 
    mutate(distance_nmi = as.numeric(st_length(.)*0.000539957))
  
  # Save results
  save(nav, nav.sf, nav.paths.sf, file = here("Data/Nav/nav_data_scs.Rdata"))
  
} else {
  if (file.exists(here("Data/Nav/nav_data_scs.Rdata"))) {
    # Load nav data
    load(here("Data/Nav/nav_data_scs.Rdata")) 
  }
}





