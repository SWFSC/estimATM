# Import FSV nav data from .log files from new SCS version (e.g., "GPGGA.RAW.log")
if (get.nav) {
  # Get fileSnapshot at the beginning of the knit, to compare with the end of the last knit
  logs.snapshot.start <- fileSnapshot(file.path(scs.nav.path, scs.nav.dir), full.names = FALSE)
  
  # Load fileSnapshot from last knit, or make equal to the start
  if (file.exists(here("Output/logs_snapshot_end.rds"))){
    logs.snapshot.end <- readRDS(here("Output/logs_snapshot_end.rds"))
  } else {
    logs.snapshot.end <- logs.snapshot.start
  }
  
  # Identify new and changed files
  logs.changed    <- changedFiles(logs.snapshot.end, logs.snapshot.start) 
  # List files that need to be processed
  logs.to.process <- c(logs.changed$changed, logs.changed$added)

  # List all .log files 
  gga.files <- dir_ls(scs.nav.path, regexp = scs.gga.pattern, recurse = scs.nav.recurse) %>%
    # Get file info
    file_info() %>%
    # Select only files modified during the survey
    filter(between(modification_time, ymd(erddap.survey.start) - days(1), ymd(erddap.survey.end) + days(1))) %>%
    pull(path)
  
  vtg.files <- dir_ls(scs.nav.path, regexp = scs.vtg.pattern, recurse = scs.nav.recurse) %>%
    # Get file info
    file_info() %>%
    # Select only files modified during the survey
    filter(between(modification_time, ymd(erddap.survey.start) - days(1), ymd(erddap.survey.end) + days(1))) %>%
    pull(path)
  
  # Load nav data if it exists; else process all the log files
  if (file.exists(here("Data/Nav/nav_data_scs.Rdata"))) {
    # Load data frame with already processed nav data
    load(here("Data/Nav/nav_data_scs.Rdata"))
  } else {
    # Process and format all GGA (lat/long) files
    nav.gga <- gga.files %>% 
      purrr::map_df(~read_csv(.x, skip = 1,
                              col_names = FALSE,
                              col_select = c(X1,X5:X8)), 
                    .id = "id") %>% 
      # Remove rows with any NA values
      na.omit() %>% 
      rename(time = X1, 
             lat  = X5, lat.dir  = X6, 
             long = X7, long.dir = X8) %>% 
      mutate(time = align.time(time, 1),
             lat  = scs2dd(paste0(lat, lat.dir)),
             long = scs2dd(paste0(long, long.dir))) %>%
      filter(!duplicated(time)) %>% 
      arrange(time) %>%
      # Filter data where seconds are in scs.nav.seconds
      filter(round(second(time)) %in% scs.nav.seconds) %>% 
      select(time, lat, long) %>% 
      mutate(leg = paste("Leg",
                         cut(as.numeric(date(time)), 
                             leg.breaks,
                             labels = FALSE)),
             Leg = cut(as.numeric(date(time)), 
                       leg.breaks, 
                       labels = FALSE)) 
    
    # Process and format all VTG (SOG, CMG) files
    nav.vtg <- vtg.files %>% 
      purrr::map_df(~read_csv(.x, skip = 1,
                              col_names = FALSE,
                              col_select = c(X1,X6,X8)), 
                    .id = "id") %>% 
      rename(time = X1, CMG  = X6, SOG  = X8) %>% 
      mutate(time = align.time(time, 1)) %>% 
      filter(!duplicated(time)) %>% 
      arrange(time) %>% 
      # Filter data where seconds are in scs.nav.seconds
      filter(round(second(time)) %in% scs.nav.seconds)
    
    # Add VTG data to GGA
    nav <- left_join(nav.gga, nav.vtg) 

    # Save nav data
    save(nav, file = here("Data/Nav/nav_data_scs.Rdata"))
  }
  
  # Identify new or changed log files
  if (length(logs.to.process) > 0) {
    # List only log files to process
    gga.to.process <- gga.files[fs::path_file(gga.files) %in% 
                             path_file(fs_path(logs.to.process))] 
    
    vtg.to.process <- vtg.files[fs::path_file(vtg.files) %in% 
                                  path_file(fs_path(logs.to.process))] 
    
    # Process new and/or changed log files and append to nav data
    # Process and format all GGA (lat/long) files
    nav.gga.tmp <- gga.to.process %>% 
      purrr::map_df(~read_csv(.x, skip = 1,
                              col_names = FALSE,
                              col_select = c(X1,X5:X8)), 
                    .id = "id") %>% 
      # Remove rows with any NA values
      na.omit() %>% 
      rename(time = X1, 
             lat  = X5, lat.dir  = X6, 
             long = X7, long.dir = X8) %>% 
      mutate(time = align.time(time, 1),
             lat  = scs2dd(paste0(lat, lat.dir)),
             long = scs2dd(paste0(long, long.dir))) %>%
      filter(!duplicated(time)) %>% 
      arrange(time) %>%
      # Filter data where seconds are in scs.nav.seconds
      filter(round(second(time)) %in% scs.nav.seconds) %>% 
      select(time, lat, long) %>% 
      mutate(leg = paste("Leg",
                         cut(as.numeric(date(time)), 
                             leg.breaks,
                             labels = FALSE)),
             Leg = cut(as.numeric(date(time)), 
                       leg.breaks, 
                       labels = FALSE)) 
    
    # Process and format all VTG (SOG, CMG) files
    nav.vtg.tmp <- vtg.to.process %>% 
      purrr::map_df(~read_csv(.x, skip = 1,
                              col_names = FALSE,
                              col_select = c(X1,X6,X8)), 
                    .id = "id") %>% 
      rename(time = X1, CMG  = X6, SOG  = X8) %>% 
      mutate(time = align.time(time, 1)) %>% 
      filter(!duplicated(time)) %>% 
      arrange(time) %>% 
      # Filter data where seconds are in scs.nav.seconds
      filter(round(second(time)) %in% scs.nav.seconds)
    
    # Add VTG data to GGA
    nav.tmp <- left_join(nav.gga.tmp, nav.vtg.tmp)
    
    # Combine existing and new/changed data and remove duplicate rows
    nav <- bind_rows(nav, nav.tmp) %>% 
      arrange(time) %>% 
      distinct() 
  } 
}

# Save snapshot
saveRDS(logs.snapshot.start, here("Output/logs_snapshot_end.rds"))

# Convert nav to spatial
nav.sf <- st_as_sf(nav, coords = c("long","lat"), crs = crs.geog) %>% 
  arrange(time)

# Cast nav to transects
nav.paths.sf <- nav.sf %>% 
  group_by(leg, Leg) %>% 
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING") %>% 
  ungroup() %>% 
  mutate(distance_nmi = as.numeric(st_length(.)*0.000539957))

# Save results
save(nav, nav.sf, nav.paths.sf, file = here("Data/Nav/nav_data_scs.Rdata"))
