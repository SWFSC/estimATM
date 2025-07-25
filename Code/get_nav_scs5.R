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
  log.files <- dir_ls(scs.nav.path, regexp = scs.nav.pattern, recurse = scs.nav.recurse) %>%
    # Get file info
    file_info() %>%
    # Select only files modified during the survey
    filter(between(modification_time, ymd(erddap.survey.start) - days(1), ymd(erddap.survey.end) + days(1))) %>%
    pull(path)
  
  # Load nav data if it exists
  if (file.exists(here("Data/Nav/nav_data.Rdata"))) {
    # Load data frame with already processed nav data
    load(here("Data/Nav/nav_data.Rdata"))
  } else {
    # Process and format all log files
    nav <- log.files %>% 
      purrr::map_df(~read_csv(.x, skip = 1,
                              col_names = FALSE,
                              col_select = c(X1,X5:X8)), 
                    .id = "id") %>% 
      # Remove rows with any NA values
      na.omit() %>% 
      rename(time = X1, 
             lat  = X5, lat.dir  = X6, 
             long = X7, long.dir = X8) %>% 
      mutate(lat  = scs2dd(paste0(lat, lat.dir)),
             long = scs2dd(paste0(long, long.dir))) %>%
      # Filter data where seconds are in scs.nav.seconds
      filter(round(second(time)) %in% scs.nav.seconds) %>% 
      select(time, lat, long, id) %>% 
      mutate(leg = paste("Leg",
                         cut(as.numeric(date(time)), 
                             leg.breaks,
                             labels = FALSE))) %>%
      arrange(time)
    
    # Save nav data
    save(nav, file = here("Data/Nav/nav_data.Rdata"))
  }
  
  # Identify new or changed log files
  if (length(logs.to.process) > 0) {
    # List only log files to process
    log.files.to.process <- log.files[fs::path_file(log.files) %in% 
                             path_file(fs_path(logs.to.process))]  
    
    # Print the number of new/changed log files
    cat(paste("Processing", length(logs.to.process), "new or changed SCS log files.\n"))
    
    # Process new and/or changed log files and append to nav data
    # Read and format SCS data
    nav.tmp <- log.files.to.process %>% 
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
      # Filter data where seconds are in scs.nav.seconds
      filter(round(second(time)) %in% scs.nav.seconds) %>% 
      mutate(leg      = paste("Leg", 
                              cut(as.numeric(date(time)), 
                                  leg.breaks, 
                                  labels = FALSE))) %>% 
      arrange(time)
    
    # Combine existing and new/changed data and remove duplicate rows
    nav <- bind_rows(nav, nav.tmp) %>% 
      distinct() 
  } 
} else {
  # Print the number of new/changed log files
  cat(paste("No new or changed SCS log files.\n"))
}

# Save snapshot
saveRDS(logs.snapshot.start, here("Output/logs_snapshot_end.rds"))

# Convert nav to spatial
nav.sf <- st_as_sf(nav, coords = c("long","lat"), crs = crs.geog) 

# Cast nav to transects
nav.paths.sf <- nav.sf %>% 
  group_by(leg) %>% 
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING") %>% 
  mutate(distance_nmi = as.numeric(st_length(.)*0.000539957))

# Save results
save(nav, nav.sf, nav.paths.sf, file = here("Data/Nav/nav_data.Rdata"))
