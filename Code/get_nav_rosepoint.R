if (get.nav) {
  # 1. Read the XML file 
  nav.xml <- read_xml(nav.path.nob)
  
  # 2. Find all <Track> nodes using an XPath query
  track_nodes <- xml_find_all(nav.xml, "//Track[StartTime]")
  
  cat("Successfully extracted", length(track_nodes), "track objects.\n\n")
  
  # 3. Function to parse "YYYYMMDDThhmmss XX.XXXXX N YY.YYYYY W"
  parse_timed_coordinate <- function(coord_string) {
    parts <- unlist(strsplit(trimws(coord_string), "\\s+"))
    
    # Ensure we have the correct number of parts (Timestamp, Lat, N/S, Lon, E/W)
    if (length(parts) < 5) return(NULL) 
    
    # Parse the datetime string (e.g., "20240722T060033")
    raw_datetime <- parts[1]
    
    # Convert to a POSIXct datetime object (assuming UTC time based on the 'Z' format usually found in XMLs)
    parsed_datetime <- as.POSIXct(raw_datetime, format="%Y%m%dT%H%M%S", tz="UTC")
    
    # Separate into date and time strings
    track_date <- as.Date(parsed_datetime)
    track_time <- format(parsed_datetime, "%H:%M:%S")
    
    # Parse Latitude
    lat <- as.numeric(parts[2])
    if (parts[3] == "S") lat <- -lat
    
    # Parse Longitude
    lon <- as.numeric(parts[4])
    if (parts[5] == "W") lon <- -lon
    
    return(data.frame(
      # date = track_date,
      time = paste(track_date, track_time),
      lat = lat, 
      long = lon,
      stringsAsFactors = FALSE
    ))
  }
  
  # Create df for storing nav data
  nav <- data.frame()
  
  for (i in 1:length(track_nodes)) {
    # Get the Track ID
    track_id <- xml_attr(track_nodes[i], "id")
    
    # 3. Extract specific attributes and child elements into a data frame
    # This pulls the attributes 'id' and 'created', as well as the text from 
    # the <TotalLength> and <TrackMarks> tags inside each <Track>.
    track_data <- data.frame(
      id           = xml_attr(track_nodes[i], "id"),
      created      = xml_attr(track_nodes[i], "created"),
      total_length = xml_text(xml_find_first(track_nodes[i], ".//TotalLength")),
      track_marks  = xml_text(xml_find_first(track_nodes[i], ".//TrackMarks")),
      stringsAsFactors = FALSE
    )
    
    # 4. Get the raw text of the TrackMarks
    track_marks_text <- xml_text(xml_find_first(track_nodes[i], ".//TrackMarks"))
    
    # 5. Split the text into individual coordinate strings by the semicolon delimiter
    # trimws() removes leading/trailing spaces
    marks_list <- unlist(strsplit(trimws(track_marks_text), ";"))
    
    # Remove any empty elements (often caused by a trailing semicolon)
    marks_list <- marks_list[trimws(marks_list) != ""]
    
    
    # 6. Apply parsing function and combine into a single data frame
    nav.tmp.xml <- map_dfr(marks_list, parse_timed_coordinate) %>% 
      mutate(id = track_data$created)
    
    # Combine nav data
    nav <- bind_rows(nav, nav.tmp.xml)
  }
  
  # List new columns to add
  new_cols <- c("SST", "SOG", "wind_dir", "wind_speed", "flag","wind_brg","wind_angle")
  
  # Convert time to POSIXct
  nav <- nav %>% 
    mutate(time = ymd_hms(time))
  
  # Create new columns with value NA
  nav[new_cols] <- NA
  
  # Save unfiltered nav data
  saveRDS(nav, here("Data/Nav/nav_data_raw.rds"))
  
  # Filter nav data
  nav <- nav %>%
    filter(is.na(ymd_hms(time)) == FALSE,
           # SST > 0, SST < 35,
           # is.nan(SOG) == FALSE, SOG > 0, SOG < 15,
           between(lat, min(survey.lat), max(survey.lat)), 
           between(long, min(survey.long), max(survey.long))) %>% 
    # Remove duplicates and arrange by time
    filter(!duplicated(time)) %>% 
    mutate(
      leg = paste("Leg", 
                  cut(as.numeric(date(time)), 
                      leg.breaks, 
                      labels = FALSE)),
      Leg = cut(as.numeric(date(time)), 
                leg.breaks, 
                labels = FALSE)) %>% 
    arrange(time)
  
  # Convert nav to spatial
  nav.sf <- st_as_sf(nav, coords = c("long","lat"), crs = crs.geog) 
  
  # Cast nav to transects
  nav.paths.sf <- nav.sf %>% 
    group_by(leg, Leg) %>% 
    summarise(do_union = FALSE) %>% 
    st_cast("LINESTRING") %>% 
    ungroup() %>% 
    mutate(distance_nmi = as.numeric(st_length(.)*0.000539957))
  
  # Save results
  save(nav, nav.sf, nav.paths.sf, file = here("Data/Nav/nav_data.Rdata"))
  
} else {
  if (file.exists(here("Data/Nav/nav_data.Rdata"))) {
    # Load nav data
    load(here("Data/Nav/nav_data.Rdata")) 
  }
}
