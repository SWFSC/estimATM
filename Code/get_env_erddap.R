# Get raster data from ERDDAP and add to leaflet map

# library(terra)
# library(leaflet)
# library(ncdf4)
# library(httr)
# library(lubridate)
# library(cmocean)
# library(fs)
# library(here)
# library(rerddap)

# See leaflet example here
# https://rstudio.github.io/leaflet/articles/raster.html

# Create directory for raster data
dir_create(here("Data/Raster"))

# Define data parameters
# X/Y ranges
xcoord <- c(-130, -113)
ycoord <- c(27, 51)

# Get dataset info and extract latest date 
tryCatch(
  expr = {
    # Code that might cause an error
    sar.hab.info <- rerddap::info("sardine_habitat_modis_v2", url = "https://coastwatch.pfeg.noaa.gov/erddap/")
  },
  error = function(e) {
    # Code to execute if an error occurs
    message(paste("Error extracting sardine habitat info: ", e$message))
  },
  warning = function(w) {
    # Code to execute if a warning occurs
    message(paste("Warning extracting sardine habitat info: ", w$message))
  },
  finally = {
    # Code to execute regardless of error or warning (optional)
    print(paste("Finished extracting sardine habitat info."))
  }
)

if (exists("sar.hab.info")) {
  sar.hab.date <- sar.hab.info$alldata$NC_GLOBAL[sar.hab.info$alldata$NC_GLOBAL$attribute_name == "time_coverage_end", "value"]  
  
  # Date ranges
  tcoord.sar <- as.character(c(date(sar.hab.date), date(sar.hab.date)))

  # Download netCDF files
  sar.dat <- rerddap::griddap(sar.hab.info,
                     latitude  = ycoord, 
                     longitude = xcoord, 
                     time      = tcoord.sar) 
  
  sar.dat.bin <- sar.dat$data %>%
    mutate(potential_habitat_class = cut(potential_habitat_probability, c(sar.hab.breaks), labels = FALSE))  
    
  
  # Read netCDF files with rast()
  sar.hab <- terra::rast(sar.dat$summary$filename, subds = "potential_habitat_probability")
  sar.hab.class <- terra::rast(sar.dat$summary$filename, subds = "potential_habitat_classification")
    
  # Define color palettes
  sar.pal <- colorNumeric(c("#030303", "#CCCCCC"), values(sar.hab),
                          na.color = "transparent")
  
  # Save files
  save(sar.hab, sar.pal, 
       file = here::here("Data/Raster/hab_data_erddap.Rdata"))
}

# Get dataset info and extract latest date 
tryCatch(
  expr = {
    # Code that might cause an error
    chl.info <- rerddap::info("noaacwNPPN20VIIRSDINEOFDaily", url = "https://coastwatch.noaa.gov/erddap/")
  },
  error = function(e) {
    # Code to execute if an error occurs
    message(paste("Error extracting chlorophyll-a info: ", e$message))
  },
  warning = function(w) {
    # Code to execute if a warning occurs
    message(paste("Warning extracting chlorophyll-a info: ", w$message))
  },
  finally = {
    # Code to execute regardless of error or warning (optional)
    print(paste("Finished extracting chlorophyll-a info."))
  }
)

if (exists("chl.info")) {
  chl.date <- chl.info$alldata$NC_GLOBAL[chl.info$alldata$NC_GLOBAL$attribute_name == "time_coverage_end", "value"]
  
  # Date ranges
  tcoord.chl <- as.character(c(date(chl.date), date(chl.date)))

  chl.dat <- rerddap::griddap(chl.info,
                     latitude  = ycoord, 
                     longitude = xcoord, 
                     time      = tcoord.chl)
  
  # hist(chl.dat$data$chlor_a)
  # hist(chl.dat$data$log_chlor_a)
  
  chl.dat.log <- chl.dat
  
  # Log-transform chl values
  chl.dat.log$data$chlor_a <- log(chl.dat.log$data$chlor_a + 1)
  
    # Read netCDF files with rast()
  chl.a     <- terra::rast(chl.dat$summary$filename, subds = "chlor_a")
  chl.a.log <- terra::rast(chl.dat.log$summary$filename, subds = "chlor_a")

  # ggplot() + geom_raster(data = chl.dat$data, aes(x=longitude, y=latitude, fill = chlor_a)) + 
  #   scale_fill_viridis_c(option = "magma") + coord_map()
  # ggplot() + geom_raster(data = chl.dat$data, aes(x=longitude, y=latitude, fill = log(chlor_a+1))) + 
  #   scale_fill_viridis_c(option = "magma") + coord_map()
  # ggplot() + geom_raster(data = chl.dat$data, aes(x=longitude, y=latitude, fill = log(chlor_a+1))) + 
  #   scale_fill_cmocean(name = "algae") + coord_map()
  # ggplot() + geom_raster(data = chl.dat.log$data, aes(x=longitude, y=latitude, fill = chlor_a)) +
  #   scale_fill_cmocean(name = "algae") + coord_map()
  
  # Define color palettes
  chl.pal <- colorNumeric("viridis", values(chl.a), ##0000ff(blue), #ff9900 (orange)
                          na.color = "transparent")
  
  chl.pal.log <- colorNumeric(c("#FFFFFF", "#2FED15"), values(chl.a.log), ##0000ff(blue), #ff9900 (orange)
                          na.color = "transparent")
  # Save 
  save(chl.a, chl.pal, chl.pal.log,
       file = here::here("Data/Raster/chl_data_erddap.Rdata"))
  }

# Example code for testing script
# 
# leaflet() %>% addTiles() %>%
#   addRasterImage(sar.hab, colors = sar.pal, opacity = 0.5,
#                  group = "Sardine habitat") %>%
#   addLegend(pal = sar.pal, values = values(sar.hab),
#             title = "Sardine habitat",
#             group = "Sardine habitat")
# 
# leaflet() %>% addTiles() %>%
#   addRasterImage(chl.a, colors = chl.pal, opacity = 0.5,
#                  group = "Chlorophyll a") %>%
#   addLegend(pal = chl.pal, values = values(chl.a),
#             title = "Chlorophyll a",
#             group = "Chlorophyll a")
# 
# leaflet() %>% addTiles() %>%
#   addRasterImage(chl.a.log, colors = chl.pal.log, opacity = 0.5,
#                  group = "Chlorophyll a") %>%
#   addLegend(pal = chl.pal.log, values = values(chl.a.log),
#             title = "log(Chlorophyll a)",
#             group = "Chlorophyll a-Log")


