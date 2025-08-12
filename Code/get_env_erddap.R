# Get raster data from ERDDAP and add to leaflet map

# library(terra)
# library(leaflet)
# library(ncdf4) 
# library(httr)
# library(lubridate)
# library(cmocean)

# See leaflet example here
# https://rstudio.github.io/leaflet/articles/raster.html

# Get dataset info and extract latest date 
sar.hab.info <- rerddap::info("sardine_habitat_modis_v2", url = "https://coastwatch.pfeg.noaa.gov/erddap/")
sar.hab.date <- sar.hab.info$alldata$NC_GLOBAL[sar.hab.info$alldata$NC_GLOBAL$attribute_name == "time_coverage_end", "value"]

chl.info <- rerddap::info("noaacwNPPN20VIIRSDINEOFDaily", url = "https://coastwatch.noaa.gov/erddap/")
chl.date <- chl.info$alldata$NC_GLOBAL[chl.info$alldata$NC_GLOBAL$attribute_name == "time_coverage_end", "value"]

# Define data parameters
# X/Y ranges
xcoord <- c(-130, -113)
ycoord <- c(27, 51)
# Date ranges
tcoord.sar <- as.character(c(date(sar.hab.date), date(sar.hab.date)))
tcoord.chl <- as.character(c(date(chl.date), date(chl.date)))

# Create directory for raster data
dir_create(here("Data/Raster"))

# Download netCDF files
sar.dat <- griddap(sar.hab.info,
                     latitude  = ycoord, 
                     longitude = xcoord, 
                     time      = tcoord.sar)

chl.dat <- griddap(chl.info,
                    latitude  = ycoord, 
                    longitude = xcoord, 
                    time      = tcoord.chl)

# Read netCDF files with rast()
sar.hab <- rast(sar.dat$summary$filename, subds = "potential_habitat_probability")
chl.a   <- rast(chl.dat$summary$filename, subds = "chlor_a")

# Define color palettes
sar.pal <- colorNumeric(c("#0000ff", "#ff9900"), values(sar.hab), ##0000ff(blue), #ff9900 (orange)
                    na.color = "transparent")

chl.pal <- colorNumeric("viridis", values(chl.a), ##0000ff(blue), #ff9900 (orange)
                        na.color = "transparent")

# Save 
save(sar.hab, chl.a, sar.pal, chl.pal,
     file = here("Data/Raster/env_data_erddap.Rdata"))

# Example code for testing script

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

# Relic code from first attempt at data extraction and visualization ----------------------------------
# Generate the habitat model URL
# example working URL
# sar.url <- "https://coastwatch.pfeg.noaa.gov/erddap/griddap/sardine_habitat_modis_v2.nc?potential_habitat_probability%5B(2025-07-14T12:00:00Z)%5D%5B(27.0):(51.0)%5D%5B(-130.0):(-113.0)%5D&.draw=surface&.vars=longitude%7Clatitude%7Cpotential_habitat_probability&.colorBar=%7CD%7C%7C0.17%7C0.184%7C2&.bgColor=0xffccccff"
# chl.url <- "https://coastwatch.noaa.gov/erddap/griddap/noaacwNPPN20VIIRSDINEOFDaily.nc?chlor_a%5B(2025-07-01T12:00:00Z)%5D%5B(0.0)%5D%5B(39.125):(34.125)%5D%5B(-124.375):(-120.375)%5D&.draw=surface&.vars=longitude%7Clatitude%7Cchlor_a&.colorBar=%7C%7CLog%7C0.03%7C30%7C&.bgColor=0xffccccff"

# sar.url <- URLencode(paste0(
#   "https://coastwatch.pfeg.noaa.gov/erddap/griddap/sardine_habitat_modis_v2.nc?potential_habitat_probability%5B(",
#   "last", #sar.hab.date, # "2025-07-25T12:00:00Z",
#   # format(ymd_hms(paste(Sys.Date(), "12:00:00")) - days(3), format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), # "2025-07-25T12:00:00Z",
#   ")%5D%5B(",27.0,"):(",51.0,
#   ")%5D%5B(",-130.0,"):(",-113.0,
#   ")%5D&.draw=surface&.vars=longitude%7Clatitude%7Cpotential_habitat_probability&.colorBar=%7CD%7C%7C0.17%7C0.184%7C2&.bgColor=0xffccccff"
# ))
# 
# chl.url <- URLencode(paste0(
#   "https://coastwatch.noaa.gov/erddap/griddap/noaacwNPPN20VIIRSDINEOFDaily.nc?chlor_a%5B(",
#   "last", #chl.date,
#   # format(ymd_hms(paste(Sys.Date(), "12:00:00")) - days(10), format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), # "2025-07-01T12:00:00Z",
#   ")%5D%5B(",0.0,")%5D%5B(",27.0,"):(", 51.0,
#   ")%5D%5B(",-130.0,"):(",-113.0,
#   ")%5D&.draw=surface&.vars=longitude%7Clatitude%7Cchlor_a&.colorBar=%7C%7CLog%7C0.03%7C30%7C&.bgColor=0xffccccff"
# ))
# 
# # Download netCDF files
# sar.dat <- httr::GET(sar.url, write_disk(here::here("Data/Raster/sar_hab.nc"), overwrite = TRUE))
# chl.dat <- httr::GET(chl.url, write_disk(here::here("Data/Raster/chl_a.nc"), overwrite = TRUE))

# sar.hab <- rast(here::here("Data/Raster/sar_hab.nc"))
# chl.a   <- rast(here::here("Data/Raster/chl_a.nc"))
