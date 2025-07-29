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

chl.hab.info <- rerddap::info("noaacwNPPN20VIIRSDINEOFDaily", url = "https://coastwatch.noaa.gov/erddap/")
chl.hab.date <- chl.hab.info$alldata$NC_GLOBAL[chl.hab.info$alldata$NC_GLOBAL$attribute_name == "time_coverage_end", "value"]

# Generate the habitat model URL
# example working URL
# sar.url <- "https://coastwatch.pfeg.noaa.gov/erddap/griddap/sardine_habitat_modis_v2.nc?potential_habitat_probability%5B(2025-07-14T12:00:00Z)%5D%5B(27.0):(51.0)%5D%5B(-130.0):(-113.0)%5D&.draw=surface&.vars=longitude%7Clatitude%7Cpotential_habitat_probability&.colorBar=%7CD%7C%7C0.17%7C0.184%7C2&.bgColor=0xffccccff"
# chl.url <- "https://coastwatch.noaa.gov/erddap/griddap/noaacwNPPN20VIIRSDINEOFDaily.nc?chlor_a%5B(2025-07-01T12:00:00Z)%5D%5B(0.0)%5D%5B(39.125):(34.125)%5D%5B(-124.375):(-120.375)%5D&.draw=surface&.vars=longitude%7Clatitude%7Cchlor_a&.colorBar=%7C%7CLog%7C0.03%7C30%7C&.bgColor=0xffccccff"

sar.url <- URLencode(paste0(
  "https://coastwatch.pfeg.noaa.gov/erddap/griddap/sardine_habitat_modis_v2.nc?potential_habitat_probability%5B(",
  sar.hab.date, # "2025-07-25T12:00:00Z",
  # format(ymd_hms(paste(Sys.Date(), "12:00:00")) - days(3), format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), # "2025-07-25T12:00:00Z",
  ")%5D%5B(",27.0,"):(",51.0,
  ")%5D%5B(",-130.0,"):(",-113.0,
  ")%5D&.draw=surface&.vars=longitude%7Clatitude%7Cpotential_habitat_probability&.colorBar=%7CD%7C%7C0.17%7C0.184%7C2&.bgColor=0xffccccff"
))

chl.url <- URLencode(paste0(
  "https://coastwatch.noaa.gov/erddap/griddap/noaacwNPPN20VIIRSDINEOFDaily.nc?chlor_a%5B(",
  chl.hab.date,
  # format(ymd_hms(paste(Sys.Date(), "12:00:00")) - days(10), format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), # "2025-07-01T12:00:00Z",
  ")%5D%5B(",0.0,")%5D%5B(",27.0,"):(", 51.0,
  ")%5D%5B(",-130.0,"):(",-113.0,
  ")%5D&.draw=surface&.vars=longitude%7Clatitude%7Cchlor_a&.colorBar=%7C%7CLog%7C0.03%7C30%7C&.bgColor=0xffccccff"
))

# "https://coastwatch.noaa.gov/erddap/griddap/noaacwNPPN20VIIRSDINEOFDaily.nc?chlor_a%5B(2025-07-01T12:00:00Z)%5D%5B(0.0)%5D%5B(39.125):(34.125)%5D%5B(-124.375):(-120.375)%5D&.draw=surface&.vars=longitude%7Clatitude%7Cchlor_a&.colorBar=%7C%7CLog%7C0.03%7C30%7C&.bgColor=0xffccccff"
# "https://coastwatch.noaa.gov/erddap/griddap/noaacwNPPN20VIIRSDINEOFDaily.nc?chlor_a%5B(2025-07-20T12:00:00Z):1:(2025-07-20T12:00:00Z)%5D%5B(0.0):1:(0.0)%5D%5B(89.95834):1:(-89.95834)%5D%5B(-179.9583):1:(179.9584)%5D"

# Download netCDF files
sar.file <- httr::GET(sar.url, write_disk(here::here("Data/Raster/sar_hab.nc"), overwrite = TRUE))
chl.file <- httr::GET(chl.url, write_disk(here::here("Data/Raster/chl_a.nc"), overwrite = TRUE))

# Read netCDF files with rast()
sar.hab <- rast(here::here("Data/Raster/sar_hab.nc"))
chl.a   <- rast(here::here("Data/Raster/chl_a.nc"))

# Define color palettes
sar.pal <- colorNumeric(c("#0000ff", "#ff9900"), values(sar.hab), ##0000ff(blue), #ff9900 (orange)
                    na.color = "transparent")

chl.pal <- colorNumeric("viridis", values(chl.a), ##0000ff(blue), #ff9900 (orange)
                        na.color = "transparent")

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
