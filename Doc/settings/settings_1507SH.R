# Processing controls ----------------------------------------------------
estimate.os       <- F # Estimate biomass in the offshore strata; T if offshore surveyed
estimate.ns       <- F # Estimate biomass in the nearshore strata; T if nearshore surveyed
process.offshore  <- F # Process offshore backscatter data
process.nearshore <- F # Process near backscatter data
combine.regions   <- F # Combine nearshore/offshore plots with those from the core region
process.seine     <- F # Process purse seine data

# Survey planning ---------------------------------------------------------
# Transect spacing (nautical miles)
tx.spacing.fsv  <- 20.1 # For Lasker 
tx.spacing.sd   <- tx.spacing.fsv/2 # For Saildrone
tx.spacing.ns   <- tx.spacing.fsv/2 # For nearshore sampling

# Mainland buffer distance for FSV and Saildrone transects
sd.buffer  <- 6 # nmi
fsv.buffer <- 80

# Minimum transect length
min.tx.length <- 0 # nmi

# UCTD spacing (nautical miles)
uctd.spacing   <- 15

# Number of transects to remove from the start (if near Mexico)
rm.n.transects <- 0

# Randomize
do.random <- FALSE
save.csv  <- FALSE

# Survey information ------------------------------------------------------
# Full survey name; only used in report title
survey.name.long       <- "Summer 2015 Sardine-Hake (SaKe) Survey"
survey.vessel.long     <- "Bell M. Shimada" # Full vessel name: e.g., Bell M. Shimada
survey.vessel          <- "Shimada"        # Short vessel name; e.g., Shimada
survey.vessel.primary  <- "SH"            # Primary vessel abbreviation 
survey.name            <- "1507SH"        # SWFSC/AST survey name
survey.start           <- "20 June"       # Survey start date
survey.end             <- "10 September"   # Survey end date
survey.year            <- "2015"          # Survey year, for report
survey.season          <- "Summer"        # Survey season, for report
survey.das             <- 80              # Days at sea allocated
survey.landmark.n      <- "Cape Scott, British Columbia" # Landmark - N extent of survey
survey.landmark.s      <- "San Diego, CA" # Landmark - S extent of survey
survey.twilight        <- "none"          # Sunset type for computing day/night (none, nautical, civil, astronomical)
survey.twilight.offset <- 30              # Twilight offset; minutes before sunrise/after sunset
survey.twilight.remove <- FALSE           # Remove twilight period (T/F)
daynight.filter        <- c("Day","Night")# A character string including "Day", "Night", or both

# Inport dates for classifying data by cruise leg (if desired) -----------------
leg.breaks <- as.numeric(lubridate::ymd(c("2015-06-18", "2015-07-04", 
                                          "2015-07-27", "2015-08-21",
                                          "2015-09-11")))

# Define ERDDAP data variables
erddap.vessel        <- "WTED"    # Lasker == WTEG; Shimada == WTED; add "nrt" if during survey
erddap.survey.start  <- "2015-06-19" # Start of survey for ERDDAP vessel data query
erddap.survey.end    <- "2015-09-11" # End of survey for ERDDAP vessel data query
erddap.vars          <- c("time,latitude,longitude,seaTemperature,platformSpeed")
erddap.classes       <- c("factor", "numeric", "numeric", "numeric","numeric")
erddap.headers       <- c("time", "lat", "long", "SST", "SOG")
survey.lat           <- c(32,51)
survey.long          <- c(-130,-117)

# Survey plan info --------------------------------------------------------
wpt.filename         <- "waypoints_1507SH.csv"
wpt.types            <- c(Compulsory = "Compulsory")
wpt.colors           <- c(Compulsory = "#000000") 
wpt.linetypes        <- c(Compulsory = "solid")

# Saildrone info -----------------------------------------------
# Select Saildrone numbers
sd.numbers <- c("1045", "1046", "1047")

# Set Saildrone filter method
sd.buffer.type   <- c("saildrone")
sd.buffer.dist   <- 1.5 # buffer distance (nmi) around planned transects to classify SD intervals
sd.filter.method <- "manual" # Options are c("buffer","manual")
sd.nasc.name     <- "cps_nasc_SD.csv"

# Define Saildrone sampling dates
survey.start.sd  <- NA_character_ # Start of Saildrone survey
survey.end.sd    <- NA_character_ # End of Saildrone survey

# Set date range
erddap.url.sd <- "https://ferret.pmel.noaa.gov/pmel/erddap/tabledap/saildrone_west_coast_survey_2019"
erddap.survey.start.sd <- "2019-06-18T00%3A00%3A00Z"
erddap.survey.end.sd   <- "2019-09-30T23%3A59%3A00Z"
# Configure columns and classes
erddap.vars.sd       <- c("trajectory,latitude,longitude,SOG,time")
erddap.headers.sd    <- c("saildrone", "lat", "long", "SOG", "time")
erddap.classes.sd    <- c(rep("numeric", length(erddap.headers.sd) - 1),"factor")

# Define date range for each Saildrone to remove overlapping transits
sd.date.range    <- data.frame(saildrone  = c(1045, 1046, 1047),
                               start.date = ymd(c("2019-07-09", "2019-07-09", "2019-06-20")),
                               end.date   = ymd(c("2019-08-07", "2019-08-12", "2019-08-25")))

# Adjust time in Saildrone gps.csv files, if problems with Mission Planner (e.g., 1907RL)
sd.time.offset   <- 7 # Hours to add/subtract from GPS data (typically 0)

# Filter variables for TRAWL and CUFES data on SQL Server ----------------------
cruise.name <- 201507 # May be a numeric or numeric vector (e.g., c(201704,201706,...))
cruise.ship <- "SH"   # May be a character or character vector (e.g., c("RL","SH",...))

# Growth model parameters ------------------------------------------------------
model.season  <- "summer" # spring or summer; for selecting growth model parameters
model.type    <- "glm"    # lm, nlm, or glm; for selecting growth model

# Mapping preferences -----------------------------------------------------
# Turn off S2 processing in sf
sf::sf_use_s2(FALSE)
# Configure basemap options for mapview
mapviewOptions(basemaps = c("Esri.OceanBasemap","Esri.WorldImagery","CartoDB.Positron"))

# Coordinate reference systems for geographic and projected data
crs.geog <- 4326 # WGS84
crs.proj <- 3310 # Califoria Albers Equal Area

# Default map height
map.height <- 10

# Leaflet tile options; set both to T if caching
useCachedTile  <- F # Use cached tiles
useCrossOrigin <- F # USe cross origin
leaflet.checkTransects.simple <- FALSE # Use a simple Leaflet for checkTransects

# Trawl proportion plots
scale.pies <- FALSE   # Scale pie charts (TRUE/FALSE)
pie.scale  <- 0.0125 # 0.01-0.02 works well for coast-wide survey (i.e., summer), larger values (~0.03) for spring

# Lookup table for renaming columns
pie.spp <- c("Jacksmelt"  = "Atherinopsis californiensis", "PacHerring" = "Clupea pallasii",
             "Anchovy"    = "Engraulis mordax", "Sardine"    = "Sardinops sagax",
             "PacMack"    = "Scomber japonicus", "JackMack"   = "Trachurus symmetricus",
             "RndHerring" = "Etrumeus acuminatus", "AllCPS" = "AllCPS")

# Map landmarks
label.list <- c("Monterey Bay","San Francisco","Cape Flattery","Crescent City",
                "Newport","Point Conception","Cape Mendocino","Columbia River",
                "Cape Blanco","Bodega Bay","Westport","Fort Bragg",
                "Morro Bay","Long Beach","Cape Scott","San Diego")

# Species, stock and strata for nearshore biomass plots -------------------
spp.common.ns <- "Northern Anchovy"
spp.ns        <- "Engraulis mordax"
stock.ns      <- "Central"
stratum.ns    <- 1

# Figure preferences ------------------------------------------------------
# Set species colors
sardine.color      <- '#FF0000'
anchovy.color      <- '#00CD66'
jack.mack.color    <- '#0000FF'
jacksmelt.color    <- '#A020F0'
pac.mack.color     <- '#00FFFF'
pac.herring.color  <- '#F5DEB3'

# Define species to be analysed
cps.spp            <- c("Clupea pallasii","Engraulis mordax","Sardinops sagax",
                        "Scomber japonicus","Trachurus symmetricus")
# CUFES
cufes.start        <- "2015-06-19" # Start of survey for CUFES filtering
cufes.end          <- "2015-09-07" # End of survey for CUFES filtering
# For legend objects
cufes.breaks       <- c(0, 0.1, 1, 10, 25, 50, 250, 500, 10000) 
cufes.labels       <- c("<0.1", "0.1-1", "1-10", "10-25", "25-50", 
                        "50-250", "250-500", ">500")
cufes.sizes        <- c(0.5, 1, 2, 3, 4, 5, 6, 7)
cufes.plot.spp     <- c("AnchovyEggs","JackMackerelEggs","SardineEggs")
cufes.colors       <- c("AnchovyEggs"      = anchovy.color, 
                        "JackMackerelEggs" = jack.mack.color, 
                        "SardineEggs"      = sardine.color)
cufes.spp.labels   <- c("AnchovyEggs"      = "Anchovy",
                        "JackMackerelEggs" = "J. mackerel",
                        "SardineEggs"      = "Sardine")
# Trawl
# For legend objects
trawl.breaks       <- c(0, 1, 10, 25, 50, 500, 1000, 10000) 
trawl.labels       <- c("<1", "1-10", "10-25", "25-50", "50-500", "500-1000", ">1000") 
trawl.sizes        <- c(1, 2, 3, 4, 5, 6, 7) 

# NASC
# For legend objects
nasc.breaks        <- c(0, 200, 500, 2000, 5000, 20000, 50000, 20000000)
nasc.labels        <- c("0-200", "200-500", "500-2000", "2000-5000", 
                        "5000-20,000", "20,000-50,000", ">50,000")
nasc.scale         <- 0.7 # Scale percentage (smaller for larger scale)
nasc.sizes         <- c(0.25, 2, 3, 4, 5, 6, 7)*nasc.scale
nasc.colors        <- c("#000000", "#1E90FF", "#FFFF00", "#FF8C00", 
                        "#FF0000", "#FFC0CB", "#FFFFFF")

# Acoustic biomass density map
dens.breaks        <- c(0, 1, 10, 100, 500, 1000, 10000, 50000, 1000000)
dens.labels        <- c("0-1", "1-10", "10-100", "100-500", "500-1000",
                        "1000-10,000", "10,000-50,000", ">50,000")
dens.colors        <- c("#000000", "#1E90FF", "#FFFF00", "#FF8C00", 
                        "#FF0000", "#FFC0CB", "#FFFFFF", "#00FF00") # for legend colors
dens.sizes         <- c(0.25, 1, 2.25, 3, 4.25, 5.5, 6.5, 7.5) # for legend sizes

# Catch map
# For legend objects
catch.breaks       <- c(0, 10, 100, 500, 1000)
catch.labels       <- c("0-10", "10-100", "100-500", "500-1000")
catch.pie.sizes    <- c(1, 2, 3, 4, 5, 6)

annotation.size <-  2.5    # Font size for annotations; try 4 for spring surveys, 2.5 for summer surveys

# Cluster relative length frequency
# Set number of columns in facet plot
lf.ncols <- 5

# Data sources ------------------------------------------------------------
# Backscatter data info
# Survey vessels that collected acoustic data (a character vector of vessel abbreviations)
nasc.vessels           <- c("SH") 
nasc.vessels.offshore  <- NA_character_
nasc.vessels.nearshore <- NA_character_
nasc.vessels.krill     <- "SH"

# Purse seine data info
# Survey vessels that collected purse seine data
seine.vessels          <- NA_character_
# Use seine data to apportion backscatter
use.seine.data         <- FALSE

# Combine data from all vessels?
# Should data from different vessels be combined, e.g., for Lasker and Saildrone
# in the same strata?
merge.vessels <- c(OS  = FALSE,
                   NS = FALSE)

# Combine data from all regions?
# For most cases, should include all vessels, as region shouldn't be used to stratify backscatter
# However, for LBC in 1907RL, Islands are stratified separately from mainland transects,
# so regions for that vessel are not merged
# Used in estimateNearshore.R
merge.regions <- NA_character_

# Interval length (m); from Echoview
nasc.interval          <-  100    

# Number of intervals over which to summarize NASC
nasc.summ.interval     <- 2000/nasc.interval 

# Echosounder type; e.g., EK60, EK80, other
sounder.type           <- c(SH  = "EK60") 

# Location of survey data on AST1, AST2, etc. (a vector of file paths)
# Root directory where survey data are stored; other paths relative to this
if (Sys.info()['nodename'] %in% c("SWC-KSTIERHOF-D", "SWC-STIERHOFF-L")) {
  survey.dir           <- c(SH  = "C:/SURVEY/1507SH")   
} else {
  survey.dir           <- c(SH  = "//swc-storage1/AST1/SURVEYS/20150615_SHIMADA_SummerCCE")
}

# Backscatter data (within survey.dir, typically; a vector of file paths)
nasc.dir               <- c(SH  = "PROCESSED/EV/CSV/SHIMADA") 

# Regex pattern for identifying CPS CSV files
nasc.pattern.cps       <- c(SH  = "-Final_38kHz_CPS-ACIN.csv")
# Regex pattern for identifying krill CSV files
nasc.pattern.krill     <- c(SH  = "*-Final_120kHz_Krill-ACIN.csv")
# Regex pattern for identifying nearshore transects
nasc.pattern.nearshore <- c(SH  = "\\d{3}N")
# Regex pattern for identifying offshore transects
nasc.pattern.offshore  <- c(SH  = "\\d{3}O")
# Regex pattern for identifying offshore transects
nasc.pattern.inshore   <- c(SH  = "\\d{3}I")
# Regex pattern for identifying transits
nasc.pattern.transit   <- c(SH  = "\\d{3}T")
# Recursively search NASC directories
nasc.recurse           <- c(SH = TRUE)
# Max NASC value for removing outliers
nasc.max               <- 10000*19

# If T, read cps.nasc from file; else use NASC.50 
source.cps.nasc        <- c(SH  = TRUE) # in the nearshore strata

# File containing CPS nasc from CTD app
data.cps.nasc          <- c(SH  = here("Data/Backscatter/nasc_cps_SH_1507SH.csv")) # in the nearshore strata 

# regex for matching character pattern
tx.char.pattern        <- c(SH  = "[^0-9]") 

# If T, strips numbers from transect names (i.e., would combine 105-1 and 105-2 to 105)
strip.tx.nums          <- c(SH  = TRUE) 

# If T, strips characters from transect numbers (i.e., would combine 105A and 105B to 105)
strip.tx.chars         <- c(SH  = FALSE) 

# If T, removes transects with names including "transit"
rm.transit             <- c(SH  = TRUE) 

# If T, removes transects with names including "offshore"
rm.offshore            <- c(SH  = TRUE)

# If T, removes transects with names including "inshore"
rm.inshore             <- c(SH  = TRUE)

# If T, removes transects with names including "nearshore"
rm.nearshore           <- c(SH  = TRUE) 

# If T, subtracts NASC.5 from cps.nasc
rm.surface             <- c(SH  = FALSE) 

# regex for matching number pattern
tx.num.pattern         <- c(SH  = "-\\d{1}") 

# Use transect names for transect numbers
use.tx.number          <- c(SH  = TRUE) 

# Transects to manually exclude e.g., data.frame(vessel = "RL", transect = c("085","085-2"))
tx.rm                  <- list(SH = c(75:116, 1009))

# Minimum acoustic transect length (nmi)
min.tx.length          <- c(SH  = 3)

# Enforce nearest trawl cluster distance limits?
limit.cluster.dist     <- c(OS  = FALSE,
                            NS  = FALSE) 

# Maximum distance to trawl clusters
cum.biomass.limit      <- 0.90 # Distance used to compute max.cluster.distance

# If limit.cluster.dist == TRUE, set proportions to zero at distances greater than max.cluster.dist
max.cluster.dist       <- 30

# Define transect spacing bins and values (nmi) used to characterize transect spacing
tx.spacing.bins <- c(0, 6, 15, 35, 70, 100)
tx.spacing.dist <- c(5, 10, 20, 40, 80)

tx.spacing.ns   <- 5 # Nearshore transect spacing, in nmi; set NA if calculating programatically
tx.spacing.os   <- 40 # Nearshore transect spacing, in nmi; set NA if calculating programatically

# SCS data
scs.source             <- "XLSX" # "CSV", "ELG", or "XLSX"
scs.pattern            <- "MOA*.*xlsx" # regex for MOA files

# CUFES data
cufes.source           <- "SQLite" # "SQL" or "SQLite"
cufes.dir.sqlite       <- file.path(survey.dir[survey.vessel.primary], "DATA/BIOLOGICAL/CUFES")
cufes.db.sqlite        <- "cufes201507SH.sqlite" # CUFES SQLite database
cufes.date.format      <- "ymd" # mdy (1907RL only) or ymd (most other surveys)
# Trawl data
trawl.source           <- "SQL" # "SQL" or "Access"
trawl.dsn              <- "TRAWL"  # DSN for Trawl database on SQL server
trawl.dir.access       <- file.path(survey.dir,"DATA/BIOLOGICAL/HAUL")
trawl.db.access        <- "TrawlDataEntry1907RL.accdb"
trawl.performance      <- c("Aborted", "Bad", "Poor") # Character vector; trawl performance to exclude
trawl.haul.exclude     <- NA # Numeric vector; haul numbers to exclude (e.g., for incomplete catch, etc.; NA if include all)
# CTD data
ctd.dir                <- file.path(survey.dir[survey.vessel.primary],"DATA/CTD")
ctd.hdr.pattern        <- "dCTD*.*hdr"
ctd.cast.pattern       <- "dCTD*.*asc"
ctd.depth              <- 350
# UCTD data   
uctd.dir               <- file.path(survey.dir[survey.vessel.primary],"DATA/UCTD")
uctd.hdr.pattern       <- ".*UCTD\\d{3}.*asc"
uctd.cast.pattern      <- ".*_processed.asc"

# Biomass estimation settings ------------------------------------------
# Length bins and labels for calculating length frequencies 
length.min <- 1 # Minimum length bin for length frequencies
# (max. anchovy = 20 cm, sardine & herring = 30 cm, Pac. mack = 40, and jack mack. = 60)
length.max <- data.frame("species" = c("Clupea pallasii","Engraulis mordax",
                                       "Sardinops sagax", "Scomber japonicus",
                                       "Trachurus symmetricus"),
                         "sl" = c(30,20,30,40,60))

# Species to generate point estimates
point.est.spp          <- c("Clupea pallasii","Engraulis mordax","Sardinops sagax",
                            "Scomber japonicus","Trachurus symmetricus")
# Species to generate point estimates
bootstrap.est.spp      <- c("Clupea pallasii","Engraulis mordax","Sardinops sagax",
                            "Scomber japonicus","Trachurus symmetricus")

# Number of bootstrap samples
boot.num <- 1000 # 1000 during final

# Generate biomass length frequencies
do.lf    <- TRUE

# Estimate biomass in nearshore and offshore strata
estimate.nearshore <- FALSE
estimate.offshore  <- FALSE

# Define regions to present in main Results
estimate.regions   <- c("Core")

# Define rules for selecting and pruning sampling strata -----------------------
# Defines breaks between strata
max.diff <- 3
# Defines minimum number of transects in a stratum
nTx.min <- 2

# Stratum pruning settings
nIndiv.min    <- 10
nClusters.min <- 2

# Use manually defined strata?
stratify.manually    <- TRUE
stratify.manually.os <- FALSE
stratify.manually.ns <- FALSE

# Manually define sampling strata for each species
# Create a new data frame with each species, stratum, and vector containing transects
strata.manual <- bind_rows(
  data.frame(
    scientificName = "Clupea pallasii", 
    stratum = 1,
    transect = 37:60),
  data.frame(
    scientificName = "Engraulis mordax", 
    stratum = 1,
    transect = 1:28),
  data.frame(
    scientificName = "Engraulis mordax", 
    stratum = 2,
    transect = 30:39),
  data.frame(
    scientificName = "Engraulis mordax", 
    stratum = 3,
    transect = 40:58),
  data.frame(
    scientificName = "Sardinops sagax", 
    stratum = 1,
    transect = 1:4),
  data.frame(
    scientificName = "Sardinops sagax", 
    stratum = 2,
    transect = 6:9),
  data.frame(
    scientificName = "Sardinops sagax", 
    stratum = 3,
    transect = 10:15),
  data.frame(
    scientificName = "Sardinops sagax", 
    stratum = 4,
    transect = 18:24),
  data.frame(
    scientificName = "Sardinops sagax", 
    stratum = 5,
    transect = 33:42),
  data.frame(
    scientificName = "Sardinops sagax", 
    stratum = 6,
    transect = 46:49),
  data.frame(
    scientificName = "Scomber japonicus", 
    stratum = 1,
    transect = 24:28),
  data.frame(
    scientificName = "Scomber japonicus", 
    stratum = 2,
    transect = 33:36),
  data.frame(
    scientificName = "Scomber japonicus", 
    stratum = 3,
    transect = 38:47),
  data.frame(
    scientificName = "Trachurus symmetricus", 
    stratum = 1,
    transect = 1:9),
  data.frame(
    scientificName = "Trachurus symmetricus", 
    stratum = 2,
    transect = 12:20),
  data.frame(
    scientificName = "Trachurus symmetricus", 
    stratum = 3,
    transect = 25:62)
)

# Stock boundaries --------------------------------------------------------
stock.break.anch <- 40.430520 # Latitude of Cape Mendocino
stock.break.sar  <- 34.3 # Latitude of Pt. Conception (or change based on SST)
# Transects used to define stock boundaries (primary or other)

# Used in estimateOffshore, where stock break using offshore transect ends is ambiguous
stock.break.source <- "primary" 

# Data collection settings ------------------------------------------------
# ER60 file info
raw.prefix    <- "1507SH_EK60"
raw.size      <-  25   # file size in megabytes (MB)
raw.log.range <- 700  # depth of ER60 logging (m)

# Echoview settings
er60.version  <- "v2.4.3" # ER60 version
ek80.version  <- "v1.12.2" # EK80 version
ev.version    <- "v6.1.40" # Echoview version
int.start        <-    5  # Integration start line depth (m)
int.stop         <- 1000  # Integration start line depth (m)
cps.depth        <-   70  # Integration depth for CPS (m)
krill.depth      <-  350  # Integration depth for krill (m)
hake.depth       <-  750  # integration depth for hake (m)
speed.filter     <-    5  # Speed filter threshold (kn)
vmr.krill        <-  -45  # VMR value for krill formula operator (dB)
vmr.cps          <-  -45  # VMR value for CPS (with swim bladders) formula operator (dB)
bin.depth        <-    5  # Integration bin depth (m)
bin.length       <-  100  # Integration bin width (m)
adz.range        <-    3  # Range (m) of acoustic dead zone
nasc.freq        <-   38  # Echosounder frequency used to estimate CPS biomass

# Adaptive sampling information ------------------------------------------
compulsory.spacing      <- 20  # minimum transect spacing (nmi) for compulsory acoustic transects
adaptive.spacing        <- 10  # minimum transect spacing (nmi) for adaptive acoustic transects
adaptive.cluster.size   <- 5   # minimum number of consecutive transects to define a cluster
cufes.threshold.anchovy <- 1   # egg density, eggs per minute
cufes.threshold.sardine <- 0.3 # egg density, eggs per minute

# # Calibration information ------------------------------------------------
cal.vessels        <- "SH"
cal.dir            <- "//swc-storage1.nmfs.local/AST1/SURVEYS/20150615_SHIMADA_SummerCCE/DATA/EK60/CALIBRATION/RESULTS/San Diego"
cal.datetime       <- "19 June" # Date/time of calibration
cal.plot.date      <- "2015-06-19" # Date of the calibration, used to plot cal time series
cal.window         <- 50            # Number of days around calibration date to look for results
cal.group          <- "SWFSC"      # Group conducting the calibration
cal.personnel      <- "J. Renfree, T. Sessions, D. Murfin, and D. Palance" # Calibration participants
cal.loc            <- "Shelter Island, San Diego Bay" # Location name
cal.lat.dd         <-   32.7135    # Cal location latitude in decimal degrees (for mapping, e.g. with ggmap) 37.7865°N @ Pier 30-32
cal.lon.dd         <- -117.2227   # Cal location longitude in decimal degrees (for mapping, e.g. with ggmap) -122.3844°W @ Pier 30-32
cal.lat            <- dd2decmin(cal.lat.dd)
cal.lon            <- dd2decmin(cal.lon.dd)
cal.sphere         <- "38.1-mm diameter sphere made from tungsten carbide (WC) with 6% cobalt binder material (WC38.1; _Lasker_ sphere #1)" # Cal sphere info
cal.sphere.name    <- "_Lasker_ sphere #1"
cal.sphere.z       <- 6 # Nominal depth of calibration sphere below the transducer
cal.imp.anal       <- "Agilent 4294A Precision Impedance Analyzer" # Info about impedance analyzer
# Other notes about calibration
cal.notes          <- "UPDATE CAL LOCATION. Lasker calibration sphere #1"

# Physical conditions during calibration
cal.temp           <-   18.7   # enter water temperature
cal.sal            <-   33.8   # enter salinity
cal.c              <- 1516.8   # enter sound speed (m/s)
cal.min.z          <-    5     # enter minimum water depth below transducers
cal.max.z          <-    8     # enter maximum water depth below transducers
 
# Enter ambient noise estimates (dB re 1 W) for each vessel
# Lowest to highest frequency
cal.noise          <- list(SH = NA)

# Axis options for calibration plots
cal.scales    <- "free"  # fixed or free

# Vessel echosounder info  ------------------------------------------------
if (survey.vessel.primary == "SH") {
  echo.freqs      <- "18, 38, 70, 120, and 200" # list of echosounder frequencies for Shimada
  echo.freqs.dash <- "18-, 38-, 70-, 120-, and 200-" # list of echosounder frequencies for Shimada
  echo.models     <- "ES18-11, ES38B, ES70-7C, ES120-7C, and ES200-7C" # list of echosounder models for Shimada
} else if (survey.vessel.primary == "RL") {
  echo.freqs      <- "18, 38, 70, 120, 200, and 333" # list of echosounder frequencies for Lasker
  echo.freqs.dash <- "18-, 38-, 70-, 120-, 200-, and 333-" # list of echosounder frequencies for Lasker
  echo.models     <- "ES18-11, ES38B, ES70-7C, ES120-7C, ES200-7C, and ES333-7C" # list of echosounder models for Shimada
}

# nominal centerboard positions
cb.retracted    <- 5
cb.intermediate <- 7
cb.extended     <- 9

# Set min and max values for temperature, salinity, etc.
# Temperature limits
min.T <- 0
max.T <- 25

# Salinity limits
min.S <- 30
max.S <- 35

# Set limits for latitude and longitude ----------------------------------------
min.lat  <-   31
max.lat  <-   52
min.long <- -132
max.long <- -117

# Files to manually exclude
exclude.uctd <- c(NA)
exclude.ctd  <- c(NA)

# SCS values and structure
# vessel echosounder info
tx.start.button    <- "Start Transect"
tx.end.button      <- "Break Transect"
ctd.button         <- "CTD IN"
uctd.button        <- "UCTD Deployed"
trawl.start.button <- "Begin Fishing (EQ)"
trawl.end.button   <- "Haul Back"
bongo.button       <- "Bongo IN"
pairovet.button    <- "Pairovet IN"
cb.flush.button    <- "CB Flush"
cb.int.button      <- "CB Interm"
cb.ext.button      <- "CB Extended"
gps.lat.hdr        <- "MX512-Lat"
gps.lon.hdr        <- "MX512-Lon"
order.occ.hdr      <- "Order_Occ"
notes.hdr          <- "Notes"
