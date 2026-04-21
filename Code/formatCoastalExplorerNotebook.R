# Script for formatting Rose Point Coastal Explorer Notebook (.nob) files that store attributes in .xml format
# Kevin L. Stierhoff

# Install and load pacman (library management package)
if (!require("pacman")) install.packages("pacman")

# Install and load required packages from CRAN ---------------------------------
pacman::p_load(tidyverse, here, xml2)

# Load project settings ---------------------------------
# Get project name from directory -----------------------
prj.name <- last(unlist(str_split(here(),"/")))

# Get all settings files
settings.files <- dir(here("Doc/settings"))

# Source survey settings file
prj.settings <- settings.files[str_detect(settings.files, paste0("settings_", prj.name, ".R"))]
source(here("Doc/settings", prj.settings))

# Read .xml file
nob <- xml2::read_xml(file.path(nob.dir, nob.file))

# Format routes ----------------------------------------
## Format compulsory routes
### Find all Route nodes where the Name contains "C" (Compulsory routes)
compulsory_routes <- xml_find_all(nob, "//Route[substring(Name, string-length(Name)) = 'C']")

### Add the new child nodes and update the values
xml_add_child(compulsory_routes, "DisplayLegRangeBearing", "true")
xml_add_child(compulsory_routes, "PlannedSpeed", "9.00 kn")
xml_add_child(compulsory_routes, "Color", "rgb(0, 0, 1)")

## Format adaptive routes
### Find all Route nodes where the Name contains "A" (Adaptive routes)
adaptive_routes <- xml_find_all(nob, "//Route[substring(Name, string-length(Name)) = 'A']")

### Add the new child nodes and update the values
xml_add_child(adaptive_routes, "DisplayLegRangeBearing", "true")
xml_add_child(adaptive_routes, "PlannedSpeed", "9.00 kn")
xml_add_child(adaptive_routes, "Color", "rgb(1, 0, 0)")

## Format nearshore routes
### Find all Route nodes where the Name contains "N" (Nearshore routes)
nearshore_routes <- xml_find_all(nob, "//Route[substring(Name, string-length(Name)) = 'N']")

### Add the new child nodes and update the values
xml_add_child(nearshore_routes, "DisplayLegRangeBearing", "true")
xml_add_child(nearshore_routes, "PlannedSpeed", "9.00 kn")
xml_add_child(nearshore_routes, "Color", "rgb(1, 0, 1)")

# Format waypoints ----------------------------------------
## Format UCTD waypoints
### Find all Mark nodes where the Name contains "UCTD"
uctd_marks <- xml_find_all(nob, "//Mark[starts-with(Name, 'UCTD')]")

### Add the new child nodes and update the values
xml_add_child(uctd_marks, "RangeCircleRadius", "1 NM")
xml_add_child(uctd_marks, "RangeCircleDisplayCount", "1")
xml_add_child(uctd_marks, "RangeCircleFill", "true")
xml_add_child(uctd_marks, "RangeCircleColor", "rgb(1, 0.4901961, 0)") # Blue

## Format CTD waypoints
### Find all Mark nodes where the Name contains "CTD"
ctd_marks <- xml_find_all(nob, "//Mark[starts-with(Name, 'CTD')]")

### Add the new child nodes and update the values
xml_add_child(ctd_marks, "RangeCircleRadius", "1 NM")
xml_add_child(ctd_marks, "RangeCircleDisplayCount", "1")
xml_add_child(ctd_marks, "RangeCircleFill", "true")
xml_add_child(ctd_marks, "RangeCircleColor", "rgb(0, 0, 1)")
xml_add_child(ctd_marks, "Icon", "Blue Box")

## Format surface eDNA waypoints
### Find all Mark nodes where the Name contains "eDNA"
eDNA_marks <- xml_find_all(nob, "//Mark[starts-with(Name, 'eDNA')]")

### Add the new child nodes and update the values
xml_add_child(eDNA_marks, "RangeCircleRadius", "1 NM")
xml_add_child(eDNA_marks, "RangeCircleDisplayCount", "1")
xml_add_child(eDNA_marks, "RangeCircleFill", "true")
xml_add_child(eDNA_marks, "RangeCircleColor", "rgb(0, 1, 0)")
xml_add_child(eDNA_marks, "Icon", "Green Box")

# Save the modified XML to a new file ----------------------
xml2::write_xml(nob, file.path(nob.dir, nob.file))
