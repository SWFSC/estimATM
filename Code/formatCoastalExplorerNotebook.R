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
xml_add_child(compulsory_routes, "PlannedSpeed", paste(sprintf("%.2f", survey.speed), "kn"))
xml_add_child(compulsory_routes, "Color", "rgb(0, 0, 1)")

## Format adaptive routes
### Find all Route nodes where the Name contains "A" (Adaptive routes)
adaptive_routes <- xml_find_all(nob, "//Route[substring(Name, string-length(Name)) = 'A']")

### Add the new child nodes and update the values
xml_add_child(adaptive_routes, "DisplayLegRangeBearing", "true")
xml_add_child(adaptive_routes, "PlannedSpeed", paste(sprintf("%.2f", survey.speed), "kn"))
xml_add_child(adaptive_routes, "Color", "rgb(1, 0, 0)")

## Format nearshore routes
### Find all Route nodes where the Name contains "N" (Nearshore routes)
nearshore_routes <- xml_find_all(nob, "//Route[substring(Name, string-length(Name)) = 'N']")

### Add the new child nodes and update the values
xml_add_child(nearshore_routes, "DisplayLegRangeBearing", "true")
xml_add_child(nearshore_routes, "PlannedSpeed", paste(sprintf("%.2f", survey.speed.ns), "kn"))
xml_add_child(nearshore_routes, "Color", "rgb(1, 0, 1)")

## Update all planned speeds
### Compulsory routes (typically 9 kn speed for planning)
speed_nodes_compulsory <- xml_find_all(nob, "//Route[substring(Name, string-length(Name)) = 'C']/RouteLeg[PlannedSpeed]")
xml_add_child(speed_nodes_compulsory, "PlannedSpeed", paste(sprintf("%.2f", survey.speed), "kn"))

### Adaptive routes (typically 9 kn speed for planning)
speed_nodes_adaptive <- xml_find_all(nob, "//Route[substring(Name, string-length(Name)) = 'A']/RouteLeg[PlannedSpeed]")
xml_add_child(speed_nodes_adaptive, "PlannedSpeed", paste(sprintf("%.2f", survey.speed), "kn"))

### Nearshore routes (typically 8 kn speed for planning)
speed_nodes_nearshore <- xml_find_all(nob, "//Route[substring(Name, string-length(Name)) = 'N']/RouteLeg[PlannedSpeed]")
xml_add_child(speed_nodes_nearshore, "PlannedSpeed", paste(sprintf("%.2f", survey.speed.ns), "kn"))

# Format waypoints ----------------------------------------
## Format UCTD waypoints
### Find all Mark nodes where the Name contains "UCTD"
uctd_marks <- xml_find_all(nob, "//Mark[starts-with(Name, 'UCTD')]")

### Add the new child nodes and update the values
xml_add_child(uctd_marks, "RangeCircleRadius", rangeCircleRadius["uctd"])
xml_add_child(uctd_marks, "RangeCircleDisplayCount", rangeCircleCount["uctd"])
xml_add_child(uctd_marks, "RangeCircleFill", rangeCircleFill["uctd"])
xml_add_child(uctd_marks, "RangeCircleColor", rangeCircleColor["uctd"])
xml_add_child(uctd_marks, "Icon", waypointIcon["uctd"])

## Format CTD waypoints
### Find all Mark nodes where the Name contains "CTD"
ctd_marks <- xml_find_all(nob, "//Mark[starts-with(Name, 'CTD')]")

### Add the new child nodes and update the values
xml_add_child(ctd_marks, "RangeCircleRadius", rangeCircleRadius["ctd"])
xml_add_child(ctd_marks, "RangeCircleDisplayCount", rangeCircleCount["ctd"])
xml_add_child(ctd_marks, "RangeCircleFill", rangeCircleFill["ctd"])
xml_add_child(ctd_marks, "RangeCircleColor", rangeCircleColor["ctd"])
xml_add_child(ctd_marks, "Icon", waypointIcon["ctd"])

## Format surface eDNA waypoints
### Find all Mark nodes where the Name contains "eDNA"
eDNA_marks <- xml_find_all(nob, "//Mark[starts-with(Name, 'eDNA')]")

### Add the new child nodes and update the values
xml_add_child(eDNA_marks, "RangeCircleRadius", rangeCircleRadius["eDNA"])
xml_add_child(eDNA_marks, "RangeCircleDisplayCount", rangeCircleCount["eDNA"])
xml_add_child(eDNA_marks, "RangeCircleFill", rangeCircleFill["eDNA"])
xml_add_child(eDNA_marks, "RangeCircleColor", rangeCircleColor["eDNA"])
xml_add_child(eDNA_marks, "Icon", waypointIcon["eDNA"])

# Save the modified XML to a new file ----------------------
xml2::write_xml(nob, file.path(nob.dir, nob.file.final))
