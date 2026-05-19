library(atm)
library(tidyverse)
library(ggoce)

header.filename <- "C:/KLS/CODE/Github/estimATM/2606RL/Data/UCTD/mvp_2022-10-09_002331.m1"
cast.filename <- header.filename

# Extract cast info -------------------------
dat <- extract_ctd_cast(header.filename, "UCTD")

# # If MVP200 UCTD
# cast <- tail(stringr::str_split(cast.filename, "/")[[1]], n = 1) %>%
#   stringr::str_replace(".m1", "")
# 
# min.Z = 2
# 
# # Read cast data and rename columns
# dat <- read.table(cast.filename, skip = 62, sep = ",", 
#            col.names = c("P","Z","Sv","T","C","S","Dens","LOPC","ANLG0")) %>%
#   # Include only downcast data
#   dplyr::slice(which(Z > min.Z)[1]:which.max(Z)) %>%
#   dplyr::mutate(
#     scan = seq_along(P),
#     # t    = lubridate::ymd_hms(paste(date, time)),
#     # Calculate time interval
#     # dt   = as.numeric(difftime(t, dplyr::lag(t, 1, default = t[1]), units = "secs")),
#     # s    = cumsum(dt),
#     # Calculate change in depth (dZ, m)
#     dZ   = c(1, diff(Z)),
#     # dZt  = as.numeric(forecast::ma(dZ/dt, order = 5)),
#     # dZt  = dplyr::na_if(dZt, Inf),
#     Z    = -Z,
#     cast = cast,
#     path = cast.filename)

dat.ctd <- oce::as.ctd(salinity = dat$S, temperature = dat$T, pressure = dat$P, station = dat$cast[1])

sbe.filename <- "C:/SURVEY/2506SH/DATA/CTD/PROCESSED/003.cnv"
sbe.dat <- read.ctd.sbe(sbe.filename)
plot(sbe.dat)

read.ctd.aml(cast.filename, format = 2)

ggplot(
  sbe.dat,
  aes(
    x = salinity,
    y = oce::swTheta(
      salinity,
      temperature,
      pressure
    )
  )
) +
  geom_isopycnal() +
  geom_point() +
  labs(
    x = "Practical salinity",
    y = "Potential Temperature (°C)"
  )

ggplot(
  ctd,
  aes(
    x = salinity,
    y = oce::swTheta(
      salinity,
      temperature,
      pressure
    ),
    colour = timeS
  )
) +
  geom_isopycnal() +
  geom_point() +
  labs(
    x = "Practical salinity",
    y = "Potential Temperature (°C)"
  )

# Extract header info -------------------------
extract_ctd_header(header.filename, "UCTD")

header.txt <- readLines(header.filename)

# Extract cast date as dttm
cast.date  <- lubridate::dmy_hms(
  paste(
    # Extract the date
    stringr::str_extract(
      unlist(
        stringr::str_extract_all(header.txt,
                                 pattern = 'Date \\(dd/mm/yyyy\\): \\d{2}/\\d{2}/\\d{4}')),
      '\\d{2}/\\d{2}/\\d{4}'),
    
    # Extract the time
    stringr::str_extract(
      unlist(
        stringr::str_extract_all(header.txt,
                                 pattern = 'Time \\(hh\\|mm\\|ss.s\\): \\d{2}:\\d{2}:\\d{2}')),
      '\\d{2}:\\d{2}:\\d{2}'))
)

sn <- as.numeric(stringr::str_extract(
  unlist(
    stringr::str_extract_all(header.txt,
                             pattern = 'SER1 Serial Number: \\d{1,5}'))[1],
  ' \\d{1,5}'))

# Extract cast name from file name
cast <- tail(stringr::str_split(header.filename, "/")[[1]], n = 1) %>%
  stringr::str_replace(".m1", "")

