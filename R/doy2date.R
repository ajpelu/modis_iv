# Convert day of year into date format 
# Function from Verbesselt et. al 2016 (Remote Sensing for Ecologist)
doy2date <- function(year, doy){ 
  as.Date(doy - 1, origin = paste0(year, "-01-01"))
}  