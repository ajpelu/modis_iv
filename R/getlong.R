getlong <- function(x){
  # get long in the modis download asc
  # example 'Lat37.1964115002029Lon-3.2653977048134Samp1Line1' 
  
  # Remove "Samp1Line1_pixel1" 
  raw_latlong <- str_replace(x, pattern = "\\Samp..*", "") 
  
  # Get long 
  long <- as.numeric(unlist(stringr::str_split(raw_latlong, "Lon", n=2))[2])
  long
} 