getlat <- function(x){
  # get lat in the modis download asc
  # example 'Lat37.1964115002029Lon-3.2653977048134Samp1Line1' 
  
  # Remove "Samp1Line1_pixel1" 
  raw_latlong <- str_replace(x, pattern = "\\Samp..*", "") 
  
  # Get lat 
  aux_lat <- str_replace(raw_latlong, pattern = "\\Lon..*", "")
  lat <- as.numeric(unlist(stringr::str_split(aux_lat, "Lat", n=2))[2])
  
  lat
}
