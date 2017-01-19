# --- 
# Script to process downloaded MOD13Q1 data for Q. pyrenaica forests of Sierra Nevada # 
# Author: Perez-Luque AJ (@ajpelu)
# version: 1
# date: 2017 January 
# --- 

# --- 
# Set directory 
machine <- 'ajpelu'
di <- paste('/Users/', machine, '/Dropbox/phd/phd_repos/modis_iv', sep='')
# --- 

# --- 
library(stringr)
library(dplyr)
library(reshape2)
library(lubridate)
# ---


# Read data 
raw <- read.csv(paste0(di, '/data_raw/modis_gee/iv_qp_raw.csv'), header=TRUE) 

# --- 
# Prepare Data 
# 1 # Date 
# * Get date of the image (from hdf title, system.index). 
# * Store as date (date format) and create new variable for year 
# 2 # Coordinates
# * Get lat and long from '.geo' variable 
# * Store as numeric 
# 3 # Select and rename variables of interest 

rawdata <- raw %>% 
  mutate(
    # Date
    aux_date = stringr::str_replace(system.index, 
                                         pattern = "MOD13Q1_005_", 
                                         ""),
    date = as.Date(substr(aux_date,1,10), format = "%Y_%m_%d"),
    year = lubridate::year(date), 
    
    # GEE index
    gee_index = as.numeric(
      stringr::str_replace(
        substr(aux_date,11, nchar(aux_date)), pattern = '_', "")),
    
    # Lat/Long
    aux_geo = stringr::str_replace(.geo, pattern = "..*\\[", ""),
    longitude = as.numeric(stringr::str_replace(aux_geo, pattern = "\\,..*", "")),
    latitude = as.numeric(
      stringr::str_replace(
        stringr::str_replace(aux_geo, pattern = "..*\\,", ""), 
        pattern = "\\]..*", ""))) %>%
  # Select 
  dplyr::select(doy = DayOfYear, evi = EVI, ndvi = NDVI, summQA = SummaryQA, iv_malla_modi_id,
                pop = poblacion, date, gee_index, long = longitude, lat = latitude)











aux_geo <- stringr::str_replace(geo, pattern = "..*\\[", "")
aux_latlong <- stringr::str_replace(aux_geo, pattern = "\\]..*", "")
longitude <- as.numeric(stringr::str_split(aux_latlong, pattern =",")[[1]][1])
latitude <- as.numeric(stringr::str_split(aux_latlong, pattern =",")[[1]][2])







str_replace(miname, pattern = "\\Samp..*", "")







       

         
         
         
# --- 
# LOOP TO PROCESS DATA 
for (i in modis_bands){ 
  
  ### Get name of the band 
  name_band <- tolower(str_replace(i, pattern = "250m_16_days_", ""))
  
  ### Create a MODIS_Temporal_Series
  mimodis_ts <- MODISTimeSeries(Dir = path_original_MODIS, 
                                Band = i, 
                                Simplify = FALSE) 
  
  # Create a empty data.frame
  aux_data_out <- c() 
  
  for (j in 1:length(mimodis_ts)) { 
    
    ### Get id of PIXEL and coordinates 
    miname <- colnames(mimodis_ts[[j]])
    ### Remove "Samp1Line1_pixel1" 
    aux_miname <- str_replace(miname, pattern = "\\Samp..*", "")
    
    ### milat 
    aux_miname_lat <- str_replace(aux_miname, pattern = "\\Lon..*", "")
    milat <- as.numeric(unlist(stringr::str_split(aux_miname_lat, "Lat", n=2))[2])
    
    ### milong
    milong <- as.numeric(unlist(stringr::str_split(aux_miname, "Lon", n=2))[2])
    
    ### Coordinates
    mis_coord <- as.data.frame(cbind(milat, milong))
    
    # Get id of pixel 
    myid <- mis_coord %>% 
      inner_join(mydf, by= c('milat' = 'lat', 'milong' = 'long'))
    
    # Change name "Lat37.1964115002029Lon-3.2653977048134Samp1Line1_pixel1" for 
    # pixel ID
    colnames(mimodis_ts[[j]]) <- myid$ID
    
    aux_data <- mimodis_ts[[j]] %>% 
      melt() %>% 
      select('modis_date' = Var1,
             'id_pixel' = Var2,
             name_band = value)
    
    aux_data_out <- rbind(aux_data_out, aux_data)
    
  }
  
  names(aux_data_out)[3] <- name_band
  assign(name_band, aux_data_out)
  
} 

# --- 
# Merge dataframes 
iv_df <- ndvi %>% 
  inner_join(evi, by=c("modis_date", "id_pixel")) %>% 
  inner_join(pixel_reliability, by=c("modis_date", "id_pixel")) %>% 
  inner_join(vi_quality, by=c("modis_date", "id_pixel")) %>% 
  inner_join(composite_day_of_the_year, by=c("modis_date", "id_pixel")) 
# --- 


# ---
# Function from Verbesselt et. al 2016 (Remote Sensing for Ecologist)
doy2date <- function(year, doy){ 
  as.Date(doy - 1, origin = paste0(year, "-01-01"))}  


# Get year from modis_date and stored as variable 
iv_df$year <- str_sub(iv_df$modis_date, start = 2, end = -4)

iv_df$date <- doy2date(iv_df$year, iv_df$composite_day_of_the_year)
# See Testa et al. 2014 http://server-geolab.agr.unifi.it/public/completed/2014_EuJRS_47_285_305_Testa.pdf
# --- 


# ---
# Export evi dataframe
write.csv(iv_df, file=paste(di, "/data/iv_raw_2015.csv", sep=""), row.names = FALSE)
# ---

