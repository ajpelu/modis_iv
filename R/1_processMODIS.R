# --- 
# Script to process downloaded NDVI data for Q. pyrenaica forests of Sierra Nevada # 
# Author: Perez-Luque AJ (@ajpelu)
# version: 1
# date: 2016 July 
# --- 


# --- 
# Set directory 
machine <- 'ajpelu'
di <- paste('/Users/', machine, '/Dropbox/phd/phd_repos/modis_iv', sep='')
# --- 


# --- 
# Install and load required pacakges 
# install_github("seantuck12/MODISTools", build_vignettes=TRUE) 
library(MODISTools) 
library(stringr)
library(dplyr)
library(reshape2)
# ---


# --- 
# GENERAL SETTINGS TO DOWNLOAD DATA 
## Create dir to store MODIS original 
path_original_MODIS <- file.path(di, "data_raw/modis")

## Bands (see https://lpdaac.usgs.gov/dataset_discovery/modis/modis_products_table/mod13q1)
modis_bands <- c("250m_16_days_NDVI",
                 "250m_16_days_EVI",
                 "250m_16_days_VI_Quality",
                 "250m_16_days_pixel_reliability",
                 "250m_16_days_composite_day_of_the_year")
# --- 


# --- 
# SPECIFY GEOLOCATION FOR DOWNLOAD DATA 
## Read coordinates data 
mydf <- read.csv(file=paste0(di,"/data_raw/coord_qpyr.csv"), header=TRUE, sep=",")

# Format the data of sites 
names(mydf) <- c('ID', 'long', 'lat', 'pob')

# Add dates 
mydf$start.date <- rep(2000, nrow(mydf))
mydf$end.date <- rep(2015, nrow(mydf))
# --- 


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

