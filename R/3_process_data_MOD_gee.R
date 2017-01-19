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
                pop = poblacion, date, year, gee_index, long = longitude, lat = latitude)

# --- 
# How many images per year 
n_images_pixel <- rawdata %>% 
  group_by(year) %>% 
  summarise(total = length(ndvi),
            bypixel = total / length(unique(iv_malla_modi_id)))
# ---


# ---
# Export evi dataframe
write.csv(rawdata, file=paste(di, "/data/iv_raw_2016.csv", sep=""), row.names = FALSE)
# ---

