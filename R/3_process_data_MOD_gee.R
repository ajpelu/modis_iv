# --- 
# Script to process downloaded MOD13Q1 data for Q. pyrenaica forests of Sierra Nevada # 
# Author: Perez-Luque AJ (@ajpelu)
# version: 1
# date: 2017 January
# --- 

# --- 
# Set directory 
# machine <- 'ajpeluLap'
machine <- 'ajpelu'
di <- paste('/Users/', machine, '/Dropbox/phd/phd_repos/modis_iv', sep='')
# --- 

# --- 
library('stringr')
library('dplyr')
library('reshape2')
library('lubridate')
library('binaryLogic')
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

decodeQA <- function(x){
  bit <- intToBits(x)
  paste(tail(rev(as.integer(bit)), 16), collapse="")}

decodeQAv <- Vectorize(decodeQA)



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
    
    # Detailed QA 
    bit = decodeQAv(DetailedQA),
    qa_quality = substr(bit, 15, 16), 
    qa_usefulness = substr(bit, 11, 14), 
    qa_aerosol = substr(bit, 9, 10),
    qa_adj_cloud = substr(bit, 8, 8),
    qa_atmos = substr(bit, 7, 7),
    qa_mix_cloud = substr(bit, 6, 6),
    qa_landwater = substr(bit, 3, 5),
    qa_snow = substr(bit, 2, 2),
    qa_shadow = substr(bit, 1, 1),
    
    # Lat/Long
    aux_geo = stringr::str_replace(.geo, pattern = "..*\\[", ""),
    longitude = as.numeric(stringr::str_replace(aux_geo, pattern = "\\,..*", "")),
    latitude = as.numeric(
      stringr::str_replace(
        stringr::str_replace(aux_geo, pattern = "..*\\,", ""), 
        pattern = "\\]..*", ""))) %>%
  # Select 
  dplyr::select(doy = DayOfYear, evi = EVI, ndvi = NDVI, summQA = SummaryQA, iv_malla_modi_id,
                pop = poblacion, date, year, gee_index, long = longitude, lat = latitude, bit,
                qa_quality, qa_usefulness, 
                qa_aerosol, qa_adj_cloud, 
                qa_atmos, qa_mix_cloud, qa_landwater, qa_snow, qa_shadow)


rr <- rawdata %>% filter(summQA == 1)




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




qabit = paste(as.binary(2116, n=16), collapse="")

qa_quality = str_sub(qabit, 1, 2)
