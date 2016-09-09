# --- 
# Script to process downloaded NDVI data for Q. pyrenaica forests of Sierra Nevada # 
# Author: Perez-Luque AJ (@ajpelu)
# version: 1
# date: 2016 July 
# --- 


# --- 
# Set directory 
machine <- 'ajpelu'
# machine <- 'ajpeluLap'
di <- paste('/Users/', machine, '/Dropbox/phd/phd_repos/modis_iv', sep='')
# --- 


# --- 
# Install and load required pacakges 
# install_github("seantuck12/MODISTools", build_vignettes=TRUE) 
library(MODISTools) 
library(stringr)
library(dplyr)
library(tidyr)
library(reshape2)
source(file=paste0(di, '/R/getlat.R'))
source(file=paste0(di, '/R/getlong.R'))
source(file=paste0(di, '/R/doy2date.R'))
# ---


# --- 
# GENERAL SETTINGS TO DOWNLOAD DATA 
## Create dir to store MODIS original 
path_original_MODIS <- file.path(di, "data_raw/modis/")


# --- 
# List all files downloaded (.asc format)
myfiles <- list.files(path_original_MODIS)

# --- 
iv_df <- data.frame()
  
for (i in 1:length(myfiles)){ 
  
  # Read file 
  asc_file <- read.csv(paste0(path_original_MODIS, myfiles[i]), header=FALSE, as.is=TRUE)
  
  # Get name of my pixel 
  mypixel_name  <- as.numeric(str_replace(myfiles[i], pattern = "___MOD13Q1.asc", ""))
  
  # Create name for colums:
    # nrow, ncol: number of tile rows, tile cols 
    # xll, yll: x-coordinate (y-coordinate) for lowert left corner of the tile (MODIS DATUM longitude)
    # pixelsize: 231.6564
    # row.id: Unique Identifier 
    # product code: Shortname code for the MODIS product requested
    # MODIS.acq.date: Date code for this string of data, year and Julian day (A[YYYYDDD]) 
    # where: Input coordinates and the width (Samp) and height (Line) in number of pixels of the tile surrounding the input coordinate
    # MODIS.proc.date: Date–time that MODIS data product was processed (YYYYDDDHHMMSS)
    # mod_value: value of the variable. NOTE: It's a modification of See Tuck et al. 2014 doi:10.1002/ece3.1273
  names(asc_file) <- c("nrow", "ncol", "xll", "yll", "pixelsize", "row.id", "product.code", "MODIS.acq.date",
                        "where", "MODIS.proc.date", "mod_value")
  
  ##  and select variables 
  aux_file <- asc_file %>% 
    # Get lat and long
    mutate(lat = getlat(where), 
           long = getlong(where)) %>% 
    # Change name of MODIS.proc.date (mpd) and MODIS.acq.date (mad)
    mutate(mpd = MODIS.proc.date,
           year_adq = as.numeric(stringr::str_sub(MODIS.acq.date, start = 2, end = -4)),
           jday_adq = as.numeric(stringr::str_sub(MODIS.acq.date, start = 6, end = -1)), 
           date_adq = doy2date(year=year_adq, doy=jday_adq)) %>%  
    # Variable with pixel id 
    mutate(iv_malla_modi_id = mypixel_name) %>%
    # Get name of the modis variable
    separate(row.id, c("row.id_dup", "mod_variable"), ".250m_16_days_", remove = FALSE) %>% 
    # Select variables of interest  
    select(iv_malla_modi_id, lat, long, mod_value, mod_variable, year_adq, jday_adq, date_adq) %>% 
    # Remove a duplicate day 
    unique() %>% 
    dcast(iv_malla_modi_id + lat + long + year_adq + jday_adq + date_adq ~ mod_variable, value.var='mod_value')
  

  iv_df <- rbind(iv_df, aux_file)
} 
 


# ---
# Export evi dataframe
write.csv(iv_df, file=paste(di, "/data/iv_raw.csv", sep=""), row.names = FALSE)
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

