# --- 
# Script to download NDVI data for Q. pyrenaica forests of Sierra Nevada # 
# Author: Perez-Luque AJ (@ajpelu)
# version: 1
# date: 2016 July 
# --- 


# --- 
# Set directory 
machine <- 'ajpeluLap'
di <- paste('/Users/', machine, '/Dropbox/phd/phd_repos/modis_iv', sep='')
# --- 


# --- 
# Install and load required pacakges 
# install_github("seantuck12/MODISTools", build_vignettes=TRUE) 
library(MODISTools) 
library(stringr)
library(dplyr)
library(reshape2)
library(mailR) # To send info 
# ---


# --- 
# GENERAL SETTINGS TO DOWNLOAD DATA 
## Create dir to store MODIS original 
path_original_MODIS <- file.path(di, "data_raw/modis")

dir.create(path_original_MODIS, recursive = TRUE) 

## Set product name 
vi_product <- "MOD13Q1"

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
# DOWNLOAD DATA 
## NOTA: Por si falla el script, compruebo los datos que se han descargado, y los descargo de la descarga.

# 1 # Get name of files dowloaded 
filesdone <- list.files(path=path_original_MODIS)
# Extract ids downloaded 
filesdone <- as.numeric(str_replace(filesdone, pattern = "\\___..*", ""))

# 2 # Get number of procesados 
noprocesados <- mydf[!(mydf$ID %in% filesdone), ]


# AUX # Si necesitas particionar (para acelerar la descarga) 
# noprocesados <- noprocesados[1:100,]


# 3 # Download the info for the coordinate supplies 
MODISSubsets(LoadDat=noprocesados, 
             Product=vi_product, 
             Bands=modis_bands,
             Size = c(0,0),
             SaveDir = path_original_MODIS,
             StartDate = TRUE) 

# 4 # How many files have been downloaded?  
filesdone <- list.files(path=path_original_MODIS)
filesdone <- as.numeric(str_replace(filesdone, pattern = "\\___..*", ""))
# Get number of procesados 
noprocesados <- mydf[!(mydf$ID %in% filesdone),] 

# 5 # Mail notification 
task <- "Download and process MODIS data"
sender <- "ajrtask@gmail.com"
recipients <- c("ajpelu@gmail.com")
msg <- paste0("The task ", task, " has finished. Quedan ", nrow(noprocesados)) 

send.mail(from = sender,
          to = recipients,
          subject="Task finished",
          body = msg,
          smtp = list(host.name = "smtp.gmail.com", port = 465, 
                      user.name="ajrtask@gmail.com", passwd="mlavlsad0r", ssl=TRUE),
          authenticate = TRUE,
          send = TRUE)

# --- 


