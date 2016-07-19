# General settings to dowload data 

library(MODISTools)
library(mailR)
library(stringr)
library(dplyr)
library(reshape2)


## Time-comsuption
start.time <- Sys.time()

## Create dir to store MODIS original 
path_original_MODIS <- file.path(di, "data_raw/modis")
                                 
dir.create(path_original_MODIS, recursive = TRUE) 

## Set product name 
vi_product <- "MOD13Q1"

## bands (see https://lpdaac.usgs.gov/dataset_discovery/modis/modis_products_table/mod13q1)
modis_bands <- c("250m_16_days_NDVI",
             "250m_16_days_EVI",
             "250m_16_days_VI_Quality",
             "250m_16_days_pixel_reliability",
             "250m_16_days_composite_day_of_the_year")


                                            
# Format the data of sites 
mydf <- data.frame(ID=c(142799,142801),
                   lat=c(37.1964115002029, 37.1964115002029),
                   long=c(-3.2653977048134, -3.26016693812872)) 
# Add dates 
mydf$start.date <- rep(2000, nrow(mydf))
mydf$end.date <- rep(2001, nrow(mydf))


# Call to download the subsets 
MODISSubsets(LoadDat=mydf, 
             Product=vi_product, 
             Bands=modis_bands,
             Size = c(0,0),
             SaveDir = path_original_MODIS,
             StartDate = TRUE) 





ndvi <- MODISTimeSeries(Dir = path_original_MODIS, 
                  Band = "250m_16_days_NDVI", 
                  Simplify = FALSE) 






 


# ID of pixels and coordinates 
## Extract coordinates 
miname <- colnames(ndvi[[1]]) 
### Remove "Samp1Line1_pixel1" 
aux_miname <- str_replace(miname, pattern = "\\Samp..*", "")
aux_miname

### milat 
aux_miname_lat <- str_replace(aux_miname, pattern = "\\Lon..*", "")
milat <- as.numeric(
  unlist(stringr::str_split(aux_miname_lat, "Lat", n=2))[2])

### milong 
milong <- as.numeric(unlist(stringr::str_split(aux_miname, "Lon", n=2))[2])

### Coordinates
mis_coord <- as.data.frame(cbind(milat, milong))

# Get id of pixel 
myid <- mis_coord %>% 
  inner_join(mydf, by= c('milat' = 'lat', 'milong' = 'long'))


# Change name "Lat37.1964115002029Lon-3.2653977048134Samp1Line1_pixel1" for 
# pixel ID
colnames(ndvi[[1]]) <- myid$ID


aux_data <- ndvi[[1]] %>% 
  melt() %>% 
  select(modis_date = Var1,
         id_pixel = Var2,
         ndvi = value)
  





% modis_bands[i]
% 


# 
aux_data_out <- c() 
aux_data_2 <- c() 


for (i in modis_bands){ 
  
  ### Get name of the band 
  name_band <- tolower(str_replace(modis_bands[i], pattern = "250m_16_days_", ""))
  
  ### Create a MODIS_Temporal_Series
  mimodis_ts <- MODISTimeSeries(Dir = path_original_MODIS, 
                  Band = i, 
                  Simplify = FALSE) 
  
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
        select(modis_date = Var1,
               id_pixel = Var2,
               name_band = value)
      
      aux_data_out <- rbind(aux_data_out, aux_data)
      
    }
  
  names(aux_data_out)[3] <- name_band
  
  ##
  aux_data_2 <- cbind(aux_data_2, aux_data_out) 
  } 
  
  


  
  
  
  # Change name "Lat37.1964115002029Lon-3.2653977048134Samp1Line1_pixel1" for 
  # pixel ID
  colnames(ndvi[[1]]) <- myid$ID
  
  
  aux_data <- ndvi[[1]] %>% 
    melt() %>% 
    select(modis_date = Var1,
           id_pixel = Var2,
           ndvi = value)
  
  
  
  }

modis_bands[1]










# 
# MODISSummaries(mydf, 
#                Dir = path_original_MODIS,  
#                Product = vi_product, 
#                Bands = modis_bands[1:2], 
#                ValidRange = c(-2000, 10000), 
#                NoDataFill = -3000, 
#                ScaleFactor = 0.0001, 
#                StartDate = FALSE, 
#                Interpolate = TRUE, 
#                Yield = TRUE, 
#                QualityScreen = TRUE, 
#                QualityBand = "250m_16_days_pixel_reliability", QualityThreshold = 1)


ndvi <- as.data.frame(ndvi)
ndvi$jday_raw <- row.names(ndvi)















# --- NO BORRAR --- 

# Time comsuption 
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# Mail notification 
task <- "Download and process MODIS data"
sender <- "ajrtask@gmail.com"
recipients <- c("ajpelu@gmail.com")
msg <- paste0("The task ", task, " has finished. Time consumed:", time.taken) 

send.mail(from = sender,
          to = recipients,
          subject="Task finished",
          body = msg,
          smtp = list(host.name = "smtp.gmail.com", port = 465, 
                      user.name="ajrtask@gmail.com", passwd="mlavlsad0r", ssl=TRUE),
          authenticate = TRUE,
          send = TRUE)

# --- END NO BORRAR ---








ndvi <- MODISTimeSeries(Dir = path_original_MODIS, 
                Band = "250m_16_days_NDVI", 
                Simplify = TRUE)

ndvi <- as.data.frame(ndvi)
ndvi$jday_raw <- row.names(ndvi)















MODISSummaries(LoadDat = mydf,
               Dir = path_original_MODIS,
               Product = "MOD13Q1", Bands = "250m_16_days_EVI",
               ValidRange = c(-2000,10000), NoDataFill = -3000, ScaleFactor = 0.0001,
               StartDate = TRUE)


# Finally read the output
read.table("MODIS_Summary_MOD13Q1_2014-08-10.csv",header = T,sep = ",")













MODISGrid(Dir = path_original_MODIS,
          NoDataValues = list("MOD13Q1" = c("250m_16_days_EVI" = -3000,
                                            "250m_16_days_NDVI" = -3000,
                                            "250m_16_days_VI_Quality" = 65535,
                                            "250m_16_days_pixel_reliability" = -1,
                                            "250m_16_days_composite_day_of_the_year" = -1)))
          
          
          

colnames(ndvi)

library()