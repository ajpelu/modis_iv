# General settings to dowload data 

library(MODISTools)
library(mailR)
library(stringr)


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

FindID(ndvi, mydf)

ID=mydf, 



# Extract coordinates 
miname <- colnames(ndvi[[1]]) 
# Remove "Samp1Line1_pixel1" 
aux_miname <- str_replace(miname, pattern = "\\Samp..*", "")
aux_miname

# milat 
aux_miname_lat <- str_replace(aux_miname, pattern = "\\Lon..*", "")
milat <- as.numeric(
  unlist(stringr::str_split(aux_miname_lat, "Lat", n=2))[2])

# milong 
milong <- as.numeric(unlist(stringr::str_split(aux_miname, "Lon", n=2))[2])

mis_coord <- cbind(milat, milong)







temp <- getwd()
setwd(path_original_MODIS)
xxx <- read.csv(list.files(pattern = ".asc")[1], header = FALSE, as.is = TRUE)

setwd(temp)






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