# General settings to dowload data 


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


                                            

mydf <- data.frame(id=c(142799,142801),
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
                Simplify = TRUE)

MODISGrid(Dir = path_original_MODIS,
          NoDataValues = list("MOD13Q1" = c("250m_16_days_EVI" = -3000,
                                            "250m_16_days_NDVI" = -3000,
                                            "250m_16_days_VI_Quality" = 65535,
                                            "250m_16_days_pixel_reliability" = -1,
                                            "250m_16_days_composite_day_of_the_year" = -1)))
          
          
          

colnames(ndvi)

library()