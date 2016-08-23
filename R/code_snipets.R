## CODE SNIPETTSS 

library(tictoc) # Clock time 
# Start time 
tic() 




# 1 # ----------------------------------
# See the downloaded files 

## Get name of files dowloaded 
filesdone <- list.files(path=path_original_MODIS)

# Extract ids downloaded 
filesdone <- as.numeric(str_replace(filesdone, pattern = "\\___..*", ""))
# --------------------------------------


# 2 # ---------------------------------- 
# Get number of procesados 
noprocesados <- mydf[!(mydf$ID %in% filesdone), ]



# 3 # ----------------------------------
if (nrow(noprocesados) > 0){ 
  mensaje <- "Aún quedan por procesar"
  cat(mensaje)
  
  # Download the info for the coordinate supplies 
  MODISSubsets(LoadDat=noprocesados, 
               Product=vi_product, 
               Bands=modis_bands,
               Size = c(0,0),
               SaveDir = path_original_MODIS,
               StartDate = TRUE) 
  
  
  # Vuelve a conseguir el nº de imagens descargadas 
  filesdone <- list.files(path=path_original_MODIS)
  # Extract ids downloaded 
  filesdone <- as.numeric(str_replace(filesdone, pattern = "\\___..*", ""))
  
  
  # Enviamos un correo de error 
  msg_error <- paste0("An error was found in the downloading of MODIS. There are ",
                      length(filesdone), " files downloaded") 
  
  send.mail(from = "ajrtask@gmail.com", to = "ajpelu@gmail.com", subject="Task with Error",
            body = msg_error,
            smtp = list(host.name = "smtp.gmail.com", port = 465, 
                        user.name="ajrtask@gmail.com", passwd="mlavlsad0r", ssl=TRUE),
            authenticate = TRUE, send = TRUE)
  
  
  noprocesados <- mydf[!(mydf$ID %in% filesdone), ]
} else { 
  msg_goodjob <- 'Yuhuuuuuuu!!!!! Ya se han terminado de descargar!!!!'
  
  # Mail notification 
  send.mail(from = "ajrtask@gmail.com",
            to = "ajpelu@gmail.com",
            subject="Task finished",
            body = msg_goodjob,
            smtp = list(host.name = "smtp.gmail.com", port = 465, 
                        user.name="ajrtask@gmail.com", passwd="mlavlsad0r", ssl=TRUE),
            authenticate = TRUE,
            send = TRUE)
}




# notes about bulk download MODIS r 
# https://conservationecology.wordpress.com/2014/08/11/bulk-downloading-and-analysing-modis-data-in-r/






## Get name of files dowloaded 
filesdone <- list.files(path=path_original_MODIS)
# Extract ids downloaded 
filesdone <- as.numeric(str_replace(filesdone, pattern = "\\___..*", ""))
# --------------------------------------
# 2 # ---------------------------------- 
# Get number of procesados 
noprocesados <- mydf[!(mydf$ID %in% filesdone), ]

# 1 de Agosto --> Cojo los 500 primeros 
noprocesados <- noprocesados[1:,]

# Download the info for the coordinate supplies 
MODISSubsets(LoadDat=noprocesados, 
             Product=vi_product, 
             Bands=modis_bands,
             Size = c(0,0),
             SaveDir = path_original_MODIS,
             StartDate = TRUE) 

## Get name of files dowloaded 
filesdone <- list.files(path=path_original_MODIS)
# Extract ids downloaded 
filesdone <- as.numeric(str_replace(filesdone, pattern = "\\___..*", ""))

# Get number of procesados 
noprocesados <- mydf[!(mydf$ID %in% filesdone),] 
# Mail notification 
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


# HASTA AQUI EL VALIDO 





















# Download the info for the coordinate supplies 
MODISSubsets(LoadDat=mydf, 
             Product=vi_product, 
             Bands=modis_bands,
             Size = c(0,0),
             SaveDir = path_original_MODIS,
             StartDate = TRUE) 










# Stop the clock!! and save as variable 
exectime <- toc()
exectime <- exectime$toc - exectime$tic










# General settings to dowload data 
# install_github("seantuck12/MODISTools", build_vignettes=TRUE) Ojo última version github
library(MODISTools)

library(stringr)
library(dplyr)
library(reshape2)
library(mailR) # To send infor 
library(tictoc) # Clock time 


# Start time 
tic() 

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


# Read coordinates data 
mydf <- read.csv(file=paste0(di,"/data_raw/coord_qpyr.csv"), header=TRUE, sep=",")

# Format the data of sites 
names(mydf) <- c('ID', 'long', 'lat', 'pob')

# Add dates 
mydf$start.date <- rep(2000, nrow(mydf))
mydf$end.date <- rep(2015, nrow(mydf))

# # Format the data of sites 
# mydf <- data.frame(ID=c(142799,142801),
#                    lat=c(37.1964115002029, 37.1964115002029),
#                    long=c(-3.2653977048134, -3.26016693812872)) 
# # Add dates 
# mydf$start.date <- rep(2000, nrow(mydf))
# mydf$end.date <- rep(2001, nrow(mydf))


# 1 # ----------------------------------
# See the downloaded files 

## Get name of files dowloaded 
filesdone <- list.files(path=path_original_MODIS)

# Extract ids downloaded 
filesdone <- as.numeric(str_replace(filesdone, pattern = "\\___..*", ""))
# --------------------------------------


# 2 # ---------------------------------- 
# Get number of procesados 
noprocesados <- mydf[!(mydf$ID %in% filesdone), ]



# 3 # ----------------------------------
if (nrow(noprocesados) > 0){ 
  mensaje <- "Aún quedan por procesar"
  cat(mensaje)
  
  # Download the info for the coordinate supplies 
  MODISSubsets(LoadDat=noprocesados, 
               Product=vi_product, 
               Bands=modis_bands,
               Size = c(0,0),
               SaveDir = path_original_MODIS,
               StartDate = TRUE) 
  
  
  # Vuelve a conseguir el nº de imagens descargadas 
  filesdone <- list.files(path=path_original_MODIS)
  # Extract ids downloaded 
  filesdone <- as.numeric(str_replace(filesdone, pattern = "\\___..*", ""))
  
  
  # Enviamos un correo de error 
  msg_error <- paste0("An error was found in the downloading of MODIS. There are ",
                      length(filesdone), " files downloaded") 
  
  send.mail(from = "ajrtask@gmail.com", to = "ajpelu@gmail.com", subject="Task with Error",
            body = msg_error,
            smtp = list(host.name = "smtp.gmail.com", port = 465, 
                        user.name="ajrtask@gmail.com", passwd="mlavlsad0r", ssl=TRUE),
            authenticate = TRUE, send = TRUE)
  
  
  noprocesados <- mydf[!(mydf$ID %in% filesdone), ]
} else { 
  msg_goodjob <- 'Yuhuuuuuuu!!!!! Ya se han terminado de descargar!!!!'
  
  # Mail notification 
  send.mail(from = "ajrtask@gmail.com",
            to = "ajpelu@gmail.com",
            subject="Task finished",
            body = msg_goodjob,
            smtp = list(host.name = "smtp.gmail.com", port = 465, 
                        user.name="ajrtask@gmail.com", passwd="mlavlsad0r", ssl=TRUE),
            authenticate = TRUE,
            send = TRUE)
}




# notes about bulk download MODIS r 
# https://conservationecology.wordpress.com/2014/08/11/bulk-downloading-and-analysing-modis-data-in-r/






## Get name of files dowloaded 
filesdone <- list.files(path=path_original_MODIS)
# Extract ids downloaded 
filesdone <- as.numeric(str_replace(filesdone, pattern = "\\___..*", ""))
# --------------------------------------
# 2 # ---------------------------------- 
# Get number of procesados 
noprocesados <- mydf[!(mydf$ID %in% filesdone), ]

# 1 de Agosto --> Cojo los 500 primeros 
noprocesados <- noprocesados[1:,]

# Download the info for the coordinate supplies 
MODISSubsets(LoadDat=noprocesados, 
             Product=vi_product, 
             Bands=modis_bands,
             Size = c(0,0),
             SaveDir = path_original_MODIS,
             StartDate = TRUE) 

## Get name of files dowloaded 
filesdone <- list.files(path=path_original_MODIS)
# Extract ids downloaded 
filesdone <- as.numeric(str_replace(filesdone, pattern = "\\___..*", ""))

# Get number of procesados 
noprocesados <- mydf[!(mydf$ID %in% filesdone),] 
# Mail notification 
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


# HASTA AQUI EL VALIDO 



# La nueva function 
# Ver esto https://github.com/Mkamar/MODISTools/blob/master/R/GetProducts.R 
GetProducts <-
  function()
  {
    getproducts.xml <- paste('
                             <soapenv:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" 
                             xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:mod="http://daac.ornl.gov/MODIS_webservice">
                             <soapenv:Header/>
                             <soapenv:Body>
                             <mod:getproducts soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/"/>
                             </soapenv:Body>
                             </soapenv:Envelope>',
                             sep = "")
    
    header.fields <- c(Accept = "text/xml",
                       Accept = "multipart/*",
                       'Content-Type' = "text/xml; charset=utf-8",
                       SOAPAction = "")
    
    reader <- basicTextGatherer()
    header <- basicTextGatherer()
    
    curlPerform(url = "http://daacmodis.ornl.gov/cgi-bin/MODIS/GLBVIZ_1_Glb_subset/MODIS_webservice.pl",
                httpheader = header.fields,
                postfields = getproducts.xml,
                writefunction = reader$update,
                verbose = FALSE)
    
    # Check the server is not down by insepcting the XML response for internal server error message.
    if(grepl("Internal Server Error", reader$value())){
      stop("Web service failure: the ORNL DAAC server seems to be down, please try again later. 
           The online subsetting tool (http://daac.ornl.gov/cgi-bin/MODIS/GLBVIZ_1_Glb/modis_subset_order_global_col5.pl) 
           will indicate when the server is up and running again.")
    }
    
    xmlres <- xmlRoot(xmlTreeParse(reader$value()))
    productsres <- xmlSApply(xmlres[[1]], 
                             function(x) xmlSApply(x,
                                                   function(x) xmlSApply(x,xmlValue)))
    
    if(colnames(productsres) == "Fault"){
      if(length(productsres['faultstring.text', ][[1]]) == 0){
        stop("Downloading from the web service is currently not working. Please try again later.")
      }
      stop(productsres['faultstring.text', ])
    } else{
      return(as.vector(productsres))
    }
    }





# Vuelve a conseguir el nº de imagens descargadas 
filesdone <- list.files(path=path_original_MODIS)
# Extract ids downloaded 
filesdone <- as.numeric(str_replace(filesdone, pattern = "\\___..*", ""))























# Download the info for the coordinate supplies 
MODISSubsets(LoadDat=mydf, 
             Product=vi_product, 
             Bands=modis_bands,
             Size = c(0,0),
             SaveDir = path_original_MODIS,
             StartDate = TRUE) 




# Loop for process the data 
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


# Merge dataframes 
iv_df <- ndvi %>% 
  inner_join(evi, by=c("modis_date", "id_pixel")) %>% 
  inner_join(pixel_reliability, by=c("modis_date", "id_pixel")) %>% 
  inner_join(vi_quality, by=c("modis_date", "id_pixel")) %>% 
  inner_join(composite_day_of_the_year, by=c("modis_date", "id_pixel")) 



# Function from Verbesselt et. al 2016 (Remote Sensing for Ecologist, chapter 11. Book)
doy2date <- function(year, doy){ 
  as.Date(doy - 1, origin = paste0(year, "-01-01"))}  


# Get year from modis_date and stored as variable 
iv_df$year <- str_sub(iv_df$modis_date, start = 2, end = -4)

iv_df$date <- doy2date(iv_df$year, iv_df$composite_day_of_the_year)
# See Testa et al. 2014 http://server-geolab.agr.unifi.it/public/completed/2014_EuJRS_47_285_305_Testa.pdf


# Export evi dataframe
write.csv(iv_df, file=paste(di, "/data/iv_raw_2015.csv", sep=""), row.names = FALSE)




# Stop the clock!! and save as variable 
exectime <- toc()
exectime <- exectime$toc - exectime$tic







---
  title: "Script to download data of NDVI"
author: "AJ Perez-Luque (@ajpelu)"
date: "2016 July"
output:  
  md_document:
  variant: markdown_github
---
  
  
  ```{r wd, echo=FALSE}
#---------------------------------
machine <- 'ajpelu'
machine <- 'ajpeluLap'
di <- paste('/Users/', machine, '/Dropbox/phd/phd_repos/modis_iv', sep='')
#---------------------------------
```

## Packages 
```{r packages, warning=FALSE}
library("rgdal")
library("RCurl")
library("MODIS")
library("MODISTools")
```

# Read spatial info for centroids of Pyrenean oaks 
* id = 142799;  year = 2000; evi= 4.5445; lng = -3.2653977048134; lat = 37.1964115002029 

142801,2001,6.099,0,"annual",-3.26016693812872,37.1964115002029,1


```{r}
mydf <- data.frame(id=c(142799,142801),
                   lat=c(37.1964115002029, 37.1964115002029),
                   long=c(-3.2653977048134, -3.26016693812872)) 

# Add dates 
mydf$star.date <- rep(2000, nrow(mydf))
mydf$end.date <- rep(2001, nrow(mydf))


GetBands(Product = "MOD13Q1")
GetDates(Product = "MOD13Q1", Lat = mydf$lat[1], Long = mydf$long[1])

MODISSubsets(LoadDat = mydf, Products = "MOD13Q1",
             Bands = "250m_16_days_EVI",
             Size = c(0,0))

subset.string <- read.csv(list.files(pattern = ".asc")[1], 
                          header = FALSE, as.is = TRUE)

subset.string[1, ]


MODISSummaries(LoadDat = mydf, Product = "MOD13Q1", Bands = "250m_16_days_EVI", ScaleFactor = 0.0001)


MODISGrid(NoDataValues = list("MOD13Q1" = c("250m_16_days_EVI" = -3000)))

#### Specify the options for MODIS package: 

* Paths where downloaded files are stored: 
  * `localArcPath`: `/Users/ajpelu/iv_modis` 
* `outDirPath`: `/Users/ajpelu/iv_modis` 

```{r MODISoptions} 
path_descarga <- "/Users/ajpelu/iv_modis"

MODISoptions(localArcPath = path_descarga,
             outDirPath = path_descarga)
```

## MODIS product options and paths
```{r}
viname <- "ndvi"
product <- "MOD13Q1"
ofilename <- paste0(product, "_", viname, "_brick.grd")

pth <- paste0(path_descarga, "/raster_data/", product)
fileout <- paste(pth, "/", ofilename, sep="")



# function to create directories
if ( !file.exists(fileout)) { 
  if (!file.exists(pth)){ 
    print("the outfolder does not exist and will be created")
    print(pth)
    dir.create(pth)
  }
}


### info about MODIS tile

tileH <- 17
tileV <- 5
begin <- "2002.01.01"
end <- "2002.01.31"

modis.hdf <- getHdf(product= product,
                    begin = begin,
                    end = end,
                    tileH = tileH,
                    tileV = tileV,
                    checkIntegrity = TRUE)


