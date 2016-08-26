Test Validation
===============

Test for validation for the downloaded data using [`MODISTool` R package](https://cran.r-project.org/web/packages/MODISTools/index.html) and data coming from [Sierra Nevada Global Change Observatory](https://obsnev.es)

Prepare data
------------

``` r
#---------------------------------
machine <- 'ajpelu'
# machine <- 'ajpeluLap'
di <- paste('/Users/', machine, '/Dropbox/phd/phd_repos/modis_iv', sep='')
#---------------------------------
```

``` r
library("rgdal")
library("sp")
library("raster")
library("leaflet")
library("dplyr")


# library("lubridate")
# library("printr") # To install see https://github.com/yihui/printr
# library("ggplot2")
# library("ggthemes")
# 
# 
# 
# library("raster")
# library("rasterVis")
# source(paste0(di,"/R/exportpdf.R")) # function to export raster levelplots maps as pdf
# library("dplyr")
# library("ggplot2")
# library("reshape2")
# library("purrr")
# # library("pander")
# library("knitr")
```

``` r
# Polygonine. SEE this tutorial http://neondataskills.org/working-with-field-data/Field-Data-Polygons-From-Centroids 

# Prepare spatial data 
iv <- read.csv(file=paste0(di,"/data_raw/coord_qpyr.csv"), header=TRUE, sep=",")
# rename 
names(iv) <- c('iv_malla_modi_id','x','y', 'pob')

# Set coordinates 
coordinates(iv) <- c('x', 'y') 
# Set projection 
proj4string(iv) <- CRS("+init=epsg:4326") 
# Reproject 
iv_re <- spTransform(iv, CRS("+init=epsg:23030")) 

# Get dataframe 
ivdf <- as.data.frame(iv_re)

# Define radius 
radius <- 250/2

# define the boundaries around each centroide 
yPlus <- ivdf$y+radius
xPlus <- ivdf$x+radius
yMinus <- ivdf$y-radius
xMinus <- ivdf$x-radius

# Create a closed polygon (five points, first and last are equal )
square=cbind(xMinus,yPlus, xPlus,yPlus, xPlus,yMinus, xMinus,yMinus,xMinus,yPlus,xMinus,yPlus)

ID <- ivdf$iv_malla_modi_id

# Option 1 (more efficient ) 
#create spatial polygons
polys <- SpatialPolygons(
  mapply(function(poly, id) {
  xy <- matrix(poly, ncol=2, byrow=TRUE)
  Polygons(list(Polygon(xy)), ID=id)
}, split(square, row(square)), ID),
 proj4string=CRS(as.character("+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")))

# Create SpatilPolygonDataFrame -- this step is required to output multiple polygons.
polys.df <- SpatialPolygonsDataFrame(polys, data.frame(id=ID, row.names=ID))




# # Option 2 (less efficient )
# a <- vector('list', length(2))
# 
# # loop through each centroid value and create a polygon
# for (i in 1:nrow(ivdf)) {
#  a[[i]]<-Polygons(list(Polygon(matrix(square[i, ], ncol=2, byrow=TRUE))), ID[i])
# }
# 
# polys<-SpatialPolygons(a,
#                        proj4string=CRS(as.character("+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")))

# Reproject 
aux_polys <- spTransform(polys.df, CRS("+init=epsg:4326"))
```

``` r
# Map the polygons 
m <- leaflet() %>% 
  addWMSTiles('http://www.ideandalucia.es/services/toporaster10/wms?',
              layers = 'toporaster10',
              options = WMSTileOptions(format = "image/png", transparent = FALSE),
              attribution = '<a href="http://www.juntadeandalucia.es/institutodeestadisticaycartografia" target="_blank">Instituto de Estadística y Cartografía de Andalucía</a>',
              group = 'Topographical') %>%
  
  addTiles(urlTemplate = "http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png",
            attribution = '<a href="https://carto.com/attributions">CARTO</a>',
           group = 'Basemap') %>%
  
  addProviderTiles("Esri.WorldImagery", group='Satellite') %>% 
  
    # Layers control
  addLayersControl(position = 'bottomright',
                   baseGroups = c("Topographical","Basemap", "Satellite"),
                   overlayGroups = 'Pixel MODIS', 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  
  addPolygons(data = aux_polys, 
              popup=paste0("<strong>iv_malla_modi_id: </strong>", aux_polys$id), 
              group='Pixel MODIS')
              
print(m)


# # SEE some urls: 
# http://amsantac.co/blog/en/r/2015/08/11/leaflet-R.html
# http://spatialrecology.org/posts/leafletmapping.html
# https://rstudio.github.io/leaflet/basemaps.html
```
