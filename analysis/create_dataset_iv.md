``` r
#---------------------------------
# machine <- 'ajpelu'
machine <- 'ajpeluLap'
di <- paste('/Users/', machine, '/Dropbox/phd/phd_repos/modis_iv', sep='')
#---------------------------------
```

``` r
library("dplyr")
library("lubridate")
```

``` r
#iv data
iv_raw <- read.csv(file=paste0(di,"/data/iv_raw_2015.csv"), header=TRUE, sep=",")
# spatial info
iv_coord <- read.csv(file=paste0(di,"/data_raw/coord_qpyr.csv"), header=TRUE, sep=",")
```

``` r
iv <- iv_raw %>% 
  mutate(iv_malla_modi_id = id_pixel) %>% 
  inner_join(iv_coord, by='iv_malla_modi_id') %>% 
  select(iv_malla_modi_id, ndvi, evi, composite_day_of_the_year, date, lng, lat, poblacion)

write.csv(iv, file = paste0(di,'/data/iv_quercus_pyrenaica.csv'), row.names = FALSE)
```
