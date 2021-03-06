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
library("dplyr")
library("lubridate")
library("printr") # To install see https://github.com/yihui/printr
library("ggplot2")
library("ggthemes")
```

``` r
## obsnev_data 
obsnev <- read.csv(file=paste0(di,"/data/iv_obsnev.csv"), header=TRUE, sep=",")

## Downloade data with MODISTool 
md <- read.csv(file=paste0(di,"/data/iv_raw_2015.csv"), header=TRUE, sep=",")
```

Exploring data
--------------

``` r
head(obsnev)
```

|  iv\_malla\_modi\_id| fecha      |   evi|  ndvi|  red|   nir|  blue|  composite\_day\_year|        lng|       lat|  poblacion|
|--------------------:|:-----------|-----:|-----:|----:|-----:|-----:|---------------------:|----------:|---------:|----------:|
|               142799| 2003-12-02 |  1614|  6641|  175|   867|   160|                   332|  -3.265398|  37.19641|          1|
|               142800| 2003-12-02 |  1893|  6171|  272|  1149|   160|                   332|  -3.262782|  37.19641|          1|
|               142801| 2003-12-02 |  3202|  7381|  285|  1892|   141|                   332|  -3.260167|  37.19641|          1|
|               142802| 2003-12-02 |  3347|  7677|  250|  1903|   141|                   332|  -3.257552|  37.19641|          1|
|               142803| 2003-12-02 |  2621|  7394|  213|  1422|   156|                   332|  -3.254936|  37.19641|          1|
|               142804| 2003-12-02 |  2392|  6741|  275|  1413|   156|                   332|  -3.252321|  37.19641|          1|

``` r
head(md)
```

| modis\_date |  id\_pixel|  ndvi|   evi|  pixel\_reliability|  vi\_quality|  composite\_day\_of\_the\_year|  year| date       |
|:------------|----------:|-----:|-----:|-------------------:|------------:|------------------------------:|-----:|:-----------|
| A2000049    |     142799|  5327|  1921|                   0|         2116|                             61|  2000| 2000-03-01 |
| A2000065    |     142799|  4941|  2134|                   0|         2116|                             70|  2000| 2000-03-10 |
| A2000081    |     142799|  4739|  1736|                   0|         2116|                             91|  2000| 2000-03-31 |
| A2000097    |     142799|  5017|  2346|                   0|         2116|                            111|  2000| 2000-04-20 |
| A2000113    |     142799|  5010|  1937|                   0|         2116|                            114|  2000| 2000-04-23 |
| A2000129    |     142799|  5263|  2160|                   0|         2116|                            137|  2000| 2000-05-16 |

Test
----

Get all the data for a pixel (e.g. `142799`) in a year (e.g. `2000`)

``` r
myid <- 142799 
myyear <- 2010 
```

``` r
obsnev %>% 
  filter(iv_malla_modi_id == myid) %>%
  filter(
    year(ymd(fecha)) == myyear) %>% 
  arrange(composite_day_year) %>% 
  select(iv_malla_modi_id, fecha, composite_day_year, evi, ndvi)
```

|  iv\_malla\_modi\_id| fecha      |  composite\_day\_year|   evi|  ndvi|
|--------------------:|:-----------|---------------------:|-----:|-----:|
|               142799| 2010-01-16 |                     6|  1155|  4239|
|               142799| 2010-02-01 |                    18|  2125|  7027|
|               142799| 2010-02-17 |                    33|  1936|  2498|
|               142799| 2010-03-05 |                    63|  1797|  5844|
|               142799| 2010-03-21 |                    74|  1982|  3284|
|               142799| 2010-04-06 |                    93|  1864|  4944|
|               142799| 2010-04-22 |                   100|  2037|  5629|
|               142799| 2010-05-08 |                   127|  2597|  5570|
|               142799| 2010-05-24 |                   143|  2943|  5560|
|               142799| 2010-06-09 |                   159|  3081|  5248|
|               142799| 2010-06-25 |                   173|  2867|  5911|
|               142799| 2010-07-11 |                   191|  2953|  5371|
|               142799| 2010-07-27 |                   196|  2586|  5687|
|               142799| 2010-08-12 |                   212|  2272|  5101|
|               142799| 2010-08-28 |                   235|  2320|  5780|
|               142799| 2010-09-13 |                   246|  2204|  5116|
|               142799| 2010-09-29 |                   257|  2751|  5196|
|               142799| 2010-10-15 |                   278|  1937|  5309|
|               142799| 2010-10-31 |                   298|  2253|  5349|
|               142799| 2010-11-16 |                   310|  1923|  6195|
|               142799| 2010-12-02 |                   323|  2184|  6189|
|               142799| 2010-12-18 |                   346|  1477|  4742|
|               142799| 2010-01-03 |                   361|  2574|  6628|

``` r
md %>%
  filter(id_pixel == myid) %>%
  filter(year %in% myyear) %>% 
  select(id_pixel, date, composite_day_of_the_year, evi, ndvi)
```

|  id\_pixel| date       |  composite\_day\_of\_the\_year|   evi|  ndvi|
|----------:|:-----------|------------------------------:|-----:|-----:|
|     142799| 2010-01-06 |                              6|  1155|  4239|
|     142799| 2010-01-18 |                             18|  2125|  7027|
|     142799| 2010-02-02 |                             33|  1936|  2498|
|     142799| 2010-03-04 |                             63|  1797|  5844|
|     142799| 2010-03-15 |                             74|  1982|  3284|
|     142799| 2010-04-03 |                             93|  1864|  4944|
|     142799| 2010-04-10 |                            100|  2037|  5629|
|     142799| 2010-05-07 |                            127|  2597|  5570|
|     142799| 2010-05-23 |                            143|  2943|  5560|
|     142799| 2010-06-08 |                            159|  3081|  5248|
|     142799| 2010-06-22 |                            173|  2867|  5911|
|     142799| 2010-07-10 |                            191|  2953|  5371|
|     142799| 2010-07-15 |                            196|  2586|  5687|
|     142799| 2010-07-31 |                            212|  2272|  5101|
|     142799| 2010-08-23 |                            235|  2320|  5780|
|     142799| 2010-09-03 |                            246|  2204|  5116|
|     142799| 2010-09-14 |                            257|  2751|  5196|
|     142799| 2010-10-05 |                            278|  1937|  5309|
|     142799| 2010-10-25 |                            298|  2253|  5349|
|     142799| 2010-11-06 |                            310|  1923|  6195|
|     142799| 2010-11-19 |                            323|  2184|  6189|
|     142799| 2010-12-12 |                            346|  1477|  4742|
|     142799| 2010-12-29 |                            363|  1655|  6771|

Exploring temporal series of both datasets
------------------------------------------

``` r
## Exploring two temporal series 
# Preparing data 

aux_md <- md %>%
  mutate(composite_day_year = composite_day_of_the_year,
         iv_malla_modi_id = id_pixel, 
         fecha = as.Date(date), 
         ndvi = ndvi*0.0001,
         evi = evi*0.0001, 
         dataset = 'MTr_package') %>% 
  select(iv_malla_modi_id, fecha, composite_day_year, evi, ndvi, dataset)

aux_obsnev <- obsnev %>% 
  mutate(fecha = as.Date(fecha), 
         ndvi = ndvi*0.0001,
         evi = evi*0.0001,
         dataset = 'obsnev') %>% 
  select(iv_malla_modi_id, fecha, composite_day_year, evi, ndvi, dataset)

aux_df <- rbind(aux_obsnev, aux_md) %>%
  mutate(fecha=as.Date(fecha))


# Export aux dataframe
write.csv(aux_df, file=paste(di, "/data/aux_comparison.csv", sep=""), row.names = FALSE)
# ---
```

See this [shinyapp](https://ajpelu.shinyapps.io/plot_validation/) with interactive plots to compare the both dataset for EVI and NDVI variables. You can also see an example (three pixels) below:

``` r
myids <- sample(md$id_pixel, 3)

aux_md <- md %>%
  filter(id_pixel %in% myids) %>%
  mutate(composite_day_year = composite_day_of_the_year,
         iv_malla_modi_id = id_pixel,
         fecha = date,
         dataset = 'MTr_package') %>% 
  select(iv_malla_modi_id, fecha, composite_day_year, evi, ndvi, dataset)

aux_obsnev <- obsnev %>% 
  filter(iv_malla_modi_id %in% myids) %>%
  mutate(dataset = 'obsnev') %>% 
  select(iv_malla_modi_id, fecha, composite_day_year, evi, ndvi, dataset)

aux <- rbind(aux_md, aux_obsnev)


ggplot(aux, aes(as.Date(fecha), y=ndvi*0.0001, colour=dataset)) +
  geom_line(size=1.1) + 
  xlab('date') + ylab('ndvi') + 
  facet_wrap(~iv_malla_modi_id, ncol=1) +
  theme_few() + scale_colour_fivethirtyeight() 
```

![](test_validation_files/figure-markdown_github/unnamed-chunk-6-1.png)
