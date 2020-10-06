---
title: "Dorling Shapefile Code"
author: "Melia Petersen"
date: "6/6/2020"
output: html_document
---




```r
library( geojsonio )   # read shapefiles
library( sp )          # work with shapefiles
library( sf )          # work with shapefiles - simple features format
library( mclust )      # cluster analysis 
library( tmap )        # theme maps
library( ggmap )       # map tiles 
library( ggplot2 )     # graphing 
library( ggthemes )    # nice formats for ggplots
library( dplyr )       # data wrangling 
library( pander )      # formatting RMD tables
library( tidycensus )

library( cartogram )  # spatial maps w/ tract size bias reduction
library( maptools )   # spatial object manipulation 

library(here)
```




```r
### HELPER FUNCTIONS FOR LAYOUT

theme_bare <- theme(
  axis.line = element_blank(), 
  axis.text.x = element_blank(), 
  axis.text.y = element_blank(),
  axis.ticks = element_blank(), 
  axis.title.x = element_blank(), 
  axis.title.y = element_blank(),
  legend.text=element_text(size=7),
  legend.title=element_text(size=8),
  panel.background = element_blank(),
  panel.border = element_rect(colour = "gray", fill=NA, size=0.5)
)


# Multiple plot function
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
```


```r
here()
```

```
## [1] "C:/Users/jdlecy/Dropbox/00 - PEDA/00 - GITHUB/usa-dorling-shapefiles"
```



```r
census_api_key( "b431c35dad89e2863681311677d12581e8f24c24" )

crosswalk <- readRDS(here("data/data-raw/cbsa-crosswalk.rds") ) 

these.seattle <- crosswalk$msaname == "SEATTLE-BELLEVUE-EVERETT, WA"
these.fips <- crosswalk$fipscounty[ these.seattle ]
these.fips <- na.omit( these.fips )

state.fips <- substr( these.fips, 1, 2 )
county.fips <- substr( these.fips, 3, 5 )

seattle.pop <-
  get_acs( geography = "tract", 
           variables = "B01003_001", 
           state = "53", 
           county = county.fips[state.fips=="53"], 
           geometry = TRUE ) %>%
  select( GEOID, estimate ) %>%
  rename( POP = estimate )
```




```r
census.dat <- read.csv( here("data/data-raw/LTDB_Std_2010_fullcount.csv") )

# merge shapefile data with census data in new dataframe
seattle <- merge( seattle.pop, census.dat, by.x="GEOID", by.y="tractid" )
seattle2 <- seattle[ ! st_is_empty( seattle ) , ]
seattle.sp <- as_Spatial( seattle2 )
# class( seattle.sp )

plot(seattle.sp)
```

<img src="usa-dorling-shapefiles-code_files/figure-html/unnamed-chunk-5-1.png" width="672" />

## Map Tiles



```r
# project map and remove empty tracts
seattle <- spTransform( seattle.sp, CRS("+init=epsg:3395"))
seattle <- seattle[ seattle$POP != 0 & (! is.na( seattle$POP )) , ]

# convert census tract polygons to dorling cartogram
# no idea why k=0.03 works, but it does - default is k=5
seattle$pop.w <- seattle$POP / 9000 # max(msp.sp$POP)   # standardizes it to max of 1.5
seattle_dorling <- cartogram_dorling( x=seattle, weight="pop.w", k=0.05 )
plot( seattle_dorling )
```

<img src="usa-dorling-shapefiles-code_files/figure-html/unnamed-chunk-6-1.png" width="672" />

```r
dorling.map <- spTransform( seattle_dorling, CRS("+proj=longlat +datum=WGS84") )
plot( dorling.map )
```

<img src="usa-dorling-shapefiles-code_files/figure-html/unnamed-chunk-6-2.png" width="672" />

```r
class( dorling.map )
```

```
## [1] "SpatialPolygonsDataFrame"
## attr(,"package")
## [1] "sp"
```

```r
dorling.sf <-  as( dorling.map, "sf" )
class( dorling.sf )
```

```
## [1] "sf"         "data.frame"
```

```r
### CREATE BOUNDING BOX

sf::st_crs( dorling.sf ) # already in WGS84
```

```
## Coordinate Reference System:
##   User input: unknown 
##   wkt:
## GEOGCRS["unknown",
##     DATUM["World Geodetic System 1984",
##         ELLIPSOID["WGS 84",6378137,298.257223563,
##             LENGTHUNIT["metre",1]],
##         ID["EPSG",6326]],
##     PRIMEM["Greenwich",0,
##         ANGLEUNIT["degree",0.0174532925199433],
##         ID["EPSG",8901]],
##     CS[ellipsoidal,2],
##         AXIS["longitude",east,
##             ORDER[1],
##             ANGLEUNIT["degree",0.0174532925199433,
##                 ID["EPSG",9122]]],
##         AXIS["latitude",north,
##             ORDER[2],
##             ANGLEUNIT["degree",0.0174532925199433,
##                 ID["EPSG",9122]]]]
```

```r
# seattle <- sf::st_transform( seattle, crs = 4326 ) # WGS84 is crs 4326

sf::st_bbox( dorling.sf )
```

```
##       xmin       ymin       xmax       ymax 
## -122.72832   47.17889 -121.39524   48.38813
```

```r
# needs names left, top, right, left for ggmap
bbox <- sf::st_bbox( dorling.sf )
names(bbox) <- c("left","bottom","right","top")




### GET MAP TILES

seattle_stamen <- ggmap::get_stamenmap( bbox=bbox, maptype="toner-lite", zoom=10 )
ggmap( seattle_stamen )
```

<img src="usa-dorling-shapefiles-code_files/figure-html/unnamed-chunk-6-3.png" width="672" />


## Choropleth Maps


```r
### CHOROPLETH MAP

seattle_sf <- dorling.sf

pop.quant <- quantile( seattle_sf$POP, c(0,0.2,0.4,0.6,0.8,1) ) %>% round(0)
labels <- paste0( pop.quant[-length(pop.quant)], "-", pop.quant[-1] )
seattle_sf <- mutate( seattle_sf, pop.cat = cut( POP, breaks=pop.quant, labels=labels ) ) 


seattle.sp <- spTransform( seattle.sp, CRS("+proj=longlat +datum=WGS84") )
seattle_sp <- dorling.sf <-  as( seattle.sp, "sf" )

pop.quant <- quantile( seattle_sp$POP, c(0,0.2,0.4,0.6,0.8,1) ) %>% round(0)
labels <- paste0( pop.quant[-length(pop.quant)], "-", pop.quant[-1] )
seattle_sp <- mutate( seattle_sp, pop.cat = cut( POP, breaks=pop.quant, labels=labels ) ) 

ggplot( seattle_sf ) + 
    geom_sf( aes( fill=pop.cat ) ) +
    scale_fill_brewer( palette = "OrRd" ) 
```

<img src="usa-dorling-shapefiles-code_files/figure-html/unnamed-chunk-7-1.png" width="960" />

```r
ggplot( seattle_sp ) + 
    geom_sf( aes( fill=pop.cat ) ) +
    scale_fill_brewer( palette = "OrRd" ) 
```

<img src="usa-dorling-shapefiles-code_files/figure-html/unnamed-chunk-7-2.png" width="960" />

```r
### CHOROPLETH MAP WITH BACKGROUND TILES


# single map

ggmap( seattle_stamen, extent="device" ) +
    geom_sf( data=seattle_sf, col="darkorange4", alpha=0.5, inherit.aes = FALSE ) +
    # geom_sf( data=seattle_sf, aes( fill=pop.cat ), alpha=0.5, inherit.aes = FALSE ) +
    # scale_fill_brewer( type="seq", palette = "Blues" ) +
    ggtitle( "Dorling Cartogram" ) +
    theme_bare
```

<img src="usa-dorling-shapefiles-code_files/figure-html/unnamed-chunk-7-3.png" width="960" />



# Demographics


```r
# multiple maps together

map1 <-
    ggmap( seattle_stamen ) +
    geom_sf( data=seattle_sp, aes( fill=pop.cat), alpha=0.5, inherit.aes = FALSE ) +
    scale_fill_brewer( type="seq", palette = "Purples" ) +
    ggtitle( "MAP1 Regular" ) +
    theme_bare

map2 <-
    ggmap( seattle_stamen ) +
    geom_sf( data=seattle_sf, aes( fill=pop.cat), alpha=0.5, inherit.aes = FALSE ) +
    scale_fill_brewer( type="seq", palette = "Purples" ) +
    ggtitle( "MAP1 Dorling" ) +
    theme_bare

map3 <-
    ggmap( seattle_stamen ) +
    geom_sf( data=seattle_sp, aes( fill=pop.cat), alpha=0.5, inherit.aes = FALSE ) +
    scale_fill_brewer( type="seq", palette = "Oranges" ) +
    ggtitle( "MAP2 Regular" ) +
    theme_bare

map4 <-
    ggmap( seattle_stamen, extent="device" ) +
    geom_sf( data=seattle_sf, aes( fill=pop.cat), alpha=0.5, inherit.aes = FALSE ) +
    scale_fill_brewer( type="seq", palette = "Oranges" ) +
    ggtitle( "MAP2 Dorling" ) +
    theme_bare


multiplot( map1, map3, map2, map4, cols=2 )
```

<img src="usa-dorling-shapefiles-code_files/figure-html/unnamed-chunk-8-1.png" width="960" />



### Color Options 


```r
# multiple maps together

map1 <-
    ggmap( seattle_stamen ) +
    geom_sf( data=seattle_sp, aes( fill=pop.cat), alpha=0.5, inherit.aes = FALSE ) +
    scale_fill_brewer( type="seq", palette = "Greens" ) +
    ggtitle( "MAP1" ) +
    theme_bare

map2 <-
    ggmap( seattle_stamen ) +
    geom_sf( data=seattle_sf, aes( fill=pop.cat), alpha=0.5, inherit.aes = FALSE ) +
    scale_fill_brewer( type="seq", palette = "Purples" ) +
    ggtitle( "MAP2" ) +
    theme_bare

map3 <-
    ggmap( seattle_stamen ) +
    geom_sf( data=seattle_sp, aes( fill=pop.cat), alpha=0.5, inherit.aes = FALSE ) +
    scale_fill_brewer( type="seq", palette = "Oranges" ) +
    ggtitle( "MAP3" ) +
    theme_bare

map4 <-
    ggmap( seattle_stamen, extent="device" ) +
    geom_sf( data=seattle_sf, aes( fill=pop.cat), alpha=0.5, inherit.aes = FALSE ) +
    scale_fill_brewer( type="seq", palette = "Blues" ) +
    ggtitle( "BLUES" ) +
    theme_bare


multiplot( map1, map3, map2, map4, cols=2 )
```

<img src="usa-dorling-shapefiles-code_files/figure-html/unnamed-chunk-9-1.png" width="672" />

## Census Variables with Seattle Map 

```r
d1 <- seattle.sp@data


keep.these <- c("family10", "vac10",  "agewht10",  "own10" )

d2 <- select( d1, keep.these )
d3 <- apply( d2, 2, scale, )

head( d3[,1:4] ) %>% pander()
```


-------------------------------------------
 family10     vac10     agewht10    own10  
---------- ----------- ---------- ---------
  -1.022    -0.009816    -0.863    -0.7558 

  -2.356     -0.7414     -1.796    -2.377  

  0.7065      0.771      0.8173    0.8776  

  1.067      0.2856      0.7746    0.7579  

  1.105      0.4052      0.9733     1.192  

 -0.8033     0.2575      -1.002    -0.9994 
-------------------------------------------

```r
d3 <- as.data.frame(d3)
```

## Map Tiles own10 


```r
### CHOROPLETH MAP

seattle_sf <- dorling.sf

pop.quant <- quantile( seattle_sf$own10, c(0,0.2,0.4,0.6,0.8,1) ) %>% round(0)
labels <- paste0( pop.quant[-length(pop.quant)], "-", pop.quant[-1] )
seattle_sf <- mutate( seattle_sf, pop.cat = cut( own10, breaks=pop.quant, labels=labels ) ) 


seattle.sp <- spTransform( seattle.sp, CRS("+proj=longlat +datum=WGS84") )
seattle_sp <- dorling.sf <-  as( seattle.sp, "sf" )

pop.quant <- quantile( seattle_sp$own10, c(0,0.2,0.4,0.6,0.8,1) ) %>% round(0)
labels <- paste0( pop.quant[-length(pop.quant)], "-", pop.quant[-1] )
seattle_sp <- mutate( seattle_sp, pop.cat = cut( own10, breaks=pop.quant, labels=labels ) ) 

ggplot( seattle_sf ) + 
    geom_sf( aes( fill=pop.cat ) ) +
    scale_fill_brewer( palette = "OrRd" ) 
```

<img src="usa-dorling-shapefiles-code_files/figure-html/unnamed-chunk-11-1.png" width="672" />

```r
ggplot( seattle_sp ) + 
    geom_sf( aes( fill=pop.cat ) ) +
    scale_fill_brewer( palette = "OrRd" ) 
```

<img src="usa-dorling-shapefiles-code_files/figure-html/unnamed-chunk-11-2.png" width="672" />

```r
### CHOROPLETH MAP WITH BACKGROUND TILES


# single map

ggmap( seattle_stamen, extent="device" ) +
    geom_sf( data=seattle_sf, col="darkorange4", alpha=0.5, inherit.aes = FALSE ) +
    # geom_sf( data=seattle_sf, aes( fill=pop.cat ), alpha=0.5, inherit.aes = FALSE ) +
    # scale_fill_brewer( type="seq", palette = "Blues" ) +
    ggtitle( "Dorling Cartogram" ) +
    theme_bare
```

<img src="usa-dorling-shapefiles-code_files/figure-html/unnamed-chunk-11-3.png" width="672" />

## Map Tiles vac10


```r
# project map and remove empty tracts
seattle <- spTransform( seattle.sp, CRS("+init=epsg:3395"))
seattle <- seattle[ seattle$vac10 != 0 & (! is.na( seattle$vac10 )) , ]

# convert census tract polygons to dorling cartogram
# no idea why k=0.03 works, but it does - default is k=5
seattle$pop.w <- seattle$vac10 / 9000 # max(msp.sp$POP)   # standardizes it to max of 1.5
seattle_dorling <- cartogram_dorling( x=seattle, weight="pop.w", k=0.05 )

dorling.map <- spTransform( seattle_dorling, CRS("+proj=longlat +datum=WGS84") )

class( dorling.map )
```

```
## [1] "SpatialPolygonsDataFrame"
## attr(,"package")
## [1] "sp"
```

```r
dorling.sf <-  as( dorling.map, "sf" )
class( dorling.sf )
```

```
## [1] "sf"         "data.frame"
```

```r
### CREATE BOUNDING BOX

sf::st_crs( dorling.sf ) # already in WGS84
```

```
## Coordinate Reference System:
##   User input: unknown 
##   wkt:
## GEOGCRS["unknown",
##     DATUM["World Geodetic System 1984",
##         ELLIPSOID["WGS 84",6378137,298.257223563,
##             LENGTHUNIT["metre",1]],
##         ID["EPSG",6326]],
##     PRIMEM["Greenwich",0,
##         ANGLEUNIT["degree",0.0174532925199433],
##         ID["EPSG",8901]],
##     CS[ellipsoidal,2],
##         AXIS["longitude",east,
##             ORDER[1],
##             ANGLEUNIT["degree",0.0174532925199433,
##                 ID["EPSG",9122]]],
##         AXIS["latitude",north,
##             ORDER[2],
##             ANGLEUNIT["degree",0.0174532925199433,
##                 ID["EPSG",9122]]]]
```

```r
# seattle <- sf::st_transform( seattle, crs = 4326 ) # WGS84 is crs 4326

sf::st_bbox( dorling.sf )
```

```
##       xmin       ymin       xmax       ymax 
## -122.72586   47.18316 -121.39535   48.38646
```

```r
# needs names left, top, right, left for ggmap
bbox <- sf::st_bbox( dorling.sf )
names(bbox) <- c("left","bottom","right","top")

### GET MAP TILES

seattle_stamen <- ggmap::get_stamenmap( bbox=bbox, maptype="toner-lite", zoom=10 )

### CHOROPLETH MAP

seattle_sf <- dorling.sf

pop.quant <- quantile( seattle_sf$vac10, c(0,0.2,0.4,0.6,0.8,1) ) %>% round(0)
labels <- paste0( pop.quant[-length(pop.quant)], "-", pop.quant[-1] )
seattle_sf <- mutate( seattle_sf, pop.cat = cut( vac10, breaks=pop.quant, labels=labels ) ) 


seattle.sp <- spTransform( seattle.sp, CRS("+proj=longlat +datum=WGS84") )
seattle_sp <- dorling.sf <-  as( seattle.sp, "sf" )

pop.quant <- quantile( seattle_sp$vac10, c(0,0.2,0.4,0.6,0.8,1) ) %>% round(0)
labels <- paste0( pop.quant[-length(pop.quant)], "-", pop.quant[-1] )
seattle_sp <- mutate( seattle_sp, pop.cat = cut( vac10, breaks=pop.quant, labels=labels ) ) 

ggplot( seattle_sf ) + 
    geom_sf( aes( fill=pop.cat ) ) +
    scale_fill_brewer( palette = "OrRd" ) 
```

<img src="usa-dorling-shapefiles-code_files/figure-html/unnamed-chunk-12-1.png" width="672" />

```r
ggplot( seattle_sp ) + 
    geom_sf( aes( fill=pop.cat ) ) +
    scale_fill_brewer( palette = "OrRd" ) 
```

<img src="usa-dorling-shapefiles-code_files/figure-html/unnamed-chunk-12-2.png" width="672" />

```r
### CHOROPLETH MAP WITH BACKGROUND TILES

# single map

ggmap( seattle_stamen, extent="device" ) +
    geom_sf( data=seattle_sf, col="darkorange4", alpha=0.5, inherit.aes = FALSE ) +
    # geom_sf( data=seattle_sf, aes( fill=pop.cat ), alpha=0.5, inherit.aes = FALSE ) +
    # scale_fill_brewer( type="seq", palette = "Blues" ) +
    ggtitle( "Dorling Cartogram" ) +
    theme_bare
```

<img src="usa-dorling-shapefiles-code_files/figure-html/unnamed-chunk-12-3.png" width="672" />

## Map Tiles agewht10 


```r
# project map and remove empty tracts
seattle <- spTransform( seattle.sp, CRS("+init=epsg:3395"))
seattle <- seattle[ seattle$agewht10  != 0 & (! is.na( seattle$agewht10)) , ]

# convert census tract polygons to dorling cartogram
# no idea why k=0.03 works, but it does - default is k=5
seattle$pop.w <- seattle$agewht10  / 9000 # max(msp.sp$POP)   # standardizes it to max of 1.5
seattle_dorling <- cartogram_dorling( x=seattle, weight="pop.w", k=0.05 )

dorling.map <- spTransform( seattle_dorling, CRS("+proj=longlat +datum=WGS84") )

class( dorling.map )
```

```
## [1] "SpatialPolygonsDataFrame"
## attr(,"package")
## [1] "sp"
```

```r
dorling.sf <-  as( dorling.map, "sf" )
class( dorling.sf )
```

```
## [1] "sf"         "data.frame"
```

```r
### CREATE BOUNDING BOX

sf::st_crs( dorling.sf ) # already in WGS84
```

```
## Coordinate Reference System:
##   User input: unknown 
##   wkt:
## GEOGCRS["unknown",
##     DATUM["World Geodetic System 1984",
##         ELLIPSOID["WGS 84",6378137,298.257223563,
##             LENGTHUNIT["metre",1]],
##         ID["EPSG",6326]],
##     PRIMEM["Greenwich",0,
##         ANGLEUNIT["degree",0.0174532925199433],
##         ID["EPSG",8901]],
##     CS[ellipsoidal,2],
##         AXIS["longitude",east,
##             ORDER[1],
##             ANGLEUNIT["degree",0.0174532925199433,
##                 ID["EPSG",9122]]],
##         AXIS["latitude",north,
##             ORDER[2],
##             ANGLEUNIT["degree",0.0174532925199433,
##                 ID["EPSG",9122]]]]
```

```r
# seattle <- sf::st_transform( seattle, crs = 4326 ) # WGS84 is crs 4326

sf::st_bbox( dorling.sf )
```

```
##       xmin       ymin       xmax       ymax 
## -122.73024   47.17747 -121.39298   48.38897
```

```r
# needs names left, top, right, left for ggmap
bbox <- sf::st_bbox( dorling.sf )
names(bbox) <- c("left","bottom","right","top")

### GET MAP TILES

seattle_stamen <- ggmap::get_stamenmap( bbox=bbox, maptype="toner-lite", zoom=10 )

### CHOROPLETH MAP

seattle_sf <- dorling.sf

pop.quant <- quantile( seattle_sf$agewht10, c(0,0.2,0.4,0.6,0.8,1) ) %>% round(0)
labels <- paste0( pop.quant[-length(pop.quant)], "-", pop.quant[-1] )
seattle_sf <- mutate( seattle_sf, pop.cat = cut( agewht10, breaks=pop.quant, labels=labels ) ) 


seattle.sp <- spTransform( seattle.sp, CRS("+proj=longlat +datum=WGS84") )
seattle_sp <- dorling.sf <-  as( seattle.sp, "sf" )

pop.quant <- quantile( seattle_sp$agewht10, c(0,0.2,0.4,0.6,0.8,1) ) %>% round(0)
labels <- paste0( pop.quant[-length(pop.quant)], "-", pop.quant[-1] )
seattle_sp <- mutate( seattle_sp, pop.cat = cut( agewht10, breaks=pop.quant, labels=labels ) ) 

ggplot( seattle_sf ) + 
    geom_sf( aes( fill=pop.cat ) ) +
    scale_fill_brewer( palette = "OrRd" ) 
```

<img src="usa-dorling-shapefiles-code_files/figure-html/unnamed-chunk-13-1.png" width="672" />

```r
ggplot( seattle_sp ) + 
    geom_sf( aes( fill=pop.cat ) ) +
    scale_fill_brewer( palette = "OrRd" ) 
```

<img src="usa-dorling-shapefiles-code_files/figure-html/unnamed-chunk-13-2.png" width="672" />

```r
### CHOROPLETH MAP WITH BACKGROUND TILES

# single map

ggmap( seattle_stamen, extent="device" ) +
    geom_sf( data=seattle_sf, col="darkorange4", alpha=0.5, inherit.aes = FALSE ) +
    # geom_sf( data=seattle_sf, aes( fill=pop.cat ), alpha=0.5, inherit.aes = FALSE ) +
    # scale_fill_brewer( type="seq", palette = "Blues" ) +
    ggtitle( "Dorling Cartogram" ) +
    theme_bare
```

<img src="usa-dorling-shapefiles-code_files/figure-html/unnamed-chunk-13-3.png" width="672" />


## Map Tiles family10 


```r
# project map and remove empty tracts
seattle <- spTransform( seattle.sp, CRS("+init=epsg:3395"))
seattle <- seattle[ seattle$family10 != 0 & (! is.na( seattle$family10 )) , ]

# convert census tract polygons to dorling cartogram
# no idea why k=0.03 works, but it does - default is k=5
seattle$pop.w <- seattle$family10  / 9000 # max(msp.sp$POP)   # standardizes it to max of 1.5
seattle_dorling <- cartogram_dorling( x=seattle, weight="pop.w", k=0.05 )

dorling.map <- spTransform( seattle_dorling, CRS("+proj=longlat +datum=WGS84") )

class( dorling.map )
```

```
## [1] "SpatialPolygonsDataFrame"
## attr(,"package")
## [1] "sp"
```

```r
dorling.sf <-  as( dorling.map, "sf" )
class( dorling.sf )
```

```
## [1] "sf"         "data.frame"
```

```r
### CREATE BOUNDING BOX

sf::st_crs( dorling.sf ) # already in WGS84
```

```
## Coordinate Reference System:
##   User input: unknown 
##   wkt:
## GEOGCRS["unknown",
##     DATUM["World Geodetic System 1984",
##         ELLIPSOID["WGS 84",6378137,298.257223563,
##             LENGTHUNIT["metre",1]],
##         ID["EPSG",6326]],
##     PRIMEM["Greenwich",0,
##         ANGLEUNIT["degree",0.0174532925199433],
##         ID["EPSG",8901]],
##     CS[ellipsoidal,2],
##         AXIS["longitude",east,
##             ORDER[1],
##             ANGLEUNIT["degree",0.0174532925199433,
##                 ID["EPSG",9122]]],
##         AXIS["latitude",north,
##             ORDER[2],
##             ANGLEUNIT["degree",0.0174532925199433,
##                 ID["EPSG",9122]]]]
```

```r
# seattle <- sf::st_transform( seattle, crs = 4326 ) # WGS84 is crs 4326

sf::st_bbox( dorling.sf )
```

```
##       xmin       ymin       xmax       ymax 
## -122.72989   47.17782 -121.39361   48.38894
```

```r
# needs names left, top, right, left for ggmap
bbox <- sf::st_bbox( dorling.sf )
names(bbox) <- c("left","bottom","right","top")

### GET MAP TILES

seattle_stamen <- ggmap::get_stamenmap( bbox=bbox, maptype="toner-lite", zoom=10 )

### CHOROPLETH MAP

seattle_sf <- dorling.sf

pop.quant <- quantile( seattle_sf$family10, c(0,0.2,0.4,0.6,0.8,1) ) %>% round(0)
labels <- paste0( pop.quant[-length(pop.quant)], "-", pop.quant[-1] )
seattle_sf <- mutate( seattle_sf, pop.cat = cut( family10, breaks=pop.quant, labels=labels ) ) 


seattle.sp <- spTransform( seattle.sp, CRS("+proj=longlat +datum=WGS84") )
seattle_sp <- dorling.sf <-  as( seattle.sp, "sf" )

pop.quant <- quantile( seattle_sp$family10, c(0,0.2,0.4,0.6,0.8,1) ) %>% round(0)
labels <- paste0( pop.quant[-length(pop.quant)], "-", pop.quant[-1] )
seattle_sp <- mutate( seattle_sp, pop.cat = cut( family10, breaks=pop.quant, labels=labels ) ) 

ggplot( seattle_sf ) + 
    geom_sf( aes( fill=pop.cat ) ) +
    scale_fill_brewer( palette = "OrRd" ) 
```

<img src="usa-dorling-shapefiles-code_files/figure-html/unnamed-chunk-14-1.png" width="672" />

```r
ggplot( seattle_sp ) + 
    geom_sf( aes( fill=pop.cat ) ) +
    scale_fill_brewer( palette = "OrRd" ) 
```

<img src="usa-dorling-shapefiles-code_files/figure-html/unnamed-chunk-14-2.png" width="672" />

```r
### CHOROPLETH MAP WITH BACKGROUND TILES

# single map

ggmap( seattle_stamen, extent="device" ) +
    geom_sf( data=seattle_sf, col="darkorange4", alpha=0.5, inherit.aes = FALSE ) +
    # geom_sf( data=seattle_sf, aes( fill=pop.cat ), alpha=0.5, inherit.aes = FALSE ) +
    # scale_fill_brewer( type="seq", palette = "Blues" ) +
    ggtitle( "Dorling Cartogram" ) +
    theme_bare
```

<img src="usa-dorling-shapefiles-code_files/figure-html/unnamed-chunk-14-3.png" width="672" />



# Create Dorling Function for Metros


```r
# folder should be where RMD file is
# "C:/Users/jdlecy/Dropbox/00 - PEDA/00 - GITHUB/usa-dorling-shapefiles"
#setwd( "usa-dorling-shapefiles/code" )

# load data
census_api_key( "b431c35dad89e2863681311677d12581e8f24c24" )
crosswalk <- readRDS( "../data/data-raw/cbsa-crosswalk.rds" ) 
census.dat <- read.csv( "../data/data-raw/LTDB_Std_2010_fullcount.csv" )

# fix leading zeros issue:
# convert both IDs to numeric so they both 
# drop the leading zeros

census.dat$tractid <- as.numeric( as.character( census.dat$tractid ) )

# inside the loop below: 
# d$GEOID <- as.numeric( as.character( d$GEOID ) )




options( tigris_use_cache = TRUE )

build_dorling_metro <- function( cbsa.name, include.plot=TRUE )
{

	these.counties  <- crosswalk$cbsaname == cbsa.name
	these.fips <- crosswalk$fipscounty[ these.counties ]
	these.fips <- na.omit( these.fips )

	state.fips <- substr( these.fips, 1, 2 )
	county.fips <- substr( these.fips, 3, 5 )

	# combine all counties
	d.sf <- NULL

	for( i in 1:length(these.fips) )
	{
	   try(  

                d.temp <- 
	        get_acs( geography = "tract", 
	           variables = "B01003_001",  # population
	           state = state.fips[i], 
	           county = county.fips[i], 
	           geometry = TRUE ) %>%
	        select( GEOID, estimate ) %>%
	        rename( POP = estimate )
		   
           ) # end of try

	   d.sf <- rbind( d.sf, d.temp )
	}


	# merge shapefile data with census data in new dataframe:
	# fix leading zeros problem
	# census.dat$tractid <- as.numeric( as.character( census.dat$tractid ) )
	d.sf$GEOID <- as.numeric( as.character( d.sf$GEOID ) )
	d.sf <- merge( d.sf, census.dat, by.x="GEOID", by.y="tractid", all.x=T )

	# convert sf to sp class
	d.sf <- d.sf[ ! st_is_empty( d.sf ) , ]
	d.sp <- as_Spatial( d.sf )

	# project map and remove empty tracts
	d.sp <- spTransform( d.sp, CRS("+proj=longlat +datum=WGS84") )
	d.sp  <- d.sp[ d.sp$POP != 0 & (! is.na( d.sp$POP )) , ]

	# save simple features shapefile as geojson
	metro.name <- tolower( cbsa.name )
	metro.name <- gsub( ",", "", metro.name )
	metro.name <- gsub( " ", "-", metro.name )
	file.name <- paste0( "../maps/metros-shapefile/", metro.name, "-sf.geojson" )
	geojson_write( d.sp, file=file.name, geometry="polygon" )

	# class( d.sp )
	# plot( d.sp)

	# project map and remove empty tracts
	# d.sp <- spTransform( d.sp, CRS("+init=epsg:3395") )
	# d.sp  <- d.sp[ d.sp$POP != 0 & (! is.na( d.sp$POP )) , ]

	# standardizes pop numbers for scaling
	d.sp$pop.w <- d.sp$POP / ( 2 * median(d.sp$POP) ) 
	total.pop <- sum( d.sp$POP, na.rm=T )
        k.scale <-  0.7 * ( 1 / ( log( total.pop ) - 10 ) ) 
        
	# convert census tract polygons to dorling cartogram
        d.sp <- spTransform( d.sp, CRS("+init=epsg:3395") )
	dorling.map <- cartogram_dorling( x=d.sp, weight="pop.w", k=k.scale )  # k=0.05

	# convert to WGS84 for GitHub capatability 
	metro.j <- spTransform( dorling.map, CRS("+proj=longlat +datum=WGS84") )

	# save dorling shapefile as geojson
	metro.name <- tolower( cbsa.name )
	metro.name <- gsub( ",", "", metro.name )
	metro.name <- gsub( " ", "-", metro.name )
	file.name <- paste0( "../maps/metros-dorling/", metro.name, "-dorling.geojson" )

	geojson_write( metro.j, file=file.name, geometry="polygon" )

	#create base maps 
	
	dorling.sf <-  as( dorling.map, "sf" )
class( dorling.sf )

	sf::st_crs( dorling.sf ) # already in WGS84

sf::st_bbox( dorling.sf )

# needs names left, top, right, left for ggmap
bbox <- sf::st_bbox( dorling.sf )
names(bbox) <- c("left","bottom","right","top")

metro_stamen <- ggmap::get_stamenmap( bbox=bbox, maptype="toner-lite", zoom=10 )
ggmap( metro_stamen )


### CHOROPLETH MAP

metro_sf <- dorling.sf

pop.quant <- quantile( metro_sf$POP, c(0,0.2,0.4,0.6,0.8,1) ) %>% round(0)
labels <- paste0( pop.quant[-length(pop.quant)], "-", pop.quant[-1] )
metro_sf <- mutate( metro_sf, pop.cat = cut( POP, breaks=pop.quant, labels=labels ) ) 


d.sp <- spTransform( d.sp, CRS("+proj=longlat +datum=WGS84") )
d_sp <- dorling.sf <-  as( d.sp, "sf" )

pop.quant <- quantile( d_sp$POP, c(0,0.2,0.4,0.6,0.8,1) ) %>% round(0)
labels <- paste0( pop.quant[-length(pop.quant)], "-", pop.quant[-1] )
d_sp <- mutate( d_sp, pop.cat = cut( POP, breaks=pop.quant, labels=labels ) ) 

ggplot( metro_sf ) + 
    geom_sf( aes( fill=pop.cat ) ) +
    scale_fill_brewer( palette = "OrRd" ) 

ggplot( metro_sp ) + 
    geom_sf( aes( fill=pop.cat ) ) +
    scale_fill_brewer( palette = "OrRd" ) 


	 
# plot the base maps in the loop? 

metro_maps <- 
    ggmap( metro_stamen ) +
    geom_sf( data=metro_sp, aes( fill=pop.cat), alpha=0.5, inherit.aes = FALSE ) +
    scale_fill_brewer( type="seq", palette = "Purples" ) +
    ggtitle( "Dorling Shapefile of \n" ) +
    theme_bare


plot(metro_maps)


	# plot the maps 
	if( include.plot )
	{
	  par( mar=c(0,0,4,0), mfrow=c(1,2) )
	  plot( d.sp, main=paste0("Census Tracts of \n", cbsa.name ) )
          plot( d.sp, border="gray80", main=paste0("Dorling Cartogram of \n", cbsa.name ) )
	  plot( dorling.map, col=gray(0.5,0.5), add=TRUE  )

	  }

	
	return( dorling.map )

}



# test the function: 
# fort.worth <- "Fort Worth-Arlington, TX"
# fw <- build_dorling_metro( cbsa.name = fort.worth )
```


# Loop Over Metros 


```r
crosswalk <- read.csv( "https://raw.githubusercontent.com/DS4PS/cpp-529-master/master/data/cbsatocountycrosswalk.csv",  stringsAsFactors=F, colClasses="character" )

metro.areas <- unique( crosswalk$cbsaname )
metro.areas[ metro.areas == "" ] <- NA
metro.areas <- na.omit( metro.areas )

test.metros <- metro.areas[1:10]

for( j in test.metros )
{
  metro.j <- build_dorling_metro( cbsa.name = j )

  # plot metro with base layer ? 

}
```

