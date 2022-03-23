# USA Dorling Shapefiles

Dorling cartogram shapefiles for census tracts in US metro areas (one file for each metro).

# About 

The project uses the **tidycensus** and **cartogram** packages in R to convert regular metro shapefiles available through the US Census into Dorling cartograms.

The cartogram shapefiles for all ~1,000 US metro areas are archived in this repo for easy future deployment. 

# Use

```r
library( geojsonio )   # read shapefiles
library( sp )          # work with shapefiles
library( sf )          # work with shapefiles - simple features format

# dorling cartogram of Phoenix Census Tracts
github.url <- "https://raw.githubusercontent.com/DS4PS/cpp-529-master/master/data/phx_dorling.geojson"
phx <- geojson_read( x=github.url,  what="sp" )
plot( phx )
```

[Reference Catalog](https://ds4ps.org/usa-dorling-shapefiles/code/build-metro-dorling-shapefiles.html)

[Replication Instructions](https://github.com/DS4PS/usa-dorling-shapefiles/blob/master/code/draft-functions.R)




# Contributors 

Jesse Lecy: lecy
Melia Petersen: meliapetersen  
Jaesa Rogers: jaesar 




