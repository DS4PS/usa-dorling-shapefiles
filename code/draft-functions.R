###############

# install necessary packages

required.packages <-
c( "geojsonio", "sp", "sf", "mclust", 
   "tmap", "ggplot2", "ggthemes", 
   "dplyr", "pander", "tidycensus", 
   "cartogram", "maptools" ) 
install.packages( required.packages )

library( geojsonio )   # read shapefiles
library( sp )          # work with shapefiles
library( sf )          # work with shapefiles - simple features format
library( mclust )      # cluster analysis 
library( tmap )        # theme maps
library( ggplot2 )     # graphing 
library( ggthemes )    # nice formats for ggplots
library( dplyr )       # data wrangling 
library( pander )      # formatting RMD tables
library( tidycensus )

library( cartogram )   # spatial maps w/ tract size bias reduction
library( maptools )    # spatial object manipulation 

library(here)



# folder should be where RMD file is
# "C:/Users/jdlecy/Dropbox/00 - PEDA/00 - GITHUB/usa-dorling-shapefiles"
setwd( "usa-dorling-shapefiles/code" )

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

	d.sf <- NULL

	for( i in 1:length(these.fips) )
	{
	d.temp <- 
	get_acs( geography = "tract", 
	   variables = "B01003_001",  # population
	   state = state.fips[i], 
	   county = county.fips[i], 
	   geometry = TRUE ) %>%
	select( GEOID, estimate ) %>%
	rename( POP = estimate )

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
	geojson_write( d.sp, file=file.name, geometry="polygon", delete_dsn=TRUE )

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

	geojson_write( metro.j, file=file.name, geometry="polygon", delete_dsn=TRUE )

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




