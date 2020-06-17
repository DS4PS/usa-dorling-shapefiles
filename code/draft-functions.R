
###############


# folder should be where RMD file is
setwd( "usa-dorling-shapefiles/code" )

# load data
census_api_key( "b431c35dad89e2863681311677d12581e8f24c24" )
crosswalk <- readRDS( "data/data-raw/cbsa-crosswalk.rds" ) 
census.dat <- read.csv( "data/data-raw/LTDB_Std_2010_fullcount.csv" )

# address leading zeros issues:
census.dat$tractid <- as.numeric( as.character( census.dat$tractid ) )
d$GEOID <- as.numeric( as.character( d$GEOID ) )






build_dorling_metro <- function( cbsa.name )
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


	# merge shapefile data with census data in new dataframe
	d.sf <- merge( d.sf, census.dat, by.x="GEOID", by.y="tractid", all.x=T )
    
	# save simple features file
	metro.name <- tolower( cbsa.name )
	metro.name <- gsub( ",", "", metro.name )
	metro.name <- gsub( " ", "-", metro.name )
	file.name <- paste0( "../maps/sf-metros/", metro.name, "-sf.geojson" )
	write_sf( d.sf, file.name )

	# convert sf to sp class
	d.sf <- d.sf[ ! st_is_empty( d.sf ) , ]
	d.sp <- as_Spatial( d.sf )

	# class( d.sp )
	# plot( d.sp)

	# project map and remove empty tracts
	d.sp <- spTransform( d.sp, CRS("+init=epsg:3395") )
	d.sp  <- d.sp[ d.sp$POP != 0 & (! is.na( d.sp$POP )) , ]

	# convert census tract polygons to dorling cartogram
	# no idea why k=0.03 works, but it does - default is k=5
	# max(msp.sp$POP)   # standardizes it to max of 1.5
	d.sp$pop.w <- d.sp$POP / 9000 
	dorling.map <- cartogram_dorling( x=d.sp, weight="pop.w", k=0.05 )

        # save dorling shapefile as geojson
	metro.name <- tolower( cbsa.name )
	metro.name <- gsub( ",", "", metro.name )
	metro.name <- gsub( " ", "-", metro.name )
	file.name <- paste0( "../maps/dorling-metros/", metro.name, "-dorling.geojson" )
	metro.j <- spTransform( dorling.map, CRS("+proj=longlat +datum=WGS84") )
	geojson_write( metro.j, file=file.name, geometry="polygon" )

	# plot the map 
	plot( dorling.map, main=paste0("Dorling Cartogram of \n", cbsa.name ) )

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
  

