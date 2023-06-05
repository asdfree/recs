# 
# 
# 
sas_tf <- tempfile()

sas_url <- "https://www.eia.gov/consumption/residential/data/2020/sas/recs2020_public_v2.zip"

download.file( sas_url , sas_tf , mode = 'wb' )
library(haven)

recs_tbl <- read_sas( sas_tf )

recs_df <- data.frame( recs_tbl )

names( recs_df ) <- tolower( names( recs_df ) )

recs_df[ , 'one' ] <- 1
# recs_fn <- file.path( path.expand( "~" ) , "RECS" , "this_file.rds" )
# saveRDS( recs_df , file = recs_fn , compress = FALSE )
# recs_df <- readRDS( recs_fn )
library(survey)

recs_design <-
	svrepdesign(
		data = recs_df ,
		weight = ~ nweight ,
		repweights = 'nweight[1-9]+' ,
		type = 'JK1' ,
		combined.weights = TRUE ,
		scale = 59 / 60 ,
		mse = TRUE
	)
recs_design <- 
	
	update( 
		
		recs_design , 
		
		main_heating_fuel = 
			factor(
				fuelheat ,
				levels = c( -2 , 5 , 1 , 2 , 3 , 7 , 99 ) ,
				labels = 
					c(
						'Not applicable' , 
						'Electricity' , 
						'Natural gas from underground pipes' , 
						'Propane (bottled gas)' , 
						'Fuel oil' , 
						'Wood or pellets' , 
						'Other' 
					)
			) ,
			
		swimpool_binary =
			ifelse( swimpool %in% 0:1 , swimpool , NA )
			
	)
sum( weights( recs_design , "sampling" ) != 0 )

svyby( ~ one , ~ main_heating_fuel , recs_design , unwtd.count )
svytotal( ~ one , recs_design )

svyby( ~ one , ~ main_heating_fuel , recs_design , svytotal )
svymean( ~ totsqft_en , recs_design )

svyby( ~ totsqft_en , ~ main_heating_fuel , recs_design , svymean )
svymean( ~ state_name , recs_design )

svyby( ~ state_name , ~ main_heating_fuel , recs_design , svymean )
svytotal( ~ totsqft_en , recs_design )

svyby( ~ totsqft_en , ~ main_heating_fuel , recs_design , svytotal )
svytotal( ~ state_name , recs_design )

svyby( ~ state_name , ~ main_heating_fuel , recs_design , svytotal )
svyquantile( ~ totsqft_en , recs_design , 0.5 )

svyby( 
	~ totsqft_en , 
	~ main_heating_fuel , 
	recs_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE 
)
svyratio( 
	numerator = ~ totcsqft , 
	denominator = ~ totsqft_en , 
	recs_design 
)
sub_recs_design <- subset( recs_design , nummeal == 1 )
svymean( ~ totsqft_en , sub_recs_design )
this_result <- svymean( ~ totsqft_en , recs_design )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ totsqft_en , 
		~ main_heating_fuel , 
		recs_design , 
		svymean 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( recs_design )
svyvar( ~ totsqft_en , recs_design )
# SRS without replacement
svymean( ~ totsqft_en , recs_design , deff = TRUE )

# SRS with replacement
svymean( ~ totsqft_en , recs_design , deff = "replace" )
svyciprop( ~ swimpool_binary , recs_design ,
	method = "likelihood" , na.rm = TRUE )
svyttest( totsqft_en ~ swimpool_binary , recs_design )
svychisq( 
	~ swimpool_binary + state_name , 
	recs_design 
)
glm_result <- 
	svyglm( 
		totsqft_en ~ swimpool_binary + state_name , 
		recs_design 
	)

summary( glm_result )
sas_v1_tf <- tempfile()

sas_v1_url <- "https://www.eia.gov/consumption/residential/data/2020/sas/recs2020_public_v1.zip"

download.file( sas_v1_url , sas_v1_tf , mode = 'wb' )

recs_v1_tbl <- read_sas( sas_v1_tf )

recs_v1_df <- data.frame( recs_v1_tbl )

names( recs_v1_df ) <- tolower( names( recs_v1_df ) )

recs_v1_design <-
 svrepdesign(
 data = recs_v1_df ,
 weight = ~ nweight ,
 repweights = 'nweight[1-9]+' ,
 type = 'JK1' ,
 combined.weights = TRUE ,
 scale = 59 / 60 ,
 mse = TRUE
 )
	
recs_v1_design <- 
	
	update( 
		
		recs_v1_design , 
		
		natural_gas_mainspace_heat = as.numeric( fuelheat == 1 )
			
	)
	
result <-
	svytotal( 
		~ natural_gas_mainspace_heat , 
		recs_v1_design 
	)

stopifnot( round( coef( result ) , 0 ) == 56245389 )
stopifnot( round( SE( result ) , 0 ) == 545591 )
stopifnot( round( 100 * SE( result ) / coef( result ) , 2 ) == 0.97 )
library(srvyr)
recs_srvyr_design <- as_survey( recs_design )
recs_srvyr_design %>%
	summarize( mean = survey_mean( totsqft_en ) )

recs_srvyr_design %>%
	group_by( main_heating_fuel ) %>%
	summarize( mean = survey_mean( totsqft_en ) )
