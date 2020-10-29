

########
##
## Calculate scaled infection fatality rates by age for selected countries of interest (COI)
## based on IFR estimates for reference country (RC) China, as provided by Verity et al. (2020) 
## 
## The methodology for scaling infection fatality rates 
## between a reference country and countries of interest 
## is based on remaining life expectancy (ex)
## and adopted from the project:
## "A demographic scaling model for estimating total numbers of COVID-19 infections"
## Christina Bohk-Ewald, Christian Dudel, Mikko Myrskylä
## This article has been accepted for publication in the International Journal of Epidemiology, published by Oxford University Press, September 2020.
##
## Github repository: https://github.com/christina-bohk-ewald/demographic-scaling-model
##
########

#
## 0. Set working directory and load R-packages
#

setwd( ".")

require(openxlsx)

#
## 1. Load infection fatality rates by 10-year age groups for Hubei province, China, 
## as published by Verity et al. (2020)
## 
## and life tables from UNWPP (2019): WPP2019_MORT_F17_1_ABRIDGED_LIFE_TABLE_BOTH_SEXES.xlsx 
## available at: https://population.un.org/wpp/
#

ifr_by_age_china_verity <- read.table("infection-fatality-rates-by-age-china-Verity.txt",header=FALSE, stringsAsFactors = FALSE)

lt_1950_2020 <- read.xlsx("WPP2019_MORT_F17_1_ABRIDGED_LIFE_TABLE_BOTH_SEXES.xlsx",sheet = 1,startRow = 17)

#
## 2. Basic functions 
#

## 2a: to interpolate IFR estimates of Verity et al. into single years of age: 

to_ungroup <- function(to_ungroup,nr_grouped_years){
	
	seq_ungrouped_years <- seq(0,length(to_ungroup)*nr_grouped_years)	
	cumsum_to_ungroup <- cumsum(c(sum(to_ungroup),to_ungroup))
	grouped_time_points <- c(0,(1:length(to_ungroup))*nr_grouped_years)
	
	applied_smooth_spline <- smooth.spline(x=grouped_time_points,y=cumsum_to_ungroup)
	predict_cumsum_ungroup <- predict(applied_smooth_spline,x=seq_ungrouped_years)$y
	ungrouped <- diff(predict_cumsum_ungroup)
	return(ungrouped)
}

## 2b: to ungroup remaining life expectancy:

get_ungrouped_ex_2015_2020 <- function(country_name, lt_1950_2020){
	current_period_data <- lt_1950_2020[which(lt_1950_2020[,8]=="2015-2020"),]
	current_period_data <- current_period_data[which(current_period_data[,3]==country_name),]  
	current_ex_data <- as.numeric(current_period_data[,19])
	smooth_current_ex_data <- smooth.spline(x=c(0,1,seq(5,100,5)),y=current_ex_data)
	new_x <- c(seq(0,0.99,0.01),seq(1,4.99,0.01),seq(5,100,0.01))
	predict_smooth_current_ex_data <- predict(smooth_current_ex_data,new_x,len=new_x)
	return(predict_smooth_current_ex_data)
}

## 2c: to scale IFRs from RC (China, Verity et al.) onto COI: 

map_ifr_betw_ref_and_one_coi_thanatAge <- function(coi,low_ex,lt_1950_2020,ungrouped_ifr_by_single_age_china_sp){

	ifr_coi_mapped_rc_china_based_on_thanat_x <- matrix(NA,nr=1,nc=length(ungrouped_ifr_by_single_age_china_sp))
	
	rownames(ifr_coi_mapped_rc_china_based_on_thanat_x) <- coi

	current_pop_insert <- coi

	for(chronAge in 1:90){
		current_ref_y <- get_ungrouped_ex_2015_2020(country_name="China",
		lt_1950_2020)$y
			
		current_ref_x <- get_ungrouped_ex_2015_2020(country_name="China", 
		lt_1950_2020)$x

		current_coi_y <- get_ungrouped_ex_2015_2020(country_name=current_pop_insert, 
		lt_1950_2020)$y
			
		current_coi_x <- get_ungrouped_ex_2015_2020(country_name=current_pop_insert, 
		lt_1950_2020)$x
				 	
		current_y_ref_of_chronAge <- current_ref_y[which(current_ref_x==(chronAge-1))]
		equal_y <- which(round(current_coi_y,3)==round(current_y_ref_of_chronAge,3))[1]
			
		if(low_ex==FALSE){

			if(is.na(equal_y)){
				n <- 0
				while(is.na(equal_y)){
					equal_y <- which(round(current_coi_y,3)==(round(current_y_ref_of_chronAge,3)-n))[1]
					n <- n+0.001 
				} ## while	
			} ## if
		} ## if

		if(low_ex==TRUE){

			if(is.na(equal_y)){
				n <- 0
				while(is.na(equal_y)){
					equal_y <- which(round(current_coi_y,1)==(round(current_y_ref_of_chronAge,1)-n))[1]
					n <- n+0.2 
				} ## while	
			} ## if
		} ## if

		equivalent_x_coi <- current_coi_x[equal_y]
		
		if((round(equivalent_x_coi,0)+1)>length(ungrouped_ifr_by_single_age_china_sp)){
			equivalent_x_coi <- 89
		}

		ifr_coi_mapped_rc_china_based_on_thanat_x[1,equivalent_x_coi] <- 
		ungrouped_ifr_by_single_age_china_sp[chronAge]

	} ## for chronAge

	return(ifr_coi_mapped_rc_china_based_on_thanat_x)

} ## function

## 2d: to aggregate scaled infection fatality rates into 10-year age groups:  

#
## 3. Select countries of interest 
#

selected_coi <- c("Japan","Italy","Germany","Spain","France","United Kingdom",
			"US","Brazil","Colombia","Mexico","Morocco","India",
			"South Africa","Ethiopia","Nigeria")

low_ex <- c(rep(FALSE,12),rep(TRUE,3))

#
## 4. Build array to save output, i.e., scaled infection fatality rates
## by country (row), age (col), and quantile (matrix) 
#

row.names <- c("Verity",selected_coi)
col.names <- 0:89
matrix.names <- c("low95","mode","up95")
scaled_IFR_COIs <- array(NA,dim=c(length(selected_coi)+1,90,3),dimnames = list(row.names,col.names,matrix.names))

scaled_IFR_COIs["Verity",,"up95"] <- ungrouped_up95_ifr_by_single_age_china_sp
scaled_IFR_COIs["Verity",,"mode"] <- ungrouped_mode_ifr_by_single_age_china_sp
scaled_IFR_COIs["Verity",,"low95"] <- ungrouped_low95_ifr_by_single_age_china_sp

#
## 5. ungroup China's IFR:
#

ungrouped_mode_ifr_by_single_age_china_sp <- to_ungroup(to_ungroup=
				ifr_by_age_china_verity[,2],nr_grouped_years=10)

ungrouped_low95_ifr_by_single_age_china_sp <- to_ungroup(to_ungroup=
				ifr_by_age_china_verity[,3],nr_grouped_years=10)

ungrouped_up95_ifr_by_single_age_china_sp <- to_ungroup(to_ungroup=
				ifr_by_age_china_verity[,4],nr_grouped_years=10)

#
## 6. Map IFRs from RC onto ONE COI and fill in the few NAs after scaling:
#
 
for(coi in selected_coi){

	print(coi)
	print(low_ex[(which(selected_coi==coi))])

	coi_long <- coi
	
	if(coi=="US"){
		coi_long <- "United States of America" 	
	}
	if(coi=="Hubei"){
		coi_long <- "China"
	}
	if(coi=="Iran"){
		coi_long <- "Iran (Islamic Republic of)"
	}

	mapped_mode_ifr_thanatAge <- map_ifr_betw_ref_and_one_coi_thanatAge(coi=coi_long,
		low_ex=low_ex[(which(selected_coi==coi))],
		lt_1950_2020=lt_1950_2020,
		ungrouped_ifr_by_single_age_china_sp=ungrouped_mode_ifr_by_single_age_china_sp)

	pos_na <- which(is.na(mapped_mode_ifr_thanatAge[1,]))
		if(length(pos_na)>0){
			for(pos in 1:length(pos_na)){
				if(pos_na[pos] < 6){
					mapped_mode_ifr_thanatAge[1,pos_na[pos]] <- 
						min(mapped_mode_ifr_thanatAge[1,],na.rm=TRUE)
				}
				if(pos_na[pos] >= 6){
					mapped_mode_ifr_thanatAge[1,pos_na[pos]] <- 
						mapped_mode_ifr_thanatAge[1,pos_na[pos]-1]
				}
			} ## for pos
		} ## if

	##
	##

	mapped_low95_ifr_thanatAge <- map_ifr_betw_ref_and_one_coi_thanatAge(coi=coi_long,
		low_ex=low_ex[(which(selected_coi==coi))],
		lt_1950_2020=lt_1950_2020,
		ungrouped_ifr_by_single_age_china_sp=ungrouped_low95_ifr_by_single_age_china_sp)

	pos_na <- which(is.na(mapped_low95_ifr_thanatAge[1,]))
		if(length(pos_na)>0){
			for(pos in 1:length(pos_na)){
				if(pos_na[pos] < 6){
					mapped_low95_ifr_thanatAge[1,pos_na[pos]] <- 
						min(mapped_low95_ifr_thanatAge[1,],na.rm=TRUE)
				}
				if(pos_na[pos] >= 6){
					mapped_low95_ifr_thanatAge[1,pos_na[pos]] <- 
						mapped_low95_ifr_thanatAge[1,pos_na[pos]-1]
				}
			} ## for pos
		} ## if

	##
	##

	mapped_up95_ifr_thanatAge <- map_ifr_betw_ref_and_one_coi_thanatAge(coi=coi_long,
		low_ex=low_ex[(which(selected_coi==coi))],
		lt_1950_2020=lt_1950_2020,
		ungrouped_ifr_by_single_age_china_sp=ungrouped_up95_ifr_by_single_age_china_sp)

	pos_na <- which(is.na(mapped_up95_ifr_thanatAge[1,]))
		if(length(pos_na)>0){
			for(pos in 1:length(pos_na)){
				if(pos_na[pos] < 6){
					mapped_up95_ifr_thanatAge[1,pos_na[pos]] <- 
						min(mapped_up95_ifr_thanatAge[1,],na.rm=TRUE)
				}
				if(pos_na[pos] >= 6){
					mapped_up95_ifr_thanatAge[1,pos_na[pos]] <- 
						mapped_up95_ifr_thanatAge[1,pos_na[pos]-1]
				}
			} ## for pos
		} ## if


	#
	## Write output:
	#

	scaled_IFR_COIs[coi,,"up95"] <- mapped_up95_ifr_thanatAge
	scaled_IFR_COIs[coi,,"mode"] <- mapped_mode_ifr_thanatAge
	scaled_IFR_COIs[coi,,"low95"] <- mapped_low95_ifr_thanatAge

} ## coi

#
## 7. Save output (e.g., into Data folder)
#

scaled_IFR_COIs_c <- scaled_IFR_COIs * 10

## sum(scaled_IFR_COIs_c["Verity",1:10,"mode"]) / 10 * 100 ## in %
## ifr_by_age_china_verity[1,2] ## not in %

dump("scaled_IFR_COIs_c",file="scaled_IFR_COIs_c.R")
source("scaled_IFR_COIs_c.R")




