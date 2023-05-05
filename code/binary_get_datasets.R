rm(list = ls())
# ENTER COUNTRY OF INTEREST -----------------------------------------------
# Please capitalize the first letter of the country name and replace " " in the country name to "_" if there is.
country <-'Zambia'

# Load libraries and info ----------------------------------------------------------
# options(gsubfn.engine = "R")
# library(rgdal)
# options(warn=0)
# library(spdep)
# library(SUMMER)
# library(geosphere)
# library(stringr)
library(dplyr)
#devtools::install_github("ropensci/rdhs")
library(rdhs)

# retrieve directories
 home.dir<-"/Users/qianyu/Dropbox/binary_model"
 data.dir <- paste0(home.dir,'/data/',country) # set the directory to store the data
# res.dir <- paste0(home.dir,'/Results/',country) # set the directory to store the results (e.g. fitted R objects, figures, tables in .csv etc.)
 info.name <- paste0(country, "_general_info.Rdata")
 load(file = paste0(home.dir,'/data/Info/',info.name, sep='')) # load the country info

# set API to get DHS data -- you will need to change this to your information!
set_rdhs_config(email = "qdong14@ucsc.edu",
                project = "Small Area Estimaiton Using DHS Data")

#update_rdhs_config(email = "amcgov@uw.edu", password = T,
#               project = "Spatial Modeling for Subnational Administrative Level 2 Small-Area Estimation - Under 5 Mortality Rate")

# Find DHS surveys ----------------------------------------------------------

#get country ID
countryId <- dhs_countries()[dhs_countries()$ISO3_CountryCode==toupper(gadm.abbrev),]

potential_surveys <- dhs_datasets(countryIds = countryId$DHS_CountryCode, surveyYearStart = 2018) %>% 
   dplyr::filter( FileFormat=='Stata dataset (.dta)')#(FileType == 'Household Member Recode' &

surveys <- potential_surveys %>% filter(FileType %in% c("Household Member Recode"))
survey_year=2018
data.paths.tmp <- get_datasets(surveys[surveys$SurveyYear==survey_year,]$FileName, clear_cache = T)
raw.dat.tmp <- readRDS(paste0(data.paths.tmp))
filename<-paste0(strsplit(surveys[surveys$SurveyYear==survey_year,]$FileName, "\\.")[[1]][1][1],".rds")
save(raw.dat.tmp, file =paste0(data.dir,'/DHS_data/',filename))

