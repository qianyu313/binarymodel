##
## Function and example to obtain subnational estimates from DHS API
##

#'
#' @param country keys at: https://api.dhsprogram.com/rest/dhs/countries?returnFields=CountryName,DHS_CountryCode&f=html
#' @param survey keys at: https://api.dhsprogram.com/rest/dhs/surveys?returnFields=SurveyId,SurveyYearLabel,SurveyType,CountryName&f=html
#' @param indicator keys at: https://api.dhsprogram.com/rest/dhs/indicators?returnFields=IndicatorId,Label,Definition&f=html
#' @param simplify if TRUE only the value and region index is returned
#' 
get_api_table <- function(coutry, survey, indicator, simplify = TRUE){
	call <- paste0("https://api.dhsprogram.com/rest/dhs/data?breakdown=subnational&indicatorIds=", 
			indicator, 
			"&countryIds=",
			country,
			"&surveyIds=", 
			survey, 
			"&lang=en&f=csv")
	tab <- read.csv(call)
	if(simplify){
		tab <- tab[, c("Value", "CharacteristicLabel", "ByVariableLabel")]
	}
	return(tab)
}

country <- "ZM"
survey <- "ZM2018DHS"

# Percentage of women who had a live birth in the five (or three) years preceding the survey who had 4+ antenatal care visits
tab <- get_api_table(coutry, survey, "RH_ANCN_W_N4P")
