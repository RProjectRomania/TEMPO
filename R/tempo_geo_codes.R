#' Dataset contains TEMPO Online geocodes.

#'

#' This dataset provides the codes from the NUTS classification system, the full names and 
#' the codes used for retrieving data from TEMPO Online database service, for all 
#' macroregions, regions and counties from Romania. For macroregions and regions the package
#' provides dataframes with names in English. 

#'

#' @format A \linkS4class{list} with 5 \linkS4class{data.frame}.

#' @format Each \linkS4class{data.frame} has the following variables:

#' \describe{

#'   \item{std_names}{Codes from the NUTS classification system}

#'   \item{tempo_names}{Names as they appear in TEMPO Online database}

#'   \item{tempo_codes}{The internal codes at storing and accesing datasets by geocode
#'   from the TEMPO Online database service.}

#'   \item{tempo_names_nuts3_ab}{Present only in the codes_county dataframe. Provides
#'   the abreviation codes used at national level for county identification.}

#' }

"tempo_geo_codes"