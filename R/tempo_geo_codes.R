#' @title Tempo Geo Codes
#'
#' @description tempo_geo_codes 
#' The function returns a dataframe containing names
#' and codes for macroregions, regions and counties.
#' 
#' @param language - a string to set the language for the downloaded
#' tables. Options: "ro" - for Romanian and "en" - for English. If no parameter
#' is given, implicitly downloads tables in Romanian.
#' 
#' @return Returns a dataframe object. 
#' 
#' @details 
#' 
#' @examples \dontrun{
#' tempo_geo_codes(language = "en")
#' }
#' 
#' @import curl
#' @import jsonlite
#' @export

# TODO 
# abbreviations for the names of macroregions, regions and counties. 

tempo_geo_codes <- function(language = c("ro", "en")) {
  lang <- ""
  if(language[1] == "en") {
    lang <- "/?lang=en/"
  }
  code_matrix <- "POP107A"
  url <- paste0("http://statistici.insse.ro:8077/tempo-ins/matrix/", code_matrix, lang)
  response <- curl_fetch_memory(url)
  responsetext <- readBin(response$content, what = "text")
  tempo_toc <- fromJSON(responsetext, flatten = TRUE)
  codes <- tempo_toc$dimensionsMap$options[[4]][1:2]
  names(codes) <- c("name", "code")
  
  return(codes)
}





