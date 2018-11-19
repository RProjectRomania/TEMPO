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
# particular cases Bistrita-Nasaud(B-), Caras-Severin(C-)

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
  codes$geoCodes <- toupper(abbreviate(codes$name, minlength = 2))
  
  return(codes)
}

#judete + Bucuresti
tempo_geo_nuts3 <- function() {
  
}

#regiuni (8)
tempo_geo_nuts2 <- function(language = c("ro", "en")) {
  
}

#macroregiuni (4)
tempo_geo_nuts1 <- function(language = "ro") {
  
  if(language != "ro" && language != "en")
    stop("unsupported language")
  
  
  std_names <- c("RO1", "RO2", "RO3", "RO4")
  if(language == "ro")
    tempo_names <- c("MACROREGIUNEA UNU", "MACROREGIUNEA DOI", "MACROREGIUNEA TREI", "MACROREGIUNEA PATRU")
  else
    tempo_names <- c("MACROREGION 1", "MACROREGION 2", "MACROREGION 3", "MACROREGION 4")
  
  tempo_codes <- c(21295, 20676, 20677, 20678)
  codes <- data.frame(std_names = std_names, tempo_names = tempo_names, tempo_codes = tempo_codes)
  return (codes)
  
}






