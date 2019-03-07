#' @title Set language 
#'
#' @description 
#' Sets the language used for downloading a table from TEMPO ONLINE.
#'
#' @param language - a string to set the language for the downloaded
#' tables. Options: "ro" - for Romanian and "en" - for English. If no parameter
#' is given, implicitly downloads tables in Romanian
#' 
#' @export

set_language <- function(language = c("ro", "en")){
  language  <- tolower(language)
  language  <- match.arg(language)
  tempoEnv$language <- language
}


