#' @title Get language 
#'
#' @description 
#' Gets the language used for downloading a table from TEMPO ONLINE.
#'
#' @param language - a string to set the language for the downloaded
#' tables. Options: "ro" - for Romanian and "en" - for English. If no parameter
#' is given, implicitly downloads tables in Romanian
#' 
#' @export


get_language <- function(){
  tempoEnv$language
}