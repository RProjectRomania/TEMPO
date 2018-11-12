#' @title Get TOC for Tempo Online database filtered by keyword
#'
#' @description tempo_search
#' The function searches for datasets codes in TOC based on specified keywords, 
#' to be used with tempo_bulk function. 
#' 
#' @param keyword - a character vector with multiple keywords to be matched 
#'
#' @param language - a string to set the language for the downloaded
#' tables. Options: "ro" - for Romanian and "en" - for English. If no parameter
#' is given, implicitly downloads tables in Romanian
#' 
#' @return Returns a dataframe object. 
#' 
#' @details Downloads Table Of Contents (TOC) for Tempo Online database  
#' and returns a TOC subset based on specified keywords.
#' E.g. all TOC entries related to education.
#' 
#' @examples \dontrun{
#' 
#' tempo_search(c("education", "industry"), language = "en")
#' 
#' }
#' @import curl
#' @import jsonlite
#' @export


#TODO
#add the language parameter for tempo_toc function
tempo_search <- function(keyword = c(), language = c("ro", "en")){
  toc <- tempo_toc(full_description = TRUE)
  vchar <- apply(toc, 1 ,paste, collapse = " ")
  matched <- sapply(tolower(keyword), grep, tolower(vchar))
  index <- sort(unlist(matched))
  
  return(toc[index,])
}

