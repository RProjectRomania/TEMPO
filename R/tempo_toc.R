#' @title tempo_toc
#' 
#' @description Get TOC for Tempo Online
#' function gets a parameter full_description implicitly false 
#' if fullDescription == TRUE then starts collecting dates for last updates 
#' 
#' @param full_description - implicitly set as false  
#' if fullDescription == TRUE then starts collecting dates for last updates
#' 
#' @return Returns a dataframe object. 
#' 
#' 
#' @details This functions sends one or multiple GET requests and parses the content into 
#' a dataframe
#' 
#' @examples 
#' tempo_toc(fullDescription = FALSE)
#' 
#' @import curl
#' @import jsonlite
#' @import utils 
#' @export

tempo_toc <- function(fullDescription = FALSE){
  
  urlToc    <- paste0(tempoEnv$matrices, tempoEnv$language)
  tempoToc  <- tempo_content(targetUrl = urlToc)[,c(1,2)] 
  
  if(fullDescription == TRUE){
    
    for(i in seq_along(tempoToc[,2])){
      targetUrl  <- paste0(tempoEnv$matrix, tempoToc[i, 2], "?lang=", tempoEnv$language)
      response   <- tempo_content(targetUrl = targetUrl)
      
      tempoToc$statisticalDomain[i]     <- response$ancestors$name[2]
      tempoToc$statisticalSubDomain[i]  <- response$ancestors$name[3]
      tempoToc$surveyName[i]            <- response$ancestors$name[4]
      tempoToc$lastUpdate[i]            <- response$ultimaActualizare 
    }
  }
  return(tempoToc) 
  
}