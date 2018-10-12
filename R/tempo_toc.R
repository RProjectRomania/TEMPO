# Get TOC for Tempo Online
# function gets a parameter last_update implicitly false 
# if fulldescription == TRUE then starts collecting dates for last updates 
#' @title tempo_toc
#' 
#' @description Get TOC for Tempo Online
#' function gets a parameter full_description implicitly false 
#' if fulldescription == TRUE then starts collecting dates for last updates 
#' 
#' @param fulldescription - implicitly set as false  
#' if fulldescription == TRUE then starts collecting dates for last updates
#' 
#' @return Returns a dataframe object. 
#' 
#' 
#' @details This functions sends one or multiple GET requests and parses the content into 
#' a dataframe
#' 
#' @examples tempo_toc(full_description = FALSE)
#' @export


tempo_toc <- function(full_description = FALSE){
  get_response <- GET("http://statistici.insse.ro:8077/tempo-ins/matrix/matrices")
  content_response <- content(get_response)
  tempo_toc <- toJSON(content_response)
  tempo_toc <- fromJSON(tempo_toc, flatten = TRUE)
  tempo_toc <- lapply(tempo_toc, function(x){unlist(x)})
  tempo_toc <- data.frame(tempo_toc, stringsAsFactors = FALSE)
  tempo_toc <- tempo_toc[, c(1, 2)]
  colnames(tempo_toc) <- c("Denumire", "Cod")
  if(full_description == TRUE){
    message("This will take a while. Grab some coffee!")
    for(i in 1:length(tempo_toc[,2])){
      lu_response <- GET(paste0("http://statistici.insse.ro:8077/tempo-ins/matrix/", tempo_toc[i,2]))
      lu_content <- content(lu_response)
      tempo_toc$Domeniu_statistic[i] <- lu_content$ancestors[[2]]$name
      tempo_toc$Sub_domeniu_statistic[i] <-  lu_content$ancestors[[3]]$name
      tempo_toc$Ancheta[i] <- lu_content$ancestors[[4]]$name 
      tempo_toc$Ultima_actualizare[i] <- lu_content$ultimaActualizare
    }

    

  } 
  assign("TOC_TEMPO", tempo_toc, envir = .GlobalEnv)

}