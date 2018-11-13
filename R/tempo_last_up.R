#' @title TEMPO Last Updates 
#'
#' @description 
#' Returns a dataframe containing the last updates operated on TEMPO tables.
#' 
#' @param code  String representing the code for
#' the table/matrix in TEMPO Online database. See more on
#' how to obtain a matrix code from \code{\link{tempo_toc}}.
#' 
#' @param simplified  Logical indicating whether results 
#' should be simplified. The default value is FALSE.
#' 
#' @param language  String to set the language for the downloaded
#' tables. Options: "ro" - for Romanian and "en" - for English. If no parameter
#' is given, implicitly downloads tables in Romanian.
#' 
#' @return Returns a dataframe object. 
#' 
#' @details 
#' 
#' @examples \dontrun{
#' tempo_last_up(code = "POP108B", language = "en")
#' }
#' 
#' @import curl
#' @import jsonlite
#' @import rvest
#' @export

tempo_last_up <- function (code = NULL, simplified = FALSE, language = c("ro", "en")) {
  if (language[1] == "ro") {
    update <- read_html("http://statistici.insse.ro:8077/tempo-ins/news/")
  } else if (language[1] == "en") {
    update <- read_html("http://statistici.insse.ro:8077/tempo-ins/news/?lang=en")
  } else{
    cat("Invalid argument for language: ", language[1], "\nArguments accepted: \"ro\" or \"en\".\n")
    return (NULL)
  }
  
  update <- update %>% html_node("table") %>% html_table(header = TRUE, fill = TRUE)
  
  if (!is.null(code)) {
    index <- which(!is.na(match(update[,4], code)))
    update <- update[index,]
  }
  
  if (simplified == TRUE) {
    update <- update[,c(2,4,5)]
  }
  
  return(update)
}