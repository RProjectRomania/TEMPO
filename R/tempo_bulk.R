#' @title Download one or multiple tables from TEMPO Online database
#'
#' @description tempo_bulk
#' TEMPO Online database bulk download method. The function checks the
#' directory supplied as parameter or the working directory if has named
#' files as TEMPO Online matrices. If those files are more recent than
#' the last date upon which the TEMPO matrices have been updated,
#' the function skips them at download.
#'
#' @param codes - a list containing one or more strings containing the code for
#' the table/matrix in TEMPO Online database. See more on
#' how to obtain a matrix code from \code{\link{tempo_toc}}
#'
#' @param language - a string to set the language for the downloaded
#' tables. Options: "ro" - for Romanian and "en" - for English. If no parameter
#' is given, implicitly downloads tables in Romanian
#' 
#' @param directory - a valid path where the files will be downloaded. The implicit value 
#' is NULL, i.e. current working directory.
#' 
#' @details 
#' 
#' @examples \dontrun{
#' # Download a single table and write on disk
#' tempo_bulk("SOM101D", "ro")
#' 
#' # Downloads the whole TEMPO db and write each table on disk.
#' # Can take a long time.
#' # tempo_bulk(TOC_TEMPO$code, "ro") 
#' }
#' @import curl
#' @import jsonlite
#' @export


tempo_bulk <- function(codes = NULL, directory =  NULL, check_date = FALSE){
  
  stopifnot(!is.null(codes)) 
  
  
  if(is.null(directory)){
    
    message("No directory has been supplied. \nData will be downloaded in the curent working directory")
    directory <- paste0(getwd(), "/")
    
  } else {
    
    if(!dir.exists(directory)) {
    directory <- tryCatch(expr = dir.create(directory, showWarnings = TRUE),
               error = function(e){
                 message("Something happend.")
                 message("Here is the original error message:")
                 message(e)
               },
               finally = {
                directory <-  gsub("\\", "/", directory)
               })
    }
  setwd(directory)
  }
  # Check date for files on disk
  if(check_date){
    
  tempo_files <- list.files(directory, pattern = "[[:alnum:]].csv")
  
  message("File on disk | Date on disk (modified) | Date on TEMPO ")
  
    for(i in tempo_files){
    j <- gsub("\\.csv", "",i) 
    time_on_tempo <- get_last_date(j)
    time_on_tempo <- as.POSIXct.Date(as.Date(time_on_tempo, format = "%d-%m-%Y"))
    time_on_disk <- file.info(i)[4][[1]]
    message(paste0(i, "  | " , time_on_disk, "   | ", time_on_tempo))
      if( time_on_tempo  < time_on_disk){
        codes <- codes[codes != j]
      }
    }
  
    if(length(codes) == 0){
      stop("Requested files are up to date.")
    }
  } else {
  
  message("The following matrices will be downloaded: ")
    print(codes)
  }

    lapply(codes, function(x){
      tempoData <- tempo_options(x)
      tempoData <- tempo_payloads(tempoData)
      tempo_download(tempoData)
    })

}

get_last_date <- function(i)
{ 
  lastDate <- tempo_toc(fullDescription = TRUE)
  return(lastDate[[6]])
}
