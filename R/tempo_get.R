#' @title Download a table from TEMPO Online database
#' 
#' @description tempo_get downloads a a table from 
#' TEMPO Online database
#' 
#' @param matrix_code - string containing the code for 
#' the table/matrix in TEMPO Online database. See more on
#' how to obtain a matrix code from \code{\link{tempo_toc}}
#' 
#' @param language - a string to set the language for the downloaded 
#' tables. Options: "ro" - for Romanian and "en" - for English. If no parameter
#' is given, implicitly downloads tables in Romanian
#' 
#' @param clean - calls \code{\link{tempo_clean}} function. Implicitly is set as FALSE
#' 
#' @return Returns a dataframe object, which contains a comment attribute with
#' the full name of the table.
#' 
#' @details This function sends GET/POST requests to TEMPO Online JSON service
#' and parses the content of the responses, using  \code{\link{httr}} wrappers
#' for \code{\link{RCurl::httpGET}} and \code{\link{RCurl::httpPOST}}.
#' The content of the \code{\link{httr::POST}} requests is parsed into a dataframe.
#' 
#' @examples tempo_get("SOM101D", "ro")
#' @export


tempo_get <- function(matrix_code = NULL, language = NULL, clean = FALSE){
  if(is.null(matrix_code)){
   stop("matrix_code cannot be NULL.") } 
url_csv <- "http://statistici.insse.ro:8077/tempo-ins/pivot"
   
url_get_matrix <- "http://statistici.insse.ro:8077/tempo-ins/matrix/"
url_matrix <- paste0(url_get_matrix, matrix_code)

matrix_encQuery <- GET(url_matrix)


matrix_content <- content(matrix_encQuery)
matrix_json <- toJSON(matrix_content$dimensionsMap)
matrix_df <- fromJSON(matrix_json, simplifyDataFrame = TRUE)

encQuery <- NULL
matrix_df_loc_list <- NULL

for(i in 1:length(matrix_df$options)){
  matrix_df_loc <- matrix_df$options[[i]]$nomItemId
  matrix_df_loc <- unlist(matrix_df_loc)
  
  if(length(matrix_df_loc) > 400){
    matrix_df_loc_list <- split(matrix_df_loc, ceiling(seq_along(matrix_df_loc)/400)) 
    encQuery <- paste0(encQuery, "replace_me", ":")
   
  } else {
    matrix_df_loc <- paste0(matrix_df_loc, sep = ",", collapse = " ")
    encQuery <- paste0(encQuery, matrix_df_loc, sep = ":")
    encQuery <- gsub(",:", ":", encQuery)
  } 
}
encQuery <- sub("\\:$", "", encQuery)
matrix_df_loc_list <- matrix_df_loc_list 


if(is.null(matrix_df_loc_list)){
  payload_csv <- list(language = language,  encQuery = encQuery, matCode = matrix_code, matMaxDim = matrix_content$details$matMaxDim, matUMSpec = matrix_content$details$matUMSpec, matRegJ = matrix_content$details$matRegJ)
  csv_content <- tempo_post(payload_csv, url_csv)

} else {
  mencQuery <- NULL
  csv_content <- NULL

################################################################
# This can take very long time
# Some form of concurrency is needed
  
  message("Beard growing in progress...")
  for(j in 1:length(matrix_df_loc_list)){
    matrix_df_loc <- paste0(matrix_df_loc_list[[j]], sep = ",", collapse = " ")
    matrix_df_loc <- gsub(",:", ":", matrix_df_loc)
    mencQuery <- sub("replace_me", matrix_df_loc, encQuery)
    mencQuery <- sub(",:", ":", mencQuery)
    mencQuery <- sub("\\:$", "", mencQuery)
    payload_csv <- list(language = language,  encQuery = mencQuery, matCode = matrix_code, matMaxDim = matrix_content$details$matMaxDim, matUMSpec = matrix_content$details$matUMSpec, matRegJ = matrix_content$details$matRegJ)
    csv_content_resp <- tempo_post(payload_csv, url_csv)
    csv_content <- rbind(csv_content, csv_content_resp)
    }
}
####################3#########################################

if(clean == TRUE){
 csv_content <-  tempo_clean(csv_content, matrix_code)
}

comment(csv_content) <- matrix_content$matrixName
assign(matrix_code, csv_content, envir = .GlobalEnv)

}

tempo_post <- function(payload_csv = NULL, url_csv = NULL){
 
  payload_json_csv <- toJSON(payload_csv, pretty =  TRUE, auto_unbox = TRUE, null = "null")
  csv_response <- POST(url = url_csv, body = payload_json_csv, add_headers(.headers = c("Content-Type" = "application/json")), encode = "json")
  csv_content <- content(csv_response, type = "text/csv", encoding = "UTF-8")
  return(csv_content)
}




