tempo_content <- function(targetUrl = NULL){
  
  response  <- tempo_online(targetUrl = targetUrl)
  response  <- readBin(response$content, "text") 
  
  responseJson  <- tryCatch(expr ={
    message("Parsing response: ", targetUrl ,"\n")
    jsonlite::fromJSON(response, simplifyDataFrame = TRUE)
  }, 
  error = function(error){
    message("Error occured: ", error)
  },
  finally = {
    message("Response processed.")
  })
  
  return(responseJson)
}