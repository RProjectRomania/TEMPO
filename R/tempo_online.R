tempo_online <- function(targetUrl = NULL, ...){
  
  if(is.null(targetUrl)){
    message("No targetUrl supplied.")
  } else {  
    response <- curl::curl_fetch_memory(url = targetUrl, ...)
    status  <- response$status_code
    if(!identical(status, 200L)){
      stop(status, " Not OK. \n Try again later.")
    }
  }
  return(response)
}
