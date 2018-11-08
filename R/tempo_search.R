# TODO 
# return matrices
# add roxygen comments

tempo_search <- function(keyword = c(), language = c("ro", "en")){
  url_get_domain <- "http://statistici.insse.ro:8077/tempo-ins/context/" 
  
  if (language[1] == "en") {
    url_get_domain <- "http://statistici.insse.ro:8077/tempo-ins/context/?lang=en/" 
  }
  
  results <- curl_fetch_memory(url_get_domain)
  parsed_url <- readBin(results$content, what = "character")
  parsed_list <- fromJSON(parsed_url, flatten = TRUE, simplifyDataFrame = TRUE)
  
  context.name.mod <- sub(".+? ", "", parsed_list$context.name)
  matched <- match(tolower(context.name.mod), tolower(keyword))
  pos <- which(!is.na(matched))
  df <- parsed_list[pos,]
  return(df)
}

# Examples
tempo_search(c("education"), language = "en")
tempo_search(c("educatie"), language = "ro")
tempo_search(c("educatie", "industrie"))

