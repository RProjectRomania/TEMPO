tempo_download <- function(payload_list = NULL){
  
  url_csv <- tempoEnv$dataCsv
  
#  my_pool <- new_pool()
  
  my_data <- data.frame()
  
  fn <- NULL
#  succes2 <- function(req){
    
#    fn <- parse_headers(req$headers)[3]
    
#    fn <- strsplit(fn, split = "_")
    
#    fn <- fn[[1]][2]
#    
 #   res <- readBin(req$content, "text")
#    
#    res <- utils::read.csv2(text = res, header = TRUE ,sep = ",",stringsAsFactors = FALSE)
    
#    my_data <- rbind(my_data, res)
    
#  }
  
#  failure <- function(e){
#    print("Something happend", e)
 # }
  
  for (i  in payload_list) {
    
    payload <- toJSON(i, pretty = TRUE, auto_unbox = TRUE, null = "null") 
    
    h2 <- new_handle()
    
    handle_setheaders(h2,"Content-Type" = "application/json", encode = "json")
    
    handle_setopt(h2,  postfields = payload)
    
    req <- curl_fetch_memory(url_csv, handle =  h2)
    
    h2 <- NULL
    
    if (is.null(fn)) {
      
    fn <- parse_headers(req$headers)[3]
    
    fn <- strsplit(fn, split = "_")
    
    fn <- fn[[1]][2]
    
    }
    
    res <- readBin(req$content, "text")
    
    res <- utils::read.csv2(text = res, header = TRUE ,sep = ",",stringsAsFactors = FALSE)
    
    my_data <- rbind(my_data, res)
    
    
  }
#  multi_run(pool = my_pool)
  write.csv(my_data, paste0(fn), sep = ",", row.names = FALSE)
}


