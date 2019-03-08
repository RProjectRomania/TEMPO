tempo_download <- function(payload_list = NULL){
  
  url_csv <- tempoEnv$dataCsv
  
  my_pool <- new_pool()
  
  my_data <- data.frame()
  fn <- 
  succes2 <- function(req){
    
    fn <- parse_headers(req$headers)[3]
    
    fn <- strsplit(fn, split = "_")
    
    fn <<- fn2[[1]][2]
    
    res <- readBin(req$content, "text")
    
    res <- utils::read.csv2(text = res, header = TRUE ,sep = ",",stringsAsFactors = FALSE)
    
    my_data <<- cbind(my_data, res)
    
  }
  
  failure <- function(e){
    print("Something happend", e)
  }
  
  for(i  in seq_along(payload_list)){
    
    payload <- toJSON(payload_list[[i]], pretty = TRUE, auto_unbox = TRUE, null = "null") 
    
    h2 <- new_handle()
    
    handle_setheaders(h2,"Content-Type" = "application/json", encode = "json")
    
    handle_setopt(h2,  postfields = payload)
    
    curl_fetch_multi(url_csv, done = succes2, fail = failure, handle =  h2, pool = my_pool)
    
    
  }
  multi_run(pool = my_pool)
  write.csv(my_data, paste0(fn, ".csv"), sep = ",", row.names = FALSE)
}


