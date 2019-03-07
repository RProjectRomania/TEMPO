tempo_download <- function(payload_list = NULL){
  
  url_csv <- tempoEnv$dataCsv
  
  my_pool <- new_pool()
  
  
  succes2 <- function(req){
    
    fn <- parse_headers(req$headers)
    
    fn <- fn[3]
    
    fn2 <- strsplit(fn, split = "_")
    
    fn2 <- fn2[[1]][2]
    
    con <- file(fn2, open = "wb")
    
    res <- readBin(req$content, "text")
    
    res <- read.csv2(text = res, sep = ",", stringsAsFactors = FALSE)
    
    write.csv(res, con, sep = "," ,append = TRUE)
    
    close(con)
  }
  
  
  for(i  in seq_along(payload_list)){
    
    payload <- toJSON(payload_list[[i]], pretty = TRUE, auto_unbox = TRUE, null = "null") 
    
    h2 <- new_handle()
    
    handle_setheaders(h2,"Content-Type" = "application/json", encode = "json")
    
    handle_setopt(h2,  postfields = payload)
    
    curl_fetch_multi(url_csv, done = succes2,  handle =  h2, pool = my_pool)
    
    
  }
  
  
  multi_run(pool = my_pool)   
}


