tempo_payloads <- function(tempo_matrix_opts = NULL){
  
  stopifnot(is.list(tempo_matrix_opts) & length(tempo_matrix_opts) == 2)
  
  cutoff <- 200
  
  z <- lapply(tempo_matrix_opts[[1]], function(y){
    if(length(y) < cutoff){
      m <- paste0(unlist(y), sep = ",", collapse = " ")
    } else{
      f <- unlist(y)
      f <- split(f, ceiling(seq_along(f)/cutoff))
      q <- sapply(f, function(j){
        j <- unique(j)
        j <- paste0(j, sep = ",", collapse = " ")
      })
    }
  })
  
  z <- do.call("paste", c(z, sep = ":"))
  tempo_matrix_opts[[1]] <- lapply(z, function(x){ 
    m <- gsub(",:", ":", x) 
    m <- gsub(",$", "", m)})
  
  encQueryList <- lapply(tempo_matrix_opts[[1]], function(k){
    encQ <- c(language = tempoEnv$language,
              encQuery = k,
              matCode = tempo_matrix_opts$codes$matCode,
              matMaxDim = tempo_matrix_opts$codes$matMaxDim,
              matUMSpec = tempo_matrix_opts$codes$matUMSpec,
              matRegJ = tempo_matrix_opts$codes$matRegJ)
    encQ <- as.list(unlist(encQ))
  })
  
  return(encQueryList)
}

