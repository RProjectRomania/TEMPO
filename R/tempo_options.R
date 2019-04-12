#' @title tempo_options
#' @description Get options for a Tempo Online marix
#' 
#' @param codes - a list containing one or more strings containing the code for
#' the table/matrix in TEMPO Online database. See more on
#' how to obtain a matrix code from \code{\link{tempo_toc}}
#'
#' 
#' @return Returns a list containing named vectors. 
#' 
#' 
#' @details This functions sends one GET request and parses the content into 
#' a list of named vector containing options for subsetting a TEMPO Online matrix.
#' 
#' @examples 
#' tempo_options("AGR111A")
#' 
#' @import curl
#' @import jsonlite
#' @export

tempo_options <- function(code = NULL){
  
  
  matrices_list <- tempo_toc()
  # partial matching to suggest valid matrix code and language
  code      <- toupper(code)
  code      <- match.arg(code, matrices_list$code)
  
  tempo_matrix <- tempo_content(paste0(tempoEnv$matrix, code, "?lang=", tempoEnv$language))
  
  matrix_codes <- list()
  matrix_codes$language   <- tempoEnv$language
  matrix_codes$matCode    <- code
  matrix_codes$matMaxDim  <- tempo_matrix$details$matMaxDim
  matrix_codes$matUMSpec  <- tempo_matrix$details$matUMSpec
  matrix_codes$matRegJ    <- tempo_matrix$details$matRegJ
  
  names(tempo_matrix$dimensionsMap$options) <- tempo_matrix$dimensionsMap$label
  tempo_matrix <- tempo_matrix$dimensionsMap$options
  
  tempo_matrix <- lapply(tempo_matrix, "[", -3)
  tempo_matrix <- lapply(tempo_matrix, function(x) x[,colSums(is.na(x)) < nrow(x)])
 
  for(i in order(seq_along(tempo_matrix), decreasing = TRUE)){

# If number of columns is 3 then merge the two dataframes
# After merge, replace the columns in the initial dataframes
# by addressing the directly the column indices
    
    if(length(tempo_matrix[[i]]) == 3){
      merged_tmp <- merge(tempo_matrix[[i]], tempo_matrix[[i-1]], by.x = 3, by.y = 2)
      tempo_matrix[[i-1]] <- merged_tmp[,-c(2,3)]
      tempo_matrix[[i-1]][,c(1,2)] <- tempo_matrix[[i-1]][,c(2,1)]
      tempo_matrix[[i]][,c(1,2)] <- merged_tmp[,c(2,3)]
      tempo_matrix[[i]][,3] <- NULL    
    }
  }
  tempo_matrix <- lapply(tempo_matrix, function(x){
    y <- x[,2]
    names(y) <- x[,1]
    return(y)
    })
  tempo_matrix_opts   <- list()
  tempo_matrix_opts$opts  <- tempo_matrix
  tempo_matrix_opts$codes <- matrix_codes
  
  return(tempo_matrix_opts)
  
}

