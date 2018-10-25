#' @title Clean a table downloaded from TEMPO Online database
#' 
#' @description tempo_clean cleans a table downloaded from 
#' TEMPO Online database
#' 
#' @param matrix - the R dataframe object to be cleaned, representing 
#' the table/matrix downloaded from TEMPO Online database 
#' 
#' @param matrix_code - string containing the code for 
#' the table/matrix in TEMPO Online database. See more on
#' how to obtain a matrix code from \code{\link{tempo_codes}}
#'
#' @return Returns a R dataframe object. 

#' @details This function removes redundant columns or redundant information from columns
#' 
#' @examples 
#' tempo_clean(SOM101D, "SOM101D")
#' 
#' @export
tempo_clean <- function(matrix, matrix_code){
  if (nargs() != 2) {
    print("Wrong number of arguments!")
    return (NULL)
  }
  
  tmp <- deparse(substitute(matrix))
  if (!exists(tmp)) {
    cat("Matrix not found:", tmp, "\n")
    return (NULL)
  }
  
  if (is.null(matrix) | !is.data.frame(matrix)) {
    type <- class(matrix)
    cat("Invalid type (",type, ") of argument!\n", sep = "")
    return (NULL)
  }
  
  if (is.null(matrix_code) | !is.character(matrix_code)) {
    type <- class(matrix_code)
    cat("Invalid type (",type, ") of argument!\n", sep = "")
    return (NULL)
  }
  
  column_names <- names(matrix)
  pos_ani <- grep("([aA]ni|[yY]ears)", column_names)
  pos_um <- grep("(UM:|MU:)", column_names)
  
  if (length(pos_ani) > 0) {
    pos_ani <- pos_ani[1]
    matrix <- as.data.frame(matrix)
    n <- nrow(matrix)
    matrix[1:n,pos_ani] <- gsub("([aA]nul|[yY]ear) *", "", matrix[1:n,pos_ani])
    matrix[[pos_ani]] <- as.integer(matrix[[pos_ani]])
  }
  
  if (length(pos_um) > 0) {
    pos_um <- pos_um[1]
    um <- gsub("(UM:|MU:) *", "", column_names[pos_um])
    matrix <- matrix[,-c(pos_um)]
    names(matrix)[names(matrix) == "Valoare" | names(matrix) == "Value"] <- paste0(matrix_code, "/", um)
  }
  
  # Here the function overwrites the object in the Global Environment
  # which has the same name as the name given as parameter, i.e. matrix_code
  assign(matrix_code, matrix, envir = .GlobalEnv)
}

