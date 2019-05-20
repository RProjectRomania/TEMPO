#' @title Clean a table downloaded from TEMPO Online database
#' 
#' @description tempo_clean cleans a table downloaded from 
#' TEMPO Online database
#' 
#' @param matrix - the R dataframe object to be cleaned, representing 
#' the table/matrix downloaded from TEMPO Online database 
#' 
#' @return Returns a R dataframe object. 

#' @details This function removes redundant columns or redundant information from columns
#' 
#' @examples 
#' tempo_clean(SOM101D)
#' 
#' @export
tempo_clean <- function(matrix){
  if (nargs() != 1) {
    stop("Wrong number of arguments!")
  }
  
  tmp <- deparse(substitute(matrix))
  if (!exists(tmp)) {
    stop("Matrix not found:", tmp, "\n")
  }
  
  if (is.null(matrix) | !is.data.frame(matrix)) {
    type <- class(matrix)
    stop("Invalid type (",type, ") of argument!\n", sep = "")
  }
  
  column_names <- names(matrix)
  #pos_ani <- grep("([aA]ni|[yY]ears)", column_names, fixed = TRUE) #Ani, Years
  pos_ani <- which(names(matrix)=="Ani" | names(matrix) == "Years")
  pos_um <- grep("(UM|MU|masura)", column_names)
  
  if (length(pos_ani) > 0) {
    pos_ani <- pos_ani[1]
    matrix <- as.data.frame(matrix)
    lv <- levels(matrix[[pos_ani]])
    
    if (!is.null(lv)) {
      lv <- gsub("([aA]nul|[yY]ear) *", "", lv)
      lv <- as.integer(lv)
      levels(matrix[[pos_ani]]) <- lv
    } else {
      n <- nrow(matrix)
      matrix[1:n,pos_ani] <- gsub("([aA]nul|[yY]ear) *", "", matrix[1:n,pos_ani])
      matrix[[pos_ani]] <- as.integer(matrix[[pos_ani]])
    }
  }
  
  if (length(pos_um) > 0) {
    pos_um <- pos_um[1]
    um <- matrix[1,c(pos_um)]
    pos_val <- grep(("valoare|value"), tolower(column_names))
    names(matrix)[pos_val] <- paste0(names(matrix)[pos_val], "/", um)
    matrix <- matrix[,-c(pos_um)]
  }
  
  return(matrix)
}

