#' @title Date Conversion from TEMPO Time Format 
#' 
#' @description A function to convert time values from a table
#' downloaded from TEMPO Online database to objects of class 
#' \code{\link{Date}} representing calendar dates. 
#' 
#' @param matrix - the R dataframe object with time information
#' in TEMPO time format, representing the table/matrix 
#' downloaded from TEMPO Online database.
#' 
#' @param matrix_code - string containing the code for 
#' the table/matrix in TEMPO Online database. See more on
#' how to obtain a matrix code from \code{\link{tempo_codes}}
#'
#' @return Returns a R dataframe object. 

#' @details This function converts a column of class character
#' representing time periods (month, quarter) to a column 
#' of class \code{\link{Date}} representing calendar dates. 
#' 
#' @examples 
#' tempotime2date(AMG157G, "AMG157G")
#' 
#' @export

tempotime2date <- function(matrix, matrix_code){
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
  hasDate <- grep("([lL]uni|[mM]onths|[tT]rimestre|[qQ]uarters)", column_names)
  
  if (length(hasDate) == 0) {
    cat("No column of TEMPO time format!\n")
    return (NULL)
  }
  
  pos <- hasDate[1]
  keyWord <- tolower(column_names[pos])
  date <- tolower(matrix[[pos]])
  date <- gsub("[[:space:]]", "", date)
  years <- gsub("[[:alpha:]]", "", date)
  years <- substring(years, 3, 4)
  months <- gsub("[[:digit:]]+", "", date)

  if (keyWord == "trimestre" || keyWord == "quarters") {
    months <- gsub("(trimestrul|quarter)", "", months)
    map <- c("i" = "01", "ii" = "04", "iii" = "07", "iv" = "10")
    months <- map[months]
  } 
  
  if (keyWord == "luni") {
    months <- gsub("luna", "", months)
    map <- c("ianuarie" = "01","februarie" = "02", "martie" = "03",
             "aprilie" = "04", "mai" = "05", "iunie" = "06", 
             "iulie" = "07", "august" = "08", "septembrie" = "09", 
             "octombrie" = "10", "noiembrie" = "11", "decembrie" = "12")
    months <- map[months]
  }
  
  if (keyWord == "months") {
    map <- c("january" = "01","february" = "02", "march" = "03",
             "april" = "04", "may" = "05", "june" = "06", 
             "july" = "07", "august" = "08", "september" = "09", 
             "october" = "10", "november" = "11", "december" = "12")
    months <- map[months]
  }
  
  if (sum(is.na(months)) > 0) {
    cat("No column of TEMPO time format!\n")
    return (NULL)
  }
  
  date <- paste("01", months, years, sep = "-")
  date <- as.Date(date, format = "%d-%m-%y")
  matrix[[pos]] <- date
  
  return(matrix)
}
