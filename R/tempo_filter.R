#' @title tempo_filter
#' @description Get names for subsetting a Tempo Online marix
#' 
#' @param codes - a list containing one or more strings containing the code for
#' the table/matrix in TEMPO Online database. See more on
#' how to obtain a matrix code from \code{\link{tempo_toc}}
#'
#' 
#' @return Returns a list containing string vectors. 
#' 
#' @examples 
#' tempo_filter("AGR111A")
#' 
#' @import curl
#' @import jsonlite
#' @export


tempo_filter <- function(codes = NULL){
  filters <- lapply(codes, function(x){
    tempoFilters <- tempo_options(x)
    tempoFilters <- lapply(tempoFilters$opts, function(y){
      names(y)
    })
  })
  names(filters) <- codes
  return(filters)
}