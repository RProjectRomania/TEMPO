#' @title Visualize Geospatial Data
#' 
#' @description tempo_plot creates a map for visualize Geospatial Data 
#' downloaded from TEMPO Online database
#' 
#' @param matrix - the table/matrix downloaded from TEMPO Online database
#' containing Geospatial Data
#' 
#' @param year - a string containing the year used to plot Geospatial Data
#' 
#' @param area - a string to set the type of area (counties, regions, macroregions)
#' used to group Geospatial Data
#' 
#' @return Returns a ggplot object. 
#' 
#' @examples 
#' tempo_plot(matrix=AGR111A, year="2000", area="counties")
#' 
#' @import dplyr
#' @import ggplot2
#' @export

tempo_geo_reg <- function(matrix, year, area) {
  matrix <- subset_default(matrix)
  
  # year parameter
  column_names <- names(matrix)

  pos_column_year <- which(names(matrix)=="Ani" | names(matrix) == "Year")
  lv <- levels(matrix[,pos_column_year])
  pos_year <- grep(year, lv)
  
  if(length(pos_year) < 1) {
    cat("No data available for the specified time! Data only available for: ", lv, "\n")
    return (NULL)
  }

  matrix <- subset(matrix, matrix[,pos_column_year] == year)
  
  
  # area parameter
  pos_column_jud <- grep("(judete)", tolower(column_names))
  pos_column_reg <- grep("(regiuni)", tolower(column_names)) 
  pos_column_macroreg <- grep("(macroregiuni)", tolower(column_names)) 
  pos_column_val <- grep("(valoare)", tolower(column_names))
  pos_column_loc <- which(names(matrix)=="Localitati")
  
  #load("data/df_coordinates.rda")
  
  if(area == "counties") {
    if(length(pos_column_jud)==0) {
      print("No data available for jud")
      return(NULL)
    }
    if(length(pos_column_loc)>0){
      matrix <- subset(matrix, trimws(as.character(matrix[,pos_column_loc]))=="TOTAL")
    }
    print(nrow(matrix))
    rows_to_remove <- grep("(regiunea|macroregiunea|total)", tolower(matrix[,pos_column_jud]))
    matrix <- matrix[-rows_to_remove,]
    label <- aggregate(cbind(long, lat) ~ mnemonic, data=df_coordinates, FUN=function(x)mean(range(x)))
    matrix[,pos_column_jud] <- as.character(matrix[,pos_column_jud])
    title <- paste(unname(as.matrix(matrix[1,-c(pos_column_jud, pos_column_val, pos_column_year)])), collapse = "")
    df_coordinates <- left_join(df_coordinates, matrix[,c(pos_column_jud, pos_column_val)], by = c("county"=column_names[pos_column_jud]))
  }
  
  if(area == "regions") {
    if(length(pos_column_reg)==0) {
      print("No data available for regions!")
      return(NULL)
    }
    set_reg <- grep("^regiunea", tolower(trimws(as.character(matrix[,pos_column_reg]))))
    
    matrix <- matrix[set_reg,]
    label <- aggregate(cbind(long, lat) ~ region, data=df_coordinates, FUN=function(x)mean(range(x)))
    matrix[,pos_column_reg] <- trimws(as.character(matrix[,pos_column_reg]))
    matrix[,pos_column_macroreg] <- gsub(" - ","-", matrix[,pos_column_macroreg])
    title <- paste(unname(as.matrix(matrix[1,-c(pos_column_reg, pos_column_val, pos_column_year)])), collapse = "")
    df_coordinates <- left_join(df_coordinates, matrix[,c(pos_column_reg, pos_column_val)], by = c("region"=column_names[pos_column_reg]))
  }
  
  if(area == "macroregions") {
    if(length(pos_column_macroreg)==0) {
      print("No data available for macroregions!")
      return(NULL)
    }
    set_reg <- grep("(macroregiunea)", tolower(matrix[,pos_column_macroreg]))
    matrix <- matrix[set_reg,]
    label <- aggregate(cbind(long, lat) ~ macroregion, data=df_coordinates, FUN=mean)
    matrix[,pos_column_macroreg] <- trimws(as.character(matrix[,pos_column_macroreg]))
    
    title <- paste(unname(as.matrix(matrix[1,-c(pos_column_reg, pos_column_val, pos_column_year)])), collapse = "")
    df_coordinates <- left_join(df_coordinates, matrix[,c(pos_column_macroreg, pos_column_val)], by = c("macroregion"=column_names[pos_column_macroreg]))
  }
  
  title <- paste(title, "Anul", year)
  options(scipen=999)
  plot <- ggplot(df_coordinates) +  
    theme_bw() + 
    geom_polygon(aes(long, lat, group=mnemonic, fill=df_coordinates[,ncol(df_coordinates)]), colour = "white") +
    geom_text(data=label, aes(long, lat, label=label[,1]), size=3, vjust=0) +
    scale_fill_gradient(low='white', high='red', name = column_names[pos_column_val])+
    ggtitle(title) +
    coord_fixed(1)
  
  return(plot)
}

subset_default <- function(matrix) {
  matrix <- tempo_clean(matrix)
  column_names <- names(matrix)
  pos <- grep(("regiuni|judete|macroregiuni|ani$|localitati|valoare"), tolower(column_names))
  
  n = seq(ncol(matrix)) 
  n = n[-pos]
  
  for(i in n) {
    lvs <- levels(matrix[,i])
    if(length(lvs)>1){
      lv <- lvs[2]
      matrix <- subset(matrix, matrix[,i] == lv)
    }
  }
  return(matrix)
}

