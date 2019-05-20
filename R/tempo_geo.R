#' @title Visualize Geospatial Data
#' 
#' @description tempo_geo creates a map for visualize Geospatial Data 
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
#' @param filter - Character vector containing values from factor variables.
#' 
#' @param title - a string to set the map title
#' 
#' @return Returns a ggplot object. 
#' 
#' @examples 
#' tempo_geo(matrix = AGR111A, year = "2000",area = "counties", 
#'           filter = c("Wine grapes","Private sector"))
#' 
#' @import dplyr
#' @import ggplot2
#' @export

tempo_geo <- function(matrix, year, area, filter = NULL, title = NULL) {
  
  if (nargs() < 3) {
    stop("Wrong number of arguments! Provide at least 3 arguments!")
  }
  
  tmp <- deparse(substitute(matrix))
  if (!exists(tmp)) {
    stop("Matrix not found:", tmp, "\n")
  }
  
  if (is.null(matrix) | !is.data.frame(matrix)) {
    type <- class(matrix)
    stop("Invalid type (",type, ") of argument!\n", sep = "")
  }
  
  if (!(area %in% c("counties", "regions", "macroregions"))) {
    stop("Wrong argument! The argument should be \"counties\" or \"regions\" or \"macroregions\".\n")
  }
  
  matrix <- tempo_clean(matrix)
  
  # year parameter
  column_names <- names(matrix)
  
  pos_column_period <- which(names(matrix)=="Perioade" | names(matrix) == "Periods")
  if (length(pos_column_period) > 0) {
    pp <- grep("([aA]nul|[yY]ear)", matrix[,pos_column_period])
    matrix[,pos_column_period] <- as.character(matrix[,pos_column_period])
    matrix <- matrix[pp,]
    if ("Periods" %in% names(matrix)) names(matrix)[pos_column_period] <- "Years" 
    if ("Perioade" %in% names(matrix)) names(matrix)[pos_column_period] <- "Ani"
    matrix <- tempo_clean(matrix)
    matrix[,pos_column_period] <- as.factor(matrix[,pos_column_period])
  }
  
  pos_column_year <- which(names(matrix)=="Ani" | names(matrix) == "Years")
  lv <- levels(matrix[,pos_column_year])
  exact_year <- paste0("^",year,"$")
  pos_year <- grep(exact_year, lv)
  if (length(pos_year) < 1) {
    lv <- paste(lv, collapse = " ")
    stop("No data available for the specified time! Data only available for: ", lv, "\n")
  }
  
  matrix <- subset(matrix, matrix[,pos_column_year] == year)
  matrix <- subset_filter(matrix, filter)

  # area parameter
  pos_column_jud <- grep("(judete|counties)", tolower(column_names))
  pos_column_reg <- grep("(regiuni|regions)", tolower(column_names)) 
  pos_column_macroreg <- grep("(macroregiuni|macroregions)", tolower(column_names)) 
  pos_column_val <- grep("(valoare|value)", tolower(column_names))
  pos_column_loc <- which(names(matrix) == "Localitati" | names(matrix) == "Localities")
  
  
  if (length(pos_column_jud) == 0 & length(pos_column_reg) == 0 & length(pos_column_macroreg) == 0) {
    stop("Due to the lack of geospatial information, the data cannot be represented on the map.")
  }
  
  en <- 0
  if ("Years" %in% column_names) {
    df_coordinates$region <- df_coordinates$region_en
    df_coordinates$macroregion <- df_coordinates$macroregion_en
    df_coordinates_r$region <- df_coordinates_r$region_en
    df_coordinates_m$macroregion <- df_coordinates_m$macroregion_en
    en <- 1
  }
  
  if (is.factor(matrix[,pos_column_val]) | is.character(matrix[,pos_column_val])) {
    matrix[,pos_column_val] <- as.numeric(as.character(matrix[,pos_column_val]))
  }
  
  if (area == "counties") {
    if (length(pos_column_jud) == 0) {
      stop("No data available for counties!\n")
    }
    if (length(pos_column_loc) > 0) {
      matrix <- subset(matrix, trimws(as.character(matrix[,pos_column_loc]))=="TOTAL")
    }
    rows_to_remove <- grep("(regiunea|macroregiunea|total|macroregion|north|east|south|west|center)", tolower(matrix[,pos_column_jud]))
    matrix <- matrix[-rows_to_remove,]
    title_def <- paste(unname(as.matrix(matrix[1,-c(pos_column_jud, pos_column_val, pos_column_year)])), collapse = "")
    label <- aggregate(cbind(long, lat) ~ mnemonic, data=df_coordinates, FUN=function(x)mean(range(x)))
    label$lat[24] <- label$lat[24] + 0.1
    label$lat[12] <- label$lat[12] - 0.2
    matrix[,pos_column_jud] <- trimws(as.character(matrix[,pos_column_jud]))
    df_coordinates$county <- trimws(df_coordinates$county)
    df_coordinates <- left_join(df_coordinates, matrix[,c(pos_column_jud, pos_column_val)], by = c("county"=column_names[pos_column_jud]))
  }
  
  if (area == "regions") {
    if (length(pos_column_reg)==0) {
      stop("No data available for regions!\n")
    }
    set_reg <- grep("(^regiunea|north|east|south|west|center|bucharest)", tolower(trimws(as.character(matrix[,pos_column_reg]))))
    matrix <- matrix[set_reg,]
    label <- aggregate(cbind(long, lat) ~ region, data=df_coordinates_r, FUN=function(x)mean(range(x)))
    label$lat[6] <- label$lat[6] - 5
    matrix[,pos_column_reg] <- trimws(as.character(matrix[,pos_column_reg]))
    matrix[,pos_column_macroreg] <- gsub(" - ","-", matrix[,pos_column_macroreg])
    title_def <- paste(unname(as.matrix(matrix[1,-c(pos_column_reg, pos_column_val, pos_column_year)])), collapse = "")
    df_coordinates_r$region <- trimws(df_coordinates_r$region)
    df_coordinates <- left_join(df_coordinates_r, matrix[,c(pos_column_reg, pos_column_val)], by = c("region"=column_names[pos_column_reg]))
  }
  
  if (area == "macroregions") {
    if (length(pos_column_macroreg)==0) {
      stop("No data available for macroregions!\n")
    }
    set_reg <- grep("(macroregiunea|macroregion)", tolower(matrix[,pos_column_macroreg]))
    matrix <- matrix[set_reg,]
    label <- aggregate(cbind(long, lat) ~ macroregion, data=df_coordinates_m, FUN=mean)
    matrix[,pos_column_macroreg] <- trimws(as.character(matrix[,pos_column_macroreg]))
    title_def <- paste(unname(as.matrix(matrix[1,-c(pos_column_macroreg, pos_column_val, pos_column_year)])), collapse = "")
    df_coordinates_m$macroregion <- trimws(df_coordinates_m$macroregion)
    df_coordinates <- left_join(df_coordinates_m, matrix[,c(pos_column_macroreg, pos_column_val)], by = c("macroregion"=column_names[pos_column_macroreg]))
  }
  
  if (is.null(title)) {
    title <- paste(title_def, "Anul", year)
    if (en == 1) {
      title <- paste(title_def, "Year", year)
    }
  }
  
  legend_name <- column_names[pos_column_val]
  plot <- plot_map(df_coordinates, title, legend_name, label, tmp)
  
  return (plot)
}

subset_filter <- function(matrix, filter) {
  
  column_names <- names(matrix)
  pos <- grep(("regiuni|judete|macroregiuni|ani$|localitati|valoare|regions|counties|macroregions|years$|localities|value"), tolower(column_names))

  n <- seq(ncol(matrix)) 
  n <- n[-pos]
  
  if (length(n) == 0){
    return (matrix)
  }
  
  if (is.null(filter)) {
    stop("\"filter\" parameter is required! Matrix contains multiple categories!")
  }
  
  if (length(filter) != length(n)) {
    stop("Not enough filters! Add ", length(n), " filters!\n")
  }
    
  j <- 1;
  for (i in n) {
    matrix[,i] <- factor(matrix[,i])
    lvs <- tolower(trimws(levels(matrix[,i])))
    lv <- tolower(trimws(filter[j]))
      
    if (!(lv %in% lvs)) {
      lvs <- paste(lvs, collapse = ", ")
      stop("Incorrect filter! Choose filter from: ", lvs, "\n")
    }
      
    if (length(lvs) > 1) {
       matrix <- subset(matrix, tolower(trimws(matrix[,i])) == lv)
    }
    j <- j+1
  }

  return (matrix)
}


plot_map <- function (df_coordinates, title, legend_name, label, tmp) {
  options(scipen=999)

  plot <- ggplot(df_coordinates) +  
    theme_bw() + 
    geom_polygon(aes(long, lat, group=df_coordinates[,3], fill=df_coordinates[,ncol(df_coordinates)]), colour = "gray45") +
    geom_text(data=label, aes(long, lat, label=label[,1]), size=3.5, vjust=0, fontface='bold') +
    scale_fill_gradient(low='white', high='red', na.value = "gray", name = legend_name)+
    ggtitle(title) +
    coord_fixed(ratio = 1.5, xlim = c(20,30), ylim = c(43.5,48.5))
  
  if (anyNA(df_coordinates)) {
    plot <- plot +
      geom_polygon(aes(c(40), c(40), colour="")) +
      guides(colour=guide_legend("No data", override.aes=list(colour="gray1", fill="gray")))
  }
  plot_name <- paste0(tmp, "_map.png")
  ggsave(plot_name, width = 20, height = 20, units = "cm")
  
  return (plot)
}



