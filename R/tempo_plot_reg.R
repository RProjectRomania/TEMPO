# library(dplyr)
# library(ggplot2)

tempo_geo_reg <- function(matrix, year, area) {
  matrix <- subset_default(matrix)
  
  # year parameter
  column_names <- names(matrix)

  pos_column_year <- which(names(AGR202B)=="Ani" | names(AGR202B) == "Year")
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
  pos_column_loc <- which(names(AGR202B)=="Localitati")
  
  load("data/df_coordinates.rda")
  
  if(area == "judete") {
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
    title <- paste(title, "Anul", year)
    df_coordinates <- left_join(df_coordinates, matrix[,c(pos_column_jud, pos_column_val)], by = c("county"=column_names[pos_column_jud]))
  }
  
  if(area == "regiuni") {
    if(length(pos_column_reg)==0) {
      print("No data available for regions!")
      return(NULL)
    }
    set_reg <- grep("(regiunea)", tolower(matrix[,pos_column_reg]))
    matrix <- matrix[set_reg,]
    label <- aggregate(cbind(long, lat) ~ reg, data=df_coordinates, FUN=mean)
    matrix[,pos_column_reg] <- as.character(matrix[,pos_column_reg])
    title <- paste(unname(as.matrix(matrix[1,-c(pos_column_reg, pos_column_val, pos_column_year)])), collapse = "")
    title <- paste(title, "Anul", year)
    df_coordinates <- left_join(df_coordinates, matrix[,c(pos_column_reg, pos_column_val)], by = c("region"=column_names[pos_column_reg]))
  }

  options(scipen=999)
  plot <- ggplot(df_coordinates) +  
    theme_bw() + 
    geom_polygon(aes(long, lat, group=mnemonic, fill=df_coordinates[,ncol(df_coordinates)]), colour = "white") +
    geom_text(data=label, aes(long, lat, label=mnemonic), size=3, vjust=0) +
    scale_fill_gradient(low='white', high='red', name = column_names[pos_column_val])+
    ggtitle(title) +
    coord_fixed(1)
    #labs(fill=column_names[pos_column_val])
  
  return(plot)
}


subset_default <- function(matrix) {
  matrix <- tempo_clean(matrix)
  column_names <- names(matrix)
  pos <- grep(("regiuni|judete|macroregiuni|ani$|localitati|valoare"), tolower(column_names))
  
  n = seq(ncol(matrix)) # presupunem ca ultima coloana contine valoarea
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

# Test
# toc <- tempo_toc()
# tempo_bulk("AGR202B")
# AGR202B <- read.csv("AGR202B.csv")
plot <- tempo_geo_reg(AGR202B, "1990", "judete")



