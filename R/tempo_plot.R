
# library(rgdal)
# library(ggplot2)
# library(stringi)
# library(dplyr)

tempo_geo <- function(matrix){
  load("data/shapefile.rda")
  
  shapefile@data$id <- rownames(shapefile@data)
  sf_df <- fortify(shapefile, region = "id") # convert from sf object in dataframe object
  df_long_lat <- merge(sf_df, shapefile@data, by = "id")
  df_long_lat <- df_long_lat[,c(2,3,8,9,10,11,12,13)]
  df_long_lat$long <- df_long_lat$long/10000
  df_long_lat$lat <- df_long_lat$lat/10000
  
  cnames <- aggregate(cbind(long, lat) ~ mnemonic, data=df_long_lat, FUN=mean)
  
  col <- names(matrix)
  idx <- grep("judete", col)
  
  if(length(idx) > 0){
    matrix$jud <- matrix[,idx[1]]
    matrix <- matrix[,-idx[1]]
  }
  
  title <- ""
  for(i in 1:(ncol(matrix)-2)){
    lvs <- levels(matrix[,i])
    if(length(lvs)>1){
      lv <- lvs[2]
      title <- paste(title, lv)
      matrix <- subset(matrix, matrix[,i] == lv)
    }
  }
  
  reg <- grep("regiunea", tolower(matrix$jud))
  matrix <- matrix[-reg, ]
  df_long_lat$name <- stri_trim(as.character(df_long_lat$name))
  matrix$jud <- stri_trim(as.character(matrix$jud))
  join <- left_join(df_long_lat, matrix[,c(5,6)], by = c("name" = "jud"))
  
  plot <- ggplot(join) +  
    theme_bw() + 
    geom_polygon(aes(long, lat, group=mnemonic, fill=join$Valoare)) +
    geom_text(data=cnames, aes(long, lat, label=mnemonic), size=4, vjust=0) +
    scale_fill_gradient(low='white', high='red')+
    ggtitle(title) +
    labs(fill="Valoare")
  
  return(plot)
}

# Testare
# tempo_bulk("AGR111A")
# matrix = read.csv("AGR111A.csv")
# tempo_geo(matrix)
