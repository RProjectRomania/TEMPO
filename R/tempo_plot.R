
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
  
  
  
  col <- names(matrix)
  idx <- grep("judete", col)
  
  # Parametrul an
  # tc = tempo_clean(matrix)
  # lv = levels(tc$Ani)
  # pos = grep(an, lv)
  # if(length(pos) < 1){
  #   cat("No data available for the specified time! Data available for: ", lv, "\n")
  #   return (NULL)
  # }
  # 
  # tc = subset(tc, Ani == an)
  
  # Parametrul zona
  
  
  if(length(idx) > 0){
    matrix$jud <- matrix[,idx[1]]
    matrix <- matrix[,-idx[1]]
  }
  
  title <- ""
  for(i in 1:(ncol(matrix)-3)){
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
  
  val <- aggregate(cbind(long, lat) ~ Valoare, data=join, FUN=mean)
  val$long = val$long - 1
  val$lat = val$lat - 2.5
  
  
  plot <- ggplot(join) +  
    theme_bw() + 
    geom_polygon(aes(long, lat, group=mnemonic, fill=join$Valoare, colour=""), colour = "white") +
    geom_text(data=cnames, aes(long, lat, label=mnemonic), size=4, vjust=0) +
    geom_text(data=val, aes(long, lat, label=Valoare), size=4, vjust=0) +
    scale_fill_gradient(low='white', high='red', na.value = 'black')+
    ggtitle(title) +
    labs(fill="Valoare")
  
  return(plot)
}

#Testare
tempo_bulk("AGR111A")
tempo_bulk("AGR112B")
AGR111A = read.csv("AGR111A.csv")
AGR112B = read.csv("AGR112B.csv")

tempo_geo(AGR111A)

tc = tempo_clean(AGR111A, "AGR111A")

tj = tempo_search("judete")
tr = tempo_search("regiuni")
tm = tempo_search("macroregiuni")

hard = df_coordinates %>% group_by(name) %>% summarise(n = n())
h = AGR111A %>% group_by(Macroregiuni..regiuni.de.dezvoltare.si.judete) %>% summarise(n = n())
i = grep("Regiunea", h$Macroregiuni..regiuni.de.dezvoltare.si.judete)
im = grep("MACROREGIUNEA", h$Macroregiuni..regiuni.de.dezvoltare.si.judete)
h = h[-i,]
h = h[-im,]
h = h[-c(38),]

h = read.csv("C:/Users/ana.tiru/Desktop/jud.csv")
c = df_coordinates %>% group_by(name, countyCode) %>% summarise(n = n())
df_coordinates = left_join(df_coordinates, tdf[,c(2,7)], by = c("countyCode"="cod"))
dfreg = data.frame(codr=c(6,7,1,2,3,8,4,5), reg = c("Regiunea NORD-VEST", "Regiunea CENTRU", "Regiunea NORD-EST", 
                                                    "Regiunea SUD-EST", "Regiunea SUD-MUNTENIA", "Regiunea BUCURESTI-ILFOV",
                                                    "Regiunea SUD-VEST OLTENIA", "Regiunea VEST"))
df_coordinates = left_join(df_coordinates, dfreg, by = c("regionId"="codr"))
sa = sapply(matrix$jud, grep, df_coordinates$jud)
