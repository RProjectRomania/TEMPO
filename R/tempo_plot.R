
# Metoda 1 - folosind pachetul sf si functia plot
library(sf)

# Testare pe matricea AGR111A - Suprafata viilor pe rod, pe forme de proprietate, macroregiuni, regiuni de dezvoltare si judete
tempo_bulk("AGR111A")
df <- read.csv("AGR111A.csv")  

df$Ani <- as.character(df$Ani)
df$Macroregiuni..regiuni.de.dezvoltare.si.judete <- as.character(df$Macroregiuni..regiuni.de.dezvoltare.si.judete)
df$Categorii.de.vii <- as.character(df$Categorii.de.vii)
df$Forme.de.proprietate <- as.character(df$Forme.de.proprietate)

df <- subset(df, df$Categorii.de.vii == "Total- vii pe rod" & Ani == " Anul 2017" & df$Forme.de.proprietate == " Total")

pos_regiuni <- grep("MACROREGIUNEA ", df$Macroregiuni..regiuni.de.dezvoltare.si.judete)
df <- df[pos_regiuni,]

load("C:/Users/ana.tiru/Desktop/New folder/TEMPO/data/tempo_geo_codes_shp.rda")

df$geo <- tempo_geo_codes_shp$codes_macroregions_ro$geometry
df <- st_as_sf(df) # convert to an sf object

# Pe macroregiuni
plot(df[,6])


# Metoda 2 - folosind pachetul rgdal si functia ggplot
library(rgdal)
library(ggplot2)

load("C:/Users/ana.tiru/Desktop/New folder/TEMPO/data/shapefile.rda")

shapefile@data$id <- rownames(shapefile@data)
sf_df <- fortify(shapefile, region = "id") # convert from sf object in dataframe object
df_long_lat <- merge(sf_df, shapefile@data, by = "id")

# Pe judete
ggplot(df_long_lat) + 
  aes(long,lat,group=group,fill=countyMn) + 
  geom_polygon() 
# Pe regiuni
ggplot(df_long_lat) + 
  aes(long,lat,group=group,fill=region) + 
  geom_polygon() 

