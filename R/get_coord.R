

get_coordinates <- function(){
  load("data/shapefile.rda")
  shapefile@data$id <- rownames(shapefile@data)
  shp_df <- fortify(shapefile, region = "id") # convert from shp object in dataframe object
  df_coordinates <- merge(shp_df, shapefile@data, by = "id")
  df_coordinates <- df_coordinates[,c(2,3,8,9,10,11,12,13)]
  df_coordinates$long <- df_coordinates$long/10000
  df_coordinates$lat <- df_coordinates$lat/10000
  return(df_coordinates)
}

df_coordinates <- get_coordinates()

devtools::use_data(df_coordinates)

cnames <- aggregate(cbind(long, lat) ~ mnemonic, data=df_coordinates, FUN=mean)

cnames$lat[4] = cnames$lat[4]-0.5
cnames$lat[24] = cnames$lat[24]+1
cnames$long[15] = cnames$long[15]-1
cnames$lat[18] = cnames$lat[18]-1
cnames$lat[20] = cnames$lat[20]-1
cnames$lat[21] = cnames$lat[21]-1
cnames$long[21] = cnames$long[21]+1
cnames$lat[31] = cnames$lat[31]-1
cnames$long[35] = cnames$long[35]-1
cnames$lat[39] = cnames$lat[39]-1