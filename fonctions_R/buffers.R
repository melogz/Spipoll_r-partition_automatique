library(sf)
library(raster)


make_buffers<- function(pwd_carte,localisation_points){
  carte <- raster(paste0(getwd(),pwd_carte))
  df <- data.frame(id = 1:nrow(localisation_points))
  sf_points <- st_sf(df, geometry = localisation_points$geometry)
  buffers <- st_buffer(sf_points, dist = 2000,endCapStyle="SQUARE")
  values <- raster :: extract(carte,buffers)
  buffers$values <- values
  return (buffers)
}

#clc <-"/RSpatial/g250_clc12_V18_5.tif"
#buffers <- make_buffers(clc,sf_points)



