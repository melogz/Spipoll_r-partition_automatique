library(sf)
library(raster)
library(tmap)
getwd()

make_buffers_raster<- function(pwd_carte,localisation_points,taille_buffer){
  carte <- raster(paste0(getwd(),pwd_carte))
  df <- data.frame(id = 1:nrow(localisation_points))
  sf_points <- st_sf(df, geometry = localisation_points$geometry)
  buffers <- st_buffer(sf_points, dist = taille_buffer)
  values <- raster :: extract(carte,buffers)
  buffers$values <- values
  return (buffers)
}

make_buffers_vector<- function(pwd_carte,localisation_points,pwd_carte_region,numero_region,taille_buffer){
  carte <- st_read(paste0(getwd(),pwd_carte))
  region<-st_read(pwd_carte_region)
  region<-st_transform(region,crs=st_crs(carte))
  df <- data.frame(id = 1:nrow(localisation_points))
  sf_points <- st_sf(df, geometry = localisation_points$geometry)
  sf_points<-st_transform(sf_points,crs=st_crs(carte))
  st_region = st_intersection(carte,region[numero_region,])
  region_spipoll = st_intersection(sf_points,region[numero_region,])
  buffers = st_buffer(region_spipoll, taille_buffer) # creation de buffer de 1km autours des collections
  st_crs(buffers)==st_crs(st_region)
  compo_buffer = st_intersection(st_region,buffers)
  buffer <- compo_buffer[, c("CODE_12", "AREA_HA", "id")]
  buffer$CODE_12 <- substr(buffer$CODE_12, start = 1, stop = 2)
  return (buffer)
}

sous_buffers <- function(buffers,taille_buffer){
  sous_buffers <- st_buffer(region_spipoll, taille_buffer)
  compo_buffer = st_intersection(st_region,buffers)
  compo_buffer$area=st_area(compo_buffer)
  return (compo_buffer)
}


clc_raster <-"/RSpatial/g250_clc12_V18_5.tif"
buffers_2000 <- make_buffers_raster(clc_raster,sf_collection2,2000)


clc_vector <-"/CLC12_FR_RGF_SHP/CLC12_FR_RGF.shp"
region <- paste0(getwd(),"/carte_region/regions_2015_metropole_region.shp")
buffers_2000_vector <- make_buffers_vector(clc_vector,sf_collection2,region,3,2000)

length(buffers_1000$geometry[[1]])
length(buffers_1000$values[[1]])
buf<- slice(buffers_1000,1)
buffer <- buffers_2000_2[, c("CODE_12", "AREA_HA", "id")]
st_bbox(slice(buffers,1))
clc_carte <- raster(paste0(getwd(),clc_raster))
clc_petit <- crop(clc_carte,# raster to crop
                 extent(st_bbox(slice(buffers_1000,1))))
plot(clc_petit)
plot(buf,add=T)
buf_vector<- slice(buffer,1)

substr(buf_vector$CODE_12, start = 1, stop = 2)
