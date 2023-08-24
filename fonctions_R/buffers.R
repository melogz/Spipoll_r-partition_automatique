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
  buffers = st_buffer(region_spipoll, taille_buffer) # creation de buffer de 1km autour des collections
  st_crs(buffers)==st_crs(st_region)
  compo_buffer = st_intersection(st_region,buffers)
  buffer <- compo_buffer[, c("CODE_12", "AREA_HA", "id")]
  return (buffer)
}

sous_buffer <- function(buffers,pwd_carte,df,taille_sous_buffer){
  carte<- raster(paste0(getwd(),pwd_carte))
  raster_cropped <- raster::crop(carte, buffers)
  buffers <- st_buffer(df, dist = taille_sous_buffer)
  values <- raster :: extract(raster_cropped,buffers)
  buffers$values <- values
  return(buffers)
}

position <- function(df,pwd_carte){
  carte <- raster(paste0(getwd(),pwd_carte))
  df$position <- raster :: extract(carte,df)
  return(df) 
}

position_vector <- function(df,pwd_carte){
  carte <- st_read(paste0(getwd(),pwd_carte))
  point<-st_transform(df,crs=st_crs(carte))
  compo_buffer = st_intersection(carte,point)
  df$position <- compo_buffer$CODE_12
  return(df) 
}


new_make_buffers_vector<- function(pwd_carte,localisation_points,taille_buffer){
  carte <- st_read(paste0(getwd(),pwd_carte))
  sf_points<-st_transform(localisation_points,crs=st_crs(carte))
  buffers = st_buffer(sf_points, taille_buffer) # creation de buffer de 1km autour des collections
  compo_buffer = st_intersection(carte,buffers)
  if ("ID_liste" %in% names(compo_buffer)) {
    # La colonne "long" existe
    names(compo_buffer)[names(compo_buffer)=="ID_liste"] <- "id"
  }
  buffer <- compo_buffer[, 2:4]
  buffer$aire <- st_area(buffer)
  buffer$proportion <- buffer$aire / st_area(st_buffer(localisation_points[1,], taille_buffer))*100
  return (buffer)
}

# Exemple d'utilisation de sous-buffer avec valeurs de pixels

#carte <- raster(paste0(getwd(),"/RSpatial/g250_clc12_V18_5.tif"))
#values <- raster :: extract(carte,sf_collection[1:100,])
#print(values)
#clc_raster <-"/RSpatial/g250_clc12_V18_5.tif"
#buffers_1 <- make_buffers_raster(clc_raster,test,1000)
#length(sous_buffers$values[[1]])
#length(buffers_2000$values[[1]])
#length(buffers_200$values[[1]])
#length(buffers_1$values[[1]])
#sf_collection[1:100,]
#class(buffers_2000)
sf_collection <- transformation_sf(sf_point_climat_entrainement)
clc_vector <-"/CLC12_FR_RGF_SHP/CLC12_FR_RGF.shp"
carte <- st_read(paste0(getwd(),"/CLC12_FR_RGF_SHP/CLC12_FR_RGF.shp"))
test<-st_transform(sf_collection[1:15,],crs=st_crs(carte))
buffers = st_buffer(test, 1000)
compo_buffer = st_intersection(carte,buffers)
test$position <- compo_buffer$CODE_12
plot(compo_buffer[1,], col = "blue", main = "Buffers Vectoriels Ronds")
plot(points, add = TRUE, pch = 20, col = "red")
#buffers_2000_vector <- new_make_buffers_vector(clc_vector,sf_collection[1:10,],1000)


#length(buffers_1000$geometry[[1]])
#length(buffers_1000$values[[1]])
#buf<- slice(buffers_2000,1)
#sous_buf <- slice(sous_buffers ,1)
#buffer <- buffers_2000_2[, c("CODE_12", "AREA_HA", "id")]
#st_bbox(slice(buffers,1))
#clc_carte <- raster(paste0(getwd(),clc_raster))
clc_petit <- crop(carte,# raster to crop
                 extent(st_bbox(slice(compo_buffer,1))))

clc_petit <- st_crop(carte, st_bbox(slice(compo_buffer,1)))
plot(clc_petit)
#plot(buf,add=T)
#plot(sous_buf, add=T)
buf_vector<- slice(buffer,1)

#substr(buf_vector$CODE_12, start = 1, stop = 2)
