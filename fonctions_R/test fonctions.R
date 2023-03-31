# Charger les packages nécessaires
library(sf)
library(raster)
clc <- raster(paste0(getwd(),"/RSpatial/g250_clc12_V18_5.tif"))
depart_sf <- st_read(paste0(getwd(),"/Extraction/carte_ARA.shp"))
plot(depart_sf)
# Générer des points aléatoires dans la zone d'étude
points <- st_read(paste0(getwd(),"/Extraction/SPIPOLL_1_10000.shp"))
points_laea <- st_transform(points, crs = st_crs(clc))
df <- data.frame(id = 1:916)
sf_points <- st_sf(df, geometry = points_laea$geometry)


# Créer des tampons de 500 m autour des points
buffers <- st_buffer(sf_points, dist = 2000)

# Visualiser les points et les tampons
plot(buffers)
plot(sf_points, add = TRUE)

# Charger le package raster


# Créer un exemple de carte raster

# Extraire les valeurs de pixel pour chaque tampon
class(clc)
class(buffers)

values <- extract(clc, buffers)

# Ajouter les valeurs extraites au tableau des tampons
buffers$values <- values

# Visualiser les tampons avec les valeurs extraites
plot(buffers)
clc.ARA <- crop(clc,extent(st_bbox(depart_sf))) #raster to crop
plot(clc.ARA)
plot(depart_sf, col = "red", add = TRUE)
contours_st <- st_boundary(depart_sf)
plot(clc.ARA)
plot(contours_st, col = "blue", add = TRUE)
plot(points_laea, col = "red", add =TRUE)
