# Charger les packages nécessaires
library(sf)
library(raster)
clc <- raster(paste0(getwd(),"/RSpatial/g250_clc12_V18_5.tif"))
depart_sf <- st_read(paste0(getwd(),"/Extraction/carte_ARA.shp"))
plot(depart_sf)
# Générer des points aléatoires dans la zone d'étude
#points <- st_read(paste0(getwd(),"/Extraction/SPIPOLL_1_10000.shp"))
#points_laea <- st_transform(points, crs = st_crs(clc))
df <- data.frame(id = 1:nrow(sf_points))
sf_points_laea <- st_sf(df, geometry = sf_points$geometry)


# Créer des tampons de 500 m autour des points
buffers <- st_buffer(sf_points_laea, dist = 2000)
surface_proche <- st_union(buffers)
# Visualiser les points et les tampons
plot(depart_sf)
plot(buffers,add= TRUE)
plot(sf_points, add = TRUE)

surface_proche_sf <- st_as_sf(surface_proche)  # Convert to sf object
surface_proche_spdf <- st_as_sf(surface_proche_sf, as_points=FALSE)  # Convert to SpatialPolygonsDataFrame object


# Extraire les valeurs de pixel pour chaque tampon
class(clc)
class(surface_proche)
values <- raster :: extract(x =clc, y =buffers)
crs(surface_proche_spdf)
# Ajouter les valeurs extraites au tableau des tampons
buffers$values <- values
length(buffers$values[[1]])

surface_proche_spdf %>%
  ggplot() +
  geom_sf() +
  geom_sf(data = sf_points, color = "red", size = 0.1) +
  labs(title ="buffer",
       subtitle = 'Source: gouv, OpenStreet Map')

class(surface_proche_spdf[[1]])
# Visualiser les tampons avec les valeurs extraites
plot(buffers)
clc.ARA <- crop(clc,extent(st_bbox(depart_sf))) #raster to crop
plot(clc.ARA)
plot(depart_sf, col = "red", add = TRUE)
contours_st <- st_boundary(depart_sf)
plot(clc.ARA)
plot(contours_st, col = "blue", add = TRUE)
plot(points_laea, col = "red", add =TRUE)


# Charger le package raster


##### TEST avec aggregate###

# Calculer la surface de chaque polygone
buffer1 <- buffers[buffers$id == 1, ]
buffer1$area <- st_area(buffer1)
length(buffers$area)
length(buffers$values)
# Agréger les données par classe et calculer la somme de la surface pour chaque classe
class_areas <- aggregate(buffers$area, by = buffers$values,FUN = sum, na.rm=TRUE)
valid_geom <- st_is_valid(buffers$geometry)

buffers_df <- data.frame( class = buffer1$values, area = st_area(buffer1))
# Renommer les colonnes
names(class_areas) <- c("class", "area")

# Afficher le résultat
print(class_areas)



### TEST 12 je crois####

#création du buffer à voir##

class (buffers)
print(unlist(buffer1$geometry))


#### TEST SUR EXTRACTION BUFFER ###
#### TEST DE FONCTIONS ####
sf_points <- result[[1]]
carte_filtre<- result[[2]]


nrow(sf_points)
df <- data.frame(id = 1:nrow(sf_points))
sf_test <- st_sf(df, geometry = sf_points$geometry)
# Créer des tampons de 500 m autour des points
buffers <- st_buffer(sf_test, dist = 2000)


clc <- raster(paste0(getwd(),"/RSpatial/g250_clc12_V18_5.tif"))
values <- raster::extract(clc, sf_test, buffer =2000)
class (values)
buffers$values <- values
unique(buffers$values)


for (i in (1:nrow(buffers))){
  print(length(unlist(buffers$values[buffers$id == i])))
}
length(unlist(buffers$geometry[buffers$id == 1]))

table(buffers)

buffer_subset <- buffers[buffers$id == 1, ]
filtered_pixels <- unlist(buffer_subset$values) == 2
class_geom <- st_geometry(buffer_subset)[filtered_pixels]
sum(st_area(class_geom))
# sélectionner le buffer d'intérêt
buffer_subset <- buffers[buffers$id == 1, ]

# identifier les pixels qui appartiennent à la classe d'occupation des sols qui vous intéresse
filtered_pixels <- unlist(buffers$values) == 2

# sélectionner les pixels filtrés dans le buffer
buffer_class2 <- buffer_subset[filtered_pixels,]

# calculer la surface des pixels sélectionnés
area_class2 <- sum(st_area(buffer_class2))






str(unlist(buffer_subset$values))
unlist(buffer_subset$values)==2



# Calculer la surface totale du buffer
total_area <- st_area(buffers[1]) 
print(total_area)
# Extraire les valeurs uniques de la classe d'occupation des sols
unique_values <- unique(sf_test$values[[1]])
print(unique_values)
# Initialiser une liste pour stocker les résultats
results <- list()

# Sélectionner la première couche de la liste values
layer1 <- sf_test$values[[1]]
print(layer1)
# Filtrer les pixels correspondant à la classe 2 dans la première couche
filtered_pixels <- layer1 == 20
print(filtered_pixels)
# Créer un nouveau buffer uniquement avec les pixels correspondant à la classe 2

total_area <- st_area(buffers[[1]])
total_area
buffer_class2 <-sf_test$geometry[[1]][filtered_pixels]

print(buffer_class2)
# Boucler sur chaque valeur unique et calculer la surface correspondante
for (val in unique_values) {
  filtered_pixels <- sf_test$values[[1]] == val
  value_area <- sum(st_area(buffers[filtered_pixels,]))
}
class(sf_test)
filtered_pixels <- sf_test$values[[1]] == 2
print(buffers[1][filtered_pixels,])
value_area <- sum(st_area(buffers[[1]][filtered_pixels,]))
print(value_area/st_area(buffers[1,]))
# Afficher les résultats
print(results)

# Filtrer les pixels correspondant à la valeur d'occupation des sols


# Calculer la surface de la valeur d'occupation des sols
value_area <- sum(st_area(buffers[filtered_pixels]))

# Ajouter le résultat dans la liste
results[[as.character(val)]] <- value_area/total_area


# créer un exemple de dataframe avec des colonnes contenant des 0
df_test <- data.frame(x = c(1, 0, 3), y = c(0, 0, 0), z = c(0, 2, 0))

# Trouver les colonnes où toutes les valeurs sont égales à 0
cols_to_remove <- apply(df_test == 0, 2, all)

# Vérifier la taille de cols_to_remove
length(cols_to_remove) == ncol(df_test) # doit renvoyer TRUE

# Supprimer les colonnes correspondantes
df_test <- df_test[, !cols_to_remove]

df_t<- sf_points

slice(buffers,1)

buffers_occupation <- st_buffer(sf_point_allier, dist = 1000)
buffers_occupation$values <- raster::extract(clc, sf_point_allier, buffer =1000)

st_bbox(slice(buffers_allier,1))
st_crs(clc)== st_crs(buffers_allier)
clc_test <- crop(clc,# raster to crop
                  extent(st_bbox(slice(buffers_allier,2)))) # extent on which to subset it has  to be a extent type object
plot(clc)
plot(clc_test)
plot(slice(buffers_occupation,2) , col = rgb(1, 0, 0, alpha = 0.5) ,add =T)
length(clc_test)
buffer_carre <- st_buffer(sf_points_laea, dist = 2000,endCapStyle="SQUARE")
values <- raster::extract(clc, buffer_carre)
buffer_carre$occupation<-values

for (i in (1:length(buffer_carre$occupation))){
    categorie <-(unique(unlist(buffer_carre$occupation[[i]])))
    for (j in categorie){
      for (k in (1:44)){ 
        if (k ==j){
        print(table(buffer_carre$occupation[[1]])[[j]]*100/256) 
        }
    }
  }
} 
print(table((buffer_carre$occupation[[1]]))*aire_pixel)
categorie <-table(buffer_carre$occupation[[1]])
length(categorie)
for (j in (1:length(categorie))){
  for (k in (1:44)){ 
    if (k ==names(categorie)[j]){
      print(paste(k ,"=",table(buffer_carre$occupation[[1]])[[j]]))
    }
  }
}
for (i in (1:length(categorie))){
  print(table(buffer_carre$occupation[[1]])[[j]])
}
print(table(buffer_carre$occupation[[1]])[[k]]*100/256) 



test_calcul <- 0
for (j in (1:length(categorie))){
  table(buffer_carre$occupation[[1]])[[j]]*100/256
}
names(table(buffer_carre$occupation[[1]]))
length(test$occupation)
aire_pixel <-st_area(slice(buffer_carre,1))/256
test_table <-table(test$occupation)
test_table
table(buffer_carre$occupation[[1]])[[1]]*aire_pixel
       
test <- slice(buffer_carre,1)
unique(unlist(test$occupation)) 
st_area(slice(buffer_carre,1))
st_area()
length(buffer_carre$occupation)
plot(clc_test)

st_boundary(slice(buffers,1))
st_boundary(buffer_carre)
plot(st_boundary(buffer_carre),add=TRUE)
sf_points

test_ligne <- slice(sf_points_2,1)

sf_points_2[[12]][[2]]> 0

for(k in (1:nrow(sf_points_2))){
  test_calcul <- 0
  for (i in (12:39)){
    if (sf_points_2[[i]][[k]] > 0){
      test_calcul <- test_calcul + sf_points_2[[i]][[k]]
    }
  } 
  print(test_calcul)
}
 
print(test_calcul)
test_ligne[[3]]
length(names(test_ligne))


tmin <- c(10,12,14,16,18,20,22,21,19,17,15,12)
tmax <- tmin + 5
prec <- c(0,2,10,30,80,160,80,20,40,60,20,0)
class(prec)
biovars(prec, tmin, tmax)




#d'abord les températures moyennes 

climate_data_2001_2010 <- extract_nc_value(first_year = 2001, 
                                           last_year = 2010,
                                           local_file = FALSE,
                                           file_path = NULL,
                                           sml_chunk = "1995-2010",
                                           spatial_extent = fr_border,
                                           clim_variable = "mean temp", # on récupère ici les valeurs moyennes
                                           statistic = "mean",
                                           grid_size = 0.25,
                                           ecad_v = NULL,
                                           write_raster = TRUE,
                                           out = "raster_mean_temp_2001_2010.grd",
                                           return_data = TRUE)

# on le transforme en raster pour pouvoir localiser les points et extraire les valeurs d'interet 
rbk_2001 = raster::brick("raster_mean_temp_2001_2010.grd")
sf_point_climat <- st_transform(sf_climat, crs = st_crs(rbk_2001))
st_crs(rbk_2001)== st_crs(sf_point_climat)

# on récupère la moyenne de chaque année pour avoir le bon nombre de ligne
annual_avg_temp_pnts = temporal_aggregate(x = rbk_2001,
                                          y = sf_point_climat,
                                          agg_function = "mean",
                                          variable_name = "average temp",
                                          time_step = "annual")

# on récupère la moyenne de chaque mois 
monthly_avg_temp_pnts = temporal_aggregate(x = rbk_2001,
                                           y = sf_point_climat,
                                           agg_function = "mean",
                                           variable_name = "average temp",
                                           time_step = "monthly")

# on extrait pour les années 2011-2021 la température moyenne
climate_data_2011_2021 <- extract_nc_value(first_year = 2011, 
                                           last_year = 2021,
                                           local_file = FALSE,
                                           file_path = NULL,
                                           sml_chunk = "2011-2021",
                                           spatial_extent = fr_border,
                                           clim_variable = "mean temp",
                                           statistic = "mean",
                                           grid_size = 0.25,
                                           ecad_v = NULL,
                                           write_raster = TRUE,
                                           out = "raster_mean_temp_2011_2021.grd",
                                           return_data = TRUE)


rbk_2011 = raster::brick("raster_mean_temp_2011_2021.grd")
format(object.size(climate_data_2011_2021), "MB")
format(object.size(rbk_2011), "MB")
st_crs(rbk_2011)== st_crs(sf_point_climat)

# on récupère la moyenne de chaque année pour avoir le bon nombre de ligne
annual_avg_temp_2011_pnts = temporal_aggregate(x = rbk_2011,
                                               y = sf_point_climat,
                                               agg_function = "mean",
                                               variable_name = "average temp",
                                               time_step = "annual")

annual_avg_temp_pnts <-rbind(annual_avg_temp_pnts,annual_avg_temp_2011_pnts)
annual_avg_temp_pnts <- arrange(annual_avg_temp_pnts, site)

# annual mean per point
monthly_avg_temp_pnts_2011 = temporal_aggregate(x = rbk_2011,
                                                y = sf_point_climat,
                                                agg_function = "mean",
                                                variable_name = "average temp",
                                                time_step = "monthly")

monthly_avg_temp_pnts <-rbind(monthly_avg_temp_pnts,monthly_avg_temp_pnts_2011)
monthly_avg_temp_pnts <- arrange(monthly_avg_temp_pnts, site)
grille<- read.table(paste0(getwd(),"/data_entree/data_SPIPOLL/grille_prediction_2km.txt"),sep ="", header =TRUE )
