grille <- read.table(paste0(getwd(),"/data_entree/data_SPIPOLL/grille_prediction_2km.txt"),sep="",header= TRUE)
sf_points <- st_as_sf(grille, coords = c("X_barycentre_L93", "Y_barycentre_L93"), crs = 2154)
sf_points <- st_transform(sf_points, crs = 4326)
coordinates <- st_coordinates(sf_points)
sf_points$long <- coordinates[,1]
sf_points$lat <- coordinates[,2]
clc <- raster(paste0(getwd(),"/RSpatial/g250_clc12_V18_5.tif"))
st_crs(clc)== st_crs(sf_points)
sf_points <- st_transform(sf_points, crs = st_crs(clc))
names(sf_points)[names(sf_points)=="ID_liste"] <- "id"
st_crs(carte_filtre)== st_crs(sf_points)
clc_raster <-"/RSpatial/g250_clc12_V18_5.tif"
region <- paste0(getwd(),"/carte_region/regions_2015_metropole_region.shp")


st_intersection(clc, allie)

sf_point_allier<- st_intersection(sf_france, allie)
sf_point_allier$jour_julien <- 166
sf_point_allier$mois <- 6
sf_allier <- main_one_hot(clc_raster,sf_point_allier,44,1000,250,region,3)
j <- st_make_grid(allie,cellsize = 2000,what="polygons",square = T)
j1 <- st_intersection(j, allie)

grand_buffer_test <-make_buffers_raster(clc_raster,sf_points,2000) 
buffers_allier <-st_buffer(sf_point_allier[,1:3], dist = 1000,  endCapStyle = "SQUARE")
plot(buffers_allier)
allie%>%
  ggplot() +
  geom_sf() +
  labs(title = 'Allie',
       subtitle = 'Source: gouv, OpenStreet Map')

prediction_carte <- function (carte,buffer){
  for (i in (8:19)){
  test<-carte %>%
      ggplot() +
      geom_sf() +
      geom_sf(data = buffer, aes(fill = unlist(buffer[[i]])), color = NA, size = 0.01) +
      scale_fill_gradient(limits = c(0, 1),low = "blue", high = "green")+
      #guides(fill = FALSE)+
      labs(title = paste("Carte de présence en",mois[i-7]))
    plot(test)
  }
}

library(ggplot2)
library(ggspatial)


sf_france <- transformation_sf(sf_france)
class(sf_france)

france %>%
  ggplot() +
  geom_sf()+ 
  geom_sf(data =  sf_france,aes(fill = position), color = NA, size = 0.01) +
  annotation_scale(location = "bl")+
  labs(title = "Fond de carte")

  
  scale_fill_gradient(limits = c(0, 1),low ="blue", high = "green")+
  guides(fill = FALSE)+
  labs(title = "Carte de présence en fevrier")

prediction_carte (allie,buffers_allier)

prediction_carte (carte_filtre,buffers_france)
mois <- c('janvier','fevrier','mars','avril','mai','juin','juillet','août','septembre','octobre','novembre','décembre')
janvier <- carte_filtre %>%
  ggplot() +
  geom_sf() +
  geom_sf(data = buffers_france, aes(fill = presence_janvier), color = NA, size = 0.01) +
  scale_fill_gradient(limits = c(0, 1),low = "blue", high = "green")+
  guides(fill = FALSE)+
  labs(title = "Carte de présence en janvier")

plot(janvier)

septembre <- carte_filtre %>%
  ggplot() +
  geom_sf() +
  geom_sf(data = buffers_france, aes(fill = presence_septembre), color = NA, size = 0.01) +
  scale_fill_gradient(limits = c(0, 1),low = "blue", high = "green")+
  guides(fill = FALSE)+
  labs(title = "Carte de présence en septembre")

plot(septembre)

class(buffers_allier$presence_janvier)
class(as.numeric(st_drop_geometry(buffers_allier[,9])))

fevrier <- carte_filtre %>%
  ggplot() +
  geom_sf() +
  geom_sf(data =  buffers_france, aes(fill =  presence_fevrier), color = NA, size = 0.01) +
  scale_fill_gradient(limits = c(0, 1),low ="blue", high = "green")+
  guides(fill = FALSE)+
  labs(title = "Carte de présence en fevrier")

plot(fevrier)

mars <- carte_filtre  %>%
  ggplot() +
  geom_sf() +
  geom_sf(data =  buffers_france, aes(fill =  presence_mars), size = 0.01) +
  scale_fill_gradient(limits = c(0, 1),low = "blue", high = "green")+
  guides(fill = FALSE)+
  labs(title = "Carte de présence en mars")

plot(mars)

avril <- carte_filtre %>%
  ggplot() +
  geom_sf() +
  geom_sf(data =  buffers_france, aes(fill =  presence_avril), color = NA, size = 0.01) +
  scale_fill_gradient(limits = c(0, 1),low = "blue", high = "green")+
  guides(fill = FALSE)+
  labs(title = "Carte de présence en avril")

plot(avril)

mai <- carte_filtre  %>%
  ggplot() +
  geom_sf() +
  geom_sf(data =  buffers_france, aes(fill =  presence_mai), color = NA, size = 0.01) +
  scale_fill_gradient(limits = c(0, 1),low = "blue", high = "green")+
  guides(fill = FALSE)+
  labs(title = "Carte de présence en mai")

plot(mai)  

juin <- carte_filtre  %>%
  ggplot() +
  geom_sf() +
  geom_sf(data =  buffers_france, aes(fill =  presence_juin),  color = NA, size = 0.01) +
  scale_fill_gradient(limits = c(0, 1),low = "blue", high = "green")+
  guides(fill = FALSE)+
  labs(title = "Carte de présence en juin")

plot(juin) 

juillet<- carte_filtre  %>%
  ggplot() +
  geom_sf() +
  geom_sf(data =  buffers_france, aes(fill =  presence_juillet), color = NA, size = 0.01) +
  scale_fill_gradient(limits = c(0, 1),low = "blue", high = "green")+
  guides(fill = FALSE)+
  labs(title = "Carte de présence en juillet")
plot(juillet) 

aout<- carte_filtre  %>%
  ggplot() +
  geom_sf() +
  geom_sf(data =  buffers_france, aes(fill = presence_aout), color = NA, size = 0.01) +
  scale_fill_gradient(limits = c(0, 1),low = "blue", high = "green")+
  guides(fill = FALSE)+
  labs(title = "Carte de présence en août")

plot(aout)

septembre<- carte_filtre  %>%
  ggplot() +
  geom_sf() +
  geom_sf(data =  buffers_france, aes(fill =  presence_septembre),  color = NA, size = 0.01) +
  scale_fill_gradient(limits = c(0, 1),low = "blue", high = "green")+
  guides(fill = FALSE)+
  labs(title = "Carte de présence en septembre")

plot (septembre)

octobre<- carte_filtre  %>%
  ggplot() +
  geom_sf() +
  geom_sf(data =  buffers_france, aes(fill =  presence_octobre),  color = NA, size = 0.01) +
  scale_fill_gradient(limits = c(0, 1),low = "blue", high = "green")+
  guides(fill = FALSE)+
  labs(title = "Carte de présence en octobre")

plot(octobre)

novembre<- carte_filtre  %>%
  ggplot() +
  geom_sf() +
  geom_sf(data =  buffers_france, aes(fill =  presence_novembre),  color = NA, size = 0.01) +
  scale_fill_gradient(limits = c(0, 1),low = "blue", high = "green")+
  guides(fill = FALSE)+
  labs(title = "Carte de présence en novembre")

plot(novembre)

decembre<- carte_filtre  %>%
  ggplot() +
  geom_sf() +
  geom_sf(data =  buffers_france, aes(fill =  presence_decembre), color = NA, size = 0.01) +
  scale_fill_gradient(limits = c(0, 1),low = "blue", high = "green")+
  guides(fill = FALSE)+
  labs(title = "Carte de présence en décembre")

plot(decembre)

 meurthe%>%
  ggplot() +
  geom_sf() +
  geom_sf(data = buffers_meurthe, aes(fill = sf_point_meurthe$Moyenne_bio2), color = NA, size = 0.01,) +
  scale_fill_gradient(limits = c(0, 1),low = "blue", high = "yellow") 

 allie%>%
   ggplot() +
   geom_sf() +
   geom_sf(data = buffers_allier, aes(fill = buffers_allier$V15), color = NA, size = 0.01,) +
   scale_fill_gradient(limits = c(0, 1),low = "blue", high = "yellow")+
   labs(title = "Carte de la répartition du myrtil")+
   labs(fill = "probabilité de présence")
 
 carte_filtre  %>%
   ggplot() +
   geom_sf() +
   geom_sf(data =  buffers_france, aes(fill =  sf_france_climat$Moyenne_bio2),  color = NA, size = 0.01) +
   scale_fill_gradient(low = "blue", high = "green")+
   labs(title = "Carte des valeurs pour la bioclim 2")
 

 allie %>%
   ggplot() +
   geom_sf() +
   geom_sf(data =  buffers_allier, aes(fill =  sf_allier_climat$Moyenne_bio2),  color = NA, size = 0.01) +
   scale_fill_gradient(low = "blue", high = "green")+
   labs(title = "Carte des valeurs pour la bioclim 2")
 
 
 carte_filtre %>%
   ggplot() +
   geom_sf() +
   geom_sf(data = buffers_france, aes(fill = on_va_voir_test$mean...mean_min_temp.), color = NA, size = 0.01,) +
   scale_fill_gradient(low = "blue", high = "yellow")+
   labs(title = "Carte des température minimales")+
   guides(fill = FALSE)+
   labs(fill = "répartition des température minimales")
 
sf_point_morbihan<- st_intersection(sf_points, morbihan)
sf_point_morbihan$jour_julien <- 166
sf_point_morbihan$mois <- 6
sf_morbihan <- main_one_hot(clc_raster,sf_point_morbihan,44,1000,250,region,3)
buffers_morbihan <-st_buffer(sf_point_morbihan, dist = 1000,  endCapStyle = "SQUARE") 
 
prediction_carte (morbihan,buffers_morbihan)

sf_point_meurthe<- st_intersection(sf_points, meurthe)
sf_point_meurthe$jour_julien <- 166
sf_point_meurthe$mois <- 6
sf_meurthe <- main_one_hot(clc_raster,sf_point_meurthe,44,1000,250,region,3)
buffers_meurthe <-st_buffer(sf_point_meurthe, dist = 1000,  endCapStyle = "SQUARE") 
avant<- names(buffers_meurthe) 
apres<- c(avant,"presence_janvier","presence_fevrier","presence_mars","presence_avril","presence_mai","presence_juin","presence_juillet","presence_aout","presence_septembre","presence_octobre","presence_novembre","presence_decembre") 
names(buffers_meurthe)<-apres


prediction_carte (meurthe,buffers_meurthe)


sf_points$jour_julien <- 166
sf_points$mois <- 6
sf_france <- main_one_hot(clc_vector,sf_points,44,1000,250)

sf_test <- main_one_hot(clc_vector,sf_points[1,],44,1000,250)



grand_buffer_morbihan <-make_buffers_raster(clc_raster,sf_morbihan,1000)

test <- unlist(grand_buffer_morbihan$values)
table (test)

ggplot(buffers_france, aes(x = V13)) +
  geom_histogram(fill = "blue", color = "black") +
  labs(x = "Valeur de la colonne", y = "Fréquence", title = "Répartition des valeurs de la colonne")

ggplot(test2) +
  geom_histogram(fill = "blue", color = "black") +
  labs(x = "Valeur de la colonne", y = "Fréquence", title = "Répartition des valeurs de la colonne")

avant<- names(buffers_france[,1:6]) 
apres<- c(avant,"presence_janvier","presence_fevrier","presence_mars","presence_avril","presence_mai","presence_juin","presence_juillet","presence_aout","presence_septembre","presence_octobre","presence_novembre","presence_decembre") 
names(buffers_france)<-apres
buffers_france$
test3 <- unlist(buffers_allier$presence_juin)
test3 <- c(test2,buffers_allier$presence_decembre)
hist(unlist(test2), xlab = "Probabilités de présence", ylab = "Fréquence",main = NULL)


buffers_france <-st_buffer(sf_france[,1:3], dist = 1000,  endCapStyle = "SQUARE")
