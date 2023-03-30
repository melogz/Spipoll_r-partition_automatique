library(dplyr)
library(rgdal)
library(sf) 
library(raster)
library(tidyverse)
setwd("/Users/lmanceron/Documents/cartographie")

recuperation2 <- function(filename){
  resultat <- read.table(paste0(getwd(),filename), sep=',',header =TRUE )
  return(resultat)
}


transformation_sf <- function(df){
  df_geo <- st_as_sf(df, coords = c("long", "lat")) %>%
    st_set_crs("+proj=longlat +datum=WGS84 +no_defs")
  clc <- raster(paste0(getwd(),"/RSpatial/g250_clc12_V18_5.tif"))
  df_final <- st_transform(df_geo, crs = st_crs(clc))
  return(df_final)
}

# on récupère la base de données des départements
recuperation_fond_de_carte <- function(filename){
  depart_sf <- st_read(paste0(getwd(),filename)) #lit le fichier
  glimpse(depart_sf)
  clc <- raster(paste0(getwd(),"/RSpatial/g250_clc12_V18_5.tif"))
  depart_sf_LAEA <-  st_transform(depart_sf,crs = st_crs(clc)) 
  return(depart_sf_LAEA)
}



#recuperer les noms de departements
nom_dep<- function(df){
  depart.site <- dplyr::select(df,'Departement' = nom, geometry)
  return(depart.site)
  }


#  recuperer tous les departements que l'on souhaite
filtre_carte<- function(df,critere){
  depart.site.H <- df %>%
    filter (( Departement  %in% critere))
  return (depart.site.H)
}



# afficher la carte des departements 
creation_carte <- function (df,titre){
  theme_set(theme_minimal())
  df %>%
    ggplot() +
    geom_sf() +
    labs(title = titre,
         subtitle = 'Source: gouv, OpenStreet Map')
}

#df_SPIPOLL<-recuperation2("/data_entree/data_SPIPOLL/df_SPIPOLL_10_colonnes.csv")
# df_laea <- transformation_sf(df_SPIPOLL)
# depart_sf <- recuperation_fond_de_carte("/RSpatial/departements-20180101.shp")
# nom_depart<- nom_dep(depart_sf)
#carte_filtre <- filtre_carte(nom_depart,c("Allier","Loire","Puy-de-Dôme","Cantal","Haute-Loire","Ardèche","Drôme","Isère","Savoie","Haute-Savoie","Ain","Rhone","Rhône" ,"Métropole de Lyon"))
# Auvergne <- creation_carte(carte_filtre,"Les lyonnais")
#print(Auvergne)
# 
# class(carte_filtre)
# class(Auvergne)

