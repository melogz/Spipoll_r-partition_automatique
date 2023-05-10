library(dplyr)
library(rgdal)
library(sf) 
library(raster)
library(tidyverse)

source ( paste0(getwd(),"/fonctions_R/main_creation_fond_carte.R"))



ajout_point<- function(fichier,carte,debut =1,fin = nrow(fichier),filename){
  carte_ARA<-st_union(carte)
  df_echan <- fichier[debut:fin,]
  df_echan$presence <- st_within(df_echan,carte_ARA,sparse =F)
  df_presence<- subset(df_echan,(df_echan$presence ==TRUE))
  write.csv(df_presence, paste0(getwd(),filename), row.names = TRUE)
  return (df_presence)
}

carte_a_point <- function(fichier, carte,titre){
  carte %>%
    ggplot() +
    geom_sf() +
    geom_sf(data = fichier, color = "red", size = 0.1) +
    labs(title = titre,
         subtitle = 'Source: gouv, OpenStreet Map')
}


# debut =1
# fin =2000
# df_ARA <- ajout_point(fichier = df_laea , carte = carte_filtre, debut =1 , fin =5000,filename = paste0("/Extraction/SPIPOLL_", debut,"_",fin, ".csv"))
# point_en_ARA <- carte_a_point(fichier = df_ARA, carte = carte_filtre,titre ="point en ARA" )
# plot(point_en_ARA)

