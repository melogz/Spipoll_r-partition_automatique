library(dplyr)
library(rgdal)
library(sf) 
library(raster)
library(tidyverse)
setwd("/Users/lmanceron/Documents/cartographie")
source ( paste0(getwd(),"/fonctions_R/ajout_points.R"))


main_ajout_point<- function(fichier,carte,debut,fin,chemin_enregistrement,titre='point présent'){
  enregistrement = paste0(chemin_enregistrement, debut,"_",fin,".csv")
  df_laea <- transformation_sf(fichier)
  df <- ajout_point(df_laea,carte,debut,fin,paste0(enregistrement))
  point_sur_carte <- carte_a_point(df,carte,titre)
  plot(point_sur_carte)
}

#main_ajout_point(df_laea,carte_filtre,1,5000,"/Extraction/Spipoll_ARA_" )
#class(df_laea)
#st_within(df_laea[1:2000,],st_union(carte_filtre),sparse =F)
