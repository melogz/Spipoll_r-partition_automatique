library(dplyr)
library(rgdal)
library(sf) 
library(raster)
library(tidyverse)
getwd()
setwd("/Volumes/LaCie/carto_MNHN")
source ( paste0(getwd(),"/fonctions_R/main_ajout_point.R"))
source ( paste0(getwd(),"/fonctions_R/main_creation_fond_carte.R"))
source ( paste0(getwd(),"/fonctions_R/main_recuperation_donnees_SPIPOLL.R"))


main_extraction <- function (fichier_donnees, colonne_donnees, fichier_carte,departements,echantillon){
  dataframe_donnees <- main_recuperation(pwd =fichier_SPIPOLL , colonnes= colonne_donnees )
  carte_filtre <-main_creation_carte(df =dataframe_donnees , pwdcarte= pwd_carte, colonne = departements, titre_carte =  "Auvergne-Rhone-Alpes" )
  point_ajoute <- main_ajout_point(dataframe_donnees,carte_filtre,1,echantillon,"/Extraction/Spipoll_ARA_" )
  return(list(point_ajoute,carte_filtre))
}


colonne_SPIPOLL = c("id","nom_collection","lat","long","userId","flower_taxon_sc","date_de_session","heureDebut","vent","insect_taxon")
fichier_SPIPOLL = "/data_entree/data_SPIPOLL/spipoll_20220706_formated.txt"
pwd_carte = ("/RSpatial/departements-20180101.shp")
colonne_departement = c("Allier","Loire","Puy-de-Dôme","Cantal","Haute-Loire","Ardèche","Drôme","Isère","Savoie","Haute-Savoie","Ain","Rhone","Rhône" ,"Métropole de Lyon")
Echantillon =1000
result <-main_extraction(fichier_SPIPOLL,colonne_donnees = colonne_SPIPOLL , fichier_carte = pwd_carte, departements = colonne_departement, Echantillon)
sf_points <- result[[1]]
carte_filtre<- result[[2]]

#sf_points_2<-sf_points[!duplicated(paste(sf_points$lat,sf_points$long,sf_points$date_de_session)),]

