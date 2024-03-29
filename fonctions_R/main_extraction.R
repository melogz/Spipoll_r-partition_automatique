library(dplyr)
library(rgdal)
library(sf) 
library(raster)
library(tidyverse)
getwd()
setwd("C:/Users/lmanceron/Documents/carto_mnhn")
source ( paste0(getwd(),"/fonctions_R/main_ajout_point.R"))
source ( paste0(getwd(),"/fonctions_R/main_creation_fond_carte.R"))
source ( paste0(getwd(),"/fonctions_R/main_recuperation_donnees_SPIPOLL.R"))


main_extraction <- function (fichier_donnees, colonne_donnees, fichier_carte,departements,echantillon){
  dataframe_donnees <- main_recuperation(pwd =fichier_donnees , colonnes= colonne_donnees )
  carte_filtre <-main_creation_carte(df =dataframe_donnees , pwdcarte= fichier_carte, colonne = departements, titre_carte =  "Auvergne-Rhone-Alpes" )
  observation_ajoute <- main_ajout_point_observation(dataframe_donnees,carte_filtre,1,echantillon,"/Extraction/Spipoll_ARA_" )
  collection_ajoute <- main_ajout_point_collection(observation_ajoute,carte_filtre,1,echantillon )
  return(list(collection_ajoute,observation_ajoute,carte_filtre))
}



SPIPOLL <- read.table(paste0(getwd(),"/data_entree/data_SPIPOLL/spipoll_20220706_formated.txt"),sep ="\t", header =TRUE )

france <-  main_creation_carte(sf_france,pwd_carte,toute_la_france,"Fond de carte")
allie <- main_creation_carte(sf_points,pwd_carte,c("Allier"),"Allier")
morbihan <- main_creation_carte(sf_points,pwd_carte,c("Morbihan"),"Morbihan")
meurthe  <- main_creation_carte(sf_points,pwd_carte,c("Meurthe-et-Moselle"),"Meurthe-et-Moselle")
colonne_SPIPOLL = c("id","nom_collection","lat","long","userId","flower_taxon_sc","date_de_session","heureDebut","vent","insect_taxon")
fichier_SPIPOLL = "/data_entree/data_SPIPOLL/spipoll_20220706_formated.txt"
pwd_carte = ("/RSpatial/departements-20180101.shp")
colonne_departement = c("Allier","Loire","Puy-de-Dôme","Cantal","Haute-Loire","Ardèche","Drôme","Isère","Savoie","Haute-Savoie","Ain","Rhone","Rhône" ,"Métropole de Lyon")
depart_sf<-st_read(paste0(getwd(),"/RSpatial/departements-20180101.shp"))
toute_la_france=depart_sf$nom[!(depart_sf$nom %in% c(depart_sf$nom[depart_sf$code_insee>=970]))]
Echantillon =nrow(SPIPOLL)
result <-main_extraction(fichier_donnees = fichier_SPIPOLL,colonne_donnees = colonne_SPIPOLL , fichier_carte = pwd_carte, departements = toute_la_france, Echantillon)
sf_collection <- result[[1]]
sf_obs <- result[[2]]
carte_filtre<- result[[3]]
sf_obs$insect_taxon <- iconv(sf_obs$insect_taxon, from = "latin1", to = "UTF-8")
sf_collection$insect_taxon <- iconv(sf_collection$insect_taxon, from = "latin1", to = "UTF-8")


#class(sf_collection2$geometry)
#test <- read.table(paste0(getwd(),"/data_entree/data_SPIPOLL/point_clc_climat_2000.csv"),sep=";",header= TRUE)
#sf_points_2<-sf_points[!duplicated(paste(sf_points$lat,sf_points$long,sf_points$date_de_session)),]

