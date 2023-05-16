source ( paste0(getwd(),"/fonctions_R/main_one_hot.R"))
source ( paste0(getwd(),"/fonctions_R/jour_julien.R"))
getwd()

elaboration <- function (df,pwd_carte,colonnes,taille_buffers,carte_region,numero_region){
  df <-jour_julien(df)
  sf_point_climat <- main_one_hot(pwd_carte,df,colonnes,taille_buffers,carte_region,numero_region)
  return(sf_point_climat)
}
climat <- function(){
  source ( paste0(getwd(),"/fonctions_R/ajout_climat.R"))
}



donnees <- function (df){
  
}

clc_raster <-"/RSpatial/g250_clc12_V18_5.tif"
region <- paste0(getwd(),"/carte_region/regions_2015_metropole_region.shp")

sf_climat <- elaboration(sf_collection,clc_raster,44,2000,region,3)
sf_collection_test  <-jour_julien(sf_collection)
sf_point_climat_test <- main_one_hot(clc_raster,sf_collection_test,44,2000,region,3)
