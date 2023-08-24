source ( paste0(getwd(),"/fonctions_R/main_one_hot.R"))
source ( paste0(getwd(),"/fonctions_R/jour_julien.R"))
source (paste0(getwd(),"/fonctions_R/pre_traitement.R"))
getwd()

elaboration <- function (df,pwd_carte,colonnes,taille_grand_buffer,taille_moyen_buffer,carte_region,numero_region){
  df <-jour_julien(df)
  sf_point_climat <- main_one_hot(pwd_carte,df,colonnes,taille_grand_buffer,taille_moyen_buffer,carte_region,numero_region)
  return(sf_point_climat)
}
climat <- function(){
  source ( paste0(getwd(),"/fonctions_R/ajout_climat.R"))
}


clc_raster <-"/RSpatial/g250_clc12_V18_5.tif"
region <- paste0(getwd(),"/carte_region/regions_2015_metropole_region.shp")

sf_climat <- elaboration(sf_collection,clc_raster,44,1000,250,region,3)
climat()


write.table(sf_point_climat_abeille, file = paste0(getwd(),"/data_entree/data_SPIPOLL/point_collection_one_2000_abeille.csv"),sep =';', row.names = FALSE)