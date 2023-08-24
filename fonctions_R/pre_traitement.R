
pre_traitement <- function(df_col, df_obs,nom_espece){
  df_col$presence <- 0 
  test <- subset(df_obs, insect_taxon==nom_espece)
  df_donne <- test[,c("lat","long","heureDebut")]
  present <- df_col$lat %in% df_donne$lat & df_col$long %in% df_donne$long & df_col$heureDebut %in% df_donne$heureDebut
  df_col$presence[present] <- 1
  return(df_col)
}

#sf_point_climat_abeille <- pre_traitement(sf_point_climat,sf_obs,'L_Abeille mellifère (Apis mellifera)')
#sf_point_climat_Syrphe <- pre_traitement(sf_point_climat,sf_obs,'Le Syrphe ceinturé (Episyrphus balteatus)')
#sf_point_climat_test <- pre_traitement(sf_point_climat,sf_obs,'Les Bourdons noirs à bande(s) jaune(s) et cul blanc (Bombus)')
#write.table(sf_point_climat_abeille, file = paste0(getwd(),"/data_entree/data_SPIPOLL/point_collection_one_2000_abeille.csv"),sep =';', row.names = FALSE)


#sf_collection_test <- sf_collection
#sf_collection_test$presence <- 0
#test <- subset(sf_obs, insect_taxon=='Le Syrphe ceinturé (Episyrphus balteatus)')
#df_donne <- test[,c("lat","long","heureDebut")]
# Utiliser la fonction merge() pour récupérer les lignes en fonction des valeurs de A, B et C
#nouveau_dataframe <-sf_collection[sf_collection$lat %in% df_donne$lat & sf_collection$long %in% df_donne$long & sf_collection$heureDebut %in% df_donne$heureDebut, ]
#present <- sf_collection_test$lat %in% df_donne$lat & sf_collection_test$long %in% df_donne$long & sf_collection_test$heureDebut %in% df_donne$heureDebut
#sf_collection_test$presence[present] <- 1

#sf_collection$presence <- 0
#sf_collection$presence[present] <-1
#sf_obs$insect_taxon <- iconv(sf_obs$insect_taxon, from = "latin1", to = "UTF-8")
