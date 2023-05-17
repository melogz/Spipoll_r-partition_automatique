


pre_traitement <- function(df_col, df_obs,nom_espece){
  df_obs$insect_taxon <- iconv(df_obs$insect_taxon, from = "latin1", to = "UTF-8")
  df_col$insect_taxon <- iconv(df_col$insect_taxon, from = "latin1", to = "UTF-8")
  df_col$presence <- 0 
  test <- subset(df_obs, insect_taxon==nom_espece)
  df_donne <- test[,c("lat","long","heureDebut")]
  present <- df_col$lat %in% df_donne$lat & df_col$long %in% df_donne$long & df_col$heureDebut %in% df_donne$heureDebut
  df_col$presence[present] <- 1
  return(df_col)
}

sf_point_climat_abeille <- pre_traitement(test,sf_obs,'L_Abeille mellifère (Apis mellifera)')

write.table(sf_point_climat_abeille, file = paste0(getwd(),"/data_entree/data_SPIPOLL/point_collection_one_2000_abeille.csv"),sep =';', row.names = FALSE)
#test <- subset(sf_obs, insect_taxon=='L_Abeille mellifère (Apis mellifera)')
#df_donne <- test[,c("lat","long","heureDebut")]
# Utiliser la fonction merge() pour récupérer les lignes en fonction des valeurs de A, B et C
#nouveau_dataframe <-sf_collection[sf_collection$lat %in% df_donne$lat & sf_collection$long %in% df_donne$long & sf_collection$heureDebut %in% df_donne$heureDebut, ]
#present <- sf_collection$lat %in% df_donne$lat & sf_collection$long %in% df_donne$long & sf_collection$heureDebut %in% df_donne$heureDebut
#sf_collection$presence <- 0
#sf_collection$presence[present] <-1
#sf_obs$insect_taxon <- iconv(sf_obs$insect_taxon, from = "latin1", to = "UTF-8")
