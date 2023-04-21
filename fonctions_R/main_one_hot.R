getwd()

source (paste0(getwd(),"/fonctions_R/one_hot_encoding.R"))
source (paste0(getwd(),"/fonctions_R/buffers.R"))


main_one_hot<-function(pwd_carte_occupation_sol,df,colonnes){
  buffers <- make_buffers(pwd_carte_occupation_sol,df)
  new_df <- one_hot(df,colonnes,buffers)
  new_df_2 <- simplification(new_df,colonnes)
  return(new_df_2)
}

sf_points_2 <- main_one_hot("/RSpatial/g250_clc12_V18_5.tif",sf_collection,44)
write.table(sf_points_2, file = paste0(getwd(),"/data_entree/data_SPIPOLL/point_collection_ARA_one_hot_clc.csv"),sep =';', row.names = FALSE)
sf_collection2<- st_read(paste0(getwd(),"/data_entree/data_SPIPOLL/df_collection_1_150000.csv"),sep = ';')



