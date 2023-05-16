getwd()

source (paste0(getwd(),"/fonctions_R/one_hot_encoding.R"))
source (paste0(getwd(),"/fonctions_R/buffers.R"))


main_one_hot<-function(pwd_carte_occupation_sol,df,colonnes,taille_buffers,carte_region,numero_region){
  if (tools::file_ext(pwd_carte_occupation_sol) == "tif"){
    buffers <- make_buffers_raster(pwd_carte_occupation_sol,df,taille_buffers)
    new_df <- new_one_hot(df,colonnes,buffers,taille_buffers)
  }
  else {
    buffers <- make_buffers_vector(pwd_carte_occupation_sol,df,carte_region,numero_region,taille_buffers)
    new_df <- new_one_hot_vector(df,colonnes,buffers,taille_buffers)
  }
  #new_df_2 <- simplification(new_df,colonnes)
  return(new_df)
}


#region <- paste0(getwd(),"/carte_region/regions_2015_metropole_region.shp")
#buffers_2000_vector <- make_buffers_vector(clc_vector,sf_collection2,region,3,2000)
#one_hot_collection_2000_vector <- new_one_hot_vector(sf_collection2,44,buffers_2000_vector,2000)

#tools::file_ext("/RSpatial/g250_clc12_V18_5.tif")
#clc <- raster(paste0(getwd(),"/RSpatial/g250_clc12_V18_5.tif"))
#sf_points_2 <- main_one_hot("/RSpatial/g250_clc12_V18_5.tif",sf_collection2,44)
#write.table(sf_points_2, file = paste0(getwd(),"/data_entree/data_SPIPOLL/point_collection_ARA_one_hot_clc.csv"),sep =';', row.names = FALSE)
#sf_collection2<- st_read(paste0(getwd(),"/data_entree/data_SPIPOLL/df_collection_1_150000.csv"),sep = ';')



#new_df3 <- new_one_hot(sf_collection2,44,buffers_2000)
