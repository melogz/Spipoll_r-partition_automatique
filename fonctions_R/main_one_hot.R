getwd()

source (paste0(getwd(),"/fonctions_R/one_hot_encoding.R"))
source (paste0(getwd(),"/fonctions_R/buffers.R"))


main_one_hot<-function(pwd_carte_occupation_sol,df,colonnes,taille_grand_buffer,taille_moyen_buffer){
  if (tools::file_ext(pwd_carte_occupation_sol) == "tif"){
    grand_buffers <- make_buffers_raster(pwd_carte_occupation_sol,df,taille_grand_buffer)
    grand_df <- new_one_hot_raster(df,colonnes,grand_buffers,taille_grand_buffer)
    moyen_buffers <- sous_buffer(grand_buffers,pwd_carte_occupation_sol,df,taille_moyen_buffer)
    moyen_df <- new_one_hot_raster(df,colonnes,moyen_buffers,taille_moyen_buffer)
    df_1 <- cbind(moyen_df,grand_df)
    df_2 <- position(df,pwd_carte_occupation_sol)
    new_df<- cbind(df_2,df_1)
  }
  else {
    grand_buffers <- new_make_buffers_vector(pwd_carte_occupation_sol,df,taille_grand_buffer)
    grand_df <- new_one_hot_vector(df,colonnes,grand_buffers,taille_grand_buffer)
    moyen_buffers <- new_make_buffers_vector(pwd_carte_occupation_sol,df,taille_moyen_buffer)
    moyen_df <- new_one_hot_vector(df,colonnes,moyen_buffers,taille_moyen_buffer)
    position_df <- position_vector(df,pwd_carte_occupation_sol)
    new_df <- cbind(position_df,moyen_df[,ncol(df)+1:44],grand_df[,ncol(df)+1:44])
  }
  #new_df_2 <- simplification(new_df,colonnes)
  return(new_df)
}

main_one_hot(clc_vector,sf_points,44,1000,250)
#region <- paste0(getwd(),"/carte_region/regions_2015_metropole_region.shp")
buffers_2000_vector <- new_make_buffers_vector(clc_vector,sf_points[1,],1000)

one_hot_collection_2000_vector <- new_one_hot_vector(sf_points[1,],44,buffers_2000_vector,1000)

buffers_500_vector <- new_make_buffers_vector(clc_vector,sf_collection[1:10,],250)

one_hot_collection_500_vector <- new_one_hot_vector(sf_collection[1:10,],44,buffers_500_vector,250)

position_df <- position_vector(sf_collection[1:10,],clc_vector)
df_1 <- cbind(position_df,one_hot_collection_500_vector[,ncol(sf_collection)+1:44],one_hot_collection_2000_vector[,ncol(sf_collection)+1:44])
#tools::file_ext("/RSpatial/g250_clc12_V18_5.tif")
#clc <- raster(paste0(getwd(),"/RSpatial/g250_clc12_V18_5.tif"))
sf_points_2 <- main_one_hot(clc_vector,sf_collection,44,1000,250)
#write.table(sf_points_2, file = paste0(getwd(),"/data_entree/data_SPIPOLL/point_collection_ARA_one_hot_clc.csv"),sep =';', row.names = FALSE)
#sf_collection2<- st_read(paste0(getwd(),"/data_entree/data_SPIPOLL/df_collection_1_150000.csv"),sep = ';')
#region <- paste0(getwd(),"/carte_region/regions_2015_metropole_region.shp")
#test <- main_one_hot("/RSpatial/g250_clc12_V18_5.tif",sf_collection[1:10,],44,2000,500,region,3)

#new_df3 <- new_one_hot(sf_collection2,44,buffers_2000)
#grand_buffers <- make_buffers_raster("/RSpatial/g250_clc12_V18_5.tif",sf_collection[1:10,],2000)
#grand_df <- new_one_hot(sf_collection[1:10,],44,grand_buffers,2000)
#moyen_buffers <- sous_buffer(grand_buffers,"/RSpatial/g250_clc12_V18_5.tif",sf_collection[1:10,],500)
#moyen_df <- new_one_hot(sf_collection[1:10,],44,moyen_buffers,500)
#new_df <- cbind(grand_df,moyen_df)
#new2_df <- position(sf_collection[1:10,],"/RSpatial/g250_clc12_V18_5.tif")
#new_df <- cbind(new2_df,new_df)
