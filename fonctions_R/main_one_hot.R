getwd()

source (paste0(getwd(),"/fonctions_R/one_hot_encoding.R"))
source (paste0(getwd(),"/fonctions_R/buffers.R"))


main_one_hot<-function(pwd_carte_occupation_sol,df,colonnes){
  buffers <- make_buffers(pwd_carte_occupation_sol,df)
  new_df <- one_hot(df,colonnes,buffers)
  new_df_2 <- simplification(new_df,colonnes)
  return(new_df_2)
}

sf_points_2 <- main_one_hot("/RSpatial/g250_clc12_V18_5.tif",sf_points,44)
