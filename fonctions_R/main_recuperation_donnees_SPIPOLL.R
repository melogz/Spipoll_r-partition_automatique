source ( paste0(getwd(),"/fonctions_R/recuperation.R"))
library(dplyr)


main_recuperation <- function(pwdfile,colonnes){
  df<- recuperation(pwdfile)
  df_filtre <-filtre(df,"df$protocole!=1","df$nb_validation==3")#pour rajouter d'autres conditions respecter la syntaxe df$condition
  df_final <- subset(df_filtre,select = (colonnes))
  return(df_final)
}

#df_laea <-main_recuperation(pwdfile ="/data_entree/data_SPIPOLL/spipoll_20220706_formated.txt" , colonnes = c("id","nom_collection","lat","long","userId","flower_taxon_sc","date_de_session","heureDebut","vent","insect_taxon") )
#class(df_laea) 

