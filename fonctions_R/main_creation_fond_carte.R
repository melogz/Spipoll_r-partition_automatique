source ( paste0(getwd(),"/fonctions_R/creation_fond_carte.R"))
library(dplyr)




main_creation_carte <- function(df,pwdcarte,colonne,titre_carte){
  df_laea <- transformation_sf(df)
  depart_sf <- recuperation_fond_de_carte(pwdcarte)
  nom_depart<- nom_dep(depart_sf)
  carte_filtre <- filtre_carte(nom_depart,colonne)
  carte <- creation_carte(carte_filtre,titre_carte)
  print(carte)
  write.table(carte_filtre, file = paste0(getwd(),"/data_entree/data_carto/fond_de_carte_ARA.csv"), sep = ",", row.names = FALSE)
  return (carte_filtre)
}

#carte_filtre <-main_creation_carte(df= df_laea, pwdcarte = "/RSpatial/departements-20180101.shp", colonne = c("Allier","Loire","Puy-de-Dôme","Cantal","Haute-Loire","Ardèche","Drôme","Isère","Savoie","Haute-Savoie","Ain","Rhone","Rhône" ,"Métropole de Lyon"), titre_carte =  "Auvergne-Rhone-Alpes" )
