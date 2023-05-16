library(dplyr)
getwd()

one_hot <- function(df,nombre_de_colonne,carte_buffers ){
  noms_colonnes <- paste("colonne", 1:nombre_de_colonne,"_", sep = "")
  for (nom_colonne in noms_colonnes) {
    df[[nom_colonne]] <- 0
  }
  for (i in (1:nrow(carte_buffers))){ # on regarde le nombre de buffers qu'on a 
    for (j in (1:length(unique(carte_buffers$values[[i]])))){ #on lit les valeurs d'occupation des sols
      for (k in (1:nombre_de_colonne)){
        if (unique(carte_buffers$values[[i]])[j] == k){
          df[[paste("colonne", k,"_", sep = "")]][i] = table(carte_buffers$values[[i]])[[j]]*100/length(carte_buffers$values[[i]])  #remplacer par 1 si marche plus
        }
      }
    }
  }
  return(df)
}

new_one_hot <-  function(df,nombre_de_colonne,carte_buffers,taille_buffer ){
  noms_colonnes <- paste("colonne", 1:nombre_de_colonne,"_",taille_buffer, sep = "")
  new_df <- data.frame(matrix(0,nrow= nrow(df),ncol=nombre_de_colonne))
  names(new_df) <- noms_colonnes
  df_1 <- cbind(df,new_df)
  for (i in (1:nrow(carte_buffers))){
    for (j in (1:length(unique(carte_buffers$values[[i]])))){
      df_1[[paste("colonne", unique(buffers_test$values[[i]])[[j]],"_",taille_buffer, sep = "")]][i] = table(carte_buffers$values[[i]])[[j]]*100/length(carte_buffers$values[[i]])
    }
  }
  return(df_1)
}

new_one_hot_vector<- function (df,nombre_de_colonne,carte_buffers,taille_buffer ){
  noms= c(111,112,121,122,123,124,131,132,133,141,142,211,212,213,221,222,223,231,241,242,243,244,311,312,313,321,322,323,324,331,332,333,334,335,411,412,421,422,423,511,512,521,522,523)
  for (i in noms) {
    nom_colonne <- paste("colonne", i,"_",taille_buffer, sep = "")
    df[[nom_colonne]] <- 0
  }
  for (i in (unique(carte_buffers$id))){
    carte_subset <- subset(carte_buffers, carte_buffers$id == i)
    aire <- sum(carte_subset$AREA_HA)
    for (k in (1:nrow(carte_subset))){
      df[[paste("colonne",carte_subset$CODE_12[k],"_",taille_buffer, sep = "")]][i] = 100*carte_subset$AREA_HA[k]/aire
    }
  } 
  return(df)
}

simplification <- function (df,nombre_de_colonne){
  df <- subset(df, select = -presence)
  cols_to_remove <- c()
  for (i in (1:nombre_de_colonne)) {
    colonne <- paste("colonne", i, sep = "")
    if (all(df[[colonne]]==0)){
      cols_to_remove<- c(cols_to_remove,(which(names(df) == colonne)))
    }
  }
  cols_to_remove
  df <- df[,-cols_to_remove]
  write.csv(df, file =paste0(getwd(), "/Extraction/donnees_SPIPOLL_one_hot.csv"), row.names = FALSE)
  return(df)
}

#test <- data.frame(sf_collection2)
#noms= c(111,112,121,122,123,124,131,132,133,141,142,211,212,213,221,222,223,231,241,242,243,244,311,312,313,321,322,323,324,331,332,333,334,335,411,412,421,422,423,511,512,521,522,523)
#for (i in noms) {
 # nom_colonne <- paste("colonne", i,"_",2000, sep = "")
#  test[[nom_colonne]] <- 0
#}
#old_hot_collection_2000_raster <- one_hot(sf_collection2,44,buffers_2000)  
#one_hot_collection_2000_raster <- new_one_hot(sf_collection2,44,buffers_2000,2000)
#one_hot_collection_2000_vector <- new_one_hot_vector(sf_collection2,44,buffers_2000_vector,2000)
#noms_colonnes <- paste("colonne", 1:44,"_",2000, sep = "")
#new_df <- data.frame(matrix(0,nrow= nrow(sf_collection2),ncol=44))
#names(new_df) <- noms_colonnes
#df_1 <- cbind(sf_collection2,new_df)
#carte_subset$CODE_12[1]
#paste("colonne", carte_subset_1$CODE_12[1], sep = "")
#carte_subset_1 <- subset(buffers_2000_vector,buffers_2000_vector$id == 1)
#carte_subset_11 <- subset(buffers_2000,buffers_2000$id == 1)
#carte_subset_2 <- subset(buffers_2000_vector,buffers_2000_vector$id == 2)
#unique(buffers_2000_vector$CODE_12)
#table(carte_subset_11$values)
#aire <- sum(carte_subset$AREA_HA)
#for (k in (1:nrow(carte_subset_1))){
#  test[[paste("colonne", carte_subset_1$CODE_12[k],"_",2000, sep = "")]][1] = 100*carte_subset_1$AREA_HA[k]/aire
#}


#for (i in (unique(buffers_2000_vector$id))){
#  carte_subset <- subset(buffers_2000_vector,buffers_2000_vector$id == i)
#  aire <- sum(carte_subset$AREA_HA)
#  for (k in (1:nrow(carte_subset))){
#    df_1[[paste("colonne", carte_subset$CODE_12[k], sep = "")]][i] = 100*carte_subset$AREA_HA[k]/aire + df_1[[paste("colonne", carte_subset$CODE_12[k], sep = "")]][i]
#  }
#} 
