library(dplyr)
getwd()

one_hot <- function(df,nombre_de_colonne,carte_buffers ){
  noms_colonnes <- paste("colonne", 1:nombre_de_colonne,"_",taille_buffer, sep = "")
  for (nom_colonne in noms_colonnes) {
    df[[nom_colonne]] <- 0
  }
  for (i in (1:nrow(carte_buffers))){ # on regarde le nombre de buffers qu'on a 
    for (j in (1:length(unique(carte_buffers$values[[i]])))){ #on lit les valeurs d'occupation des sols
      for (k in (1:nombre_de_colonne)){
        if (unique(carte_buffers$values[[i]])[j] == k){
          df[[paste("colonne", k,"_",taille_buffer, sep = "")]][i] = table(carte_buffers$values[[i]])[[j]]*100/length(carte_buffers$values[[i]])  #remplacer par 1 si marche plus
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
      df_1[[paste("colonne", carte_buffers$CODE_12[k],"_",taille_buffer, sep = "")]][i] = table(carte_buffers$values[[i]])[[j]]*100/length(carte_buffers$values[[i]])
    }
  }
  return(df_1)
}

new_one_hot_vector<- function (df,nombre_de_colonne,carte_buffers,taille_buffer ){
  noms_colonnes <- paste("colonne", 1:nombre_de_colonne,"_",taille_buffer, sep = "")
  new_df <- data.frame(matrix(0,nrow= nrow(df),ncol=nombre_de_colonne))
  names(new_df) <- noms_colonnes
  df_1 <- cbind(df,new_df)
  for (i in (1:unique(carte_buffers$id))){
    carte_subset <- subset(carte_buffers, carte_buffers$id == i)
    aire <- sum(carte_subset$AREA_HA)
    for (k in (1:nrow(carte_subset))){
      df_1[[paste("colonne",carte_buffers$CODE_12[k],"_",taille_buffer, sep = "")]][i] = 100*carte_subset$AREA_HA[k]/aire + +  df_1[[paste("colonne", carte_buffers$CODE_12[k],"_",2000, sep = "")]][i]
    }
  } 
  return(df_1)
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
  
one_hot_collection_2000_raster <- new_one_hot(sf_collection2,44,buffers_2000,2000)

paste("colonne",carte_bu$CODE_12[1],"_",taille_buffer, sep = "")
noms_colonnes <- paste("colonne", 1:44,"_",2000, sep = "")
new_df <- data.frame(matrix(0,nrow= nrow(sf_collection2),ncol=44))
names(new_df) <- noms_colonnes
df_1 <- cbind(sf_collection2,new_df)


for (i in (1:unique(buffer$id))){
  carte_subset <- subset(buffer,buffers$id == i)
  aire <- sum(carte_subset$AREA_HA)
  for (k in (1:nrow(carte_subset))){
    df_1[[paste("colonne", substr(buffer$CODE_12[k], start = 1, stop = 2),"_",2000, sep = "")]][i] = 100*carte_subset$AREA_HA[k]/aire
  }
} 
