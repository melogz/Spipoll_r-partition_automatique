library(dplyr)
getwd()

one_hot <- function(df,nombre_de_colonne,carte_buffers ){
  noms_colonnes <- paste("colonne", 1:nombre_de_colonne, sep = "")
  for (nom_colonne in noms_colonnes) {
    df[[nom_colonne]] <- 0
  }
  for (i in (1:length(carte_buffers$values))){
    for (j in (1:length(unique(carte_buffers$values[[i]])))){
      for (k in (1:nombre_de_colonne)){
        if (unique(carte_buffers$values[[i]])[j] == k){
          df[[paste("colonne", k, sep = "")]][i] = table(carte_buffers$values[[i]])[[j]]*100/256 #remplacer par 1 si marche plus
        }
      }
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
  
  





