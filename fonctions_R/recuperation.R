### récupération des données ###

library(dplyr)


recuperation <- function(filename){
  resultat <- read.table(paste0(getwd(),filename),sep ="\t", header =TRUE )
  return(resultat)
}

#filter les données selon des critères fixées en amont
filtre <- function(df, ...){
  critere <- list(...)
  for (i in seq_along(critere)) {
    crit <- quote(eval(parse(text = critere[i])))
    df<- filter(df,eval(crit))
  }
  #vous pouvez ajouter des criteres en respectant la meme syntaxe a$critere 
  return(df)
}

bonne_colonne <-function(df, colonnes) {
  select(df,all_of(colonnes))
}

