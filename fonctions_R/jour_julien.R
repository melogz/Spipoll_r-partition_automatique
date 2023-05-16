
jour_julien <- function (df){
  df$date_de_session <- as.Date(df$date_de_session, format = "%d/%m/%Y") # ça marche 
  df$jour_julien <- format(df$date_de_session, "%j")
  return (df)
}

#sf_collection_jj <- jour_julien(sf_collection)
