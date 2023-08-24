
jour_julien <- function (df){
  df$date_de_session <- as.Date(df$date_de_session, format = "%Y-%m-%d") # ça marche 
  df$jour_julien <- as.numeric(format(df$date_de_session, "%j"))
  df$jour <- as.numeric(format(df$date_de_session, "%d"))
  df$mois <- as.numeric(format(df$date_de_session, "%m"))
  df$annee <- as.numeric(format(df$date_de_session, "%Y"))
  df$quinzaine <- ceiling(week(as.Date(df$date_de_session))/2)
  return (df)
}

sf_collection_climat <- jour_julien(sf_collection_climat)
as.numeric(format(as.Date(2019-06-12), "%j"))
sf_point_climat <- jour_julien(sf_point_climat)
sf_collection_climat <- sf_point_climat_entrainement[,c(1,7)]
sf_collection_climat <- jour_julien(sf_point_climat_entrainement)
format(2019-06-12, "%j")


