


jour_julien <- function (df){
  df$date_de_session <- as.Date(df$date_de_session, format = "%d/%m/%Y") # ça marche 
  df$jour_julien <- format(df$date_de_session, "%j")
  return (df)
}


#df_test <- sf_obs
#df_test <- jour_julien(df_test)
