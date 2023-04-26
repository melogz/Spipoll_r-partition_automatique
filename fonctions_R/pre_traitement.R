
df_test <- df_test[!duplicated(paste(df_test$lat,df_test$long,df_test$date_de_session)),]