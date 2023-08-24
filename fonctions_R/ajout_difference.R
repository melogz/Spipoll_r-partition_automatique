library(httr)
library(jsonlite)

# Définir les informations de requête
api_key <- "59ZDY9LC44BV5NJZVWMGKUHUM" #changer la clef par la votre. 
lat <- 45.36381 # Latitude du point
lon <- 6.5149472   # Longitude du point
date <- "2023-06-20"  # Date souhaitée au format "YYYY-MM-DD"
print("https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/timeline/45.36381%2C%206.5149472/2023-06-20/2023-06-20?unitGroup=metric&include=days%2Cobs&key=59ZDY9LC44BV5NJZVWMGKUHUM&contentType=csv")
# Construire l'URL de requête avec les paramètres
url <- paste0("https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/timeline/",
               lat,"%2C%20", lon,'/',date,'/',date,'?',
              "unitGroup=metric&include=days%2Cobs&key=",api_key,"&contentType=csv")

print(url=="https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/timeline/45.36381%2C%206.5149472/2023-06-20/2023-06-20?unitGroup=metric&include=days%2Cobs&key=59ZDY9LC44BV5NJZVWMGKUHUM&contentType=csv")
# Envoyer la requête à l'API OpenWeatherMap
response <- GET(url)

# Vérifier le statut de la réponse
if (status_code(response) == 200) {
  # Convertir la réponse JSON en dataframe
  download.file(url, destfile = "/Users/lmanceron/Documents/carto_mnhn/data_entree/data_SPIPOLL/weather_data.csv", mode = "wb")
  test_down<-read.csv("/Users/lmanceron/Documents/carto_mnhn/data_entree/data_SPIPOLL/weather_data.csv", sep =",")
  
  # Afficher les données météo
  print(data)
} else {
  # Afficher un message d'erreur en cas de problème avec la requête
  print("Erreur lors de la récupération des données météo.")
}

as.numeric(format(as.Date(sf_point_climat_entrainement$date_de_session[[1]]),"%Y"))
class(test)
premiertest<- test[1,]
class(premiertest)

### TEST pour récupération des jours ###
fr_border <- sf::st_as_sf(raster::getData("GADM", country = "FRA", level = 0))

pluie <- recup_meteo (sf_collection_climat,paste0(getwd(),"/rr_ens_mean_0.1deg_reg_2011-2022_v27.0e.nc"),"precipitation")
sf_collection_climat <- transformation_sf(sf_collection_climat)
nv_df$vent<-test$temperature_moy
fleur<-read.csv(paste0(getwd(),"/data_entree/data_SPIPOLL/famille_fleur.csv"), sep =";")
fleur_collection <-  fleur[!duplicated(paste(fleur$lat,fleur$long,fleur$heureDebut)),]

result <- anti_join(test, df, by = "id")
result2 <- anti_join(test, result, by = "id")
rm(result)

df$flower_taxon_sc <- result2$flower_taxon_sc
recup_meteo <- function (data,fichier, variable){ 
  df <- data
  nv_df <- data.frame()
  for (i in (1:length(unique(df$annee)))){
    filtre_data <-filtre(df,paste("df$annee==",unique(df$annee)[i] ))
    climate_data_2019 <- extract_nc_value(first_year = unique(df$annee)[i], 
                                          last_year = unique(df$annee)[i],
                                          local_file = TRUE,
                                          file_path = fichier,
                                          sml_chunk = "2011-2022",
                                          spatial_extent = fr_border,
                                          clim_variable = variable,
                                          statistic = "mean",
                                          grid_size = 0.1,
                                          ecad_v = NULL,
                                          write_raster = TRUE,
                                          out = "raster_mean_temp_year.grd",
                                          return_data = TRUE)
    rbk_2011 = raster::brick("raster_mean_temp_year.grd")
    filtre_data <- st_transform(filtre_data, crs = st_crs(rbk_2011))
    for (j in (1:nrow(filtre_data))){
      day <- filtre_data[j,]
      daily_min_temp_pnts = temporal_aggregate(x = rbk_2011,
                                               y = day,
                                               variable_name = "mean temp",
                                               time_step = "daily")
      present <- day$jour==as.numeric(daily_min_temp_pnts$day) & day$mois == as.numeric(daily_min_temp_pnts$month) 
      filtre_data$temperature_moy[[j]] <- as.numeric(daily_min_temp_pnts$mean_mean_temp[present])
    }
    nv_df <- rbind(nv_df,filtre_data)
  }
  return(nv_df)
  }
class(nv_df$vent)
pluie$temperature_moy <- as.numeric(pluie$temperature_moy)
nv_df$temperature_moy <- as.numeric(nv_df$temperature_moy)
write.table(nv_df, file = paste0(getwd(),"/data_entree/data_SPIPOLL/entrainement_collection_temp_moyenne.csv"),sep =';', row.names = FALSE)
write.table(test, file = paste0(getwd(),"/data_entree/data_SPIPOLL/entrainement_collection_vitesse_vent.csv"),sep =';', row.names = FALSE)
write.table(pluie, file = paste0(getwd(),"/data_entree/data_SPIPOLL/entrainement_collection_precipitation.csv"),sep =';', row.names = FALSE)

df$temp_moyenne <- nv_df$temperature_moy
df$prep <- pluie$temperature_moy
df$wind <- test$temperature_moy

nv_df <- data.frame()
for (i in (3:length(unique(df$annee)))){
  filtre_data <-filtre(df,paste("df$annee==",unique(df$annee)[i] ))
  climate_data_2019 <- extract_nc_value(first_year = unique(df$annee)[i], 
                                        last_year = unique(df$annee)[i],
                                        local_file = TRUE,
                                        file_path = paste0(getwd(),"/rr_ens_mean_0.1deg_reg_2011-2022_v27.0e.nc"),
                                        sml_chunk = "2011-2022",
                                        spatial_extent = fr_border,
                                        clim_variable = "wind speed",
                                        statistic = "mean",
                                        grid_size = 0.1,
                                        ecad_v = NULL,
                                        write_raster = TRUE,
                                        out = "raster_mean_temp_year.grd",
                                        return_data = TRUE)
  rbk_2011 = raster::brick("raster_mean_temp_year.grd")
  filtre_data <- st_transform(filtre_data, crs = st_crs(rbk_2011))
  for (j in (1:nrow(filtre_data))){
    day <- filtre_data[j,]
    daily_min_temp_pnts = temporal_aggregate(x = rbk_2011,
                                             y = day,
                                             variable_name = "mean temp",
                                             time_step = "daily")
    present <- day$jour==as.numeric(daily_min_temp_pnts$day) & day$mois == as.numeric(daily_min_temp_pnts$month) 
    filtre_data$temperature_moy[[j]] <- as.numeric(daily_min_temp_pnts$mean_mean_temp[present])
  }
  nv_df <- rbind(nv_df,filtre_data)
}


unique(sf_collection_climat$annee)
filtre_2018 <- filtre(sf_point_climat,"sf_collection_climat$annee==2018")

climate_data_2019 <- extract_nc_value(first_year = 2018, 
                                      last_year = 2018,
                                      local_file = TRUE,
                                      file_path = paste0(getwd(),"/rr_ens_mean_0.1deg_reg_2011-2022_v27.0e.nc"),
                                      sml_chunk = "2011-2022",
                                      spatial_extent = fr_border,
                                      clim_variable = "precipitation",
                                      statistic = "mean",
                                      grid_size = 0.1,
                                      ecad_v = NULL,
                                      write_raster = TRUE,
                                      out = "raster_min_temp_2018.grd",
                                      return_data = TRUE)
filtre_test <- sf_collection_climat[1:5,]


rbk_2011 = raster::brick("raster_min_temp_2018.grd")
filtre_test <- st_transform(filtre_test, crs = st_crs(rbk_2011))

for (i in (1:nrow(filtre_test))){
  day <- filtre_test[1,]
  daily_min_temp_pnts = temporal_aggregate(x = rbk_2011,
                                           y = day,
                                           variable_name = "min temp",
                                           time_step = "daily")
  present <- day$jour==as.numeric(daily_min_temp_pnts$day) & day$mois == as.numeric(daily_min_temp_pnts$month) 
  filtre_test$temperature_moy[[i]] <- daily_min_temp_pnts$mean_min_temp[present]
}

