library(climateExtract)
library(dismo)
library(gtools)

ecad_version
# liste des varaibles : clim_variable : "mean temp", "min temp","max temp","precipitation","sea level pressure","relative humidity","global radiation"

set.seed(42876) 
fr_border <- sf::st_as_sf(raster::getData("GADM", country = "FRA", level = 0))

#pour 10 ans 2001-2010, on récupère les différentes info qu'on veut 

# on récupère la moyenne la plus basse de chaque mois
# Ensuite les températures min 

climate_data_2001_2010 <- extract_nc_value(first_year = 2001, 
                                           last_year = 2010,
                                           local_file = TRUE,
                                           file_path = paste0(getwd(),"/tn_ens_mean_0.1deg_reg_1995-2010_v27.0e.nc"),
                                           sml_chunk = "1995-2010",
                                           spatial_extent = fr_border,
                                           clim_variable = "min temp", # on récupère ici les valeurs min
                                           statistic = "mean",
                                           grid_size = 0.1,
                                           ecad_v = 27.0,
                                           write_raster = TRUE,
                                           out = "raster_min_temp_2001_2010.grd",
                                           return_data = TRUE)

# on le transforme en raster pour pouvoir localiser les points et extraire les valeurs d'interet 
rbk_2001 = raster::brick("raster_min_temp_2001_2010.grd")
sf_point_climat <- transformation_sf(sf_point_climat_entrainement)
sf_point_climat <- st_transform(sf_point_climat, crs = st_crs(rbk_2001))
st_crs(rbk_2001)== st_crs(sf_points_ajout)

# on récupère la moyenne de chaque année pour avoir le bon nombre de ligne
annual_min_temp_pnts = temporal_aggregate(x = rbk_2001,
                                          y = sf_point_climat,
                                          agg_function = "mean",
                                          variable_name = "min temp",
                                          time_step = "annual")

# on récupère la moyenne de chaque mois 
monthly_min_temp_pnts = temporal_aggregate(x = rbk_2001,
                                           y = sf_point_climat,
                                           agg_function = "mean",
                                           variable_name = "min temp",
                                           time_step = "monthly")

# on extrait pour les années 2011-2021 la température min
climate_data_2011_2021 <- extract_nc_value(first_year = 2011, 
                                           last_year = 2021,
                                           local_file = TRUE,
                                           file_path = paste0(getwd(),"/tn_ens_mean_0.1deg_reg_2011-2022_v27.0e.nc"),
                                           sml_chunk = "2011-2021",
                                           spatial_extent = fr_border,
                                           clim_variable = "min temp",
                                           statistic = "mean",
                                           grid_size = 0.1,
                                           ecad_v = 27.0,
                                           write_raster = TRUE,
                                           out = "raster_min_temp_2011_2021.grd",
                                           return_data = TRUE)


rbk_2011 = raster::brick("raster_min_temp_2011_2021.grd")
#format(object.size(climate_data_2011_2021), "MB")
#format(object.size(rbk_2011), "MB")
st_crs(rbk_2011)== st_crs(sf_point_climat)



# on récupère la moyenne de chaque année pour avoir le bon nombre de ligne
annual_min_temp_2011_pnts = temporal_aggregate(x = rbk_2011,
                                               y = sf_point_climat,
                                               agg_function = "mean",
                                               variable_name = "min temp",
                                               time_step = "annual")

annual_min_temp_pnts <-rbind(annual_min_temp_pnts,annual_min_temp_2011_pnts)
annual_min_temp_pnts <-annual_min_temp_pnts[mixedorder(annual_min_temp_pnts$site),]

monthly_min_temp_pnts_2011 = temporal_aggregate(x = rbk_2011,
                                                y = sf_point_climat,
                                                agg_function = "mean",
                                                variable_name = "min temp",
                                                time_step = "monthly")



monthly_min_temp_pnts <-rbind(monthly_min_temp_pnts,monthly_min_temp_pnts_2011)
monthly_min_temp_pnts <- monthly_min_temp_pnts[mixedorder(monthly_min_temp_pnts$site),]

#Enfin on récupère la moyenne max de chaque mois. 

climate_data_2001_2010 <- extract_nc_value(first_year = 2001, 
                                           last_year = 2010,
                                           local_file = TRUE,
                                           file_path = paste0(getwd(),"/tx_ens_mean_0.1deg_reg_1995-2010_v27.0e.nc"),
                                           sml_chunk = "1995-2010",
                                           spatial_extent = fr_border,
                                           clim_variable = "max temp", # on récupère ici les valeurs min
                                           statistic = "mean",
                                           grid_size = 0.1,
                                           ecad_v = 27.0,
                                           write_raster = TRUE,
                                           out = "raster_max_temp_2001_2010.grd",
                                           return_data = TRUE)

# on le transforme en raster pour pouvoir localiser les points et extraire les valeurs d'interet 
rbk_2001 = raster::brick("raster_max_temp_2001_2010.grd")
st_crs(rbk_2001)== st_crs(sf_point_climat)

monthly_max_temp_pnts <- temporal_aggregate(x = rbk_2001,
                                            y = sf_point_climat,
                                            agg_function = "mean",
                                            variable_name = "max temp",
                                            time_step = "monthly")

# on extrait pour les années 2011-2021 la température max
climate_data_2011_2021 <- extract_nc_value(first_year = 2011, 
                                           last_year = 2021,
                                           local_file = TRUE,
                                           file_path = paste0(getwd(),"/tx_ens_mean_0.1deg_reg_2011-2022_v27.0e.nc"),
                                           sml_chunk = "2011-2021",
                                           spatial_extent = fr_border,
                                           clim_variable = "max temp",
                                           statistic = "mean",
                                           grid_size = 0.1,
                                           ecad_v = 27.0,
                                           write_raster = TRUE,
                                           out = "raster_max_temp_2011_2021.grd",
                                           return_data = TRUE)


rbk_2011 = raster::brick("raster_max_temp_2011_2021.grd")


monthly_max_temp_pnts_2011 = temporal_aggregate(x = rbk_2011,
                                                y = sf_point_climat,
                                                agg_function = "mean",
                                                variable_name = "max temp",
                                                time_step = "monthly")

monthly_max_temp_pnts <-rbind(monthly_max_temp_pnts,monthly_max_temp_pnts_2011)
monthly_max_temp_pnts <- monthly_max_temp_pnts[mixedorder(monthly_max_temp_pnts$site),]


# on les met tous dans un dataframe

df_climat <- data.frame(site = 1:nrow(monthly_min_temp_pnts))
df_climat$site <- monthly_min_temp_pnts$site
df_climat$year <- monthly_min_temp_pnts$year
df_climat$min_avg <-  monthly_min_temp_pnts$mean_min_temp
df_climat$max_avg <-  monthly_max_temp_pnts$mean_max_temp


# on récupère les précipitations journalières pour les années 2001-2010
climate_data_2001_2010_precipitation <- extract_nc_value(first_year = 2001, 
                                           last_year = 2010,
                                           local_file = TRUE,
                                           file_path = paste0(getwd(),"/rr_ens_mean_0.1deg_reg_1995-2010_v27.0e.nc"),
                                           sml_chunk = "1995-2010",
                                           spatial_extent = fr_border,
                                           clim_variable = "precipitation",
                                           statistic = "mean",
                                           grid_size = 0.1,
                                           ecad_v = 27.0,
                                           write_raster = TRUE,
                                           out = "raster_precipitation_2001_2010.grd",
                                           return_data = TRUE)

rbk_2001_precipitation = raster::brick("raster_precipitation_2001_2010.grd")

# on récupère la moyenne des précipitations de chaque mois 
monthly_avg_prep_pnts = temporal_aggregate(x = rbk_2001_precipitation,
                                           y = sf_point_climat,
                                           agg_function = "mean",
                                           variable_name = "prep",
                                           time_step = "monthly")



# on récupère les précipitations journalières pour les années 2011-2021
climate_data_2011_2021_precipitation <- extract_nc_value(first_year = 2011, 
                                           last_year = 2021,
                                           local_file = TRUE,
                                           file_path = paste0(getwd(),"/rr_ens_mean_0.1deg_reg_2011-2022_v27.0e.nc"),
                                           sml_chunk = "2011-2021",
                                           spatial_extent = fr_border,
                                           clim_variable = "precipitation",
                                           statistic = "mean",
                                           grid_size = 0.1,
                                           ecad_v = 27.0,
                                           write_raster = TRUE,
                                           out = "raster_precipitation_2011_2021.grd",
                                           return_data = TRUE)

rbk_precipitation = raster::brick("raster_precipitation_2011_2021.grd")

# on récupère la moyenne des précipitations de chaque mois 
monthly_avg_prep_pnts_2011 = temporal_aggregate(x = rbk_precipitation,
                                           y = sf_point_climat,
                                           agg_function = "mean",
                                           variable_name = "prep",
                                           time_step = "monthly")

monthly_avg_prep_pnts <-rbind(monthly_avg_prep_pnts,monthly_avg_prep_pnts_2011)
monthly_avg_prep_pnts <- monthly_avg_prep_pnts[mixedorder(monthly_avg_prep_pnts$site),]


df_climat$prep <-  monthly_avg_prep_pnts$mean_prep

#création des biovars 
df_climat_annuel <- df_climat %>% 
  group_by(site, year) %>%
  do(data.frame(biovars(.$prep, .$min_avg, .$max_avg)))

df_climat_annuel$Digits <- as.numeric(sub("site_(\\d+)", "\\1", df_climat_annuel$site))
annuel_test <- df_climat_annuel[order(df_climat_annuel$Digits), ]


on_va_voir <- annual_min_temp_pnts %>% 
  group_by(site) %>%
  do(data.frame(mean(.$mean_min_temp)))
on_va_voir_test <- on_va_voir[mixedorder(on_va_voir$site),]

on_va_voir$Digits <- as.numeric(sub("site_(\\d+)", "\\1", on_va_voir$site))
on_va_voir_test <- on_va_voir[order(on_va_voir$Digits), ]



on_va_voir_mensuel <- monthly_min_temp_pnts %>% 
  group_by(site, year) %>%
  do(data.frame(mean(.$mean_min_temp)))
on_va_voir_test <- on_va_voir[mixedorder(on_va_voir$site),]

on_va_voir_mensuel$Digits <- as.numeric(sub("site_(\\d+)", "\\1", on_va_voir_mensuel$site))
on_va_voir_mensuel_test <- on_va_voir_mensuel[order(on_va_voir_mensuel$Digits), ]



test_moyenne2  <- annuel_test %>%
  group_by(site) %>%
  summarize(Moyenne_bio1 = mean(bio1),
            Moyenne_bio2 = mean(bio2),
            Moyenne_bio3 = mean(bio3),
            Moyenne_bio4 = mean(bio4),
            Moyenne_bio5 = mean(bio5),
            Moyenne_bio6 = mean(bio6),
            Moyenne_bio7 = mean(bio7),
            Moyenne_bio8 = mean(bio8),
            Moyenne_bio9 = mean(bio9),
            Moyenne_bio10 = mean(bio10),
            Moyenne_bio11 = mean(bio11),
            Moyenne_bio12 = mean(bio12),
            Moyenne_bio13 = mean(bio13),
            Moyenne_bio14 = mean(bio14),
            Moyenne_bio15 = mean(bio15),
            Moyenne_bio16 = mean(bio16),
            Moyenne_bio17 = mean(bio17),
            Moyenne_bio18 = mean(bio18),
            Moyenne_bio19 = mean(bio19))

test_moyenne2$Digits <- as.numeric(sub("site_(\\d+)", "\\1", test_moyenne2$site))
test_moyenne2 <- test_moyenne2[order(test_moyenne2$Digits), ]


sf_france_climat<- cbind(sf_france,test_moyenne2[,2:20])
write.table(test_moyenne2[,2:20], file = paste0(getwd(),"/data_entree/data_SPIPOLL/meteo_france.csv"),sep =';', row.names = FALSE)
write.table(sf_france_climat, file = paste0(getwd(),"/data_entree/data_SPIPOLL/point_modele_france.csv"),sep =';', row.names = FALSE)
write.table(sf_point_climat_entrainement, file = paste0(getwd(),"/data_entree/data_SPIPOLL/point_collection_100000_pré_traité_départ.csv"),sep =';', row.names = FALSE)
write.table(sf_obs, file = paste0(getwd(),"/data_entree/data_SPIPOLL/point_observation_100000.csv"),sep =';', row.names = FALSE)
write.table(sf_points_2, file = paste0(getwd(),"/data_entree/data_SPIPOLL/point_collection_100000_vector.csv"),sep =';', row.names = FALSE)
write.table(sf_collection, file = paste0(getwd(),"/data_entree/data_SPIPOLL/point_collection_100000.csv"),sep =';', row.names = FALSE)
write.table(sf_point_climat_entrainement, file = paste0(getwd(),"/data_entree/data_SPIPOLL/point_collection_modele.csv"),sep =';', row.names = FALSE)

write.table(sf_france, file = paste0(getwd(),"/data_entree/data_SPIPOLL/point_france_2km_clc.csv"),sep =';', row.names = FALSE)

sf_france <- read.csv (paste0(getwd(),"/data_entree/data_SPIPOLL/point_modele_france.csv"),sep =';' )
sf_obs <- read.csv("/Users/lmanceron/Documents/carto_mnhn/data_entree/data_SPIPOLL/point_observation_100000.csv", sep =";")
sf_collection <- read.csv("/Users/lmanceron/Documents/carto_mnhn/data_entree/data_SPIPOLL/point_collection_100000.csv", sep =";")
sf_point_vector <- read.csv("/Users/lmanceron/Documents/carto_mnhn/data_entree/data_SPIPOLL/point_collection_100000_vector.csv", sep =";") 
sf_points_ajout <- st_as_sf(sf_collection, coords = c("long", "lat"), crs = st_crs(rbk_2001))

### extractions des départements###

# Sélection des lignes dont les chiffres sont présents dans la liste

sf_point_allier$Digits <- sub("pred_(\\d+)", "\\1", sf_point_allier$id)
liste_de_pred <- unique(sf_point_allier$Digits)
sf_france_climat$Digits <- sub("pred_(\\d+)", "\\1", sf_france_climat$id)
sf_allier_climat <- subset(sf_france_climat, Digits %in% liste_de_pred)




### TEST pour récupération des jours ###

climate_data_2019 <- extract_nc_value(first_year = 2019, 
                                           last_year = 2019,
                                           local_file = TRUE,
                                           file_path = paste0(getwd(),"/tn_ens_mean_0.1deg_reg_2011-2022_v27.0e.nc"),
                                           sml_chunk = "2011-2022",
                                           spatial_extent = fr_border,
                                           clim_variable = "min temp",
                                           statistic = "mean",
                                           grid_size = 0.1,
                                           ecad_v = NULL,
                                           write_raster = TRUE,
                                           out = "raster_min_temp_2019.grd",
                                           return_data = TRUE)
rbk_2011 = raster::brick("raster_min_temp_2019.grd")
sf_point_climat <- st_transform(sf_point_climat, crs = st_crs(rbk_2011))

daily_min_temp_pnts = temporal_aggregate(x = rbk_2011,
                                         y = sf_point_climat[1,],
                                         variable_name = "min temp",
                                         time_step = "daily")
