library(climateExtract)
library(dismo)
ecad_version
# liste des varaibles : clim_variable : "mean temp", "min temp","max temp","precipitation","sea level pressure","relative humidity","global radiation"

set.seed(42876) 
fr_border <- sf::st_as_sf(raster::getData("GADM", country = "FRA", level = 0))

#pour 10 ans 2001-2010, on récupère les différentes info qu'on veut 
#d'abord les températures moyennes 

climate_data_2001_2010 <- extract_nc_value(first_year = 2001, 
                                           last_year = 2010,
                                           local_file = FALSE,
                                           file_path = NULL,
                                           sml_chunk = "1995-2010",
                                           spatial_extent = fr_border,
                                           clim_variable = "mean temp", # on récupère ici les valeurs moyennes
                                           statistic = "mean",
                                           grid_size = 0.25,
                                           ecad_v = NULL,
                                           write_raster = TRUE,
                                           out = "raster_mean_temp_2001_2010.grd",
                                           return_data = TRUE)

# on le transforme en raster pour pouvoir localiser les points et extraire les valeurs d'interet 
rbk_2001 = raster::brick("raster_mean_temp_2001_2010.grd")
sf_point_climat <- st_transform(sf_collection_jj, crs = st_crs(rbk_2001))
st_crs(rbk_2001)== st_crs(sf_point_climat)

# on récupère la moyenne de chaque année pour avoir le bon nombre de ligne
annual_avg_temp_pnts = temporal_aggregate(x = rbk_2001,
                                           y = sf_point_climat,
                                           agg_function = "mean",
                                           variable_name = "average temp",
                                           time_step = "annual")

# on récupère la moyenne de chaque mois 
monthly_avg_temp_pnts = temporal_aggregate(x = rbk_2001,
                                           y = sf_point_climat,
                                           agg_function = "mean",
                                           variable_name = "average temp",
                                           time_step = "monthly")

# on extrait pour les années 2011-2021 la température moyenne
climate_data_2011_2021 <- extract_nc_value(first_year = 2011, 
                                           last_year = 2021,
                                           local_file = FALSE,
                                           file_path = NULL,
                                           sml_chunk = "2011-2021",
                                           spatial_extent = fr_border,
                                           clim_variable = "mean temp",
                                           statistic = "mean",
                                           grid_size = 0.25,
                                           ecad_v = NULL,
                                           write_raster = TRUE,
                                           out = "raster_mean_temp_2011_2021.grd",
                                           return_data = TRUE)


rbk_2011 = raster::brick("raster_mean_temp_2011_2021.grd")
format(object.size(climate_data_2011_2021), "MB")
format(object.size(rbk_2011), "MB")
st_crs(rbk_2011)== st_crs(sf_point_climat)

# on récupère la moyenne de chaque année pour avoir le bon nombre de ligne
annual_avg_temp_2011_pnts = temporal_aggregate(x = rbk_2011,
                                          y = sf_point_climat,
                                          agg_function = "mean",
                                          variable_name = "average temp",
                                          time_step = "annual")

annual_avg_temp_pnts <-rbind(annual_avg_temp_pnts,annual_avg_temp_2011_pnts)
annual_avg_temp_pnts <- arrange(annual_avg_temp_pnts, site)

# annual mean per point
monthly_avg_temp_pnts_2011 = temporal_aggregate(x = rbk_2011,
                                           y = sf_point_climat,
                                           agg_function = "mean",
                                           variable_name = "average temp",
                                           time_step = "monthly")

monthly_avg_temp_pnts <-rbind(monthly_avg_temp_pnts,monthly_avg_temp_pnts_2011)
monthly_avg_temp_pnts <- arrange(monthly_avg_temp_pnts, site)

# on récupère la moyenne la plus basse de chaque mois
# Ensuite les températures min 

climate_data_2001_2010 <- extract_nc_value(first_year = 2001, 
                                           last_year = 2010,
                                           local_file = FALSE,
                                           file_path = NULL,
                                           sml_chunk = "1995-2010",
                                           spatial_extent = fr_border,
                                           clim_variable = "min temp", # on récupère ici les valeurs min
                                           statistic = "mean",
                                           grid_size = 0.25,
                                           ecad_v = NULL,
                                           write_raster = TRUE,
                                           out = "raster_min_temp_2001_2010.grd",
                                           return_data = TRUE)

# on le transforme en raster pour pouvoir localiser les points et extraire les valeurs d'interet 
rbk_2001 = raster::brick("raster_min_temp_2001_2010.grd")
st_crs(rbk_2001)== st_crs(sf_point_climat)


# on récupère la moyenne de chaque mois 
monthly_min_temp_pnts = temporal_aggregate(x = rbk_2001,
                                           y = sf_point_climat,
                                           agg_function = "mean",
                                           variable_name = "min temp",
                                           time_step = "monthly")

# on extrait pour les années 2011-2021 la température min
climate_data_2011_2021 <- extract_nc_value(first_year = 2011, 
                                           last_year = 2021,
                                           local_file = FALSE,
                                           file_path = NULL,
                                           sml_chunk = "2011-2021",
                                           spatial_extent = fr_border,
                                           clim_variable = "min temp",
                                           statistic = "mean",
                                           grid_size = 0.25,
                                           ecad_v = NULL,
                                           write_raster = TRUE,
                                           out = "raster_min_temp_2011_2021.grd",
                                           return_data = TRUE)


rbk_2011 = raster::brick("raster_min_temp_2011_2021.grd")
format(object.size(climate_data_2011_2021), "MB")
format(object.size(rbk_2011), "MB")
st_crs(rbk_2011)== st_crs(sf_point_climat)


monthly_min_temp_pnts_2011 = temporal_aggregate(x = rbk_2011,
                                                y = sf_point_climat,
                                                agg_function = "mean",
                                                variable_name = "min temp",
                                                time_step = "monthly")

monthly_min_temp_pnts <-rbind(monthly_min_temp_pnts,monthly_min_temp_pnts_2011)
monthly_min_temp_pnts <- arrange(monthly_min_temp_pnts, site)



#Enfin on récupère la moyenne max de chaque mois. 

climate_data_2001_2010 <- extract_nc_value(first_year = 2001, 
                                           last_year = 2010,
                                           local_file = FALSE,
                                           file_path = NULL,
                                           sml_chunk = "1995-2010",
                                           spatial_extent = fr_border,
                                           clim_variable = "max temp", # on récupère ici les valeurs min
                                           statistic = "mean",
                                           grid_size = 0.25,
                                           ecad_v = NULL,
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
                                           local_file = FALSE,
                                           file_path = NULL,
                                           sml_chunk = "2011-2021",
                                           spatial_extent = fr_border,
                                           clim_variable = "max temp",
                                           statistic = "mean",
                                           grid_size = 0.25,
                                           ecad_v = NULL,
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
monthly_max_temp_pnts <- arrange(monthly_max_temp_pnts, site)


# on les met tous dans un dataframe

df_climat <- data.frame(id = 1:nrow(monthly_avg_temp_pnts))
df_climat$id <- monthly_avg_temp_pnts$site
df_climat$year <- monthly_avg_temp_pnts$year
df_climat$avg <- monthly_avg_temp_pnts$mean_average_temp
df_climat$min_avg <-  monthly_min_temp_pnts$mean_min_temp
df_climat$max_avg <-  monthly_max_temp_pnts$mean_max_temp




# on récupère les précipitations journalières pour les années 2001-2010
climate_data_2001_2010_precipitation <- extract_nc_value(first_year = 2001, 
                                           last_year = 2010,
                                           local_file = FALSE,
                                           file_path = NULL,
                                           sml_chunk = "1995-2010",
                                           spatial_extent = fr_border,
                                           clim_variable = "precipitation",
                                           statistic = "mean",
                                           grid_size = 0.25,
                                           ecad_v = NULL,
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
                                           local_file = FALSE,
                                           file_path = NULL,
                                           sml_chunk = "2011-2021",
                                           spatial_extent = fr_border,
                                           clim_variable = "precipitation",
                                           statistic = "mean",
                                           grid_size = 0.25,
                                           ecad_v = NULL,
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
monthly_avg_prep_pnts <- arrange(monthly_avg_prep_pnts, site)


df_climat$prep <- monthly_avg_prep_pnts$mean_prep

 
# on récupère les biovariables
biovariable <- data.frame()
for (i in (unique(df_climat$id))) {
  for (j in (unique(df_climat$year))){
    annee_subset <- subset(df_climat, df_climat$year == j & df_climat$id == i)
    prec <- as.numeric(annee_subset$prep)
    tmin<- as.numeric(annee_subset$min_avg)
    tmax <- as.numeric(annee_subset$max_avg)
    biovariable <- rbind(biovariable,biovars(prec, tmin, tmax))
  }
}

df_climat_annuel <- data.frame(id = 1:nrow(annual_avg_temp_pnts))
df_climat_annuel$id <- annual_avg_temp_pnts$site
df_climat_annuel$year <- annual_avg_temp_pnts$year
df_climat_annuel<- cbind(df_climat_annuel,biovariable)


# on calcule la moyenne des biovariables
sf_point_biovariable <- data.frame(id = 1:nrow(sf_collection))
sf_point_biovariable$id <- sf_collection$id
# créer un dataframe vide avec les noms de colonnes
moyenne_biovar <- data.frame(matrix(ncol = 19, nrow = 0))
for (i in (unique(df_climat_annuel$id))) {
  annee_subset <- subset(df_climat_annuel,df_climat_annuel$id == i)
  moyenne_biovar <- rbind(moyenne_biovar,colMeans(annee_subset[,3:21]))
}
names(moyenne_biovar) <- names(biovariable)
sf_point_biovariable<- cbind(sf_point_biovariable,moyenne_biovar)

sf_point_climat<- cbind(sf_point_climat,moyenne_biovar)



