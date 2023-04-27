install.packages("devtools")
devtools::install_github("RetoSchmucki/climateExtract")

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
format(object.size(climate_data_2001_2010), "MB")
format(object.size(rbk_2001), "MB")
sf_point_climat <- st_transform(sf_collection, crs = st_crs(rbk_2001))
st_crs(rbk_2001)== st_crs(sf_point_climat)

# on récupère la moyenne de chaque année pour avoir le bon nombre de ligne
annual_avg_temp_pnts = temporal_aggregate(x = rbk_2001,
                                           y = sf_point_climat,
                                           agg_function = "mean",
                                           variable_name = "average temp",
                                           time_step = "annual")

df_climat_annuel <- data.frame(id = 1:nrow(annual_avg_temp_pnts))
df_climat_annuel$id <- annual_avg_temp_pnts$site
df_climat_annuel$year <- annual_avg_temp_pnts$year

biovars <- data.frame(matrix(0,ncol=19,nrow =nrow(df_climat_annuel)))
colnames(biovars)<- paste0("biovars",1:19)
df_climat_annuel<- cbind(df_climat_annuel,biovars)


# on récupère la moyenne de chaque mois 
monthly_avg_temp_pnts = temporal_aggregate(x = rbk_2001,
                                           y = sf_point_climat,
                                           agg_function = "mean",
                                           variable_name = "average temp",
                                           time_step = "monthly")

# on récupère la moyenne la plus basse de chaque mois

monthly_min_temp_pnts <- temporal_aggregate(x = rbk_2001,
                                           y = sf_point_climat,
                                           agg_function = "min",
                                           variable_name = "average temp",
                                           time_step = "monthly")


#on récupère la moyenne max de chaque mois. 
monthly_max_temp_pnts <- temporal_aggregate(x = rbk_2001,
                                            y = sf_point_climat,
                                            agg_function = "max",
                                            variable_name = "average temp",
                                            time_step = "monthly")

# on les met tous dans un dataframe

df_climat <- data.frame(id = 1:nrow(monthly_avg_temp_pnts))
df_climat$id <- monthly_avg_temp_pnts$site
df_climat$year <- monthly_avg_temp_pnts$year
df_climat$avg <- monthly_avg_temp_pnts$mean_average_temp
df_climat$min_avg <-  monthly_min_temp_pnts$min_average_temp
df_climat$max_avg <-  monthly_max_temp_pnts$max_average_temp




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

df_climat$prep <- monthly_avg_prep_pnts$mean_prep


#on crée des listes pour utiliser la fonction biovar
prec <- list()
# boucle pour ajouter des valeurs à la liste
for (i in 1:12) {
  prec <- append(prec,df_climat$prep[i] )
}

tmin <- list()
# boucle pour ajouter des valeurs à la liste
for (i in 1:12) {
  tmin <- append(tmin,df_climat$min[i])
}

tmax2 <- list()
# boucle pour ajouter des valeurs à la liste
for (i in 1:12) {
  tmax2<- append(tmax2,df_climat$max[i] )
}
class(tmax2)

df_annual <- df_climat %>%
  group_by(id, year)

tmin2<- as.numeric(tmin)
prec2<- as.numeric(prec)
tmax3<- as.numeric(tmax2)
class(my_numeric_vector)
test <- biovars(prec2, tmin2, tmax3)
test
(unique(df_climat$id))
 
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



biovariable <- data.frame()
annee_subset <- subset(df_climat, df_climat$year == 2001 & df_climat$id == "site_1")
prec <- as.numeric(annee_subset$prep)
tmin<- as.numeric(annee_subset$min_avg)
tmax <- as.numeric(annee_subset$max_avg)
biovariable <- rbind(biovariable,biovars(prec, tmin, tmax))

df_climat_annuel$biovars1 <- biovariable[1]

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


# annual mean per point
monthly_avg_temp_pnts = temporal_aggregate(x = rbk_2011,
                                           y = sf_point_test,
                                           agg_function = "mean",
                                           variable_name = "average temp",
                                           time_step = "monthly")