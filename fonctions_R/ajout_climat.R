#install.packages('remotes')
remotes::install_github("RetoSchmucki/climateExtract")
source ( paste0(getwd(),"/fonctions_R/climateExtract.R"))

ecad_v = 27.0
ecad_version

set.seed(42876) 
fr_border_test <- sf::st_as_sf(raster::getData("GADM", country = "FRA", level = 0))
sf_point_test <- sf::st_sf(sf::st_sample(x = fr_border, size = 25, type = "random"))


climate_data <- extract_nc_value(first_year = 2012, 
                                last_year = 2015,
                                local_file = FALSE,
                                file_path = NULL,
                                sml_chunk = "2011-2021",
                                spatial_extent = fr_border_test,
                                clim_variable = "mean temp",
                                statistic = "mean",
                                grid_size = 0.25,
                                ecad_v = 23.1,
                                write_raster = TRUE,
                                out = "raster_mean_temp.grd",
                                return_data = TRUE)

