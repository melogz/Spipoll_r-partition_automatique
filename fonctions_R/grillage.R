grille <- read.table(paste0(getwd(),"/data_entree/data_SPIPOLL/grille_prediction_2km.txt"),sep="\t",header= TRUE)
sf_points <- st_as_sf(grille, coords = c("X_barycentre_L93", "Y_barycentre_L93"), crs = 2154)
clc <- raster(paste0(getwd(),"/RSpatial/g250_clc12_V18_5.tif"))
st_crs(clc)== st_crs(sf_points)

sf_points <- st_transform(sf_points, crs = st_crs(clc))
sf_france <- main_one_hot(clc_raster,sf_points,44,2000,region,3)

