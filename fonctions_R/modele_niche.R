set.seed(1)

df_insect <- as.data.frame(sf_obs$insect_taxon)
names(df_insect) <- 'insect_taxon'

frequences <- df_insect %>%
  count(insect_taxon) %>%
  filter(n > 5000) %>%
  arrange(desc(n))

print(frequences)

nom_espece = "Le Syrphe ceinturé (Episyrphus balteatus)"
sf_point_climat_espece <- pre_traitement(sf_point_climat_entrainement,sf_obs,nom_espece)

sf_point_climat_espece <- st_drop_geometry(sf_point_climat_espece)

train_index <- createDataPartition(y = sf_point_climat_espece$presence, p = 0.8, list=FALSE)
indices_colonnes <- c(3,4,11:120,125:129)
dataset <- sf_point_climat_espece[,indices_colonnes]
testset   <- dataset[-train_index,]
trainset  <- dataset[train_index,]

xtrain <- as.data.frame(trainset[,-3])
ytrain <- as.data.frame(trainset[,3])
xtest <- as.data.frame(testset[,-3])
ytest <- as.data.frame(testset[,3])
names(ytest) <- 'presence'
names(ytrain) <- 'presence'
prNum <- as.numeric(table(trainset$presence)["1"]) # number of presence records
spsize <- c("0" = prNum, "1" = prNum) # sample size for both classes

# RF with down-sampling

rf_dws <- randomForest(as.factor(trainset$presence) ~ .,
                       data = trainset,
                       ntree = 1000,
                       sampsize = spsize,
                       replace = TRUE) # make sure samples are with replacement (default)

# predict to raster layers
pred_dws <-  predict(rf_dws,xtest , type = "prob", index = 2)
rf.pred_dws <- predict(rf_dws,xtest)