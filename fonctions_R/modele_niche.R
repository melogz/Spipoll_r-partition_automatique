set.seed(1)

df_insect <- as.data.frame(sf_obs$insect_taxon)
names(df_insect) <- 'insect_taxon'

frequences <- df_insect %>%
  count(insect_taxon) %>%
  filter(n > 500) %>%
  arrange(desc(n))

print(frequences)
performance <- subset(frequences, select = insect_taxon)
performance['modele_complet']<-NULL
frequences$insect_taxon[1]
nom_insecte <- frequences$insect_taxon
performance[1,2]<- 5
sf_point_climat_entrainement$geometry<-NULL

for (i in 1:nrow(performance)){
  set.seed(1)
  sf_point_climat_espece <- pre_traitement(sf_point_climat_entrainement,sf_obs,nom_insecte[i])
  sf_point_climat_espece <- st_drop_geometry(sf_point_climat_espece)
  train_index <- createDataPartition(y = sf_point_climat_espece$presence, p = 0.8, list=FALSE)
  indices_colonnes <- c(3,4,11:128)
  
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
  
  roc_obj <- roc(ytest$presence, as.numeric(rf.pred_dws))
  
  # Calculer l'AUC
  auc_val <- auc(roc_obj)
  performance[i,3]<-auc_val
  print(i)
}


nom_espece = "Les Citrons (Gonepteryx)"
sf_point_climat_espece_verif <- pre_traitement(sf_point_climat_entrainement,sf_obs,nom_espece)

sf_point_climat_espece <- st_drop_geometry(sf_point_climat_espece)
sf_point_climat_espece_verif <- st_drop_geometry(sf_point_climat_espece_verif)


ggplot(sf_point_climat_espece_verif, aes(x =  presence)) +
  geom_bar() +
  labs(x = "presence/absence", y = "Fréquence") +
  ggtitle("Distribution de l'espece sur les sites")




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

