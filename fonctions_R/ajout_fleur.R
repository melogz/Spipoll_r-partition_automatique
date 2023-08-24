
SPIPOLL <- read.table(paste0(getwd(),"/data_entree/data_SPIPOLL/spipoll_20220706_formated.txt"),sep ="\t", header =TRUE )
SPIPOLL$date_de_session <- as.Date(SPIPOLL$date_de_session, format = "%d/%m/%Y")
SPIPOLL$annee <- as.numeric(format(SPIPOLL$date_de_session, "%Y"))
filtre_2018 <-filtre(SPIPOLL,"SPIPOLL$annee<=2016")

subset_df <- subset(SPIPOLL, grepl("\\b(genre|famille)\\b", flower_taxon_sc))
subset_df <- sf_obs[,1:10]
#subset_df$flower_taxon_sc<-  iconv(subset_df$flower_taxon_sc, from = "latin1", to = "UTF-8")
#subset_df$classif <- sub("^.*\\b(genre|famille)\\b.*$", "\\1", filtre_2018$flower_taxon_sc, ignore.case = TRUE)
subset_df$classif <- ifelse(grepl("\\bgenre\\b", subset_df$flower_taxon_sc, ignore.case = TRUE), "partie d'un genre",
                            ifelse(grepl("\\bgenres\\b", subset_df$flower_taxon_sc, ignore.case = TRUE), "partie de plusieurs genres",
                        ifelse(grepl("\\bfamille\\b", subset_df$flower_taxon_sc, ignore.case = TRUE), "famille",
                               ifelse(grepl("\\bPlante\\b", subset_df$flower_taxon_sc, ignore.case = TRUE), "Plante inconnue",
                                      ifelse(grepl("\\bsais\\b", subset_df$flower_taxon_sc, ignore.case = TRUE), "Plante inconnue",
                                    ifelse(grepl("\\bsp\\b", subset_df$flower_taxon_sc, ignore.case = TRUE), "genre", "espece"))))))


### pour récupérer les genre et les familles 
install.packages("taxize") #package de recuperation de genre et famille
use_entrez() #création d'une API pour récupération sur NCBI 

install.packages("usethis") #package pour installer l'API
library(usethis)
edit_r_environ() #rentrez l'API dans le nouveau fichier ouvert 
# Chargez le package taxize
library(taxize)
####

subset_df$espece <- ifelse(subset_df$classif == "espece",subset_df$flower_taxon_sc , NA)




### POur les genres 

# Création un nouveau dataframe avec une colonne pour les noms uniques 
noms_uniques <- unique(subset_df$espece)
nouveau_dataframe_espece <- data.frame(espece = noms_uniques)
nouveau_dataframe <- na.omit(nouveau_dataframe)

nouveau_dataframe$genre <- tax_name(nouveau_dataframe$espece, db = "itis", get = "genus", messages = FALSE)[[3]]
indices_correspondances <- match(subset_df$espece, nouveau_dataframe$espece)

nvdf <- data.frame(espece =nouveau_dataframe[1:150,])
nvdf3<- tax_name(nvdf$espece, db = "itis", get = "genus", messages = FALSE)
#
itis_name(query="Helianthus annuus", get="family")
tax_name(sci = "Helianthus annuus", get = "family", db = "ncbi")
print(indices_correspondances)
#subset_df$genre <- nouveau_dataframe$genre[indices_correspondances]


subset_df$genre <- ifelse(subset_df$classif == "genre",
                          str_extract(subset_df$flower_taxon_sc, "\\b\\S+(?=\\ssp\\b|\\))"),
                          ifelse(subset_df$classif == "partie d'un genre",
                                 str_extract(subset_df$flower_taxon_sc, "(?<=genre\\s)\\w+"),
                                 ifelse(subset_df$classif == "partie de plusieurs genres",
                                        str_extract(subset_df$flower_taxon_sc, "(?<=genres\\s)\\w+"),
                                 ifelse(subset_df$classif == "espece",
                                        str_extract(subset_df$espece, "\\b\\w+"),
                                        NA))))
####


### Pour les familles

# Création un nouveau dataframe avec une colonne pour les noms uniques
noms_uniques <- unique(subset_df$genre)
nouveau_dataframe <- data.frame(genre = noms_uniques)
nouveau_dataframe <- na.omit(nouveau_dataframe)
nouveau_dataframe$famille <- NA


for (i in (556:nrow(nouveau_dataframe))){
  nouveau_dataframe$famille[[i]]<- tax_name(nouveau_dataframe$genre[[i]], db = "ncbi", get = "family", messages = FALSE)[[3]]
}

nouveau_dataframe$genre[[456]]<-"Althea"
nouveau_dataframe$famille[[667]]<- "Boraginaceae"


# Charger la bibliothèque "taxize" si ce n'est pas déjà fait
library(taxize)

# Charger la bibliothèque "taxize" si ce n'est pas déjà fait
library(taxize)


indices_correspondances <- match(subset_df$genre, nouveau_dataframe$genre)
 class(indices_correspondances)



indices_correspondances
nouveau_dataframe$famille[indices_correspondances]


subset_df$famille <- ifelse(subset_df$classif == "famille",sub(".*des\\s+(\\w+).*", "\\1",  subset_df$flower_taxon_sc, ignore.case = TRUE) , 
                            ifelse(!is.na(subset_df$genre) ,nouveau_dataframe$famille[indices_correspondances], 'plante Inconnue'))


write.table(subset_df, file = paste0(getwd(),"/data_entree/data_SPIPOLL/famille_fleur.csv"),sep =';', row.names = FALSE)
df_flower <- as.data.frame(subset_df$famille)
names(df_flower)<-'famille'
frequences_flower <- df_flower %>%
  count(famille)
print(frequences_flower)



