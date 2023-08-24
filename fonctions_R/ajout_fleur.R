
SPIPOLL <- read.table(paste0(getwd(),"/data_entree/data_SPIPOLL/spipoll_20220706_formated.txt"),sep ="\t", header =TRUE )
SPIPOLL$date_de_session <- as.Date(SPIPOLL$date_de_session, format = "%d/%m/%Y")
SPIPOLL$annee <- as.numeric(format(SPIPOLL$date_de_session, "%Y"))
filtre_2018 <-filtre(SPIPOLL,"SPIPOLL$annee<=2016")

subset_df <- subset(SPIPOLL, grepl("\\b(genre|famille)\\b", flower_taxon_sc))
subset_df <- filtre_2018[,1:20]
#subset_df$classif <- sub("^.*\\b(genre|famille)\\b.*$", "\\1", filtre_2018$flower_taxon_sc, ignore.case = TRUE)
subset_df$classif <- ifelse(grepl("\\bgenre\\b", subset_df$flower_taxon_sc, ignore.case = TRUE), "partie d'un genre",
                        ifelse(grepl("\\bfamille\\b", subset_df$flower_taxon_sc, ignore.case = TRUE), "famille",
                               ifelse(grepl("\\bPlante\\b", subset_df$flower_taxon_sc, ignore.case = TRUE), "Plante inconnue",
                                    ifelse(grepl("\\bsp\\b", subset_df$flower_taxon_sc, ignore.case = TRUE), "genre", "espece"))))

install.packages("taxize")

# Charger le package taxize
library(taxize)

sub("genre.*", "\\1", "Les Orpins rouges-roses (des esp<e8>ces du genre Sedum)", ignore.case = TRUE)

!is.na(subset_df$genre[[1]]) 
tax_name("Sedum" ,db = "ncbi", get = "family")

subset_df$espece <- ifelse(subset_df$classif == "espece",subset_df$flower_taxon_sc , NA)
subset_df$genre <- ifelse(subset_df$classif == "partie d'un genre",sub(".*genre\\s+(\\w+).*", "\\1", subset_df$flower_taxon_sc, ignore.case = TRUE) ,
                              ifelse(subset_df$classif == "espece",tax_name(subset_df$espece ,db = "ncbi", get = "genus", messages =FALSE)[[3]], NA ))
subset_df$famille <- ifelse(subset_df$classif == "famille",sub(".*des\\s+(\\w+).*", "\\1",  subset_df$flower_taxon_sc, ignore.case = TRUE) , 
                            ifelse(!is.na(subset_df$genre) ,tax_name(subset_df$genre ,db = "ncbi", get = "family", messages =FALSE)[[3]], NA ))



df_flower <- as.data.frame(subset_df$flower_taxon_sc)
names(df_flower)<-'flower_taxon'
frequences_flower <- df_flower %>%
  count(flower_taxon)
print(frequences_flower)



