#Setup ----

# Vérifier si les packages sont installés
required_packages <- c("carData", "factoextra", "lme4", "Matrix", 
                       "readxl", "openxlsx", "lubridate", "dplyr", 
                       "stringr", "ggplot2")

new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Charger les bibliothèques
lapply(required_packages, library, character.only = TRUE)

source("scripts/wrangling/functions.R")


#Data ----

#Importer les différents datasets de la base de données Data_Biodiversity
#1. data_especes récupère les informations de la feuille 1

data_especes <- read.xlsx("data/raw/Data_Biodiversity.xlsx",
                             sheet = 1,
                             startRow = 1,
                             colNames = TRUE,
                             rowNames = FALSE,
                             detectDates = TRUE,
                             sep.names = "_",
                             na.strings = "NA")

#Traitement pour exploiter efficacement la colone data_espece$Heure_d'observation
#a. Remplacer la valeur manquante par celle qui est correcte dans le dataset
#b. Transformer la variable en datetime au format HH MM SS.

data_especes$`Heure_d'observation`[1] <- c("12:38")
data_especes$`Heure_d'observation` <- hms(
  paste0(data_especes$`Heure_d'observation`,
         ":00"))


#2. data_stations récupère les données de la feuille 2

data_stations <- read.xlsx("data/raw/Data_Biodiversity.xlsx",
                           sheet = 2,
                           startRow = 1,
                           colNames = TRUE,
                           rowNames = FALSE,
                           detectDates = TRUE,
                           sep.names = "_",
                           na.strings = "NA")

#Corriger l'erreur au niveau du nom des stations. Transformer à MolakoA20 en Molako20A
#pour éviter la formation de données manquantes lors de la jointure à venir.

data_stations <- data_stations %>%
  mutate(
    Numero_de_la_station = str_replace(Numero_de_la_station, "([A-Z])([0-9]{1,2})", "\\1\\2"),
    Stations_globales = str_replace(Stations_globales, "([A-Za-z]+)([A-Z])([0-9]+)", "\\1\\3\\2")
  )


#A - Créer un score de pression anthropique, pour mesurer l'intensité de la pression anthropique.
#Cette pression diminue quand la distance augmente.


#Etape 1 . Inverser les distances en proximité

data_stations <- data_stations %>%
  mutate(
    Proximite_village = 1/(Distance_Village + 1), # +1 pour éviter la division par zéro
    Proximite_route = 1/(Distance_Route + 1),
    Proximite_eau = 1/(Distance_eau + 1),
    Proximite_camp_de_chasse = 1/(Distance_camp_de_chasse + 1)
  )

#Etape 2 . Standardiser les variables

arc <- scale(
  data_stations[, c(
    "Proximite_village",
    "Proximite_route",
    "Proximite_eau",
    "Proximite_camp_de_chasse"
  )])                                                                                        

#Etape 3 . Créer la variable composite

acp_arc <- prcomp(arc, center = TRUE, scale. = TRUE)
summary(acp_arc) #Les 3 premières composantes résument 86,9 % de la variance totale


#Etape 4 . Créer la variable synthétique

# Récupérer les scores des 3 premières variables composantes de l'acp

scores_arc <- acp_arc$x[, 1:3]

# Afficher les coefficients (loadings) de l'acp, pour comprendre la contribution
#de chaque variable corresponsante

loadings_arc <- acp_arc$rotation
round(loadings_arc[, 1:3], 3)  # Pour PC1 à PC3


#Ajouter les 03 variables au jeu de données
#En faisant cela, je crée non pas une seule, mais trois variables composites d'importance
#variées, sur base de la proximité des points évoqués par rapport aux stations


data_stations <- cbind(
  data_stations,
  pression_humaine_globale = scores_arc[, 1],
  gradient_route_eau = scores_arc[, 2],
  accessibilite_chasse_traditionnelle = scores_arc[, 3]
)



#B - Créer un score de pression environnementale globale, pour mesurer l'intensité de la pression anthropique
#et des autres variables environnementale (Pente, MODIS_Degradation).


#Etape 1 . Standardiser les variables

envir <- scale(
  data_stations[, c(
    "Proximite_village",
    "Proximite_route",
    "Proximite_eau",
    "Proximite_camp_de_chasse",
    "MODIS_Degradation",
    "Pente"
  )])


#Etape 2 . Créer la variable composite pour les variables environnementales

acp_envir <- prcomp(envir, center = TRUE, scale. = TRUE)
summary(acp_envir) #Les 4 premières composantes résument 77.51% de la variance totale

#Etape 3 . Créer la variable synthétique

# Récupérer les scores des 3 premières variables composantes de l'acp

scores_envir <- acp_envir$x[, 1:4]

# Afficher les coefficients (loadings) de l'acp
loadings_envir <- acp_envir$rotation
round(loadings_envir[, 1:4], 4)  # Pour PC1 à PC4, 77.51% 

#Ajouter les 3 variables au jeu de données
#En faisant cela, je crée non pas une seule, mais trois variables composites d'importance
#variées, sur base de la proximité des points évoqués par rapport aux stations


data_stations <- cbind(
  data_stations,
  pression_environnementale_globale = scores_envir[, 1],
  gradiant_ecologique = scores_envir[, 2],
  contraste_zone_accessible_degradee = scores_envir[, 3],
  accessibilite_point_eau = scores_envir[, 4]
)




#3. autres_infos récupère dans la feuille 4, les données sur les espèces
#de cephalophes

autres_infos <- read.xlsx("data/raw/Data_Biodiversity.xlsx",
                           sheet = 4,
                           startRow = 1,
                           row = c(1:7),
                           colNames = TRUE,
                           rowNames = FALSE,
                           detectDates = TRUE,
                           sep.names = "_",
                           na.strings = "NA")

#a. Extraire les espèces de cephalophes

cephalophes <- autres_infos$Espèces

#4. geographie récupère dans la feuille 4, les données sur la superficie
#des villages étudiés

geographie <- read.xlsx("data/raw/Data_Biodiversity.xlsx",
                                sheet = 4,
                                startRow = 10,
                                row = c(10:15),
                                colNames = TRUE,
                                rowNames = FALSE,
                                detectDates = TRUE,
                                sep.names = "_",
                                na.strings = "NA")


#5. Joindre l'ensemble des données dans un unique dataset

all_data <- data_stations %>%
  select(-Villages, -Numero_de_la_station) %>%
  right_join(data_especes,
            by = c("Stations_globales" = "Stations_d'échantillonnage")) %>%
  mutate(
    Moment_journee = case_when(
      hour(`Heure_d'observation`) >= 6 & hour(`Heure_d'observation`) < 18 ~ "jour",
      TRUE ~ "nuit"
    ))

 #Explore ----

#Nous souhaitons évaluer l'impact de l'activité humaine sur la 
#espèces de céphalophes sur le territoire de cinq villages.
#Faire attention à leur nombre, leur répartition géographique.

#Ce que nous avons comme information :

#  * L'étude observe le comportement de 06 espèces de céphalophes
#  * L'étude porte sur 05 villages (Lolam, Mapi, Gonou, Sekom, Molako)
#  * Dans chaque village, n stations d'observations sont installées
#  * Caractéristiques du site d'observation (pente, MODIS Degradation)
#  * Proximité avec les besoins vitaux des animaux (MODIS Degradation, Distance eau)
#  * Proximité avec l'activité humaine (Distance village, route, eau, camp de chasse, MODIS Degradation)

#  * Les revelés de présence des espèces, ainsi que leur moment d'échantillonage
#  * Le nombre total de jours d'échantillonnage


#I- Dispositif de l'étude


#1. Le nombre de stations d'échantillonnage par villages

total_stations_par_village <- data_stations %>%
  distinct(Villages, Stations_globales) %>%
  count(Villages, name = "Nb_Station_Globales")


#2. Le nombre de stations d'échantillonnage par villages
#pour lesquels il y a eu observation de cephalophes

total_stations_cephalophes <- data_especes %>%
  distinct(Villages, `Stations_d'échantillonnage`) %>%
  count(Villages, name = "Nb_Station_Cephalophes")

#3. Le nombre de stations où le MODIS_Degratation est de 1 (en forêt)

total_stations_en_foret <- data_stations %>%
  filter(MODIS_Degradation == 1) %>%
  group_by(Villages) %>%
  count(Villages, name = "Nb_Station_Forestiere")


#4. Comparatif des stations de détection des cephalophes

stations <- total_stations_par_village %>%
  left_join(total_stations_en_foret,
            by = c("Villages")) %>%
  left_join(total_stations_cephalophes,
            by = "Villages")



#II - Dénombrement de la population de cephalophes sur le territoire


#1. Le nombre total de céphalophes observées par village

total_cephalophes_par_village <- data_especes %>%
  group_by(Villages) %>%
  summarise(Nombre_total_cephalophes = n(), .groups = "drop")


#2. Le nombre total de céphalophes par type

total_cephalophes_par_type <- data_especes %>%
  group_by(Espèces) %>%
  summarise(Nombre_total_cephalophes = n(), .groups = "drop")


#3. Le nombre total de cephalophes par stations

total_cephalophes_par_station <- data_especes %>%
  group_by(`Stations_d'échantillonnage`, Villages) %>%
  summarise(Nombre_total_cephalophes = n(), .groups = "drop")


#4. Le nombre total de cephalophes par MODIS Degradation

total_cephalophes_par_modis <- all_data %>%
  group_by(MODIS_Degradation) %>%
  summarise(Nombre_cephalophes = n(), .groups = "drop")


#5. Le nombre total de cephalophes par moment de la journée

total_cephalophes_par_moment_journee <- all_data %>%
  group_by(Moment_journee) %>%
  summarise(Nombre_cephalophes = n(), .groups = "drop")


#6. Le nombre total de cephalophes par date d'observation

total_cephalophes_par_date <- data_especes %>%
  group_by(`Dates_d'échantillonnage`) %>%
  summarise(Nombre_total_cephalophes = n(), .groups = "drop")


#7. Le nombre total de cephalophes par espèce par moment de la journée

total_cephalophes_par_type_moment_journee <- all_data %>%
  group_by(Espèces, Moment_journee) %>%
  summarise(Nombre_cephalophes = n(), .groups = "drop")

#8. Le nombre total de cephalophes par village par moment de la journée

total_cephalophes_par_village_moment_journee <- all_data %>%
  group_by(Villages, Moment_journee) %>%
  summarise(Nombre_cephalophes = n(), .groups = "drop")


#9. Le nombre total de cephalophes par type et MODIS Degradation

total_cephalophes_par_type_modis <- all_data %>%
  group_by(Espèces, MODIS_Degradation) %>%
  summarise(Nombre_observations = n(), .groups = "drop")


#10. Le nombre de céphalophes observées par type et villages

total_cephalophes_par_type_villages <- data_especes %>%
  group_by(Espèces, Villages) %>%
  summarise(Nombre_observations = n(), .groups = "drop")


#11. Le de cephalophes par type et par station

total_cephalophes_par_type_station <- data_especes %>%
  group_by(`Stations_d'échantillonnage`, Espèces) %>%
  summarise(Nombre_observations = n(), .groups = "drop")


#12. Le nombre de céphalophes observées par type et villages

total_cephalophes_par_type_villages <- data_especes %>%
  group_by(Villages, Espèces) %>%
  summarise(Nombre_observations = n(), .groups = "drop")


#0. Graphe résumé du dénombrement




#III - La géographie (superficie, relief, végétation)


geographie <- geographie %>%
  left_join(all_data %>%
              group_by(Villages) %>%
              summarise(
                Pente_min = min(Pente, na.rm = TRUE),
                Pente_max = max(Pente, na.rm = TRUE),
                Pente_med = median(Pente, na.rm = TRUE),
                Pente_mean = mean(Pente, na.rm = TRUE),
                Amplitude = Pente_max - Pente_min,
                Pente_sd = sd(Pente, na.rm = TRUE),
                Pente_cv = Pente_sd / Pente_mean
              ),
             by = c("Site" = "Villages")) %>%
  left_join(
    stations,
    by = c("Site" = "Villages")) %>%
  left_join(
    data_stations %>%
      filter(MODIS_Degradation == 1 & Stations_globales %in% unique(data_especes$`Stations_d'échantillonnage`)) %>%
      group_by(Villages) %>%
      summarise(
        Nb_Station_Forestiere_Cephalophes = n(),
      ),
    by = c("Site" = "Villages")) %>%
 mutate(
    Couverture_Foret_Globale = (Nb_Station_Forestiere/Nb_Station_Globales),
    Couverture_Foret_Cephalophes = (Nb_Station_Forestiere_Cephalophes/Nb_Station_Cephalophes)
  )


  
#III - Activité humaine (distances de l'homme par rapport à l'abondance
#des cephalophes)


#Problématique centrale : Comment la pression anthropique influence-t-elle la présence
#et l'abondance des différentes espèces de céphalophes dans les villages du bassin du Congo,
#en interaction avec les caractéristiques écologiques naturelles de leur environnement ?


#Préparer les données pour analyse et interprétation

dath1 <- total_cephalophes_par_station %>%
  left_join(
    data_stations %>%
      select(-Villages,
             -Numero_de_la_station
      ),
    by = c("Stations_d'échantillonnage" = "Stations_globales"))


dath2 <- total_cephalophes_par_type_station %>%
  left_join(
    data_stations %>%
      select(-Villages,
             -Numero_de_la_station
      ),
    by = c("Stations_d'échantillonnage" = "Stations_globales"))



#Test arbritraire pour expliquer à travers une régression linéaire
#le nombre total de cephalophes.


firm <- lm(Nombre_total_cephalophes ~ Distance_Village + Distance_Route + Distance_eau + Distance_camp_de_chasse + MODIS_Degradation, data = dath1)
summary(firm) #20 % de variance expliquée

secm <- lm(log(Nombre_total_cephalophes + 1) ~ log(Distance_Route + 1), data = dath1)
summary(secm)

thim <- lm(Nombre_total_cephalophes ~ Pente + MODIS_Degradation, data = dath1)
summary(thim) #29 % de la variance expliquée


#Etudier les colinéarités avec le vif test (variance influence factor)

vif(firm) # VIF entre 1,01 et 1,25 : aucune inquiétude de multicolinéarité.
vif(thim) # VIF entre 1,01 et 1,25 : aucune inquiétude de multicolinéarité.



#Nous avons plus haut, créer deux groupes de variables composites.

#Groupe A - 3 variables composites, qui chacune représente une facette
#d'interprétation de la pression anthropique.

#Utiliser PC1 (pression_humaine_globale) - Pression humaine globale (proximité des villages et des camps de chasse)
#Utiliser PC2 (gradient_route_eau) - Gradiant route vs eau (proximité aux routes et aux points d'eau)
#Utiliser PC3 (accessibilite_chasse_traditionnelle) - Accessibilité (proximité route, camp_de_chasse, eau)


#Groupe B - 4 variables composites, qui chacune représente une facette
#d'interprétation de la pression environnementale.

#Utiliser PC1 (pression_environnementale_globale) - Pression environnementale globale (proximité des villages et des camps de chasse)
#Utiliser PC2 (gradiant_ecologique) - Gradiant écologique (Proximité eau, Pente, MODIS_Degradation)
#Utiliser PC3 (contraste_zone_accessible_degradee) - Accessibilité de la zone (Proximité route, Pente, MODIS_Degradation)
#Utiliser PC4 (accessibilite_point_eau) - Accessibilité (Route, eau, pente)




#Hypothèses globales que nous souhaitons vérifier ----


#H1 . Plus la pression anthropique est forte, moins on rencontre les céphalophes.
#Objectif : Mesurer l'effet global de l'anthropisation

forglm <- glm(Nombre_total_cephalophes ~ pression_humaine_globale, family = poisson, data = dath1)
summary(forglm)

#H1 . Vailde et significatif
#La pression humaine globale a un effet significatif (p = 0.0489) sur le nombre total de céphalophes,
#avec un effet négatif : lorsque la pression humaine augmente d'une unité, le nombre de céphalophes diminue de 30%.




#H2 . Certaines espèces de céphalophes sont plus tolérantes à la présence humaine que d'autres (espèces "adaptatives" vs "sensibles")
#Objectif : Mesurer la sensibilité à l'homme selon les espèces

sixlmer <- lmer(Nombre_observations ~ pression_humaine_globale + (1|Espèces), data = dath2)
summary(sixlmer)


#Rejeté, non significatif

#Le t value pour la pression humaine est -1.41, ce qui suggère une tendance à la baisse, mais pas statistiquement significative au seuil classique (généralement t > 2 ou p < 0.05 pour significativité)
#La pression humaine globale a donc un effet non significatif (t < 2) sur le nombre total de céphalophes,
#avec un effet négatif : lorsque la pression humaine augmente d'une unité, le nombre de céphalophes diminue de 30%.




#H3 . L’effet de la pression humaine est modulé par les caractéristiques naturelles du territoire
#(relief, végétation, accessibilité aux ressources).
#Objectif : Mesurer les interactions entre variables naturelles et humaines

sepglm <- glm(Nombre_total_cephalophes ~ gradiant_ecologique, family = poisson, data = dath1)
summary(sepglm)

huiglm <- glm(Nombre_total_cephalophes ~ pression_humaine_globale * MODIS_Degradation, family = poisson, data = dath1)
summary(huiglm)

neuglm <- glm(Nombre_total_cephalophes ~ pression_humaine_globale * Pente, family = poisson, data = dath1)
summary(neuglm)



#Hypothèses Spéficiques ----


#H4 . Les zones à forte pente sont moins fréquentées par l’homme et donc plus favorables à la présence des céphalophes.
#Objectif : Effets de la pente sur la présence de cephalophes

dixglm <- glm(Nombre_total_cephalophes ~ Pente, family = poisson, data = dath1)
summary(dixglm)

#Valide.

#Le coefficient de la pente est négatif et hautement significatif : une augmentation d'une unité de pente entraîne une
#baisse de ~ 5.2 % du nombre attendu de céphalophes.


#H5 . Proximité des points d'eau
#Les céphalophes sont plus souvent observés à proximité des sources d’eau, nécessaires à leur survie.

onzglm <- glm(Nombre_total_cephalophes ~ Distance_eau, family = poisson, data = dath1)
summary(onzglm)


#H6 . Proximité des routes ou chemins
#Les zones proches des routes sont moins fréquentées par les céphalophes en raison du dérangement humain.

douglm <- glm(Nombre_total_cephalophes ~ Proximite_route, family = poisson, data = dath1)
summary(douglm)

#Valide. Une proximité à la route réduit drastiquement l’abondance attendue de céphalophes. C’est un indicateur fort de l’impact de
#l’infrastructure humaine sur leur distribution.



#H7 . Les effets sont variables d'un village à l'autre

trelmer <- lmer(Nombre_total_cephalophes ~ pression_humaine_globale + gradiant_ecologique + (1|Villages), data = dath1)
summary(trelmer)

#Rejeté. Il exite bien des différences, mais elles sont statistiquement non significatives.


