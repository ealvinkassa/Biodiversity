#Setup ----

library(openxlsx)
library(lubridate)
library(dplyr)

source("scripts/wrangling/functions.R")

#Data ----

#Importer les différents datasets de la base de données Data_Biodiversity
#1. data_especes récupère les informations de la feuille 1
#2. data_stations récupère les données de la feuille 2

data_especes <- read.xlsx("data/raw/Data_Biodiversity.xlsx",
                             sheet = 1,
                             startRow = 1,
                             colNames = TRUE,
                             rowNames = FALSE,
                             detectDates = TRUE,
                             sep.names = ".",
                             na.strings = "NA")

data_stations <- read.xlsx("data/raw/Data_Biodiversity.xlsx",
                           sheet = 2,
                           startRow = 1,
                           colNames = TRUE,
                           rowNames = FALSE,
                           detectDates = TRUE,
                           sep.names = ".",
                           na.strings = "NA")




#Traitement pour exploiter efficacement la colone data_espece$Heure.d'observation
#1. Vérifier le type de la variable
#2. Remplacer la valeur manquante par celle qui est correcte dans le dataset
#3. Transformer le type de la variable


str(data_especes$`Heure.d'observation`)
data_especes$`Heure.d'observation`[1] <- c("12:38")
data_especes$`Heure.d'observation` <- hms(
                                        paste0(data_especes$`Heure.d'observation`,
                                        ":00")
)

#Explore ----

#Nous souhaitons évaluer l'impact de l'activité humaine sur la présence des
#espèces de céphalophes autour de cinq villages.

#Ce que nous avons comme information :

#  * L'étude observe le comportement de 06 espèces de céphalophes
#  * L'étude porte sur 05 villages (Lolam, Mapi, Gonou, Sekom, Molako)
#  * Dans chaque village, n stations d'observations sont installées
#  * Caractéristiques du site d'observation (pente, MODIS Degradation)
#  * Proximité avec les besoins vitaux des animaux (MODIS Degradation, Distance eau)
#  * Proximité avec l'activité humaine (Distance village, route, eau, camp de chasse, MODIS Degradation)

#  * Les revelés de présence des espèces, ainsi que leur moment d'échantillonage
#  * Le nombre total de jours d'échantillonnage



#Connaitre le nombre de stations d'échantillonnage par villages

table(data_stations$Villages)
total_stations <- data_stations %>%
  distinct(Villages, Stations.globales) %>%
  count(Villages, name = "Station_Globales")


#Connaitre le nombre de stations d'échantillonnage par villages
#pour lesquels il y a eu observation de cephalophes

cephalophes_stations <- data_especes %>%
  distinct(Villages, `Stations.d'échantillonnage`) %>%
  count(Villages, name = "Station_Cephalophes")


#Connaitre le nombre d'espèces de céphalophes observées par type d'espèce

cephalophes_apparitions <- data_especes %>%
  select(Espèces) %>%
  count(Espèces, name = "Nombre_observations")


#Connaitre le nombre d'espèces de céphalophes observées par type d'espèce
#par village

cephalophes_villages <- data_especes %>%
  group_by(Villages, Espèces) %>%
  summarise(Nombre_observations = n(), .groups = "drop")


#Connaitre le nombre d'espèces de céphalophes observées par type d'espèce
#par MODIS Degradation

# Joindre les données des deux feuilles par le nom de station
# "Stations d'échantillonnage" dans data_especes correspond à "Stations globales" dans data_stations

joint_data <- data_especes %>%
  left_join(data_stations, by = c("Stations.d'échantillonnage" = "Stations.globales"))

# Compter le nombre d'espèces distinctes par niveau de MODIS dégradation

cephalophes_modis <- joint_data %>%
  group_by(`MODIS.Degradation`, Espèces) %>%
  summarise(Nombre_observations = n(), .groups = "drop")


#Graphes ----


summary(data_especes)



