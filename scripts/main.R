#Setup ----

install.packages("carData")


library(readxl)
library(openxlsx)
library(lubridate)
library(dplyr)
library(stringr)
library(ggplot2)
library(car)


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
  mutate(Numero_de_la_station = str_replace(Numero_de_la_station, "([A-Z])([0-9]{1,2})", pattern(Numero_de_la_station)),
         Stations_globales = str_replace(Stations_globales, "([A-Za-z]+)([A-Z])([0-9]+)", "\\1\\3\\2"))


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


#0. Graphe résumé de l'évolution des apparitions selon la géographie


  
#III - Activité humaine (distances de l'homme par rapport à l'abondance
#des cephalophes)




#H1. La présence humaine à un impact significatif sur l'abondance des céphalophes dans une zone donnée.
#Cela veut dire que plus une station d'échantillonnage est proche d'une route, moins on observe des céphalophes.


#Préparer les données pour interprétation

dt <- total_cephalophes_par_station %>%
  left_join(
    data_stations %>%
      select(-Villages,
             -Numero_de_la_station
             ),
  by = c("Stations_d'échantillonnage" = "Stations_globales"))


#Vérifier à travers une régression linéaire

first <- lm(Nombre_total_cephalophes ~ Pente + Distance_Village + Distance_Route + Distance_eau + Distance_camp_de_chasse, data = dt)
summary(first) #20 % de variance expliquée

second <- lm(log(Nombre_total_cephalophes + 1) ~ log(Distance_Route + 1), data = dt)
summary(second)

third <- lm(Nombre_cephalophes ~ Moment_journee, data = total_cephalophes_par_village_moment_journee)
summary(third) #29 % de la variance expliquée


#Etudier les colinéarités avec le vif test (variance influence factor)

vif(first) # VIF entre 1,01 et 1,25 : aucune inquiétude de multicolinéarité.
 

#Conclusion
#Seule la variable Distance_Route est significative à un niveau de 0.001.


#Graphes ----





