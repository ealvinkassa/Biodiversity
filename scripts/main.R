#Setup ----

library(openxlsx)
library(lubridate)

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

#Analysis ----





