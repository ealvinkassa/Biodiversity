source("scripts/main.R")


ggplot(total_cephalophes_par_village, aes(x = Villages, y = Nombre_total_cephalophes, fill = Villages)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Nombre total de céphalophes observés par village")


data_especes %>% ggplot(aes(x = Villages, fill = Espèces)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Répartition des espèces par village", x = "Village", y = "Nombre d'observations")


ggplot(total_cephalophes_par_type_villages, aes(x = Villages, y = Nombre_observations, fill = Espèces)) +
  geom_bar(stat = "identity", position = "dodge") +  # position "dodge" pour avoir des barres côte à côte
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotation des étiquettes sur l'axe X pour les rendre lisibles
  labs(title = "Effectifs des espèces par village", x = "Village", y = "Nombre d'observations") +
  scale_fill_brewer(palette = "Set3")  # Choix d'une palette de couleurs agréable pour les espèces



