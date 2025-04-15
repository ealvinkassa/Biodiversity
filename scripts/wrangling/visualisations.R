source("scripts/main.R")


ggplot(total_cephalophes_par_village, aes(x = Villages, y = Nombre_total_cephalophes, fill = Villages)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Nombre total de céphalophes observés par village")



