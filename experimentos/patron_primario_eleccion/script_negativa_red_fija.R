#### Establecer directorio de trabajo y leer paqueterias ####
setwd("C:/Users/ulise/Documents/Github/Modelo-redes-preferenciales/experimentos/patron_primario_eleccion/")
library(tidyverse)

#### Lectura y procesamiento de datos ####
# Se leen los datos obtenidos del experimento
redes_negativa_fija <-  read.csv("behavior_space_datos/modelo_redes_preferenciales neg-best-fixed-pref-network-table.csv", skip = 6)

# Se leen los datos obtenidos de la encuesta, para validar el modelo
encuestas_preferencias_general <- read.csv("behavior_space_datos/patron_voto_general.csv")

# Se cambian los nombres de las variables y se seleccionan unicamente las de interes
redes_negativa_fija_procesada <- redes_negativa_fija %>% 
  rename(
    tick = X.step., 
    run_number = X.run.number.,
    pct_A = percentage.A,
    pairs_per_tick = pairs.per.tick,
    learning_rate = learning.rate,
    confidence_threshold = confidence.threshold
  ) %>% 
  select(run_number, tick, pct_A, pairs_per_tick, learning_rate, confidence_threshold)

# Se filtran los datos, para seleccionar unicamente aquellos dias que tengan datos en las encuestas
redes_negativa_fija_filtrada <- redes_negativa_fija_procesada %>% 
  filter(tick == 69 | tick == 166 | tick == 259)  

redes_rmse_negativa_fija <- redes_negativa_fija_filtrada %>% 
  inner_join(encuestas_preferencias_general, by = "tick") %>% 
  mutate(            
    err_sq_A = (pct_A - pct_A_real)^2 
  ) %>% 
  group_by(run_number, pairs_per_tick, learning_rate, confidence_threshold) %>%  
  summarise(
    RMSE_run = sqrt(mean(err_sq_A)),
    .groups = "drop"
  ) %>% 
  mutate(etiqueta = paste0("Pairs:", pairs_per_tick, 
                           " | LR:", learning_rate,
                           " | CT:", confidence_threshold))

redes_negativa_fija_stats <- redes_rmse_negativa_fija %>% 
  summarise(
    mean_RMSE = mean(RMSE_run),
    sd_RMSE = sd(RMSE_run),
    min_RMSE = min(RMSE_run),
    max_RMSE = max(RMSE_run),
    se_RMSE = sd_RMSE / sqrt(n()),
    .groups = "drop"
  ) 
  

redes_negativa_fija_lollipop_plot <- ggplot(redes_negativa_fija_stats, aes(x = etiqueta, y = mean_RMSE)) +
  geom_errorbar(aes(ymin = mean_RMSE - sd_RMSE, ymax = mean_RMSE + sd_RMSE), 
                width = 0.3, color = "#2c3e50", linewidth = 0.8) +
  geom_point(size = 4, color = "#e67e22") + 
  coord_flip() +
  labs(
    title = "Top 5 configurations for negative influence with complex social networks",
    subtitle = "Ranked by lowest mean RMSE | Error bars show Standard Deviation (30 runs)",
    x = "Parameters (Agents | Learning Rate)",
    y = "Mean RMSE (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(family = "mono", size = 10), 
    plot.title = element_text(face = "bold")
  )

print(redes_negativa_fija_lollipop_plot)
