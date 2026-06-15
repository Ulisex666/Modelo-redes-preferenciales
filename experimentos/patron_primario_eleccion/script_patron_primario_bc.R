#### Este script procesa los datos obtenidos del experimento main-pattern-bc
#### para el modelo de redes preferenciales. Se busca, de manera similar al modelo
#### base, evaluar la capacidad de reproducir el cambio de opinion observado en la 
#### eleccion, sin tomar en cuenta patrones secundarios como el sexo del electorado.

#### Establecer directorio de trabajo y leer paqueterias ####
setwd("C:/Users/ulise/Documents/Github/Modelo-redes-preferenciales/experimentos/patron_primario_eleccion/")
library(tidyverse)

#### Lectura y procesamiento de datos ####
# Se leen los datos obtenidos del experimento
redes_patron_principal_bc <-  read.csv("behavior_space_datos/modelo_redes_preferenciales main-pattern-bc-table.csv", skip = 6)

# Se leen los datos obtenidos de la encuesta, para validar el modelo
encuestas_preferencias_general <- read.csv("behavior_space_datos/patron_voto_general.csv")

# Se cambian los nombres de las variables y se seleccionan unicamente las de interes
redes_patron_principal_procesada <- redes_patron_principal_bc %>% 
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
redes_patron_principal_filtrada <- redes_patron_principal_procesada %>% 
  filter(tick == 69 | tick == 166 | tick == 259) %>% 
  filter(confidence_threshold <= 1.1) # Se tiene esta cota dado que numeros mas altos dan un resultado
                                      # igual al modelo de influencia positiva

#### Calculo del RMSE ####

# Se calcula el RMSE por cada una de las ejecuciones
redes_rmse_ejecucion_bc <- redes_patron_principal_filtrada %>% 
  inner_join(encuestas_preferencias_general, by = "tick") %>% 
  mutate(            
    err_sq_A = (pct_A - pct_A_real)^2 
  ) %>% 
  group_by(run_number, pairs_per_tick, learning_rate, confidence_threshold) %>%  
  summarise(
    RMSE_run = sqrt(mean(err_sq_A)),
    .groups = "drop"
  )

# Se calculan las estadisticas del RMSE por combinacion de parametros, teniendo en cuenta las
# 30 repeticiones
redes_rmse_stats_bc <- redes_rmse_ejecucion_bc %>% 
  group_by(pairs_per_tick, learning_rate, confidence_threshold) %>% 
  summarise(
    mean_RMSE = mean(RMSE_run),
    sd_RMSE = sd(RMSE_run),
    min_RMSE = min(RMSE_run),
    max_RMSE = max(RMSE_run),
    se_RMSE = sd_RMSE / sqrt(n()),
    .groups = "drop"
  )

# Se eligen las 5 configuraciones con el menor RSME promedio
redes_best_configs_bc <- redes_rmse_stats_bc %>% 
  slice_min(mean_RMSE, n = 5) %>% 
  mutate(etiqueta = paste0("Pairs:", pairs_per_tick, 
                           " | LR:", learning_rate,
                           " | CT:", confidence_threshold)) %>% 
  mutate(etiqueta = reorder(etiqueta, -mean_RMSE))

# Se realiza una grafica de calor, comparado el RMSE promedio obtenido por cada 
# configuración de parámetros, 30 repeticiones
redes_sensitivity_plot_rmse_bc <- ggplot(redes_rmse_stats_bc, aes(x = pairs_per_tick, y = learning_rate, 
                                                                              fill = mean_RMSE)) + 
  geom_tile() + 
  facet_wrap(~ confidence_threshold, 
             labeller = as_labeller(function(x) paste("CT =", x))) + 
  scale_fill_viridis_c(option = "magma", direction = -1) +
  labs(
    title = "RMSE across 3 parameters for bounded confidence 
    with complex social network",
    x = "Pairs per tick",
    y = "Learning rate",
    fill = "RMSE (%)"
  ) +
  theme_minimal() +
  theme(panel.spacing = unit(1, "lines")) 
print(redes_sensitivity_plot_rmse_bc)
ggsave("resultados_bc/redes_rmse_sensitivity_plot.png", plot = redes_sensitivity_plot_rmse_bc,
       width = 8, height = 5, dpi = 300)

# Se visualizan las 5 configuraciones con menor error, observando su variabilidad
redes_bc_lollipop_plot <- ggplot(redes_best_configs_bc, aes(x = etiqueta, y = mean_RMSE)) +
  geom_errorbar(aes(ymin = mean_RMSE - sd_RMSE, ymax = mean_RMSE + sd_RMSE), 
                width = 0.3, color = "#2c3e50", linewidth = 0.8) +
  geom_point(size = 4, color = "#e67e22") + 
  coord_flip() +
  labs(
    title = "Top 5 configurations for bounded confidence with complex social networks",
    subtitle = "Ranked by lowest mean RMSE | Error bars show Standard Deviation (30 runs)",
    x = "Parameters (Agents | Learning Rate)",
    y = "Mean RMSE (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(family = "mono", size = 10), 
    plot.title = element_text(face = "bold")
  )

print(redes_bc_lollipop_plot)
ggsave("resultados_bc/redes_rmse_lollipot_plot.png", plot = redes_bc_lollipop_plot,
       width = 8, height = 5, dpi = 300)

#### Analisis de mejor configuracion ####
# Se selecciona la configuracion con el menor error de la base de datos cruda
best_lr = 0.1
best_pairs = 50
best_confidence = 1.1

redes_mejor_config_bc <- redes_patron_principal_procesada %>% 
  filter(learning_rate == best_lr & pairs_per_tick == best_pairs & confidence_threshold == best_confidence)

redes_mejor_config_stats_bc <- redes_mejor_config_bc %>% 
  group_by(tick) %>% 
  summarise(
    mean_pct_A = mean(pct_A),
    sd_pct_A = sd(pct_A),
    max_pct_A = max(pct_A),
    min_pct_A = min(pct_A)
  )

redes_mejor_config_distance <- encuestas_preferencias_general %>%
  inner_join(redes_mejor_config_stats_bc, by = "tick") %>%
  mutate(
    diferencia = pct_A_real - mean_pct_A,
    y_centro = (pct_A_real + mean_pct_A) / 2 # Para centrar el texto en la línea
  ) %>% 
  select(tick, pct_A_real, diferencia, mean_pct_A, y_centro)

# Grafica que muestra la evolucion de las preferencias bajo esta configuracion, 
# junto a la desviacion estandar de las 30 repeticiones
redes_mejor_config_bc_plot <- ggplot(redes_mejor_config_stats_bc, aes(x = tick)) +
  geom_ribbon(aes(ymin = mean_pct_A - sd_pct_A, ymax = mean_pct_A + sd_pct_A), 
              fill = "brown", alpha = 0.2) +
  geom_line(aes(y = mean_pct_A), color = "brown4", size = 1) +
  geom_point(data = encuestas_preferencias_general, 
             aes(x = tick, y = pct_A_real, shape = "Datos reales"), 
             color = "red", size = 3, shape = 18) +
  geom_segment(data = redes_mejor_config_distance,
               aes(x = tick, xend = tick, y = pct_A_real, yend = mean_pct_A),
               linetype = "dashed", color = "grey40", linewidth = 0.5) +
  geom_text(data = redes_mejor_config_distance,
            aes(x = tick, y = y_centro, 
                label = paste0(ifelse(diferencia > 0, "+", ""), round(diferencia, 1), " (%)")),
            hjust = +1.2, size = 3, fontface = "italic", color = "grey20") +
  theme_minimal() +
  labs(
    title = "Preference evolution best configuration in bounded confidence model with complex networks",
    subtitle = paste0(c("Pairs per tick = ", "learning rate =  ", "confidence threshold = "), c(best_pairs, best_lr, best_confidence), collapse = ", "),
    x = "Ticks (days)",
    y = "Preference share for A (%)"
  ) +
  scale_y_continuous(limits = c(60,70), breaks = c(60:70))

print(redes_mejor_config_bc_plot)
ggsave("resultados_bc/bc_best_config_plot.png", plot = redes_mejor_config_bc_plot,
       width = 8, height = 5, dpi = 300)

