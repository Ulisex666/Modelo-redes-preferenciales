## INFLUENCIA POSITIVA
## Experimento para ver el efecto de los parámetros en la distribución de opiniones
## obtenida cuando se utiliza un sistema preferencial para crear la red social 
## del modelo. 

setwd("C:/Users/ulise/Documents/GitHub/Modelo-redes-preferenciales/experimentos/parametros_y_opiniones/")
library(tidyverse)

positiva_redes_convergencia_table <- read.csv('behavior_space_tables/experiment-parameters-positive-table.csv', skip = 6)
positiva_redes_convergencia_table <- positiva_redes_convergencia_table %>%
  rename(
    tick = X.step., 
    run_number = X.run.number.,
    pairs_per_tick = pairs.per.tick,
    learning_rate = learning.rate,
    total_ticks = total.ticks,
    final_opinion_mean = mean.opinion,
    final_opinion_sd = sd.opinion
  ) %>% 
  select(run_number, learning_rate, pairs_per_tick, tick, final_opinion_mean, final_opinion_sd)

positiva_redes_convergencia_stats <- positiva_redes_convergencia_table %>%
  group_by(pairs_per_tick, learning_rate) %>% 
  summarise(
    final_opinion_mean = mean(final_opinion_mean),
    final_opinion_mean_sd = mean(final_opinion_sd),
    final_opinion_max_sd = max(final_opinion_sd),
    final_opinion_min_sd = min(final_opinion_sd),
    .groups = 'drop'
  )

positiva_redes_convergencia_lograda <- positiva_redes_convergencia_table %>%
  filter(final_opinion_sd <= 0.1)

ggplot(positiva_redes_convergencia_stats, aes(x = pairs_per_tick, y = learning_rate, fill = final_opinion_mean_sd)) +
  geom_tile() +
  geom_text(aes(
    label = scales::scientific(final_opinion_mean_sd, digits = 2),
    color = ifelse(pairs_per_tick == 10 & learning_rate == 0.1, "white", "black")
  ), size = 2.5) +
  scale_color_identity() +
  scale_fill_viridis_c(option = "magma", direction = -1) + 
  labs(
    title = "Standard deviation for final opinions: learning rate vs agents updated per tick",
    y = "Learning rate",
    x = "Agents updated per tick",
    fill = "SD for final opinions"
  ) +
  theme_minimal()  


 # ggplot(positiva_redes_convergencia_stats, aes(x = factor(pairs_per_tick), y = final_opinion_mean)) +
 #  geom_errorbar(aes(ymin = final_opinion_mean - final_opinion_mean_sd, 
 #                    ymax = final_opinion_mean + final_opinion_mean_sd),
 #                width = 0.2, color = "gray50") +
 #  geom_point(size = 2, color = "#2c3e50") +
 #  geom_hline(yintercept = mean(positiva_redes_convergencia_stats$mean_final_opinion), 
 #             linetype = "dashed", color = "#e74c3c", alpha = 0.6) +
 #  facet_wrap(~ learning_rate, labeller = label_both) +
 #  labs(
 #    title = "Stability of final opinion across parameters",
 #    subtitle = "Consistent mean across all conditions",
 #    x = "Agents updated per tick",
 #    y = "Mean final opinion"
 #  ) +
 #  theme_minimal() +
 #  coord_cartesian(ylim = c(-0.2, 0.5)) 
