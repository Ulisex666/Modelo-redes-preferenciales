### INFLUENCIA POSITIVA
### Este script se utiliza para evaluar que configuración minimiza el RMSE para el voto
### entre hombres y entre mujeres. De esta forma, se verá el comportamiento del resto de
### patrones al optimizar solamente uno

ruta <- r"(C:\Users\ulise\Documents\GitHub\Modelo-redes-preferenciales\experimentos\patron_primario_redes_fijas)"
setwd(ruta)
library(tidyverse)
library(haven)
#library(labelled)

#### Lectura y procesamiento de datos de la simulacion ####

# Leo la tabla de datos
patron_small_world_positiva <- read.csv("redes_small_world_data/main-pattern-positive-fixed-small-world-table.csv", skip = 6)

# Se leen los datos empiricos de preferencia en el tiempo, para validar.
# Unicamente se considera la opcion A
encuestas_preferencias_general <- read.csv("redes_small_world_data/encuestas_preferencias_general.csv")
encuestas_preferencias_hombres <- read.csv("redes_small_world_data/encuestas_preferencias_A_hombres.csv") 
encuestas_preferencias_mujeres <- read.csv("redes_small_world_data/encuestas_preferencias_A_mujeres.csv")

# Cambio nombre de variables y selecciono unicamente las necesarias
patron_small_world_positiva <- patron_small_world_positiva %>% 
  rename(
    tick = X.step., 
    run_number = X.run.number.,
    #pref_A = pref.A,
    pct_A = percentage.A,
    pairs_per_tick = pairs.per.tick,
    learning_rate = learning.rate,
    #male_pref_A = male.pref.A, 
    male_pct_A = male.percentage.A,
    #female_pref_A = female.pref.A,
    female_pct_A = female.percentage.A,
  )

# Selecciono unicamente las variables de interes
patron_small_world_positiva <- patron_small_world_positiva %>% 
  select(run_number, tick, pairs_per_tick, learning_rate, pct_A, male_pct_A, female_pct_A)


# Aqui se tiene el registro de todas las preferencias obtenidas en la simulacion,
# de acuerdo a la configuracion de los parametros
patrones_por_configuracion <- patron_small_world_positiva %>% 
  group_by(pairs_per_tick, learning_rate, tick) %>% 
  summarise(
    across(
      .cols = c(pct_A, male_pct_A, female_pct_A), 
      .fns = list(
        mean = ~mean(., na.rm = TRUE),
        sd = ~sd(., na.rm = TRUE),
        min = ~min(., na.rm = TRUE),
        max = ~max(., na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}" 
    ),
    n_reps = n(), 
    .groups = "drop" 
  )

#### Extraccion de patrones ####
# Se congsigue una tabla que contenga la información relevante para todos los patrones
# estudiados, de manera que se pueda identificar fácilmente la configuración óptima para 
# un patrón dado

# Se extraen los datos para validacion y calculo del RMSE obtenidos de las encuestas
val_general <- encuestas_preferencias_general %>% 
  filter(tick %in% c(69, 166, 259)) %>% 
  select(tick, real_general = pct_A_real)

val_hombres <- encuestas_preferencias_hombres %>% 
  filter(tick %in% c(69, 166)) %>% 
  select(tick, real_hombres = porcentaje_votos)

val_mujeres <- encuestas_preferencias_mujeres %>% 
  filter(tick %in% c(69, 166)) %>% 
  select(tick, real_mujeres = porcentaje_votos)

validaciones_todas <- val_general %>%
  left_join(val_hombres, by = "tick") %>%
  left_join(val_mujeres, by = "tick")


#### Calculo del RMSE ####
# Se calcula el RMSE por cada corrida individual, para hombres, mujeres y general
rmse_por_corrida <- patron_small_world_positiva %>%
  filter(tick %in% c(69, 166, 259)) %>%
  left_join(validaciones_todas, by = "tick") %>%
  mutate(
    sq_err_general = (pct_A - real_general)^2,      
    sq_err_hombres = (male_pct_A - real_hombres)^2, 
    sq_err_mujeres = (female_pct_A - real_mujeres)^2
  ) %>%
  group_by(run_number, pairs_per_tick, learning_rate) %>%
  summarise(
    rmse_general = sqrt(mean(sq_err_general, na.rm = TRUE)),
    rmse_hombres = sqrt(mean(sq_err_hombres, na.rm = TRUE)),
    rmse_mujeres = sqrt(mean(sq_err_mujeres, na.rm = TRUE)),
    .groups = "drop"
  )

evaluacion_rmse_completa <- rmse_por_corrida %>%
  group_by(pairs_per_tick, learning_rate) %>%
  summarise(
    across(
      .cols = c(rmse_general, rmse_hombres, rmse_mujeres),
      .fns = list(
        mean = ~mean(., na.rm = TRUE),
        sd   = ~sd(., na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

#### Configuraciones optimas para cada patron ####
# Esto se puede obtener directamente de la tabla general con el RMSE obtenida
# anteriormente
best_config_general <- evaluacion_rmse_completa %>% 
  slice_min(order_by = rmse_general_mean, n = 1)

best_config_hombres <- evaluacion_rmse_completa %>% 
  slice_min(order_by = rmse_hombres_mean, n=1)

best_config_mujeres <- evaluacion_rmse_completa %>% 
  slice_min(order_by = rmse_mujeres_mean, n = 1)

# Se extraen los parametros optimos de cada patron. Coincidio que es el mismo para hombres
# y para mujeres bajo influencia positiva
best_learning_rate_general = best_config_general$learning_rate
best_pairs_per_tick_general = best_config_general$pairs_per_tick

best_learning_rate_hombres = best_config_hombres$learning_rate
best_pairs_per_tick_hombres = best_config_hombres$pairs_per_tick

best_learning_rate_mujeres = best_config_mujeres$learning_rate
best_pairs_per_tick_mujeres = best_config_mujeres$pairs_per_tick

#### Simulación de mejor configuración en hombres####
# Ya que se obtuvieron las mejores configuraciones para cada patrón, ahora se
# busca evaluar los patrones generados por estas configuraciones, viendo si logra 
# replicar múltiples a la vez

simulation_best_config_hombres <- read.csv("redes_small_world_data/modelo_redes_preferenciales best-config-pos-hombres-small-world-table",
                                           skip= 6) 
simulation_best_config_hombres <- simulation_best_config_hombres %>% 
  rename(
    tick = X.step., 
    run_number = X.run.number.,
    #pref_A = pref.A,
    pct_A = percentage.a,
    pairs_per_tick = pairs.per.tick,
    learning_rate = learning.rate,
    #male_pref_A = male.pref.A, 
    male_pct_A = male.percentage.a,
    #female_pref_A = female.pref.A,
    female_pct_A = female.percentage.a,
  ) %>% 
  select(run_number, tick, pairs_per_tick, learning_rate, pct_A, male_pct_A, female_pct_A)


simulation_best_config_hombres_stats <- simulation_best_config_hombres %>% 
  group_by(tick) %>% 
  summarise(
    across(
      .cols = c(pct_A, male_pct_A, female_pct_A), 
      .fns = list(
        mean = ~mean(., na.rm = TRUE),
        sd = ~sd(., na.rm = TRUE),
        min = ~min(., na.rm = TRUE),
        max = ~max(., na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}" 
    ),
    n_reps = n(), 
    .groups = "drop" 
  )

# Se extraen los datos correspondientes a cada uno de los patrones evaluados

simulation_best_config_hombres_general <- simulation_best_config_hombres_stats %>% 
  select(tick, pct_A_mean, pct_A_sd, pct_A_min, pct_A_max)

simulation_best_config_hombres_hombres <- simulation_best_config_hombres_stats %>% 
  select(tick, male_pct_A_mean, male_pct_A_sd, male_pct_A_max, male_pct_A_min)

simulation_best_config_hombres_mujeres <- simulation_best_config_hombres_stats %>% 
  select(tick, female_pct_A_mean, female_pct_A_sd, female_pct_A_max, female_pct_A_min)

# Se unen en una unica tabla, para facilitar su visualizacion
simulation_best_config_hombres_largo <- bind_rows(
  simulation_best_config_hombres_general %>% select(tick, media = pct_A_mean, sd = pct_A_sd) %>% mutate(grupo = "General"),
  simulation_best_config_hombres_hombres %>% select(tick, media = male_pct_A_mean, sd = male_pct_A_sd) %>% mutate(grupo = "Hombres"),
  simulation_best_config_hombres_mujeres %>% select(tick, media = female_pct_A_mean, sd = female_pct_A_sd) %>% mutate(grupo = "Mujeres")
)

# Se realiza la grafica, mostrando el comportamiento al optimizar 
simulation_best_config_hombres_plot <- ggplot(simulation_best_config_hombres_largo, aes(x = tick, group = grupo)) +
  
  geom_ribbon(aes(ymin = media - sd, ymax = media + sd, fill = grupo), alpha = 0.1) +
  
  geom_line(aes(y = media, color = grupo), linewidth = 1) +
  
  
  scale_color_manual(values = c("General" = "brown4", "Hombres" = "orange3", "Mujeres" = "purple3")) +
  scale_fill_manual(values = c("General" = "brown3", "Hombres" = "orange2", "Mujeres" = "purple2")) +
  
  #scale_y_continuous(limits = c(58, 71), breaks = 58:71) +
  
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(
    title = "Intención de voto bajo influencia positiva en redes de mundo pequeño,
    optimizando patrón en hombres",
    subtitle = paste0("pairs-per-tick = ", best_pairs_per_tick_hombres,
                      ", learning-rate = ", best_learning_rate_hombres),
    x = "Ticks (días)",
    y = "Preferencia por A (%)",
    color = "Grupo",
    fill = "Grupo"
  )

print(simulation_best_config_hombres_plot)

# Exportación con las medidas calculadas para los márgenes del documento
ggsave("figures_small_world/pos_patron_hombres_small_world.pdf", 
       plot = simulation_best_config_hombres_plot,
       width = 16, height = 10, units = "cm")

#### Simulación de mejor configuración general ####
simulation_best_config_general <- read.csv("redes_small_world_data/modelo_redes_preferenciales best-config-pos-general-small-world-table.csv",
                                           skip= 6) 
simulation_best_config_general <- simulation_best_config_general %>% 
  rename(
    tick = X.step., 
    run_number = X.run.number.,
    #pref_A = pref.A,
    pct_A = percentage.a,
    pairs_per_tick = pairs.per.tick,
    learning_rate = learning.rate,
    #male_pref_A = male.pref.A, 
    male_pct_A = male.percentage.a,
    #female_pref_A = female.pref.A,
    female_pct_A = female.percentage.a,
  ) %>% 
  select(run_number, tick, pairs_per_tick, learning_rate, pct_A, male_pct_A, female_pct_A)


simulation_best_config_general_stats <- simulation_best_config_general %>% 
  group_by(tick) %>% 
  summarise(
    across(
      .cols = c(pct_A, male_pct_A, female_pct_A), 
      .fns = list(
        mean = ~mean(., na.rm = TRUE),
        sd = ~sd(., na.rm = TRUE),
        min = ~min(., na.rm = TRUE),
        max = ~max(., na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}" 
    ),
    n_reps = n(), 
    .groups = "drop" 
  )

# Se extraen los datos correspondientes a cada uno de los patrones evaluados

simulation_best_config_general_general <- simulation_best_config_general_stats %>% 
  select(tick, pct_A_mean, pct_A_sd, pct_A_min, pct_A_max)

simulation_best_config_general_hombres <- simulation_best_config_general_stats %>% 
  select(tick, male_pct_A_mean, male_pct_A_sd, male_pct_A_max, male_pct_A_min)

simulation_best_config_general_mujeres <- simulation_best_config_general_stats %>% 
  select(tick, female_pct_A_mean, female_pct_A_sd, female_pct_A_max, female_pct_A_min)

# Se unen en una unica tabla, para facilitar su visualizacion
simulation_best_config_general_largo <- bind_rows(
  simulation_best_config_general_general %>% select(tick, media = pct_A_mean, sd = pct_A_sd) %>% mutate(grupo = "General"),
  simulation_best_config_general_hombres %>% select(tick, media = male_pct_A_mean, sd = male_pct_A_sd) %>% mutate(grupo = "Hombres"),
  simulation_best_config_general_mujeres %>% select(tick, media = female_pct_A_mean, sd = female_pct_A_sd) %>% mutate(grupo = "Mujeres")
)



# Se realiza la grafica, mostrando el comportamiento al optimizar 
simulation_best_config_general_plot <- ggplot(simulation_best_config_general_largo, aes(x = tick, group = grupo)) +
  
  geom_ribbon(aes(ymin = media - sd, ymax = media + sd, fill = grupo), alpha = 0.1) +
  
  geom_line(aes(y = media, color = grupo), linewidth = 1) +
  
  
  scale_color_manual(values = c("General" = "brown4", "Hombres" = "orange3", "Mujeres" = "purple3")) +
  scale_fill_manual(values = c("General" = "brown3", "Hombres" = "orange2", "Mujeres" = "purple2")) +
  
  #scale_y_continuous(limits = c(58, 71), breaks = 58:71) +
  
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(
    title = "Intención de voto bajo influencia positiva en redes de mundo pequeño,
    optimizando patrón general",
    subtitle = paste0("pairs-per-tick = ", best_pairs_per_tick_general,
                      ", learning-rate = ", best_learning_rate_general),
    x = "Ticks (días)",
    y = "Preferencia por A (%)",
    color = "Grupo",
    fill = "Grupo"
  )

print(simulation_best_config_general_plot)

# Exportación con las medidas calculadas para los márgenes del documento
ggsave("figures_small_world/pos_patron_general_small_world.pdf", 
       plot = simulation_best_config_general_plot,
       width = 16, height = 10, units = "cm")

#### Simulación de mejor configuración en mujeres####
# Ya que se obtuvieron las mejores configuraciones para cada patrón, ahora se
# busca evaluar los patrones generados por estas configuraciones, viendo si logra 
# replicar múltiples a la vez

simulation_best_config_mujeres <- read.csv("redes_small_world_data/modelo_redes_preferenciales best-config-pos-mujeres-small-world-table",
                                           skip= 6) 
simulation_best_config_mujeres <- simulation_best_config_mujeres %>% 
  rename(
    tick = X.step., 
    run_number = X.run.number.,
    #pref_A = pref.A,
    pct_A = percentage.a,
    pairs_per_tick = pairs.per.tick,
    learning_rate = learning.rate,
    #male_pref_A = male.pref.A, 
    male_pct_A = male.percentage.a,
    #female_pref_A = female.pref.A,
    female_pct_A = female.percentage.a,
  ) %>% 
  select(run_number, tick, pairs_per_tick, learning_rate, pct_A, male_pct_A, female_pct_A)


simulation_best_config_mujeres_stats <- simulation_best_config_mujeres %>% 
  group_by(tick) %>% 
  summarise(
    across(
      .cols = c(pct_A, male_pct_A, female_pct_A), 
      .fns = list(
        mean = ~mean(., na.rm = TRUE),
        sd = ~sd(., na.rm = TRUE),
        min = ~min(., na.rm = TRUE),
        max = ~max(., na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}" 
    ),
    n_reps = n(), 
    .groups = "drop" 
  )

# Se extraen los datos correspondientes a cada uno de los patrones evaluados

simulation_best_config_mujeres_general <- simulation_best_config_mujeres_stats %>% 
  select(tick, pct_A_mean, pct_A_sd, pct_A_min, pct_A_max)

simulation_best_config_mujeres_hombres <- simulation_best_config_mujeres_stats %>% 
  select(tick, male_pct_A_mean, male_pct_A_sd, male_pct_A_max, male_pct_A_min)

simulation_best_config_mujeres_mujeres <- simulation_best_config_mujeres_stats %>% 
  select(tick, female_pct_A_mean, female_pct_A_sd, female_pct_A_max, female_pct_A_min)

# Se unen en una unica tabla, para facilitar su visualizacion
simulation_best_config_mujeres_largo <- bind_rows(
  simulation_best_config_mujeres_general %>% select(tick, media = pct_A_mean, sd = pct_A_sd) %>% mutate(grupo = "General"),
  simulation_best_config_mujeres_hombres %>% select(tick, media = male_pct_A_mean, sd = male_pct_A_sd) %>% mutate(grupo = "Hombres"),
  simulation_best_config_mujeres_mujeres %>% select(tick, media = female_pct_A_mean, sd = female_pct_A_sd) %>% mutate(grupo = "Mujeres")
)

# Se realiza la grafica, mostrando el comportamiento al optimizar 
simulation_best_config_mujeres_plot <- ggplot(simulation_best_config_mujeres_largo, aes(x = tick, group = grupo)) +
  
  geom_ribbon(aes(ymin = media - sd, ymax = media + sd, fill = grupo), alpha = 0.1) +
  
  geom_line(aes(y = media, color = grupo), linewidth = 1) +
  
  
  scale_color_manual(values = c("General" = "brown4", "Hombres" = "orange3", "Mujeres" = "purple3")) +
  scale_fill_manual(values = c("General" = "brown3", "Hombres" = "orange2", "Mujeres" = "purple2")) +
  
  #scale_y_continuous(limits = c(58, 71), breaks = 58:71) +
  
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(
    title = "Intención de voto bajo influencia positiva en redes de mundo pequeño,
    optimizando patrón en mujeres",
    subtitle = paste0("pairs-per-tick = ", best_pairs_per_tick_mujeres,
                      ", learning-rate = ", best_learning_rate_mujeres),
    x = "Ticks (días)",
    y = "Preferencia por A (%)",
    color = "Grupo",
    fill = "Grupo"
  )

print(simulation_best_config_mujeres_plot)

# Exportación con las medidas calculadas para los márgenes del documento
ggsave("figures_small_world/pos_patron_mujeres_small_world.pdf", 
       plot = simulation_best_config_mujeres_plot,
       width = 16, height = 10, units = "cm")

