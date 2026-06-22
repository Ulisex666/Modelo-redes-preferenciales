
setwd(r"(C:\Users\ulise\Documents\GitHub\Modelo-redes-preferenciales\experimentos\redes_analisis)")

library(igraph)
library(tidyverse)

ruta_redes <- r"(C:\Users\ulise\Documents\GitHub\Modelo-redes-preferenciales\networks\)"
ancho_bin <- 5

leer_red_netlogo <- function(ruta_archivo) {
  # 1. Leemos el archivo como texto plano
  lineas <- readLines(ruta_archivo, warn = FALSE)
  # 2. Corregimos el namespace. Remplazamos la URL incorrecta por el estándar oficial
  # que igraph sí puede entender.
  lineas <- gsub('xmlns="[^"]+"', 'xmlns="http://graphml.graphdrawing.org/xmlns"', lineas)
  # 3. Guardamos esta versión limpia en un archivo temporal de Windows
  archivo_temp <- tempfile(fileext = ".graphml")
  writeLines(lineas, archivo_temp)
  # 4. Ahora sí, igraph lo leerá sin problemas
  red <- read_graph(archivo_temp, format = "graphml")
  return(red)
}

red_preferencial <- leer_red_netlogo(paste0(ruta_redes, "preferential_1.graphml"))

preferencial_grados_nodos <- degree(red_preferencial, mode = "all")

preferencial_df_grados <- data.frame(
  agente = V(red_preferencial), 
  conexiones = preferencial_grados_nodos
)
preferencial_df_agrupado <- preferencial_df_grados %>%
  mutate(rango_inicio = floor(conexiones / ancho_bin) * ancho_bin) %>%
  count(rango_inicio, name = "frecuencia")

histograma_preferencial <- ggplot(preferencial_df_agrupado, aes(x = rango_inicio, y = frecuencia)) +
  geom_col(width = ancho_bin * 0.9, fill = "steelblue", color = "black", alpha = 0.8) +
  geom_text(aes(label = frecuencia), 
            vjust = -0.5,      
            size = 3.5,        
            color = "black", 
            fontface = "bold") +
  scale_y_log10() +
  scale_x_continuous(breaks = seq(0, max(preferencial_df_agrupado$rango_inicio), by = ancho_bin)) +
  theme_minimal() +
  theme(plot.margin = margin(t = 20, r = 10, b = 10, l = 10)) +
  labs(
    title = "Distribución de conexiones por agente en red de conexión preferencial",
    subtitle = "Frecuencia agrupada, bin = 5",
    x = "Número de Conexiones (Grado k)",
    y = "Frecuencia en escala logarítmica"
  )
print(histograma_preferencial)

red_small_world <- leer_red_netlogo(paste0(ruta_redes, "small_world_1.graphml"))
small_world_grados_nodos <- degree(red_small_world, mode = "all")
small_world_df_grados <- data.frame(
  agente = V(red_small_world), 
  conexiones = small_world_grados_nodos
)


histograma_small_world <- ggplot(small_world_df_grados, aes(x = conexiones)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", alpha = 0.8) +
  # Extraemos el cálculo interno del histograma y lo usamos como texto
  stat_bin(
    binwidth = 1, 
    geom = "text", 
    aes(label = after_stat(count)), 
    vjust = -0.5,       # Empuja el texto arriba de la barra
    size = 3.5, 
    fontface = "bold"
  ) +
  theme_minimal() +
  theme(plot.margin = margin(t = 20, r = 10, b = 10, l = 10)) +
  labs(
    title = "Distribución de conexiones por agente en red de mundo pequeño",
    x = "Número de Conexiones (Grado k)",
    y = "Frecuencia"
  )
print(histograma_small_world)
