setwd("C:/Users/ulise/Documents/GitHub/Modelo-redes-preferenciales/experimentos/parametros_y_opiniones/")
library(tidyverse)

redes_negative_confidence_table <- read.csv("behavior_space_tables/confidence-effect-negative-table.csv", skip = 6)
redes_negative_confidence_table <- redes_negative_confidence_table %>%
  rename(
    tick = X.step., 
    confidence_threshold = confidence.threshold,
    run_number = X.run.number.,
    pairs_per_tick = pairs.per.tick,
    learning_rate = learning.rate,
    total_ticks = total.ticks,
    opinions_list = X.opinion..of.turtles,
    percentage_A = percentage.A,
    percentage_B = percentage.B
  )

redes_negative_confidence_procesada <- redes_negative_confidence_table %>% 
  mutate(opinions_list = str_remove_all(opinions_list, "[\\[\\]]")) %>%
  mutate(opinions_list = str_trim(opinions_list)) %>%
  separate_longer_delim(opinions_list, delim = " ") %>%
  filter(opinions_list != "") %>%
  mutate(opinion = as.numeric(opinions_list)) %>%
  select(tick, opinion, run_number, percentage_A, percentage_B)

tablas_por_run <- redes_negative_confidence_procesada %>%
  group_split(run_number)

df_labels <- redes_negative_confidence_table %>%
  group_by(run_number) %>%
  filter(tick == max(tick)) %>% 
  select(confidence_threshold, percentage_A, percentage_B) %>% 
  arrange(confidence_threshold)

rm(redes_negative_confidence_table)
rm(redes_negative_confidence_procesada)

for (i in 0:20) {
  
  threshold_val <- i * 0.1
  
  p <- ggplot(tablas_por_run[[i+1]], aes(x = tick, y = opinion)) +
    
    geom_point(alpha = 0.05, size = 0.3, color = "#2c3e50") +
    
    annotate("text", x = Inf, y = 1, 
             label = paste0("Final A: ", round(df_labels$percentage_A[i+1], 1), "%"),
             hjust = 1.1, vjust = 1.5, color = "red", fontface = "bold") +
    
    
    annotate("text", x = Inf, y = -1, 
             label = paste0("Final B: ", round(df_labels$percentage_B[i+1], 1), "%"),
             hjust = 1.1, vjust = -0.5, color = "blue", fontface = "bold") +
    
    labs(
      title = "Opinion evolution for negative influence",
      subtitle = paste0('Confidence threshold = ', sprintf('%.1f', threshold_val)),
      x = "Time (Ticks)",
      y = "Opinion Value [-1, 1]"
    ) +
    
    scale_y_continuous(limits = c(-1, 1)) +
    theme_minimal()
  ggsave(filename = paste0("plots_negative/run_", i, "_threshold_", sprintf("%.1f", threshold_val), ".png"), 
         plot = p, width = 8, height = 5, dpi = 300)
  
  message(paste("Graficada ejecución", i, "con threshold", threshold_val))
}
