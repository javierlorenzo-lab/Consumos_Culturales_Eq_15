# graficos con indice de participacion cultural

library(ggplot2)
library(dplyr)
library(grid)  # Para unit() en theme si lo usaste

# ----------------------------
# 1. Preparar los datos
# ----------------------------
# Resumen por grupo para scatter y heatmap
df_ipc_summary <- df_ipc %>%
  group_by(region, grupos_edad, GENERO) %>%
  summarise(
    IPC_promedio = mean(IPC, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

heatmap_summary <- df_ipc %>%
  group_by(region, grupos_edad) %>%
  summarise(IPC_promedio = mean(IPC, na.rm = TRUE), .groups = "drop")

# ----------------------------
# 2. Crear los gráficos y guardarlos en objetos
# ----------------------------
g1 <- ggplot(df_ipc, aes(x = NSE, y = IPC, color = GENERO)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
  labs(title = "Índice de Participación Cultural según NSE y Género",
       x = "Nivel Socioeconómico (NSE)",
       y = "Índice de Participación Cultural (1-10)",
       color = "Género") +
  theme_minimal()

g2 <- ggplot(df_ipc_summary, aes(x = grupos_edad, y = IPC_promedio, color = GENERO, size = n)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~region) +
  scale_size_continuous(range = c(3, 10)) +
  labs(title = "Participación Cultural (IPC) por Grupo de Edad, Región y Género",
       x = "Grupo de Edad", y = "IPC (promedio)",
       color = "Género", size = "Cantidad de participantes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.spacing = unit(1, "lines"))

g3 <- ggplot(df_ipc, aes(x = grupos_edad, y = IPC, color = GENERO)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.6) +
  labs(title = "Distribución del IPC por Edad y Género",
       x = "Grupo de Edad", y = "IPC",
       color = "Género") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.spacing = unit(1, "lines"))

g4 <- ggplot(df_ipc, aes(x = IPC)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribución del Índice de Participación Cultural (IPC)",
       x = "IPC (1-10)", y = "Cantidad de individuos") +
  theme_minimal()

g5 <- ggplot(df_ipc, aes(x = grupos_edad, y = IPC, fill = GENERO)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "IPC por Edad y Género",
       x = "Grupo de Edad", y = "IPC") +
  theme_minimal()

g6 <- ggplot(heatmap_summary, aes(x = grupos_edad, y = region, fill = IPC_promedio)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkblue") +
  labs(title = "Mapa de calor del IPC promedio por región y grupo de edad",
       x = "Grupo de Edad", y = "Región", fill = "IPC promedio") +
  theme_minimal()

# ----------------------------
# 3. Guardar todos los gráficos en PDF
# ----------------------------
list_of_plots <- list(g1, g2, g3, g4, g5, g6)

pdf("Graficos_IPC.pdf", width = 10, height = 7)  # Abrir PDF multipágina
for(plot in list_of_plots){
  print(plot)
}
dev.off()  # Cerrar PDF
