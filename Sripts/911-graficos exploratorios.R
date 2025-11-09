library(tidyverse)
library(readr)
library(grid) # Para unit() en theme

# ============================================================
# GRÁFICOS EXPLORATORIOS (df_tp_final)
# ============================================================

# Grupos Nivel Socio Economico (Data Frame: df_tp_final)
df_tp_final %>%
  ggplot(aes(x = NSE,  fill = NSE)) +
  geom_bar() +
  labs(title = "Distribución de la Población por NSE (Ponderado)",
       y = "Población Representada")

# Frecuencia Libros Digital (Data Frame: df_tp_final)
df_tp_final %>%
  ggplot(aes(x = FREC_LIBROS_DIGITAL)) +
  geom_bar() +
  labs(y = "Población Representada")

# Frecuencia Libros Digital por NSE (dodge) (Data Frame: df_tp_final)
df_tp_final %>%
  ggplot(aes(x = FREC_LIBROS_DIGITAL, fill=NSE)) +
  geom_bar(position = "dodge") +
  labs(y = "Población Representada")

# Variable 'vj1' (Data Frame: df_tp_final)
df_tp_final %>%
  ggplot(aes(x = vj1)) +
  geom_bar() +
  labs(y = "Población Representada")

# Variable 'vj1' por 'grupos_edad' (dodge) (Data Frame: df_tp_final)
df_tp_final %>%
  ggplot(aes(x = vj1, fill=grupos_edad)) +
  geom_bar(position="dodge") +
  labs(y = "Población Representada")

# Variable 'vj2' (Data Frame: df_tp_final)
df_tp_final %>%
  ggplot(aes(x = vj2)) +
  geom_bar() +
  labs(y = "Población Representada")

# Variable 'teatro3' (Data Frame: df_tp_final)
df_tp_final %>%
  ggplot(aes(x = teatro3)) +
  geom_bar() +
  labs(y = "Población Representada")

# Frecuencia Libros Digital por NSE (fill) (Data Frame: df_tp_final)
df_tp_final %>%
  ggplot(aes(x = FREC_LIBROS_DIGITAL, fill=NSE)) +
  geom_bar(position = "fill") +
  labs(y = "Población Representada")

# Nivel Socio Economico NSE (Data Frame: df_tp_final)
df_tp_final %>% 
  ggplot(aes(x = NSE, fill = NSE)) + geom_bar() + theme_minimal()

# Nivel Educativo por NSE (fill) (Data Frame: encc_Final)
encc_Final %>%
  ggplot(aes(x = NIVEL_EDUCATIVO, fill = NSE)) + geom_bar(position = "fill")+
  labs(title = "Distribución por Nivel Socioeconómico") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# NSE por Género (fill) (Data Frame: df_tp_final)
df_tp_final %>%
  ggplot(aes(x = NSE, fill = GENERO)) +
  geom_bar(position = "fill") +
  labs(
    title = "Distribución del Nivel Socioeconómico por Género",
    x = "Nivel Socioeconómico",
    y = "Proporción",
    fill = "GENERO"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# Genero y nivel educativo (fill) (Data Frame: df_tp_final)
df_tp_final %>%
  ggplot(aes(x = NIVEL_EDUCATIVO, fill = GENERO)) +
  geom_bar(position = "fill") +
  labs(
    title = "Nivel Educativo por Género",
    x = "Nivel Educativo",
    y = "Proporción",
    fill = "Género"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# Ingresos del hogar según NSE (Data Frame: df_tp_final)
df_tp_final %>%
  ggplot(aes(x = NSE, y = INGRESOS_HOGAR, fill = NSE)) +
  geom_boxplot() +
  labs(title = "Ingresos del hogar por Nivel Socioeconómico",
       x = "Nivel Socioeconómico", y = "Ingresos del hogar") +
  theme_minimal()


# ============================================================
# GRÁFICOS DEL ÍNDICE DE PARTICIPACIÓN CULTURAL (IPC)
# ============================================================

# ----------------------------
# 1. Preparar los datos (REQUERIDO PARA g2 y g6)
# ----------------------------
# Resumen por grupo para scatter y heatmap (Data Frame: df_ipc)
df_ipc_summary <- df_ipc %>%
  group_by(region, grupos_edad, GENERO) %>%
  summarise(
    IPC_promedio = mean(IPC, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# Resumen para heatmap (Data Frame: df_ipc)
heatmap_summary <- df_ipc %>%
  group_by(region, grupos_edad) %>%
  summarise(IPC_promedio = mean(IPC, na.rm = TRUE), .groups = "drop")

# ----------------------------
# 2. Crear los gráficos y guardarlos en objetos
# ----------------------------

# Gráfico g1 (Data Frame: df_ipc)
g1 <- ggplot(df_ipc, aes(x = NSE, y = IPC, color = GENERO)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
  labs(title = "Índice de Participación Cultural según NSE y Género",
       x = "Nivel Socioeconómico (NSE)",
       y = "Índice de Participación Cultural (1-10)",
       color = "Género") +
  theme_minimal()

# Gráfico g2 (Data Frame: df_ipc_summary)
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

# Gráfico g3 (Data Frame: df_ipc)
g3 <- ggplot(df_ipc, aes(x = grupos_edad, y = IPC, color = GENERO)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.6) +
  labs(title = "Distribución del IPC por Edad y Género",
       x = "Grupo de Edad", y = "IPC",
       color = "Género") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.spacing = unit(1, "lines"))

# Gráfico g4 (Data Frame: df_ipc)
g4 <- ggplot(df_ipc, aes(x = IPC)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribución del Índice de Participación Cultural (IPC)",
       x = "IPC (1-10)", y = "Cantidad de individuos") +
  theme_minimal()

# Gráfico g5