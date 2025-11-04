# ============================================================
# 7. ANÁLISIS DETALLADO – CABA
# ============================================================

# Filtrar solo CABA
df_caba <- df_ipc %>% filter(REGION == "CABA")

# 1️⃣ Distribución general del IPC en CABA
ggplot(df_caba, aes(x = IPC)) +
  geom_histogram(bins = 15, fill = "#1f78b4", color = "white", alpha = 0.8) +
  labs(
    title = "Distribución del IPC – CABA",
    x = "Índice de Participación Cultural (IPC)",
    y = "Frecuencia"
  ) +
  theme_minimal(base_size = 13)

# 2️⃣ IPC por Nivel Socioeconómico
ggplot(df_caba, aes(x = NSE, y = IPC, fill = NSE)) +
  geom_boxplot(alpha = 0.7, outlier.color = "gray40") +
  labs(
    title = "IPC por Nivel Socioeconómico – CABA",
    x = "Nivel Socioeconómico (NSE)",
    y = "IPC"
  ) +
  theme_minimal(base_size = 13)

# 3️⃣ IPC promedio por grupo de edad
df_caba %>%
  group_by(GRUPO_EDAD) %>%
  summarise(IPC_promedio = mean(IPC, na.rm = TRUE)) %>%
  ggplot(aes(x = GRUPO_EDAD, y = IPC_promedio, group = 1)) +
  geom_line(color = "#e31a1c", size = 1) +
  geom_point(size = 3, color = "#e31a1c") +
  labs(
    title = "IPC promedio por grupo de edad – CABA",
    x = "Grupo de edad",
    y = "IPC promedio"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# 4️⃣ IPC vs Educación y NSE (gráfico combinado)
ggplot(df_caba, aes(x = NIVEL_
                    
                    