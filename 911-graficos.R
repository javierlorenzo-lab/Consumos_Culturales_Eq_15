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

# Gráfico g5 (Data Frame: df_ipc)
g5 <- ggplot(df_ipc, aes(x = grupos_edad, y = IPC, fill = GENERO)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "IPC por Edad y Género",
       x = "Grupo de Edad", y = "IPC") +
  theme_minimal()

# Gráfico g6 (Data Frame: heatmap_summary)
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

pdf("Graficos_IPC.pdf", width = 10, height = 7) # Abrir PDF multipágina
for(plot in list_of_plots){
  print(plot)
}
dev.off() # Cerrar PDF


# ============================================================
# 6. GRÁFICOS EXPLORATORIOS DEL IPC (Script 2)
# ============================================================

# -- 6.1: Scatterplot Exploratorio (IPC vs. Edad) (Data Frame: df_ipc) --
print(
  ggplot(df_ipc, aes(x = EDAD, y = IPC)) +
    geom_jitter(width = 0.5, height = 0.2, alpha = 0.2, size = 1.5) +
    geom_smooth(method = "loess", color = "red", se = TRUE) + # 'loess' es bueno para exploración
    labs(
      title = "Scatterplot Exploratorio: IPC vs. Edad",
      x = "Edad (numérica)",
      y = "Índice de Participación Cultural (IPC)",
      caption = "Jitter y alpha añadidos para ver la densidad de puntos"
    ) +
    theme_minimal()
)


# -- 6.2: Gráfico de Puntos y Suavizado (por grupos) (Data Frame: df_ipc) --
print(
  ggplot(df_ipc, aes(x = grupos_edad, y = IPC, color = NSE)) +
    geom_point(alpha = 0.6, position = position_dodge(width = 0.3)) +
    geom_smooth(method = "loess", se = TRUE) +
    labs(
      x = "Grupo de edad",
      y = "Índice de Participación Cultural (IPC)",
      color = "Nivel Socioeconómico",
      title = "Relación entre IPC, grupos de edad y NSE"
    ) +
    theme_minimal()
)


# -- 6.3: Boxplot (Data Frame: df_ipc) --
print(
  ggplot(df_ipc, aes(x = grupos_edad, y = IPC, color = NSE)) +
    geom_boxplot(position = position_dodge(width = 0.8)) +
    labs(
      x = "Grupo de edad",
      y = "Índice de Participación Cultural (IPC)",
      color = "Nivel Socioeconómico",
      title = "Distribución del IPC por grupo de edad y NSE"
    ) +
    theme_minimal()
)


# -- 6.4: Gráfico de Violín (Data Frame: df_ipc) --
print(
  ggplot(df_ipc, aes(x = grupos_edad, y = IPC, fill = NSE)) +
    geom_violin(position = position_dodge(width = 0.8), alpha = 0.5, trim = FALSE) +
    geom_boxplot(width = 0.15, position = position_dodge(width = 0.8),
                 outlier.shape = NA, color = "black") +
    labs(
      x = "Grupo de edad",
      y = "Índice de Participación Cultural (IPC)",
      fill = "Nivel Socioeconómico",
      title = "Distribución del IPC por grupo de edad y NSE"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.text.x = element_text(angle = 30, hjust = 1)
    )
)


# -- 6.5: Gráfico combinado (Violín + Boxplot + Jitter) (Data Frame: df_ipc) --
# Asegurar que NSE sea categórica
df_ipc$NSE <- as.factor(df_ipc$NSE)

p_violin_box <- ggplot(df_ipc, aes(x = grupos_edad, y = IPC, fill = NSE)) +
  geom_violin(position = position_dodge(width = 0.8), alpha = 0.5, trim = FALSE) +
  geom_boxplot(width = 0.05, position = position_dodge(width = 0.8),
               outlier.shape = NA, color = "black") +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8),
              alpha = 0.3, size = 1) +
  labs(
    x = "Grupo de edad",
    y = "Índice de Participación Cultural (IPC)",
    fill = "Nivel Socioeconómico",
    title = "Distribución del IPC por grupo de edad y NSE"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

# Mostrar el gráfico
print(p_violin_box)

# Guardar en PDF
ggsave("Nuevo_ipc_violin_box_jitter.pdf", plot = p_violin_box, width = 8, height = 5)


# -- 6.6: Gráfico de Barras (Promedio) (Data Frame: df_ipc, resumido) --
print(
  df_ipc %>%
    group_by(grupos_edad, NSE) %>%
    summarise(ipc_promedio = mean(IPC, na.rm = TRUE), .groups = 'drop') %>%
    ggplot(aes(x = grupos_edad, y = ipc_promedio, fill = NSE)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
    labs(
      x = "Grupo de edad",
      y = "IPC promedio",
      fill = "Nivel Socioeconómico",
      title = "IPC promedio por grupo de edad y NSE"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      axis.text.x = element_text(angle = 30, hjust = 1)
    )
)


# ============================================================
# 8. ANÁLISIS DE RESIDUOS (Requiere que 'modelo_ipc' exista)
# ============================================================

# Extraer valores ajustados y residuos
valores_ajustados <- fitted(modelo_ipc)
residuos <- resid(modelo_ipc)

# Crear data frame
df_res <- data.frame(
  valores_ajustados = valores_ajustados,
  residuos = residuos
)

# Gráfico simple de Residuos vs Ajustados (Data Frame: df_res)
print(
  ggplot(df_res, aes(x = valores_ajustados, y = residuos)) +
    geom_point(alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(
      x = "Valores ajustados",
      y = "Residuos",
      title = "Residuos vs Valores Ajustados"
    ) +
    theme_minimal()
)


# Gráfico con línea de tendencia de residuos (Data Frame: df_res)
print(
  ggplot(df_res, aes(x = valores_ajustados, y = residuos)) +
    geom_point(alpha = 0.6, color = "blue") +           # puntos de residuos
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) + # recta ideal
    geom_smooth(method = "lm", se = TRUE, color = "red") +    # recta de tendencia real
    labs(
      x = "Valores ajustados",
      y = "Residuos",
      title = "Residuos vs Valores Ajustados con rectas de referencia y tendencia"
    ) +
    theme_minimal(base_size = 13)
)


# Gráfico de Residuos por NSE
# Crear data frame para ggplot (Data Frame: df_ipc)
df_res_full <- data.frame(
  valores_ajustados = valores_ajustados,
  residuos = residuos,
  NSE = df_ipc$NSE,
  grupos_edad = df_ipc$grupos_edad
)

# Crear gráfico (Data Frame: df_res_full)
p_residuos_nse <- ggplot(df_res_full, aes(x = valores_ajustados, y = residuos, color = NSE)) +
  geom_point(alpha = 0.5, size = 2, position = position_jitter(width = 0.1)) +  # puntos semi-translúcidos
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +  # recta referencia
  geom_smooth(aes(group = 1), method = "lm", se = TRUE, color = "red") +        # recta tendencia general
  labs(
    x = "Valores ajustados",
    y = "Residuos",
    color = "Nivel Socioeconómico (NSE)",
    title = "Residuos vs Valores Ajustados por NSE"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

# Mostrar gráfico
print(p_residuos_nse)

# Guardar directamente en PDF
ggsave("Nuevos_residuos_ipc_NSE.pdf", plot = p_residuos_nse, width = 8, height = 5)

