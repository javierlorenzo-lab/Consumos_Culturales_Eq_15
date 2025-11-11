library(stringr) # Asegúrate de tener tidyverse cargado

# 1. Definir el orden final de los 5 niveles
niveles_final_ordenados <- c(
  "Sin Estudios",
  "Primario",
  "Secundario",
  "Universitario",
  "Posgrado"
)

# 2. Crear el nuevo dataframe agrupando categorías (Versión Robusta)
df_ipc_edu_final <- df_ipc %>%
  
  # Primero, creamos una columna temporal en minúsculas para la comparación
  mutate(NIVEL_EDUCATIVO_lower = tolower(NIVEL_EDUCATIVO)) %>%
  
  # Usamos case_when() con str_detect() (sensible al orden)
  mutate(
    NIVEL_EDUCATIVO_AGRUPADO = case_when(
      
      # 5. Posgrado (lo ponemos primero porque es más específico)
      str_detect(NIVEL_EDUCATIVO_lower, "posgrado") ~ "Posgrado",
      
      # 4. Universitario (incluye "universitario" y "terciario")
      str_detect(NIVEL_EDUCATIVO_lower, "universitario") |
        str_detect(NIVEL_EDUCATIVO_lower, "terciario") ~ "Universitario",
      
      # 3. Secundario
      str_detect(NIVEL_EDUCATIVO_lower, "secundario") ~ "Secundario",
      
      # 2. Primario (buscamos "primario completo")
      str_detect(NIVEL_EDUCATIVO_lower, "primario completo") ~ "Primario",
      
      # 1. Sin Estudios (buscamos "inicial" o "primario incompleto")
      str_detect(NIVEL_EDUCATIVO_lower, "inicial") |
        str_detect(NIVEL_EDUCATIVO_lower, "primario incompleto") ~ "Sin Estudios",
      
      # Agrupa "Ns/Nc" y cualquier otro valor para ser revisado
      TRUE ~ "Otro" 
    )
  )

# 3. ¡PASO DE DEPURACIÓN! (MUY IMPORTANTE)
# Antes de filtrar, vamos a ver qué cayó en "Otro"
print("Resultados de la agrupación:")
print(table(df_ipc_edu_final$NIVEL_EDUCATIVO_AGRUPADO, useNA = "ifany"))

# 4. Continuar con el script: Filtrar y Ordenar
df_ipc_edu_final <- df_ipc_edu_final %>%
  
  # Filtramos los "Otro" (que eran Ns/Nc o errores)
  filter(NIVEL_EDUCATIVO_AGRUPADO != "Otro") %>%
  
  # Creamos el factor ordenado usando la lista final
  mutate(
    NIVEL_EDUCATIVO_FINAL_ORD = factor(NIVEL_EDUCATIVO_AGRUPADO, 
                                       levels = niveles_final_ordenados)
  )





# Verificar que los nuevos 5 niveles se crearon bien (opcional)
# print(table(df_ipc_edu_final$NIVEL_EDUCATIVO_FINAL_ORD))




# Calcular el IPC promedio por nivel educativo FINAL (SÓLO CABA)
ipc_edu_final_caba <- df_ipc_edu_final %>%
  filter(region == "CABA") %>%
  group_by(NIVEL_EDUCATIVO_FINAL_ORD) %>%
  summarise(IPC_promedio = mean(IPC, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(NIVEL_EDUCATIVO_FINAL_ORD)) # Quitar NAs

# Graficar
ggplot(ipc_edu_final_caba, aes(x = NIVEL_EDUCATIVO_FINAL_ORD, y = IPC_promedio, group = 1)) +
  geom_line(color = "#1f78b4", size = 1.2) +
  geom_point(color = "#1f78b4", size = 3) +
  labs(
    title = "IPC promedio por Nivel Educativo (5 Grupos)",
    subtitle = "Región: CABA",
    x = "Nivel Educativo (Agrupado)",
    y = "IPC promedio"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1) 
  )





# Calcular IPC promedio por nivel educativo FINAL en CABA y GBA
ipc_edu_final_comparativa <- df_ipc_edu_final %>%
  filter(region %in% c("CABA", "GBA")) %>%
  group_by(region, NIVEL_EDUCATIVO_FINAL_ORD) %>%
  summarise(IPC_promedio = mean(IPC, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(NIVEL_EDUCATIVO_FINAL_ORD)) # Quitar NAs

# Gráfico comparativo
ggplot(ipc_edu_final_comparativa, aes(x = NIVEL_EDUCATIVO_FINAL_ORD, y = IPC_promedio, group = region, color = region)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("CABA" = "#1f78b4", "GBA" = "#e31a1c")) +
  labs(
    title = "Comparación del IPC promedio por Nivel Educativo (5 Grupos)",
    subtitle = "Regiones: CABA vs GBA",
    x = "Nivel Educativo (Agrupado)",
    y = "IPC promedio",
    color = "Región"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1) 
  )




# ============================================================
# 11. COMPARACIÓN IPC vs NIVEL EDUCATIVO (TODAS LAS REGIONES)
# ============================================================

# 1. Calcular el IPC promedio por nivel educativo FINAL y región
ipc_edu_final_todas_regiones <- df_ipc_edu_final %>%
  group_by(region, NIVEL_EDUCATIVO_FINAL_ORD) %>%
  summarise(IPC_promedio = mean(IPC, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(NIVEL_EDUCATIVO_FINAL_ORD)) # Quitar NAs

# Tabla de control (opcional)
# print(ipc_edu_final_todas_regiones)

# 2. Gráfico: líneas comparativas por región
ggplot(ipc_edu_final_todas_regiones, aes(x = NIVEL_EDUCATIVO_FINAL_ORD, y = IPC_promedio, group = region, color = region)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 2, alpha = 0.8) +
  labs(
    title = "Comparación del IPC promedio por Nivel Educativo (5 Grupos)",
    subtitle = "Todas las regiones de la ENCC 2022/23",
    x = "Nivel Educativo (Agrupado)",
    y = "IPC promedio",
    color = "Región"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
library(ggrepel)



# 1. Asegúrate de tener tus datos resumidos (corro esto por si acaso)
ipc_edu_final_todas_regiones <- df_ipc_edu_final %>%
  group_by(region, NIVEL_EDUCATIVO_FINAL_ORD) %>%
  summarise(IPC_promedio = mean(IPC, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(NIVEL_EDUCATIVO_FINAL_ORD))

# 2. Creamos el data frame SÓLO para las etiquetas
data_para_etiquetas <- ipc_edu_final_todas_regiones %>%
  filter(NIVEL_EDUCATIVO_FINAL_ORD == "Posgrado") # Filtramos solo el último nivel







ggplot(ipc_edu_final_todas_regiones, aes(x = NIVEL_EDUCATIVO_FINAL_ORD, y = IPC_promedio, group = region, color = region)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 2, alpha = 0.8) +
  
  # --- AQUÍ ESTÁ LA MAGIA ---
  # Le decimos que use el data frame filtrado "data_para_etiquetas"
  geom_text_repel(
    data = data_para_etiquetas,   
    aes(label = region),          # La etiqueta es el nombre de la región
    size = 3.5,                   # Tamaño del texto
    nudge_x = 0.25,               # Empuja la etiqueta un poco a la derecha del punto
    direction = "y",              # Prioriza mover etiquetas arriba/abajo para evitar choques
    hjust = 0,                    # Alinea el texto a la izquierda
    segment.color = "grey50"      # Color de la línea que une el punto y la etiqueta
  ) +
  # -------------------------

labs(
  title = "Comparación del IPC promedio por Nivel Educativo (5 Grupos)",
  subtitle = "Todas las regiones de la ENCC 2022/23",
  x = "Nivel Educativo (Agrupado)",
  y = "IPC promedio"
  # Ya no necesitamos 'color = "Región"' en labs()
) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none" # <-- Ocultamos la leyenda
  )

