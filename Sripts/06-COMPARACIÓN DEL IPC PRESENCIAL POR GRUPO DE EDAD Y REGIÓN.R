# ============================================================
# 11. COMPARACIÓN DEL IPC PRESENCIAL POR GRUPO DE EDAD Y REGIÓN
# ============================================================

# Definir actividades presenciales
cols_presenciales <- c("FUE_TEATRO", "FUE_CINE", "FUE_RECITALES", "pat1")

# Verificar qué columnas existen realmente en tu df_ipc
cols_presenciales <- intersect(cols_presenciales, names(df_ipc))
print(cols_presenciales)

# Calcular IPC presencial (suma de esas actividades, normalizada de 0 a 10)
df_ipc <- df_ipc %>%
  mutate(across(all_of(cols_presenciales), ~ ifelse(. %in% c("SI", "Sí", "Si"), 1,
                                                    ifelse(is.na(.), 0, .)))) %>%
  rowwise() %>%
  mutate(IPC_presencial_raw = sum(c_across(all_of(cols_presenciales)), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(IPC_presencial = round((IPC_presencial_raw / max(IPC_presencial_raw, na.rm = TRUE)) * 10, 1))

# ============================================================
# Gráfico: IPC presencial promedio por grupo de edad y región
# ============================================================

ipc_presencial_region <- df_ipc %>%
  group_by(REGION, GRUPO_EDAD) %>%
  summarise(IPC_presencial_promedio = mean(IPC_presencial, na.rm = TRUE), .groups = "drop")

# Gráfico de líneas comparativas
ggplot(ipc_presencial_region, aes(x = GRUPO_EDAD, y = IPC_presencial_promedio, group = REGION, color = REGION)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Comparación del IPC presencial por grupo de edad",
    subtitle = "Solo actividades presenciales: teatro, cine, recitales y patrimonio",
    x = "Grupo de edad",
    y = "IPC presencial promedio",
    color = "Región"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

