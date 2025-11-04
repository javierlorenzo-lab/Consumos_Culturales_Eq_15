# ============================================================
# 9. COMPARACIÓN CABA vs GBA – IPC promedio por grupo de edad
# ============================================================

# Calcular IPC promedio por grupo de edad en ambas regiones
ipc_edad_region <- df_ipc %>%
  filter(REGION %in% c("CABA", "GBA")) %>%
  group_by(REGION, GRUPO_EDAD) %>%
  summarise(IPC_promedio = mean(IPC, na.rm = TRUE), .groups = "drop")

# Verificar tabla
print(ipc_edad_region)

# Gráfico comparativo
ggplot(ipc_edad_region, aes(x = GRUPO_EDAD, y = IPC_promedio, group = REGION, color = REGION)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("CABA" = "#1f78b4", "GBA" = "#e31a1c")) +
  labs(
    title = "Comparación del IPC promedio por grupo de edad",
    subtitle = "Regiones: CABA vs GBA",
    x = "Grupo de edad",
    y = "IPC promedio",
    color = "Región"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )






# ============================================================
# 10. COMPARACIÓN DEL IPC PROMEDIO EN TODAS LAS REGIONES
# ============================================================

# Calcular el IPC promedio por grupo de edad y región
ipc_region_total <- df_ipc %>%
  group_by(REGION, GRUPO_EDAD) %>%
  summarise(IPC_promedio = mean(IPC, na.rm = TRUE), .groups = "drop")

# Tabla de control (opcional)
print(ipc_region_total %>% head(15))

# Gráfico: líneas comparativas por región
ggplot(ipc_region_total, aes(x = GRUPO_EDAD, y = IPC_promedio, group = REGION, color = REGION)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Comparación del IPC promedio por grupo de edad",
    subtitle = "Todas las regiones de la ENCC 2022/23",
    x = "Grupo de edad",
    y = "IPC promedio",
    color = "Región"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )
