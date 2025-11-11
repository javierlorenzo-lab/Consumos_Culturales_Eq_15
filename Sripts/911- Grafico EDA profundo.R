# ============================================================
# SCRIPT CONSOLIDADO DE GRÁFICOS COMPARATIVOS (VERSIÓN CORREGIDA)
# ============================================================

# --- 0. LIBRERÍAS ---
# Asegúrate de tener tidyverse y ggrepel instalados
# install.packages("ggrepel") 
library(tidyverse)
library(ggrepel) 
library(stringr)   


# ============================================================
# 1. PREPARACIÓN DE DATOS (CON TUS VALORES ESPECÍFICOS)
# ============================================================

# --- 1.A Preparación: Nivel Educativo (5 Grupos) ---
# (Esta sección no cambia, ya que usa detección de texto robusta)
niveles_final_ordenados <- c(
  "Sin Estudios", "Primario", "Secundario", "Universitario", "Posgrado"
)

df_ipc_edu_final <- df_ipc %>%
  mutate(NIVEL_EDUCATIVO_lower = tolower(NIVEL_EDUCATIVO)) %>%
  mutate(
    NIVEL_EDUCATIVO_AGRUPADO = case_when(
      str_detect(NIVEL_EDUCATIVO_lower, "posgrado") ~ "Posgrado",
      str_detect(NIVEL_EDUCATIVO_lower, "universitario") |
        str_detect(NIVEL_EDUCATIVO_lower, "terciario") ~ "Universitario",
      str_detect(NIVEL_EDUCATIVO_lower, "secundario") ~ "Secundario",
      str_detect(NIVEL_EDUCATIVO_lower, "primario completo") ~ "Primario",
      str_detect(NIVEL_EDUCATIVO_lower, "inicial") |
        str_detect(NIVEL_EDUCATIVO_lower, "primario incompleto") ~ "Sin Estudios",
      TRUE ~ "Otro" 
    )
  ) %>%
  filter(NIVEL_EDUCATIVO_AGRUPADO != "Otro") %>%
  mutate(
    NIVEL_EDUCATIVO_FINAL_ORD = factor(NIVEL_EDUCATIVO_AGRUPADO, 
                                       levels = niveles_final_ordenados)
  )

# --- 1.B Preparación: NSE (3 Grupos) ---
# !! CORREGIDO !!: Usando tus valores exactos de NSE y su orden lógico
nse_orden_logico <- c("D1+D2+E", "C2+C3", "ABC1") # Orden: Bajo -> Medio -> Alto

df_ipc_nse <- df_ipc %>%
  # Filtramos para mantener solo los valores que nos diste
  filter(NSE %in% nse_orden_logico) %>%
  mutate(
    # Creamos el factor ordenado
    NSE_ORD = factor(NSE, levels = nse_orden_logico)
  )

# --- 1.C Preparación: Edad (Grupos) ---
# !! CORREGIDO !!: Usando tus valores de edad y su orden lógico
edad_orden_logico <- c(
  "13 a 17 años",
  "18 a 29 años",
  "30 a 49 años",
  "50 a 64 años",
  "65 años y mas"
)

df_ipc_edad <- df_ipc %>%
  filter(grupos_edad %in% edad_orden_logico) %>% # Filtra solo los grupos deseados
  mutate(
    # Creamos el factor ordenado
    EDAD_ORD = factor(grupos_edad, levels = edad_orden_logico)
  )


# ============================================================
# 2. SECCIÓN DE GRÁFICOS (Ahora usarán los datos corregidos)
# ============================================================

# --- GRÁFICOS IPC vs NSE ---

# 2.1 Gráfico IPC vs NSE (Solo CABA)
ipc_nse_caba <- df_ipc_nse %>%
  filter(region == "CABA") %>%
  group_by(NSE_ORD) %>%
  summarise(IPC_promedio = mean(IPC, na.rm = TRUE), .groups = "drop")

g1 <- ggplot(ipc_nse_caba, aes(x = NSE_ORD, y = IPC_promedio, group = 1)) +
  geom_line(color = "#1f78b4", size = 1.2) +
  geom_point(color = "#1f78b4", size = 3) +
  labs(
    title = "IPC promedio según Nivel Socioeconómico (NSE)",
    subtitle = "Región: CABA",
    x = "Nivel Socioeconómico",
    y = "IPC promedio"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
print(g1)


# 2.2 Gráfico IPC vs NSE (Todas las Regiones)
ipc_nse_todas <- df_ipc_nse %>%
  group_by(region, NSE_ORD) %>%
  summarise(IPC_promedio = mean(IPC, na.rm = TRUE), .groups = "drop")

# !! CORREGIDO !!: La etiqueta final es "ABC1" (el último nivel)
etiquetas_nse <- ipc_nse_todas %>%
  filter(NSE_ORD == "ABC1") 

g2 <- ggplot(ipc_nse_todas, aes(x = NSE_ORD, y = IPC_promedio, group = region, color = region)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 2, alpha = 0.8) +
  geom_text_repel(
    data = etiquetas_nse,   
    aes(label = region),
    size = 3.5, nudge_x = 0.25, direction = "y", hjust = 0, segment.color = "grey50"
  ) +
  labs(
    title = "Comparación del IPC promedio por NSE",
    subtitle = "Todas las regiones de la ENCC 2022/23",
    x = "Nivel Socioeconómico",
    y = "IPC promedio"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none"
  )
print(g2)


# --- GRÁFICOS IPC vs NIVEL EDUCATIVO ---

# 2.3 Gráfico IPC vs Nivel Educativo (Solo CABA)
ipc_edu_caba <- df_ipc_edu_final %>%
  filter(region == "CABA") %>%
  group_by(NIVEL_EDUCATIVO_FINAL_ORD) %>%
  summarise(IPC_promedio = mean(IPC, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(NIVEL_EDUCATIVO_FINAL_ORD))

g3 <- ggplot(ipc_edu_caba, aes(x = NIVEL_EDUCATIVO_FINAL_ORD, y = IPC_promedio, group = 1)) +
  geom_line(color = "#1f78b4", size = 1.2) +
  geom_point(color = "#1f78b4", size = 3) +
  labs(
    title = "IPC promedio por Nivel Educativo",
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
print(g3)


# 2.4 Gráfico IPC según Nivel Educativo (Todas las Regiones)
ipc_edu_todas <- df_ipc_edu_final %>%
  group_by(region, NIVEL_EDUCATIVO_FINAL_ORD) %>%
  summarise(IPC_promedio = mean(IPC, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(NIVEL_EDUCATIVO_FINAL_ORD))

etiquetas_edu <- ipc_edu_todas %>%
  filter(NIVEL_EDUCATIVO_FINAL_ORD == "Posgrado") # Etiqueta al final

g4 <- ggplot(ipc_edu_todas, aes(x = NIVEL_EDUCATIVO_FINAL_ORD, y = IPC_promedio, group = region, color = region)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 2, alpha = 0.8) +
  geom_text_repel(
    data = etiquetas_edu,   
    aes(label = region),
    size = 3.5, nudge_x = 0.25, direction = "y", hjust = 0, segment.color = "grey50"
  ) +
  labs(
    title = "Comparación del IPC promedio según Nivel Educativo",
    subtitle = "Todas las regiones de la ENCC 2022/23",
    x = "Nivel Educativo (Agrupado)",
    y = "IPC promedio"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )
print(g4)


# --- GRÁFICOS IPC vs GRUPOS DE EDAD ---

# 2.5 Gráfico IPC vs Edad (Solo CABA)
ipc_edad_caba <- df_ipc_edad %>%
  filter(region == "CABA") %>%
  group_by(EDAD_ORD) %>% # !! CORREGIDO !!: Usar la variable ordenada
  summarise(IPC_promedio = mean(IPC, na.rm = TRUE), .groups = "drop")

g5 <- ggplot(ipc_edad_caba, aes(x = EDAD_ORD, y = IPC_promedio, group = 1)) + # !! CORREGIDO !!
  geom_line(color = "#1f78b4", size = 1.2) +
  geom_point(color = "#1f78b4", size = 3) +
  labs(
    title = "IPC promedio según Grupos de Edad",
    subtitle = "Región: CABA",
    x = "Grupo de Edad",
    y = "IPC promedio"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
print(g5)


# 2.6 Gráfico IPC vs Edad (Todas las Regiones)
ipc_edad_todas <- df_ipc_edad %>%
  group_by(region, EDAD_ORD) %>% # !! CORREGIDO !!: Usar la variable ordenada
  summarise(IPC_promedio = mean(IPC, na.rm = TRUE), .groups = "drop")

# !! CORREGIDO !!: Usando tu valor exacto "65 años y mas"
etiquetas_edad <- ipc_edad_todas %>%
  filter(EDAD_ORD == "65 años y mas") 

g6 <- ggplot(ipc_edad_todas, aes(x = EDAD_ORD, y = IPC_promedio, group = region, color = region)) + # !! CORREGIDO !!
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 2, alpha = 0.8) +
  geom_text_repel(
    data = etiquetas_edad,   
    aes(label = region),
    size = 3.5, nudge_x = 0.25, direction = "y", hjust = 0, segment.color = "grey50"
  ) +
  labs(
    title = "Comparación del IPC promedio según Grupos de Edad",
    subtitle = "Todas las regiones de la ENCC 2022/23",
    x = "Grupo de Edad",
    y = "IPC promedio"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )
print(g6)

