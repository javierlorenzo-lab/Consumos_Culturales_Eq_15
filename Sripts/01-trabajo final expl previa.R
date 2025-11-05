# ============================================================
# TRABAJO FINAL – Introducción a Ciencia de Datos
# ENCC 2022/23 – Índice de Participación Cultural (IPC)
# ============================================================

# ---- 0. Librerías ----
library(tidyverse)
library(ggplot2)
library(scales)
library(patchwork)

# ============================================================
# 1. EXPLORACIÓN DE LA BASE DE DATOS RAW
# ============================================================

encc_raw <- read_csv("data/raw_data/base-datos-encc-2022-2023.csv",
                     col_types = cols(musica14.1 = col_character()))

# Estructura general
dim(encc_raw)
glimpse(encc_raw)

# Verificar problemas de lectura
problems(encc_raw)

# Exploración básica
summary(encc_raw$edad)
summary(encc_raw$ponderador)

# Conteos principales
table(encc_raw$genero, useNA = "ifany")
table(encc_raw$grupos_edad, useNA = "ifany")
table(encc_raw$region, useNA = "ifany")

# Distribución por región
encc_raw %>%
  count(region) %>%
  ggplot(aes(x = reorder(region, n), y = n, fill = region)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Distribución de casos por región", x = "", y = "Frecuencia") +
  theme_minimal()

# Histograma de edades
ggplot(encc_raw, aes(x = edad)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  labs(title = "Distribución de edades en la muestra", x = "Edad", y = "Frecuencia") +
  theme_minimal()

# ============================================================
# 2. CARGA DE LIMPIEZA Y CORRECCIONES
# ============================================================

# Selección de columnas relevantes
columnas_a_mantener <- c("region", "localidad", "genero", "edad", "grupos_edad",
                         "nse_3", "ponderador", "expansor", "tv1", "tv9", "cine1",
                         "libro1", "radio1", "musica1", "teatro1", "int1")

df <- encc_raw %>% select(any_of(columnas_a_mantener))

# Renombrar variables
df <- df %>%
  rename(
    REGION = region,
    NSE = nse_3,
    GENERO = genero,
    EDAD = edad,
    GRUPO_EDAD = grupos_edad,
    FUE_CINE = cine1,
    LEE_LIBROS = libro1,
    ESCUCHA_MUSICA = musica1,
    FUE_TEATRO = teatro1,
    USA_INTERNET = int1
  )

# ============================================================
# 3. VISUALIZACIONES PREVIAS AL IPC
# ============================================================

df %>%
  count(FUE_CINE) %>%
  mutate(pct = n / sum(n) * 100)

# Gráfico de barras de consumos seleccionados
df_long <- df %>%
  pivot_longer(cols = c(FUE_CINE, LEE_LIBROS, ESCUCHA_MUSICA, FUE_TEATRO, USA_INTERNET),
               names_to = "actividad", values_to = "respuesta")

ggplot(df_long, aes(x = actividad, fill = respuesta)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  labs(title = "Participación por tipo de consumo cultural",
       x = "", y = "% de respuestas", fill = "Respuesta") +
  theme_minimal()

# ============================================================
# 4. RESUMEN DESCRIPTIVO (para tres diapositivas)
# ============================================================

# 1️⃣ Perfil de la muestra
tabla_perfil <- df %>%
  summarise(
    Casos = n(),
    Edad_promedio = mean(EDAD, na.rm = TRUE),
    NSE_mas_frec = names(sort(table(NSE), decreasing = TRUE))[1],
    Genero_mas_frec = names(sort(table(GENERO), decreasing = TRUE))[1]
  )

# 2️⃣ Distribución geográfica
muestra_region <- df %>%
  count(REGION) %>%
  mutate(pct = n / sum(n) * 100)

# 3️⃣ Participación por tipo de actividad
participacion <- df_long %>%
  filter(respuesta == "Sí" | respuesta == "SI") %>%
  count(actividad) %>%
  mutate(pct = n / sum(n) * 100)

# ============================================================
# 5. CÁLCULO DEL ÍNDICE DE PARTICIPACIÓN CULTURAL (IPC)
# ============================================================

# Puntaje simple: 1 si participó en la actividad, 0 si no
df_ipc <- df %>%
  mutate(across(c(FUE_CINE, LEE_LIBROS, ESCUCHA_MUSICA, FUE_TEATRO, USA_INTERNET),
                ~ ifelse(. %in% c("SI", "Sí", "Si"), 1, 0))) %>%
  rowwise() %>%
  mutate(IPC_raw = sum(c_across(c(FUE_CINE, LEE_LIBROS, ESCUCHA_MUSICA, FUE_TEATRO, USA_INTERNET)))) %>%
  ungroup() %>%
  mutate(IPC = round((IPC_raw / max(IPC_raw)) * 10, 1))

summary(df_ipc$IPC)

# ============================================================
# 6. VISUALIZACIONES POSTERIORES AL IPC
# ============================================================

# IPC por grupo de edad y NSE
ggplot(df_ipc, aes(x = GRUPO_EDAD, y = IPC, fill = NSE)) +
  geom_boxplot(alpha = 0.6) +
  labs(title = "Distribución d


