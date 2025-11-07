# ============================================================
# TRABAJO FINAL – Introducción a Ciencia de Datos
# ENCC 2022/23 – Índice de Participación Cultural (IPC)
#
# Versión actualizada con cálculo de IPC ponderado y modelado
# ============================================================


# ============================================================
# 0. LIBRERÍAS
# ============================================================
# Carga de paquetes necesarios para el análisis y visualización
library(tidyverse)
# Nota: 'tidyverse' ya incluye 'readr', 'dplyr' y 'ggplot2'.


# ============================================================
# 1. CARGA DE DATOS
# ============================================================

# Carga de datos
# Nota: La ruta al archivo CSV debe existir en tu computador.
encc_22_23_raw <- read_csv(
  "data/raw_data/base-datos-encc-2022-2023.csv",
  col_types = cols(
    musica14.1 = col_character()  # columna problemática
  )
)

# Revisar problemas de parsing
print(problems(encc_22_23_raw))


# ============================================================
# 2. SELECCIÓN DE COLUMNAS
# ============================================================

columnas_a_mantener <- c(
  "region", "localidad", "genero", "edad", "grupos_edad", "nse_3", "expansor", "ponderador",
  "tv1", "tv6.1", "tv6.2", "tv9", "tv13.1",
  "cine1", "cine3", "cine4",
  "noti1", "noti3.1", "noti3.2", "noti3.3",
  "libro1", "libro3.1", "libro3.2", "libro3.3", "libro5", "libro11",
  "radio1", "radio4.1", "radio4.2", "radio4.3", "radio4.4", "radio8",
  "musica1", "musica3", "musica9", "musica11", "musica12", "musica14",
  "vj1", "vj2", "vj6", "vj6.1", "vj7",
  "teatro1", "teatro3", "teatro4", "teatro5",
  "pat1", "pat3", "pat4", paste0("pat7.", 1:19),
  "int1", "int3", "int4", "int5", "int7.4", "int7.5",
  "forma1_1", "comun1_1", "comun4",
  "soc1_1", "soc1_2", "soc2", "soc3", "soc4",
  "soc5", "soc6", "soc7", "soc8", "soc9", "soc10",
  "soc11", "soc12", "soc13.1", "soc15"
)

df_tp_final <- encc_22_23_raw %>%
  select(any_of(columnas_a_mantener))


# ============================================================
# 3. RENOMBRAR COLUMNAS
# ============================================================

df_tp_final <- df_tp_final %>%
  rename(
    PONDERADOR = ponderador,
    NSE = nse_3,
    EXPANSOR = expansor,
    GENERO = genero,
    VE_TV_AIRE_CABLE = tv1,
    FREC_TV_AIRE = tv6.1,
    FREC_TV_CABLE = tv6.2,
    CONSUMO_STREAMING = tv9,
    FREC_STREAMING = tv13.1,
    FUE_CINE = cine1,
    FREC_CINE = cine3,
    CONTEO_CINE = cine4,
    LEE_LIBROS = libro1,
    FREC_LIBROS_PAPEL = libro3.1,
    FREC_LIBROS_DIGITAL = libro3.2,
    CONTEO_LIBROS = libro5,
    ESCUCHA_RADIO = radio1,
    ESCUCHA_PODCASTS = radio8,
    ESCUCHA_MUSICA = musica1,
    FREC_MUSICA = musica3,
    FUE_RECITALES = musica9,
    FUE_TEATRO = teatro1,
    FREC_TEATRO = teatro3,
    CONTEO_TEATRO = teatro4,
    USA_INTERNET = int1,
    BUSCA_CULTURA_ONLINE = int7.4,
    NIVEL_EDUCATIVO = soc13.1, # Esta es la columna correcta de educación
    EDU_ENCUESTADO = soc1_1,
    EDU_PSH = soc1_2,
    SITUACION_PSH = soc2,
    ULTIMA_OCUP_PSH = soc3,
    COBERTURA_MEDICA_PSH = soc4,
    BIEN_INTERNET = soc5,
    BIEN_AUTO = soc6,
    BIEN_TARJETA = soc7,
    BIEN_CELULAR = soc8,
    BIEN_OTRO = soc9,
    TIPO_VIVIENDA = soc10,
    DIFICULTADES_HOGAR = soc11,
    COBERTURA_SALUD = soc12
  )


# ============================================================
# 4. DEFINICIÓN DE CATEGORÍAS Y FUNCIÓN DE PUNTAJE
# ============================================================

# Columnas culturales agrupadas por categoría
cols_por_categoria <- list(
  teatro = c("FUE_TEATRO", "FREC_TEATRO", "CONTEO_TEATRO"),
  pat = c("pat1", "pat3", "pat4", paste0("pat7.", 1:19)),
  cine = c("FUE_CINE", "FREC_CINE", "CONTEO_CINE"),
  recital = c("FUE_RECITALES", "musica11"),
  libros = c("LEE_LIBROS", "FREC_LIBROS_PAPEL", "FREC_LIBROS_DIGITAL", "CONTEO_LIBROS"),
  streaming = c("CONSUMO_STREAMING", "FREC_STREAMING"),
  noticias = c("VE_TV_AIRE_CABLE", "FREC_TV_AIRE", "FREC_TV_CABLE"),
  radio = c("ESCUCHA_RADIO", "ESCUCHA_PODCASTS"),
  musica = c("ESCUCHA_MUSICA", "FREC_MUSICA"),
  internet = c("USA_INTERNET", "int3", "int4", "int5"),
  busca = c("BUSCA_CULTURA_ONLINE", "int7.5")
)

# Intersección para mantener solo columnas que existen en el dataframe
cols_por_categoria <- lapply(cols_por_categoria, function(x) intersect(x, names(df_tp_final)))

# Coeficientes de ponderación para cada categoría
coeficientes <- list(
  teatro = 1,
  pat = 1,
  cine = 1,
  recital = 1,
  libros = 1,
  streaming = 0.5,
  noticias = 0.5,
  radio = 0.5,
  musica = 0.5,
  internet = 0.5,
  busca = 0.5
)

# Función para asignar puntaje a las respuestas de frecuencia
puntuar_frecuencia <- function(x) {
  x_clean <- toupper(trimws(as.character(x)))
  nunca <- c("NUNCA", "NO FUE NUNCA", "NO", "NS/NC", "NSNC", "NS/NC (NO LEER)")
  muy_baja <- c("HACE MÁS DE 5 AÑOS", "ALGUNAS VECES AL AÑO")
  baja <- c("ENTRE 1 Y 5 AÑOS", "1 VEZ EN EL ÚLTIMO AÑO", "ALGUNAS VECES AL MES")
  media <- c("ALGUNAS VECES A LA SEMANA")
  alta <- c("TODOS O CASI TODOS LOS DÍAS", "TODOS O CASI TODAS LAS SEMANAS",
            "TODOS O CASI TODOS LOS MESES", "VARIAS VECES EN EL ÚLTIMO AÑO")
  si <- c("SI", "SÍ")
  
  dplyr::case_when(
    x_clean %in% nunca ~ 0,
    x_clean %in% muy_baja ~ 1,
    x_clean %in% baja ~ 2,
    x_clean %in% media ~ 3,
    x_clean %in% alta ~ 4,
    x_clean %in% si ~ 1, # Asigna 1 punto a respuestas "SI" genéricas
    TRUE ~ 0 # Por defecto 0
  )
}


# ============================================================
# 5. CÁLCULO DEL ÍNDICE DE PARTICIPACIÓN CULTURAL (IPC)
# ============================================================

# Aplicar puntaje y convertir a numérico
# Se usa unlist() para obtener todos los nombres de columnas
df_tp_final[unlist(cols_por_categoria)] <- lapply(
  df_tp_final[unlist(cols_por_categoria)],
  function(x) as.numeric(puntuar_frecuencia(x))
)

# Reemplazar NAs introducidos (si los hubiera) por 0
df_tp_final[unlist(cols_por_categoria)] <- lapply(
  df_tp_final[unlist(cols_por_categoria)],
  function(x) ifelse(is.na(x), 0, x)
)

# Calculo IPC
df_ipc <- df_tp_final %>%
  rowwise() %>%
  mutate(
    # Suma ponderada de todas las categorías
    IPC_raw = sum(
      sapply(names(cols_por_categoria), function(cat) {
        sum(c_across(all_of(cols_por_categoria[[cat]])), na.rm = TRUE) * coeficientes[[cat]]
      })
    )
  ) %>%
  ungroup() %>%
  mutate(
    # Normalización del IPC de 1 a 10
    IPC = round((IPC_raw / max(IPC_raw, na.rm = TRUE)) * 9 + 1, 1)
  )

# Resumen del IPC
summary(df_ipc$IPC)


# ============================================================
# 6. GRÁFICOS EXPLORATORIOS DEL IPC
# ============================================================

# -- 6.1: Scatterplot Exploratorio (IPC vs. Edad) --
# Un scatterplot es ideal para ver la relación entre dos variables numéricas.
# Usamos geom_jitter() (ruido) y alpha (transparencia) para manejar
# la sobreimpresión (overplotting) de tantos puntos.
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


# -- 6.2: Gráfico de Puntos y Suavizado (por grupos) --
# Gráfico de Puntos y Suavizado
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


# -- 6.3: Boxplot --
# Boxplot
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


# -- 6.4: Gráfico de Violín --
# Gráfico de Violín
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


# -- 6.5: Gráfico combinado (Violín + Boxplot + Jitter) --
# Gráfico combinado: violín + boxplot + puntos individuales (jitter)
# Asegurar que NSE sea categórica
df_ipc$NSE <- as.factor(df_ipc$NSE)

p_violin_box <- ggplot(df_ipc, aes(x = grupos_edad, y = IPC, fill = NSE)) +
  geom_violin(position = position_dodge(width = 0.8), alpha = 0.5, trim = FALSE) +
  geom_boxplot(width = 0.15, position = position_dodge(width = 0.8),
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
ggsave("ipc_violin_box_jitter.pdf", plot = p_violin_box, width = 8, height = 5)


# -- 6.6: Gráfico de Barras (Promedio) --
# Gráfico de Barras (IPC Promedio)
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
# 7. MODELADOS (Regresión Lineal)
# ============================================================

# Modelo simple (comentado)
# modelo_ipc_NSE <- lm(IPC ~ NSE, data = df_ipc)
# summary(modelo_ipc_NSE)

# Modelo múltiple
modelo_ipc <- lm(IPC ~ NSE + grupos_edad + GENERO, data = df_ipc)
summary(modelo_ipc)


# ============================================================
# 8. ANÁLISIS DE RESIDUOS
# ============================================================

# Extraer valores ajustados y residuos
valores_ajustados <- fitted(modelo_ipc)
residuos <- resid(modelo_ipc)

# Crear data frame
df_res <- data.frame(
  valores_ajustados = valores_ajustados,
  residuos = residuos
)

# Gráfico simple de Residuos vs Ajustados
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


# Gráfico con línea de tendencia de residuos
print(
  ggplot(df_res, aes(x = valores_ajustados, y = residuos)) +
    geom_point(alpha = 0.6, color = "blue") +               # puntos de residuos
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
# Crear data frame para ggplot
df_res_full <- data.frame(
  valores_ajustados = valores_ajustados,
  residuos = residuos,
  NSE = df_ipc$NSE,
  grupos_edad = df_ipc$grupos_edad
)

# Crear gráfico
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
ggsave("residuos_ipc_NSE.pdf", plot = p_residuos_nse, width = 8, height = 5)


# ============================================================
# 9. CÓDIGO DE DEPURACIÓN (COMENTADO)
# ============================================================

# Las siguientes líneas eran para depuración y no se ejecutan como parte del script.
# La sintaxis de la primera línea era incorrecta.
# encc_22_23_raw %>% unique(soc13.1)
# unique(soc13.1) # Esto fallaría porque 'soc13.1' no es un objeto

