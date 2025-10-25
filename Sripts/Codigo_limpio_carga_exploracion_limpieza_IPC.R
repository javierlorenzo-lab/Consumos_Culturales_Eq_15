#----------------------------
# Carga de librerías
#----------------------------
library(tidyverse)
library(readr)
library(dplyr)

# Carga de datos

encc_22_23_raw <- read_csv(
  "data/raw_data/base-datos-encc-2022-2023.csv",
  col_types = cols(
    musica14.1 = col_character()  # columna problemática
  )
)

# Revisar problemas de parsing
problems(encc_22_23_raw)

# Selección de columnas

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


# Renombrar columnas

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
    NIVEL_EDUCATIVO = soc13.1,
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


# columnas culturales


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

cols_por_categoria <- lapply(cols_por_categoria, function(x) intersect(x, names(df_tp_final)))

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
    x_clean %in% si ~ 1,
    TRUE ~ 0
  )
}

# Aplicar puntaje y convertir a numérico
df_tp_final[unlist(cols_por_categoria)] <- lapply(
  df_tp_final[unlist(cols_por_categoria)],
  function(x) as.numeric(puntuar_frecuencia(x))
)

df_tp_final[unlist(cols_por_categoria)] <- lapply(
  df_tp_final[unlist(cols_por_categoria)],
  function(x) ifelse(is.na(x), 0, x)
)

# Calculo IPC


df_ipc <- df_tp_final %>%
  rowwise() %>%
  mutate(
    IPC_raw = sum(
      sapply(names(cols_por_categoria), function(cat) {
        sum(c_across(all_of(cols_por_categoria[[cat]])), na.rm = TRUE) * coeficientes[[cat]]
      })
    )
  ) %>%
  ungroup() %>%
  mutate(
    IPC = round((IPC_raw / max(IPC_raw, na.rm = TRUE)) * 9 + 1, 1)
  )

# Resumen del IPC
summary(df_ipc$IPC)

