library(dplyr)

# Función de puntaje 0-5
puntuar_frecuencia <- function(x) {
  x_clean <- toupper(trimws(as.character(x)))
  
  nunca <- c("NUNCA", "NO FUE NUNCA", "NO", "NS/NC", "NSNC", "NS/NC (NO LEER)")
  muy_baja <- c("HACE MÁS DE 5 AÑOS", "ALGUNAS VECES AL AÑO")
  baja <- c("ENTRE 1 Y 5 AÑOS", "1 VEZ EN EL ÚLTIMO AÑO")
  media <- c("ALGUNAS VECES AL MES")
  alta <- c("ALGUNAS VECES A LA SEMANA")
  muy_alta <- c("TODOS O CASI TODOS LOS DÍAS", 
                "TODOS O CASI TODAS LAS SEMANAS", 
                "TODOS O CASI TODOS LOS MESES",
                "VARIAS VECES EN EL ÚLTIMO AÑO")
  si <- c("SI", "SÍ")
  
  dplyr::case_when(
    x_clean %in% nunca ~ 0,
    x_clean %in% muy_baja ~ 1,
    x_clean %in% baja ~ 2,
    x_clean %in% media ~ 3,
    x_clean %in% alta ~ 4,
    x_clean %in% muy_alta ~ 5,
    x_clean %in% si ~ 1,
    TRUE ~ NA_real_
  )
}

# Columnas por categoría
cols_por_categoria <- list(
  teatro = c("teatro3"),
  pat = c("pat1", "pat3", "pat4", paste0("pat7.", 1:19)),
  cine = c("FUE_CINE", "FREC_CINE", "CONTEO_CINE"),
  recital = c("FUE_RECITALES", "musica11"),
  libros = c("LEE_LIBROS", "FREC_LIBROS_PAPEL", "FREC_LIBROS_DIGITAL", "libro3.3", "CONTEO_LIBROS"),
  streaming = c("CONSUMO_STREAMING", "FREC_STREAMING", "tv17"),
  noticias = c("VE_TV_AIRE_CABLE", "FREC_TV_AIRE", "FREC_TV_CABLE"),
  radio = c("ESCUCHA_RADIO", "radio4.1", "radio4.2", "radio4.3", "radio4.4", "ESCUCHA_PODCASTS"),
  musica = c("ESCUCHA_MUSICA", "FREC_MUSICA"),
  internet = c("USA_INTERNET", "int3", "int4", "int5"),
  busca = c("BUSCA_CULTURA_ONLINE", "int7.5")
)

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

# Filtrar columnas existentes
cols_por_categoria <- lapply(cols_por_categoria, function(x) intersect(x, names(df_tp_final)))

# Calcular IPC
df_ipc <- df_tp_final %>%
  # Convertir todas las columnas culturales a carácter y puntuar
  mutate(across(unlist(cols_por_categoria), ~puntuar_frecuencia(as.character(.)))) %>%
  # Convertir explícitamente a numérico
  mutate(across(unlist(cols_por_categoria), as.numeric)) %>%
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

# Revisar resultados
summary(df_ipc$IPC)
hist(df_ipc$IPC, breaks = 20, main = "Distribución IPC", xlab = "IPC")
