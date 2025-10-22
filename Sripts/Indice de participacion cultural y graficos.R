library(dplyr)

# Vector con columnas culturales (asegurarse que existan en df_tp_final)
cols_culturales <- c(
  "VE_TV_AIRE_CABLE", "FREC_TV_AIRE", "FREC_TV_CABLE", "CONSUMO_STREAMING", "FREC_STREAMING", "tv17",
  "FUE_CINE", "FREC_CINE", "CONTEO_CINE",
  "LEE_LIBROS", "FREC_LIBROS_PAPEL", "FREC_LIBROS_DIGITAL", "libro3.3", "CONTEO_LIBROS",
  "ESCUCHA_RADIO", "radio4.1", "radio4.2", "radio4.3", "radio4.4", "ESCUCHA_PODCASTS",
  "ESCUCHA_MUSICA", "FREC_MUSICA", "FUE_RECITALES", "musica11",
  "vj1", "vj2",
  "teatro1", "teatro3",
  "pat1", "pat3", "pat4",
  paste0("pat7.", 1:19),
  "USA_INTERNET", "int3", "int4", "int5", "BUSCA_CULTURA_ONLINE", "int7.5"
)

# Solo tomar columnas que realmente existen en df_tp_final
cols_culturales <- intersect(cols_culturales, names(df_tp_final))

# Función de puntaje
puntuar_frecuencia <- function(x) {
  dplyr::case_when(
    x %in% c("Nunca", "No fue nunca", "NO", "Ns/Nc", "NsNc", "Ns/Nc (NO LEER)") ~ 0,
    x %in% c("Hace más de 5 años", "Algunas veces al año") ~ 1,
    x %in% c("Entre 1 y 5 años", "Algunas veces al mes", "1 vez en el último año") ~ 2,
    x %in% c("Algunas veces a la semana") ~ 3,
    x %in% c("Todos o casi todos los días", 
             "Todos o casi todas las semanas", 
             "Todos o casi todas los meses",
             "Varias veces en el último año") ~ 4,
    x %in% c("SI", "Sí") ~ 1,
    TRUE ~ NA_real_
  )
}

# Crear índice
df_ipc <- df_tp_final %>%
  mutate(across(all_of(cols_culturales), puntuar_frecuencia)) %>%
  rowwise() %>%
  mutate(IPC_raw = sum(c_across(all_of(cols_culturales)), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    IPC = round((IPC_raw / max(IPC_raw, na.rm = TRUE)) * 9 + 1, 1)
  )

# Revisar
df_ipc %>% select(IPC) %>% summary()



