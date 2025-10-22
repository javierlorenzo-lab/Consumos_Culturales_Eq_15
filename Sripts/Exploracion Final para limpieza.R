# ============================================================
# CARGA DE LIBRERÍAS
# ============================================================
library(tidyverse)
library(readr)

# ============================================================
# CARGA DE BASES DE DATOS
# ============================================================
df_22_23 <- read_csv("C:/Users/loren/Git-Proyectos/Consumos_Culturales_Eq_15/Data/raw_data/base-datos-encc-2022-2023.csv")

# Visualizar estructura y nombres
spec(df_22_23)
names(df_22_23)

# ============================================================
# SELECCIÓN DE COLUMNAS DE INTERÉS
# ============================================================

columnas_a_mantener <- c(
  # Datos Sociodemográficos
  "region", "localidad", "genero", "edad", "grupos_edad", 
  "nse_3", "expansor", "ponderador",
  
  # Televisión y Plataformas
  "tv1", "tv6.1", "tv6.2", "tv9", "tv13.1", "tv16", "tv17",
  
  # Cine
  "cine1", "cine3", "cine4",
  
  # Noticias
  "noti1", "noti3.1", "noti3.2", "noti3.3",
  
  # Lectura
  "libro1", "libro3.1", "libro3.2", "libro3.3", "libro5", "libro11",
  
  # Radio y Podcasts
  "radio1", "radio4.1", "radio4.2", "radio4.3", "radio4.4", "radio8",
  
  # Música
  "musica1", "musica3", "musica9", "musica11", "musica12", "musica14",
  
  # Videojuegos
  "vj1", "vj2", "vj6", "vj6.1", "vj7",
  
  # Teatro
  "teatro1", "teatro3", "teatro4", "teatro5",
  
  # Patrimonio
  "pat1", "pat3", "pat4",
  paste0("pat7.", 1:19),
  
  # Internet y Redes Sociales
  "int1", "int3", "int4", "int5", "int7.4", "int7.5",
  
  # Formación y Comunidad
  "forma1_1", "comun1_1", "comun4",
  
  # Nivel educativo (Socio)
  "soc13.1"
)

# Agregar dinámicamente todas las columnas que comienzan con "soc"
cols_soc <- names(df_22_23)[str_detect(names(df_22_23), "^soc")]
columnas_a_mantener <- unique(c(columnas_a_mantener, cols_soc))

# ============================================================
# CREACIÓN DEL DATAFRAME FINAL
# ============================================================
df_tp_final <- df_22_23 %>%
  select(all_of(columnas_a_mantener)) %>%
  rename(
    # Demográficas
    PONDERADOR = ponderador,
    NSE = nse_3,
    EXPANSOR = expansor,
    GENERO = genero,
    
    # Televisión
    VE_TV_AIRE_CABLE = tv1,
    FREC_TV_AIRE = tv6.1,
    FREC_TV_CABLE = tv6.2,
    CONSUMO_STREAMING = tv9,
    FREC_STREAMING = tv13.1,
    
    # Cine
    FUE_CINE = cine1,
    FREC_CINE = cine3,
    CONTEO_CINE = cine4,
    
    # Lectura
    LEE_LIBROS = libro1,
    FREC_LIBROS_PAPEL = libro3.1,
    FREC_LIBROS_DIGITAL = libro3.2,
    CONTEO_LIBROS = libro5,
    
    # Radio / Podcasts
    ESCUCHA_RADIO = radio1,
    ESCUCHA_PODCASTS = radio8,
    
    # Música
    ESCUCHA_MUSICA = musica1,
    FREC_MUSICA = musica3,
    FUE_RECITALES = musica9,
    
    # Teatro
    CONTEO_TEATRO = teatro4,
    
    # Internet
    USA_INTERNET = int1,
    BUSCA_CULTURA_ONLINE = int7.4,
    
    # Nivel educativo
    NIVEL_EDUCATIVO = soc13.1
  )

# ============================================================
# INSPECCIÓN DE LOS DATOS
# ============================================================
df_tp_final %>% glimpse()
df_tp_final %>% head(10)

# ============================================================
# ANÁLISIS EXPLORATORIO INICIAL
# ============================================================

# Distribución del Nivel Socioeconómico
df_tp_final %>%
  ggplot(aes(x = NSE, fill = NSE)) +
  geom_bar() +
  labs(
    title = "Distribución de la Población por NSE (Ponderado)",
    y = "Población Representada",
    x = "Nivel Socioeconómico"
  )


#----------------------------------------------
#Leer respuesta de todos los campos
#--------------------------------------------

df_tp_final %>%
  select(where(is.character) | where(is.factor)) %>%
  map(unique)
