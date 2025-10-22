library(tidyverse)
library(readr)


#CARGA DE BASES DE DATOS

df_historica <- read_csv("encc-serie-historica.csv")
df_22_23 <- read_csv("base-datos-encc-2022-2023.csv")
df_2017 <- read_csv("encc_2017.csv")


spec(df_historica)
spec(df_22_23)
spec(df_2017)
names(df_22_23)


columnas_a_mantener <- c(
  # Datos Sociodemográficos
  "region", "localidad", "genero", "edad", "grupos_edad", "nse_3", "expansor", "ponderador",
  
  # Televisión y Plataformas
  "tv1", "tv6.1", "tv6.2", "tv9", "tv13.1",
  
  # Cine
  "cine1", "cine3", "cine4",
  
  # Noticias
  "noti1", "noti3.1", "noti3.2", "noti3.3",
  
  # Lectura
  "libro1", "libro3.1", "libro3.2", "libro3.3", "libro5","libro11",
  
  # Radio y Podcasts
  "radio1", "radio4.1", "radio4.2", "radio4.3", "radio4.4", "radio8",
  
  # Música
  "musica1", "musica3", "musica9", "musica11", "musica12", "musica14",
  
  # Videojuegos
  "vj1", "vj2", "vj6","vj6.1","vj7",
  
  # Teatro
  "teatro1", "teatro3", "teatro4","teatro5",
  
  # Patrimonio
  "pat1", "pat3","pat4",
  "pat7.1", "pat7.2", "pat7.3", "pat7.4", "pat7.5", "pat7.6", "pat7.7", 
  "pat7.8", "pat7.9", "pat7.10", "pat7.11", "pat7.12", "pat7.13", "pat7.14", 
  "pat7.15", "pat7.16", "pat7.17", "pat7.18", "pat7.19",
  
  # Internet y Redes Sociales
  "int1", "int3", "int4","int5", "int7.4", "int7.5",
  
  # Formación y Comunidad
  "forma1_1", # Asumido como la primera opción de FORMA1
  "comun1_1", # Asumido como la primera opción de COMUN1
  "comun4",
  
  # Uso del Tiempo / Socio (Nivel educativo)
  "soc13.1"
)

# 4. Crear el DataFrame final seleccionando SOLO las columnas de interés
df_tp_final <- df_22_23 %>%
  select(all_of(columnas_a_mantener)) %>%
  # Renombrar variables para claridad en el análisis grupal
  rename(
    # Demográficas y Ponderación
    PONDERADOR = ponderador,
    NSE = nse_3,
    EXPANSOR = expansor,
    GENERO = genero,
    
    # Televisión y Plataformas
    VE_TV_AIRE_CABLE = tv1,
    FREC_TV_AIRE = tv6.1,
    FREC_TV_CABLE = tv6.2,
    CONSUMO_STREAMING = tv9,
    FREC_STREAMING = tv13.1, # Frecuencia de plataformas
    
    # Cine
    FUE_CINE = cine1,
    FREC_CINE = cine3,
    CONTEO_CINE = cine4,
    
    # Lectura
    LEE_LIBROS = libro1,
    CONTEO_LIBROS = libro5,
    FREC_LIBROS_PAPEL = libro3.1,
    FREC_LIBROS_DIGITAL = libro3.2,
    
    # Radio y Podcasts
    ESCUCHA_RADIO = radio1,
    ESCUCHA_PODCASTS = radio8,
    
    # Música y Teatro
    ESCUCHA_MUSICA = musica1,
    FREC_MUSICA = musica3,
    FUE_RECITALES = musica9,
    CONTEO_TEATRO = teatro4,
    
    # Internet
    USA_INTERNET = int1,
    BUSCA_CULTURA_ONLINE = int7.4,
    
    # Nivel Educativo
    NIVEL_EDUCATIVO = soc13.1
  )

# Estructura del DataFrame 


df_tp_final %>% glimpse()
df_tp_final %>% head(10)





#Grupos Nivel Socio Economico

df_tp_final %>%
  ggplot(aes(x = NSE,  fill = NSE)) +
  geom_bar() +
  labs(title = "Distribución de la Población por NSE (Ponderado)",
       y = "Población Representada")



#--------------

df_tp_final%>%
  group_by("EXPANSOR")
  summarise(n())

  
unique(df_tp_final$CINE)
glimpse(df_tp_final)

#---- Graficos 
#-----------------------------------------------------
unique(df_tp_final$FREC_LIBROS_DIGITAL)

df_tp_final %>%
  ggplot(aes(x = FREC_LIBROS_DIGITAL)) +
  geom_bar() +
  labs(y = "Población Representada")

df_tp_final %>%
  ggplot(aes(x = FREC_LIBROS_DIGITAL, fill=NSE)) +
  geom_bar(position = "dodge") +
  labs(y = "Población Representada")
#-----------------------------------------------------
df_tp_final %>%
  ggplot(aes(x = vj1)) +
  geom_bar() +
  labs(y = "Población Representada")

df_tp_final %>%
  ggplot(aes(x = vj1, fill=grupos_edad)) +
  geom_bar(position="dodge") +
  labs(y = "Población Representada")

df_tp_final %>%
  ggplot(aes(x = vj2)) +
  geom_bar() +
  labs(y = "Población Representada")
#-----------------------------------------------------
df_tp_final %>%
  ggplot(aes(x = teatro3)) +
  geom_bar() +
  labs(y = "Población Representada")
#-
df_tp_final %>%
  ggplot(aes(x = FREC_LIBROS_DIGITAL, fill=NSE)) +
  geom_bar(position = "fill") +
  labs(y = "Población Representada")
