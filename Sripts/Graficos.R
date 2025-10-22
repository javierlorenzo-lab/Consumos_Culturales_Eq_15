# Grupos Nivel Socio Económico

encc_Final %>%
  ggplot(aes(x = NSE, fill = NSE)) +
  geom_bar() +
  labs(
    title = "Distribución de la Población por NSE (Ponderado)",
    y = "Población Representada"
  )

# Exploración de variables
unique(encc_Final$FREC_CINE)
unique(encc_Final$FREC_TV_AIRE)
unique(encc_Final$FREC_STREAMING)
unique(encc_Final$FREC_CINE)
unique(encc_Final$CONTEO_CINE)
unique(encc_Final$noti3.1)
unique(encc_Final$noti3.2)
unique(encc_Final$noti3.3)
unique(encc_Final$FREC_LIBROS_PAPEL)
unique(encc_Final$FREC_LIBROS_DIGITAL)
unique(encc_Final$CONTEO_LIBROS)
unique(encc_Final$teatro3)

# Cine
encc_Final %>%
  ggplot(aes(x = FREC_CINE, fill = FREC_CINE)) +
  geom_bar()

encc_Final %>%
  ggplot(aes(x = CONTEO_CINE, fill = CONTEO_CINE)) +
  geom_bar()

# TV Aire
encc_Final %>%
  ggplot(aes(x = FREC_TV_AIRE, fill = FREC_TV_AIRE)) +
  geom_bar()

encc_Final %>%
  ggplot(aes(x = FREC_TV_CABLE, fill = FREC_TV_CABLE)) +
  geom_bar()

# Streaming
encc_Final %>%
  ggplot(aes(x = CONSUMO_STREAMING, fill = CONSUMO_STREAMING)) +
  geom_bar()

# Frecuencia de Streaming
encc_Final %>%
  ggplot(aes(x = FREC_STREAMING, fill = FREC_STREAMING)) +
  geom_bar()

# Libros
encc_Final %>%
  ggplot(aes(x = LEE_LIBROS, fill = LEE_LIBROS)) +
  geom_bar()

encc_Final %>%
  ggplot(aes(x = FREC_LIBROS_PAPEL, fill = FREC_LIBROS_PAPEL)) +
  geom_bar()

encc_Final %>%
  ggplot(aes(x = FREC_LIBROS_DIGITAL, fill = NSE)) +
  geom_bar(position = "dodge")

# Radio
encc_Final %>%
  ggplot(aes(x = ESCUCHA_RADIO, fill = NSE)) +
  geom_bar(position = "dodge")

# Podcasts
encc_Final %>%
  ggplot(aes(x = ESCUCHA_PODCASTS, fill = NSE)) +
  geom_bar(position = "dodge")

# Música
encc_Final %>%
  ggplot(aes(x = ESCUCHA_MUSICA, fill = NSE)) +
  geom_bar(position = "dodge")

# Frecuencia de música
encc_Final %>%
  ggplot(aes(x = FREC_MUSICA, fill = NSE)) +
  geom_bar(position = "dodge")

unique(encc_Final$FREC_MUSICA)
