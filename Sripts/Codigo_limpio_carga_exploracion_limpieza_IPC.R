
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)


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


#Graficos con IPC


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



#Boxplot
ggplot(df_ipc, aes(x = grupos_edad, y = IPC, color = NSE)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(
    x = "Grupo de edad",
    y = "Índice de Participación Cultural (IPC)",
    color = "Nivel Socioeconómico",
    title = "Distribución del IPC por grupo de edad y NSE"
  ) +
  theme_minimal()


#violin

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


library(ggplot2)

# Asegurar que NSE sea categórica
df_ipc$NSE <- as.factor(df_ipc$NSE)

# Gráfico combinado: violín + boxplot + puntos individuales
p <- ggplot(df_ipc, aes(x = grupos_edad, y = IPC, fill = NSE)) +
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
print(p)

# Guardar en PDF
ggsave("ipc_violin_box_jitter.pdf", plot = p, width = 8, height = 5)


#Barras

df_ipc %>%
  group_by(grupos_edad, NSE) %>%
  summarise(ipc_promedio = mean(IPC, na.rm = TRUE)) %>%
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



###Modelado




modelo_ipc <- lm(IPC ~ NSE + grupos_edad + GENERO, data = df_ipc)
summary(modelo_ipc)



# Residuos

# Extraer valores ajustados y residuos
valores_ajustados <- fitted(modelo_ipc)
residuos <- resid(modelo_ipc)

# Crear data frame
df_res <- data.frame(
  valores_ajustados = valores_ajustados,
  residuos = residuos
)

# Ver las primeras filas
head(df_res)


ggplot(df_res, aes(x = valores_ajustados, y = residuos)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Valores ajustados",
    y = "Residuos",
    title = "Residuos vs Valores Ajustados"
  ) +
  theme_minimal()


library(ggplot2)

# Extraer residuos y valores ajustados
residuos <- resid(modelo_ipc)
valores_ajustados <- fitted(modelo_ipc)

# Crear data frame
df_res <- data.frame(
  valores_ajustados = valores_ajustados,
  residuos = residuos
)

# Gráfico
ggplot(df_res, aes(x = valores_ajustados, y = residuos)) +
  geom_point(alpha = 0.6, color = "blue") +          # puntos de residuos
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +  # recta en y=0
  labs(
    x = "Valores ajustados",
    y = "Residuos",
    title = "Residuos vs Valores Ajustados"
  ) +
  theme_minimal(base_size = 13)


library(ggplot2)

library(ggplot2)

# Valores ajustados y residuos
residuos <- resid(modelo_ipc)
valores_ajustados <- fitted(modelo_ipc)
df_res <- data.frame(valores_ajustados = valores_ajustados, residuos = residuos)

# Gráfico solo con la recta
ggplot(df_res, aes(x = valores_ajustados, y = residuos)) +
  geom_abline(intercept = 0, slope = 0, color = "red", linetype = "dashed", size = 1) +
  labs(
    x = "Valores ajustados",
    y = "Residuos",
    title = "Recta de referencia sobre residuos (pendiente = 0)"
  ) +
  theme_minimal()



library(ggplot2)

# Extraer residuos y valores ajustados
residuos <- resid(modelo_ipc)
valores_ajustados <- fitted(modelo_ipc)
df_res <- data.frame(
  valores_ajustados = valores_ajustados,
  residuos = residuos
)

# Gráfico con puntos y rectas
ggplot(df_res, aes(x = valores_ajustados, y = residuos)) +
  geom_point(alpha = 0.6, color = "blue") +                    # puntos de residuos
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +  # recta ideal
  geom_smooth(method = "lm", se = TRUE, color = "red") +      # recta de tendencia real
  labs(
    x = "Valores ajustados",
    y = "Residuos",
    title = "Residuos vs Valores Ajustados con rectas de referencia y tendencia"
  ) +
  theme_minimal(base_size = 13)



library(ggplot2)

# Asegurarse de que NSE sea factor
df_ipc$NSE <- as.factor(df_ipc$NSE)

# Extraer residuos y valores ajustados
residuos <- resid(modelo_ipc)
valores_ajustados <- fitted(modelo_ipc)

# Crear data frame para ggplot
df_res <- data.frame(
  valores_ajustados = valores_ajustados,
  residuos = residuos,
  NSE = df_ipc$NSE,
  grupos_edad = df_ipc$grupos_edad
)

# Crear gráfico
p <- ggplot(df_res, aes(x = valores_ajustados, y = residuos, color = NSE)) +
  geom_point(alpha = 0.5, size = 2, position = position_jitter(width = 0.1)) +  # puntos semi-translúcidos
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +  # recta referencia
  geom_smooth(aes(group = 1), method = "lm", se = TRUE, color = "red") +        # recta tendencia
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
print(p)

# Guardar directamente en PDF
ggsave("residuos_ipc_NSE.pdf", plot = p, width = 8, height = 5)

