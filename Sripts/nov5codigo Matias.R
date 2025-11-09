Cambie algunas cosas de los modelos, mejore algunos graficos, añadi nuevos graficos, añadi el modelo solo para bsas y graficos para este (No es un buen modelo,tiene menos r2 y muestra masomenos lo mismo que el modelo general.). Falta limpiarlo mucho

library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)

# Carga de datos

encc_22_23_raw <- read_csv("asd123.csv",
                           col_types = cols(musica14.1 = col_character()))
# Revisar problemas de parsing
problems(encc_22_23_raw)
#sacar no binario
encc_22_23_raw %>% count(genero)
encc_22_23_raw <- encc_22_23_raw %>% filter(genero == "Varón" | genero == "Mujer")


glimpse("base_datos_encc_2022_2023.csv")
unique(df_ipc$NSE)
unique(df_ipc$grupos_edad)
unique(df_ipc$GENERO)
unique(df_ipc$NIVEL_EDUCATIVO)
unique(df_ipc$region)


unique(df_ipc$noti3.1)
unique(df_ipc$vj2)


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

glimpse(df_tp_final)
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

summary(df_ipc$IPC)
glimpse(df_ipc)
###############################Graficos#########################################
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
###############################Graficos#########################################
unique(df_ipc$NIVEL_EDUCATIVO)
df_ipc <- df_ipc %>%
  mutate(
    NIVEL_EDU_SIMPL = case_when(
      NIVEL_EDUCATIVO %in% c("Sin Estudios") ~ "Sin estudios",
      
      NIVEL_EDUCATIVO %in% c("Primarios Completos", "Primarios Incompletos") ~ "Primario",
      
      NIVEL_EDUCATIVO %in% c("Secundarios Completos", "Secundarios Incompletos") ~ "Secundario",
      
      NIVEL_EDUCATIVO %in% c("Terciarios Completos", "Terciarios Incompletos") ~ "Terciario",
      
      NIVEL_EDUCATIVO %in% c("Universitarios Completos", "Universitarios Incompletos") ~ "Universitario",
      
      NIVEL_EDUCATIVO %in% c("Posgrado") ~ "Posgrado",
      
      TRUE ~ "Ns/Nc"
    ),
    NIVEL_EDU_SIMPL = factor(
      NIVEL_EDU_SIMPL,
      levels = c(
        "Sin estudios",
        "Primario",
        "Secundario",
        "Terciario",
        "Universitario",
        "Posgrado",
        "Ns/Nc"
      )
    )
  )

###############################################################
ggplot(df_ipc, aes(x = IPC, y = NIVEL_EDU_SIMPL, fill = NSE)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.alpha = 0.3) +
  labs(
    title = "Distribución del IPC por nivel educativo y NSE",
    x = "Índice de Participación Cultural (IPC)",
    y = "Nivel Educativo",
    fill = "Nivel Socioeconómico"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )


################################################################################
df_summary <- df_ipc %>%
  group_by(NIVEL_EDUCATIVO, NSE) %>%
  summarise(
    mean_ipc = mean(IPC, na.rm = TRUE),
    se = sd(IPC, na.rm = TRUE) / sqrt(n())
  )

ggplot(df_summary, aes(x = NIVEL_EDUCATIVO, y = mean_ipc, color = NSE, group = NSE)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbar(
    aes(ymin = mean_ipc - se, ymax = mean_ipc + se),
    position = position_dodge(width = 0.5),
    width = 0.2
  ) +
  geom_line(position = position_dodge(width = 0.5)) +
  labs(
    title = "Promedio del IPC por nivel educativo y NSE",
    x = "Nivel educativo",
    y = "IPC promedio",
    color = "Nivel Socioeconómico"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

################################################################################
df_ipc %>%
  ggplot(aes(x = NIVEL_EDU_SIMPL, fill = NSE)) +
  geom_bar(position = "fill") +
  labs(
    title = "NSE según nivel educativo",
    x = "Nivel educativo",
    y = "Proporción",
    fill = "NSE"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


df_ipc %>%
  ggplot(aes(x = NSE, fill = NSE)) +
  geom_bar() +
  labs(
    title = "Cantidad de personas por nivel socioeconómico",
    x = "Nivel socioeconómico",
    y = "Cantidad"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

##############3
###Modelado
#Primero graficamos
df_ipc %>%
  group_by(NSE) %>%
  summarise(ipc_promedio = mean(IPC, na.rm = TRUE),
            ipc_sd = sd(IPC, na.rm = TRUE))

ggplot(df_ipc, aes(x = NSE, y = IPC, fill = NSE)) +
  geom_boxplot(alpha = 0.6) +
  theme_minimal()



mod0 <- lm(IPC ~ NSE, data = df_ipc)       
mod1 <- lm(IPC ~ NSE + grupos_edad + GENERO, data = df_ipc)
mod2 <- lm(IPC ~ NSE * grupos_edad + GENERO, data = df_ipc)
mod3 <- lm(IPC ~ NSE + grupos_edad + NIVEL_EDUCATIVO + GENERO, data = df_ipc)
mod4 <- lm(IPC ~ NSE * grupos_edad + NIVEL_EDUCATIVO + GENERO, data = df_ipc)

mod0 <- lm(IPC ~ NSE, data = df_ipc)  
summary(mod0)
mod1 <- lm(IPC ~ NSE+GENERO, data = df_ipc) #Genero no aumenta nada el r2, ni ponerlo      
summary(mod1)
mod2 <- lm(IPC ~ NSE+grupos_edad, data = df_ipc)
summary(mod2)
mod3 <- lm(IPC ~ NSE+grupos_edad, data = df_ipc)   
summary(mod3)
mod4 <- lm(IPC ~ NIVEL_EDU_SIMPL+NSE+grupos_edad_num, data = df_ipc)       
summary(mod4)
df_ipc$grupos_edad <- factor(
  df_ipc$grupos_edad,
  levels = c(
    "13 a 17 años",
    "18 a 29 años",
    "30 a 49 años",
    "50 a 64 años",
    "65 años y mas"
  )
)
df_ipc$grupos_edad_num <- as.numeric(df_ipc$grupos_edad)
mod5 <- lm(IPC ~ NIVEL_EDU_SIMPL + NSE + poly(grupos_edad_num, 2),data = df_ipc)
summary(mod5) #Agregar poly edad no mejora el modelo. Eso porque por mas que se vea esa curva, al agregar mas variables esa curva capaz desaparece o se 
anova(mod4,mod5)
mod4 <- lm(IPC ~ NIVEL_EDU_SIMPL+NSE+grupos_edad_num, data = df_ipc)       
summary(mod4)
mod6 <- lm(IPC ~ NIVEL_EDU_SIMPL+NSE+grupos_edad_num+region, data = df_ipc)       
summary(mod6)
anova(mod4, mod6) #Es significativo, pero no mejora casi nada el r2 y agrega demasiados parametros al modelo
mod7 <- lm(IPC ~ NIVEL_EDU_SIMPL*NSE+grupos_edad_num, data = df_ipc)       
summary(mod7)#Lo mismo de arriba
mod8 <- lm(IPC ~ NIVEL_EDU_SIMPL*NSE*grupos_edad_num, data = df_ipc)       
summary(mod8)#Mismo de arriba
#Modelo elegido el 4
mod4 <- lm(IPC ~ NIVEL_EDU_SIMPL+NSE+grupos_edad_num, data = df_ipc)       










anova(mod0, mod1)  # ¿Edad y género aportan algo más allá del NSE?
anova(mod1, mod2)  # ¿La interaccion NSE * edad mejora el ajuste?
anova(mod2, mod3)  # ¿Agregar nivel educativo (sin interacción) mejora aún más?
anova(mod3, mod4)  # ¿La interacción entre NSE y edad sigue siendo útil después de agregar educación?







anova(mod0,mod1, mod2) #👉 Interpretación: Al pasar del modelo 0 (solo NSE) al modelo 1 
#(sumás edad + género), la RSS baja (de 6232.1 a 6047.5), la diferencia (184.56) es significativa 
#(F = 17.146, p < 2e-16). ✅ Conclusión: agregar edad y género mejora significativamente el modelo.
#Del 1 al 2 la RSS vuelve a bajar (6047.5 → 5981.8), la mejora (65.2) también es estadísticamente 
#significativa (p ≈ 1.4×10⁻⁵). ✅ Conclusión: la interacción entre NSE y edad aporta información real: 
#el efecto del NSE sobre el IPC depende de la edad.


# Residuos

# Extraer valores ajustados y residuos
valores_ajustados <- fitted(mod4)
residuos <- resid(mod4)

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
residuos <- resid(mod4)
valores_ajustados <- fitted(mod4)

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
residuos <- resid(mod4)
valores_ajustados <- fitted(mod4)
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
residuos <- resid(mod4)
valores_ajustados <- fitted(mod4)
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
residuos <- resid(mod4)
valores_ajustados <- fitted(mod4)

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

#Haciendo graficos de residuos vs fitted
library(modelr)
df4<- df_ipc %>% add_predictions(model=mod4)
df2<- df_ipc %>% add_predictions(model=mod2)
dfsg<- df_ipc %>% add_predictions(model=mod3singenero)
dfh<- df_ipc %>% add_predictions(model=mod3)

str(df_ipc$IPC)
table(df_ipc$NSE)



df4 <- df_ipc %>%
  add_predictions(mod4) %>%
  add_residuals(mod4)

df1 <- df_ipc %>%
  add_predictions(mod1) %>%
  add_residuals(mod1)

df2 <- df_ipc %>%
  add_predictions(mod2) %>%
  add_residuals(mod2)

df3 <- df_ipc %>%
  add_predictions(mod3) %>%
  add_residuals(mod3)

df4 <- df_ipc %>%
  add_predictions(mod4) %>%
  add_residuals(mod4)


glimpse(df0)
###########################################################################################
ggplot(df4, aes(x = pred, y = resid)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "loess", se = FALSE, color = "black") +
  labs(
    x = "Valores ajustados (fitted values)",
    y = "Residuos",
    title = "Residuos vs Valores ajustados - Modelo IPC ~ NSE"
  ) +
  theme_minimal(base_size = 13)
###########################################################################################
ggplot(df4, aes(x = pred, y = resid, color = NSE)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(
    x = "Valores ajustados (fitted values)",
    y = "Residuos",
    color = "Nivel Socioeconómico (NSE)",
    title = "Residuos vs Valores Ajustados por NSE (Modelo 3)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right"
  )
###########################################################################################
ggplot(df4, aes(x = pred, y = resid, color = NSE)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(
    x = "Valores ajustados (fitted values)",
    y = "Residuos",
    color = "Nivel Socioeconómico (NSE)",
    title = "Residuos vs Valores Ajustados por NSE (Modelo 1)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right"
  )
###########################################################################################
ggplot(df4, aes(x = pred, y = resid, color = NSE)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(
    x = "Valores ajustados (fitted values)",
    y = "Residuos",
    color = "Nivel Socioeconómico (NSE)",
    title = "Residuos vs Valores Ajustados por NSE (Modelo 4)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right"
  )

summary(mod0)
summary(mod2)
summary(mod3)


df_ipc <- df_ipc %>%
  mutate(pred = predict(mod4))

ggplot(df_ipc, aes(x = pred, y = IPC, color = NSE)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Valores Observados vs Predichos por el Modelo",
    x = "Valores Predichos",
    y = "Valores Observados"
  ) +
  theme_minimal()
summary(mod4)
ggplot(df_ipc, aes(x = pred, y = IPC, color = NIVEL_EDU_SIMPL)) +
  geom_point(alpha = 0.9) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Valores Observados vs Predichos por el Modelo",
    x = "Valores Predichos",
    y = "Valores Observados"
  ) +
  theme_minimal()

ggplot(df_ipc, aes(x = pred, y = IPC, color = NIVEL_EDU_SIMPL)) +
  geom_point(alpha = 0.9) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Valores Observados vs Predichos por el Modelo",
    x = "Valores Predichos",
    y = "Valores Observados"
  ) +
  theme_minimal()


library(dplyr)

df_pred <- df_ipc %>%
  group_by(NSE, grupos_edad) %>%
  summarise(IPC_promedio = mean(IPC))

ggplot(df_pred, aes(x = grupos_edad, y = IPC_promedio, fill = NSE)) +
  geom_col(position = "dodge") +
  labs(title = "IPC promedio según NSE y grupo de edad",
       x = "Grupo de edad", y = "Índice de participación cultural (IPC)") +
  theme_minimal()




unique(df_ipc$region)
df_caba <- df_ipc %>% 
  filter(region == "CABA")

mod0 <- lm(IPC ~ NSE, data = df_caba)  
summary(mod0)

mod1 <- lm(IPC ~ NSE + GENERO, data = df_caba)  # Genero no aumenta nada el r2
summary(mod1)

mod2 <- lm(IPC ~ NSE + grupos_edad, data = df_caba)
summary(mod2)

mod3 <- lm(IPC ~ NSE + grupos_edad, data = df_caba)
summary(mod3)

mod4 <- lm(IPC ~ NIVEL_EDU_SIMPL + NSE + grupos_edad_num, data = df_caba)
summary(mod4)

df_caba$grupos_edad <- factor(
  df_caba$grupos_edad,
  levels = c(
    "13 a 17 años",
    "18 a 29 años",
    "30 a 49 años",
    "50 a 64 años",
    "65 años y mas"
  )
)

df_caba$grupos_edad_num <- as.numeric(df_caba$grupos_edad)

mod5 <- lm(IPC ~ NIVEL_EDU_SIMPL + NSE + poly(grupos_edad_num, 2),
           data = df_caba)
summary(mod5) # Agregar poly no mejora el modelo

anova(mod4, mod5)

mod4 <- lm(IPC ~ NIVEL_EDU_SIMPL + NSE + grupos_edad_num, data = df_caba)
summary(mod4)

mod6 <- lm(IPC ~ NIVEL_EDU_SIMPL + NSE + grupos_edad_num + region,
           data = df_caba)
summary(mod6)

anova(mod4, mod6) # Es significativo, pero no mejora casi nada el R2

mod7 <- lm(IPC ~ NIVEL_EDU_SIMPL * NSE + grupos_edad_num, data = df_caba)
summary(mod7)

mod8 <- lm(IPC ~ NIVEL_EDU_SIMPL * NSE * grupos_edad_num, data = df_caba)
summary(mod8)

# Modelo elegido: 4
mod4 <- lm(IPC ~ NIVEL_EDU_SIMPL + NSE + grupos_edad_num, data = df_caba)

# Extraer valores ajustados y residuos
valores_ajustados_bs <- fitted(mod4)
residuos_bs <- resid(mod4)

# Crear data frame
df_res_bs <- data.frame(
  valores_ajustados = valores_ajustados,
  residuos = residuos
)

# Ver las primeras filas
head(df_res_bs)


ggplot(df_res_bs, aes(x = valores_ajustados, y = residuos)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Valores ajustados",
    y = "Residuos",
    title = "Residuos vs Valores Ajustados"
  ) +
  theme_minimal()

# Valores ajustados y residuos

# Gráfico solo con la recta
ggplot(df_res_bs, aes(x = valores_ajustados, y = residuos)) +
  geom_abline(intercept = 0, slope = 0, color = "red", linetype = "dashed", size = 1) +
  labs(
    x = "Valores ajustados",
    y = "Residuos",
    title = "Recta de referencia sobre residuos (pendiente = 0)"
  ) +
  theme_minimal()



library(ggplot2)

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
df_caba$NSE <- as.factor(df_ipc$NSE)

# Crear data frame para ggplot
df_res_bs <- data.frame(
  valores_ajustados = valores_ajustados,
  residuos = residuos,
  NSE = df_ipc$NSE,
  grupos_edad = df_ipc$grupos_edad
)

# Crear gráfico
p_bs <- ggplot(df_res_bs, aes(x = valores_ajustados, y = residuos, color = NSE)) +
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
print(p_bs)

#Haciendo graficos de residuos vs fitted
library(modelr)
df4_bs<- df_caba %>% add_predictions(model=mod4)
#df2<- df_ipc %>% add_predictions(model=mod2)
#dfsg<- df_ipc %>% add_predictions(model=mod3singenero)
#dfh<- df_ipc %>% add_predictions(model=mod3)

str(df_caba$IPC)
table(df_caba$NSE)



df4_bs <- df_caba %>%
  add_predictions(mod4) %>%
  add_residuals(mod4)

df1 <- df_ipc %>%
  add_predictions(mod1) %>%
  add_residuals(mod1)

df2 <- df_ipc %>%
  add_predictions(mod2) %>%
  add_residuals(mod2)

df3 <- df_ipc %>%
  add_predictions(mod3) %>%
  add_residuals(mod3)

df4 <- df_ipc %>%
  add_predictions(mod4) %>%
  add_residuals(mod4)


glimpse(df0)
###########################################################################################
ggplot(df4_bs, aes(x = pred, y = resid)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "loess", se = FALSE, color = "black") +
  labs(
    x = "Valores ajustados (fitted values)",
    y = "Residuos",
    title = "Residuos vs Valores ajustados - Modelo IPC ~ NSE"
  ) +
  theme_minimal(base_size = 13)
###########################################################################################
ggplot(df4_bs, aes(x = pred, y = resid, color = NSE)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(
    x = "Valores ajustados (fitted values)",
    y = "Residuos",
    color = "Nivel Socioeconómico (NSE)",
    title = "Residuos vs Valores Ajustados por NSE (Modelo 3)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right"
  )
###########################################################################################
ggplot(df4_bs, aes(x = pred, y = resid, color = NSE)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(
    x = "Valores ajustados (fitted values)",
    y = "Residuos",
    color = "Nivel Socioeconómico (NSE)",
    title = "Residuos vs Valores Ajustados por NSE (Modelo 1)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right"
  )
###########################################################################################
ggplot(df4_bs, aes(x = pred, y = resid, color = NSE)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(
    x = "Valores ajustados (fitted values)",
    y = "Residuos",
    color = "Nivel Socioeconómico (NSE)",
    title = "Residuos vs Valores Ajustados por NSE (Modelo 4)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right"
  )


df_bs <- df_bs %>%
  mutate(pred = predict(mod4))

ggplot(df_caba, aes(x = pred, y = IPC, color = NSE)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Valores Observados vs Predichos por el Modelo",
    x = "Valores Predichos",
    y = "Valores Observados"
  ) +
  theme_minimal()
summary(mod4)
ggplot(df_ipc, aes(x = pred, y = IPC, color = NIVEL_EDU_SIMPL)) +
  geom_point(alpha = 0.9) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Valores Observados vs Predichos por el Modelo",
    x = "Valores Predichos",
    y = "Valores Observados"
  ) +
  theme_minimal()

ggplot(df_ipc, aes(x = pred, y = IPC, color = NIVEL_EDU_SIMPL)) +
  geom_point(alpha = 0.9) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Valores Observados vs Predichos por el Modelo",
    x = "Valores Predichos",
    y = "Valores Observados"
  ) +
  theme_minimal()


library(dplyr)

df_pred <- df_ipc %>%
  group_by(NSE, grupos_edad) %>%
  summarise(IPC_promedio = mean(IPC))

ggplot(df_pred, aes(x = grupos_edad, y = IPC_promedio, fill = NSE)) +
  geom_col(position = "dodge") +
  labs(title = "IPC promedio según NSE y grupo de edad",
       x = "Grupo de edad", y = "Índice de participación cultural (IPC)") +
  theme_minimal()




unique(df_ipc$region)
df_caba <- df_ipc %>% 
  filter(region == "CABA")


