# Modelo sugerido (asegúrate de que NIVEL_EDUCATIVO sea un factor)
df_ipc$NIVEL_EDUCATIVO <- as.factor(df_ipc$NIVEL_EDUCATIVO)

modelo_ipc_completo <- lm(IPC ~ NSE + grupos_edad + GENERO + NIVEL_EDUCATIVO, 
                          data = df_ipc)

# Revisa este nuevo resumen, ¡seguramente será mucho mejor!
summary(modelo_ipc_completo)


encc_22_23_raw %>% 
  pull(soc13.1) %>% 
  unique()
unique(encc_22_23_raw$soc13.1)


# Cargar las librerías necesarias
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(randomForest) # Librería para el modelo 2

# ... (Todo tu código de Carga, Selección y Renombrado va aquí) ...
# ... (Tu código de 'puntuar_frecuencia' y cálculo de IPC va aquí) ...

# Suponemos que ya tienes tu dataframe 'df_ipc' listo con la columna 'IPC'

# ---
# SECCIÓN 1: PREPARACIÓN FINAL DE VARIABLES PARA EL MODELADO
# ---

# Imprime los nombres de las columnas para verificar
# print(names(df_ipc))

# Convertir TODAS las variables predictoras categóricas a 'factor'
# ¡Añadimos NIVEL_EDUCATIVO!
df_ipc$GENERO <- as.factor(df_ipc$GENERO)
df_ipc$Region <- as.factor(df_ipc$region) # Asegúrate de que esta columna exista y tenga el nombre correcto
df_ipc$NSE <- as.factor(df_ipc$NSE)
df_ipc$grupos_edad <- as.factor(df_ipc$grupos_edad)
df_ipc$NIVEL_EDUCATIVO <- as.factor(df_ipc$NIVEL_EDUCATIVO) # <- ¡CLAVE!

# Asegurarse de que las variables numéricas sean 'numeric'
df_ipc$edad <- as.numeric(df_ipc$edad) # Usaremos 'edad' numérica en lugar de 'grupos_edad' para el modelo
df_ipc$IPC <- as.numeric(df_ipc$IPC)

# 2. Manejo de valores faltantes (NA)
# Los modelos de regresión no funcionan bien con NAs en las variables predictoras.
# Creamos un dataframe limpio solo para el modelado.
df_modelo <- df_ipc %>%
  select(IPC, edad, GENERO, Region, NSE, NIVEL_EDUCATIVO) %>%
  na.omit()

# Imprime un resumen para verificar que todo esté correcto
summary(df_modelo)


# ---
# SECCIÓN 2: DIVISIÓN DE DATOS (TRAINING Y TESTING)
# ---
# Dividimos los datos para entrenar con unos (80%) y probar con otros (20%)

# Fijamos una "semilla" (seed) para que la división aleatoria sea siempre la misma
# y tu trabajo sea reproducible.
set.seed(123)

# Creamos los índices para el 80% de los datos que irán a "entrenamiento"
indices_entrenamiento <- sample(1:nrow(df_modelo), 
                                size = 0.8 * nrow(df_modelo))

# Creamos los dos dataframes
datos_entrenamiento <- df_modelo[indices_entrenamiento, ]  # El 80% para entrenar
datos_prueba <- df_modelo[-indices_entrenamiento, ] # El 20% restante para probar

print(paste("Datos para entrenar:", nrow(datos_entrenamiento)))
print(paste("Datos para probar:", nrow(datos_prueba)))


# ---
# SECCIÓN 3: MODELO 1 - REGRESIÓN LINEAL MÚLTIPLE
# ---

# 1. Crear el modelo
# Usamos SÓLO los 'datos_entrenamiento'
# Usamos 'edad' (numérica) en lugar de 'grupos_edad' (factor) 
# porque da una interpretación más directa en regresión lineal.
modelo_lineal <- lm(IPC ~ edad + GENERO + Region + NSE + NIVEL_EDUCATIVO, 
                    data = datos_entrenamiento)

# 2. Analizar el modelo
# Este es el resultado MÁS IMPORTANTE.
# Míralo en la consola.
summary(modelo_lineal)

# QUÉ MIRAR EN EL SUMMARY:
# - 'Estimate' (Coeficientes): Cuánto sube o baja el IPC por cada variable.
# - 'Pr(>|t|)' (o las estrellas '***'): Te dice qué variables son significativas.
# - 'Multiple R-squared': Qué porcentaje de la variación del IPC explica tu modelo.


# ---
# SECCIÓN 4: MODELO 2 - RANDOM FOREST (Para comparar)
# ---

# 1. Crear el modelo
# (Puede tardar unos segundos)
modelo_rf <- randomForest(IPC ~ edad + GENERO + Region + NSE + NIVEL_EDUCATIVO,
                          data = datos_entrenamiento,
                          ntree = 500,       # 500 árboles
                          importance = TRUE) # Pedimos que calcule la importancia

# 2. Analizar el modelo
print(modelo_rf)



# MIRA EL '% Var explained': Es el R-squared del modelo.

# 3. Ver importancia de las variables
# ¡Esto es muy útil para tu presentación!
importance(modelo_rf)
varImpPlot(modelo_rf) # Muestra el gráfico de importancia en la pestaña 'Plots'


# ---
# SECCIÓN 5: COMPARACIÓN FINAL DE MODELOS
# ---

# ¿Cuál modelo es mejor? Usaremos los 'datos_prueba' (el 20% que guardamos)
# para ver cuál comete un error más pequeño.
# Usaremos la métrica RMSE (Error Cuadrático Medio Raíz).
# Un RMSE más bajo es MEJOR.

# 1. Hacer predicciones con ambos modelos sobre los datos de prueba
prediccion_lineal <- predict(modelo_lineal, newdata = datos_prueba)
prediccion_rf <- predict(modelo_rf, newdata = datos_prueba)

# 2. Calcular el error (RMSE) para cada modelo
rmse_lineal <- sqrt(mean((datos_prueba$IPC - prediccion_lineal)^2))
rmse_rf <- sqrt(mean((datos_prueba$IPC - prediccion_rf)^2))

# 3. Imprimir los resultados y comparar
print(paste("Error (RMSE) del Modelo Lineal:", round(rmse_lineal, 3)))
print(paste("Error (RMSE) del Modelo Random Forest:", round(rmse_rf, 3)))

