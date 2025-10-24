#Javier

library(tidyverse)
library(readr)



#CARGA DE BASES DE DATOS
encc_22_23_raw <- read_csv("data/raw_data/base-datos-encc-2022-2023.csv")



# Visualizar estructura y nombres
spec(encc_22_23_raw)
names(encc_22_23_raw)


# Dimensiones
dim(encc_22_23_raw)

# Vista previa
head(encc_22_23_raw)

# Estructura de columnas
str(encc_22_23_raw)

#Porblemas

problems(encc)
# problemas en la columna 299 (musica14.1)
names(encc_22_23_raw)[299]

#Posible solucion, forzar a texto

encc <- read_csv("data/raw_data/base-datos-encc-2022-2023.csv", 
                 col_types = cols(musica14.1 = col_character()))

encc$musica14.1 <- as.character(encc$musica14.1)

problems(encc)

# 0 prooblemas








