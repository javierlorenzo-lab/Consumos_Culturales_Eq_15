library(ggplot2)
library(dplyr)
library(tidyr)

# ----------------------------
# 1. Identificar columnas de actividad cultural
# ----------------------------
# Ejemplo: todas las que indican participación o frecuencia
# (según tus datos: FUE_CINE, FREC_CINE, CONTEO_CINE, ESCUCHA_MUSICA, FREC_MUSICA, FUE_RECITALES, teatro*, pat*, vj*, libro*)
# Podemos hacer un vector dinámico usando nombres que empiecen con estas raíces

columnas_culturales <- df_ipc %>%
  select(starts_with(c("FUE_", "FREC_", "CONTEO_", "ESCUCHA_", "teatro", "pat", "vj", "libro"))) %>%
  names()

# ----------------------------
# 2. Crear gráficos automáticamente
# ----------------------------
# Vamos a hacer un histograma o barplot según tipo de variable

# Abrir PDF multipágina
pdf("Graficos_IPC_Automatizados.pdf", width = 10, height = 7)

for(col in columnas_culturales){
  
  # Si es numérica, hacemos histograma
  if(is.numeric(df_ipc[[col]])){
    p <- ggplot(df_ipc, aes_string(x = col)) +
      geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
      labs(title = paste("Distribución de", col),
           x = col,
           y = "Cantidad de individuos") +
      theme_minimal()
  } else { 
    # Si es categórica, hacemos barplot
    p <- ggplot(df_ipc, aes_string(x = col, fill = col)) +
      geom_bar() +
      labs(title = paste("Distribución de", col),
           x = col,
           y = "Cantidad de individuos") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  print(p)  # cada gráfico a una página del PDF
}

dev.off()  # cerrar PDF
