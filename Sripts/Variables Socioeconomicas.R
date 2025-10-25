library(tidyverse)


colnames(encc_Final)

#Nivel Socio Economico NSE

df_tp_final %>% 
  ggplot(aes(x = NSE, fill = NSE)) + geom_bar() + theme_minimal()

#Nivel Educativo
encc_Final %>%
  ggplot(aes(x = NIVEL_EDUCATIVO, fill = NSE)) + geom_bar(position = "fill")+
  labs(title = "Distribución por Nivel Socioeconómico") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


df_tp_final %>%
  ggplot(aes(x = NSE, fill = GENERO)) +
  geom_bar(position = "fill") +
  labs(
    title = "Distribución del Nivel Socioeconómico por Género",
    x = "Nivel Socioeconómico",
    y = "Proporción",
    fill = "GENERO"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

#genero y nivel educativo

df_tp_final %>%
  ggplot(aes(x = NIVEL_EDUCATIVO, fill = GENERO)) +
  geom_bar(position = "fill") +
  labs(
    title = "Nivel Educativo por Género",
    x = "Nivel Educativo",
    y = "Proporción",
    fill = "Género"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))




# Ingresos del hogar según NSE
df_tp_final %>%
  ggplot(aes(x = NSE, y = INGRESOS_HOGAR, fill = NSE)) +
  geom_boxplot() +
  labs(title = "Ingresos del hogar por Nivel Socioeconómico",
       x = "Nivel Socioeconómico", y = "Ingresos del hogar") +
  theme_minimal()




