df_caba <- df_ipc %>%
  mutate(
    NIVEL_EDUCATIVO = factor(
      NIVEL_EDUCATIVO,
      levels = c(
        "Sin Estudios",
        "Primarios Incompletos",
        "Primarios Completos",
        "Secundarios Incompletos",
        "Secundarios Completos",
        "Terciarios Incompletos",
        "Terciarios Completos",
        "Universitarios Incompletos",
        "Universitarios Completos",
        "Posgrado",
        "Ns Nc"
      ),
      ordered = TRUE
    )
  )
#grafico por nivel educativo

ggplot(df_caba, aes(y = NIVEL_EDUCATIVO, x = IPC, fill = NSE)) +
  geom_boxplot(alpha = 0.7, outlier.color = "gray40") +
  labs(
    title = "IPC por Nivel Educativo – CABA",
    x = "Índice de Prácticas Culturales (IPC)",
    y = "Nivel educativo"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.y = element_text(size = 11)
  )



# mejorado


library(ggplot2)
library(dplyr)

# Factor ordenado (ya lo tenés bien definido)
df_caba <- df_ipc %>%
  mutate(
    NIVEL_EDUCATIVO = factor(
      NIVEL_EDUCATIVO,
      levels = c(
        "Sin Estudios",
        "Primarios Incompletos",
        "Primarios Completos",
        "Secundarios Incompletos",
        "Secundarios Completos",
        "Terciarios Incompletos",
        "Terciarios Completos",
        "Universitarios Incompletos",
        "Universitarios Completos",
        "Posgrado",
        "Ns Nc"
      ),
      ordered = TRUE
    )
  )

# 🎨 Gráfico mejorado
ggplot(df_caba, aes(y = NIVEL_EDUCATIVO, x = IPC, fill = NSE)) +
  geom_boxplot(alpha = 0.7, width = 0.6, outlier.shape = 21, outlier.size = 1.8) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Índice de Prácticas Culturales (IPC) según Nivel Educativo – CABA",
    x = "Índice de Prácticas Culturales (IPC)",
    y = "Nivel educativo",
    fill = "Nivel Socioeconómico"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 10),
    panel.grid.major.y = element_blank(),   # limpia líneas horizontales
    plot.margin = margin(10, 10, 10, 10)
  ) +
  coord_cartesian(clip = "off", expand = TRUE)


library(ggplot2)
library(dplyr)

# Factor ordenado (ya lo tenés bien definido)
df_caba <- df_ipc %>%
  mutate(
    NIVEL_EDUCATIVO = factor(
      NIVEL_EDUCATIVO,
      levels = c(
        "Sin Estudios",
        "Primarios Incompletos",
        "Primarios Completos",
        "Secundarios Incompletos",
        "Secundarios Completos",
        "Terciarios Incompletos",
        "Terciarios Completos",
        "Universitarios Incompletos",
        "Universitarios Completos",
        "Posgrado",
        "Ns Nc"
      ),
      ordered = TRUE
    )
  )

# 🎨 Gráfico mejorado
ggplot(df_caba, aes(y = NIVEL_EDUCATIVO, x = IPC, fill = NSE)) +
  geom_boxplot(alpha = 0.7, width = 0.6, outlier.shape = 21, outlier.size = 1.8) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Índice de Prácticas Culturales (IPC) según Nivel Educativo – CABA",
    x = "Índice de Prácticas Culturales (IPC)",
    y = "Nivel educativo",
    fill = "Nivel Socioeconómico"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 10),
    panel.grid.major.y = element_blank(),   # limpia líneas horizontales
    plot.margin = margin(10, 10, 10, 10)
  ) +
  coord_cartesian(clip = "off", expand = TRUE)
