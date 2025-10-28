###Modelados

# modelo 1
modelo_ipc_NSE <- lm(IPC ~ NSE, data = df_ipc)


summary(modelo_ipc_NSE)


# Modelo 2 IPC ~ NSE + GENERO


modelo_ipc_GEN <- lm(IPC ~ NSE + GENERO, data = df_ipc)

summary(modelo_ipc_GEN)


# modelo_ipc_NSE_GEN

df_ipc$NSE <- as.factor(df_ipc$NSE)
df_ipc$GENERO <- as.factor(df_ipc$GENERO)

modelo_ipc_NSE_GEN <- lm(IPC ~ NSE + GENERO, data = df_ipc)

summary(modelo_ipc_NSE_GEN)



#IPC ~ NSE + grupos_edad + GENERO
modelo_ipc <- lm(IPC ~ NSE + grupos_edad + GENERO, data = df_ipc)
summary(modelo_ipc)


