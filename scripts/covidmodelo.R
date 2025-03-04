install.packages("dplyr") 
library(dplyr)
library(readr)
datacov <- read_csv("covid_casos(datos_R).csv")

summary(datacov)
datacov$time<-1:1215

datacov_numerico <- datacov %>% select_if(is.numeric) 
matriz_correlacion <- cor(datacov_numerico, use = "complete.obs") 
print(matriz_correlacion)

plot(datacov$nacional_casos,datacov$sinaloa_casos,color = "red",data=datacov)
plot(datacov$derechohabiente,datacov$sinaloa_casos ,data,=datacov)
plot(datacov$rain,datacov$sinaloa_casos ,data=datacov)
plot(datacov$pobreza,datacov$sinaloa_casos ,data=datacov)
plot(datacov$time,datacov$sinaloa_casos ,data=datacov)

datacov$sinaloa_casos[datacov$sinaloa_casos == 0] <- 1e-9
datacov$nacional_casos[datacov$nacional_casos == 0] <- 1e-9

datacov$lnsinaloa_casos  <- log(datacov$sinaloa_casos )
datacov$lnnacional_casos  <- log(datacov$nacional_casos)
datacov$lnderechohabiente <- log(datacov$derechohabiente)
datacov$lnpobreza <- log(datacov$pobreza)


modelo1 <- lm(datacov$lnsinaloa_casos ~ datacov$lnnacional_casos + datacov$lnderechohabiente
              + datacov$lnpobreza + datacov$time)
summary(modelo1)
print(datacov$lnsinaloa_casos)

library(ggplot2)
library(readr)

# Leer el archivo CSV
datacov <- read_csv("covid_casos(datos_R).csv")

# Supongamos que ya tienes tu modelo de regresión lineal
# modelo1 <- lm(formula, data = datacov)

# Calcular los valores predichos (estimados)
valores_estimados <- predict(modelo1)

# Crear un dataframe con los valores observados y estimados
comparacion <- data.frame(
  Observados = datacov$sinaloa_casos,
  Estimados = valores_estimados
)

# Crear un gráfico de comparación
ggplot(comparacion, aes(x = Observados, y = Estimados)) +
  geom_point(color = "blue") +  # Puntos de los casos observados vs. estimados
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # Línea de referencia (y = x)
  labs(title = "Comparación entre casos observados y estimados",
       x = "Casos observados",
       y = "Casos estimados") +
  theme_minimal()

