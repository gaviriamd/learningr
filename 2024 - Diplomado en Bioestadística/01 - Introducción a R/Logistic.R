# Probabilidad de estar enfermo de gripe 
# Como función de la temperatura corporal

gripe <- as.factor(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1,
                         0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1,
                         0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0,
                         0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
                         1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0,
                         1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1,
                         1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1,
                         0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
                         0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0,
                         0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0,
                         0, 0, 0, 0, 1, 0, 0, 0, 1, 1))

temperatura <- c(36.1, 36.2, 36.3, 36.4, 36.5, 36.6, 36.7, 36.5, 36.9, 37.2, 36.3, 
                 37.8, 36.3, 38, 36.2, 36.4, 36.3, 36.6, 37.8,
                 36.3, 36.5, 37.7, 36.3, 36.9, 36.8, 36.6, 37.8, 36.3, 
                 36.2, 36.6, 36.7, 36.9, 38, 36.4, 36.5, 37.8, 36.5, 38,
                 36.2, 36.3, 36.4, 36.5, 36.6, 36.7, 37.8, 36.3, 36.5,
                 36.7, 36.9, 37.1, 37.7, 36.5, 36.7, 36.9, 38, 36.7, 36.5,
                 36.7, 36.5, 36.7, 36.9, 38, 36.4, 36.6, 36.8, 37, 36.9, 36.8, 
                 36.6, 36.4, 36.3, 36.5, 37.7, 36.6, 36.3, 36.8,
                 38, 36.7, 36.9, 36.4, 36.5, 36.7, 36.8, 38, 36.9, 37.7, 36.8, 
                 36.3, 36.4, 36.5, 38, 37, 37.9, 36.3, 36.3,
                 38, 37.9, 37.7, 36.5, 38, 36.7, 36.5, 36.3, 36.2, 36.2, 36.4, 
                 37.7, 36.4, 36.6, 37.7, 36.3, 36.3, 36.3, 38,
                 38, 36.9, 36.7,37.7, 36.8, 36.5, 36.3, 36.5, 36.3, 36.6, 36.8, 36.8,
                 36.5, 36.7, 36.9, 36.3, 36.7, 37.8, 38, 36.5,
                 36.4, 37.5, 36.8, 36.6, 36.6, 36.6, 36.4, 36.3, 36.7, 36.5, 36.6,
                 36.8, 36.7, 36.8, 37.7, 36.4, 36.3, 36.6, 36.8,
                 36.5, 37.5, 36.5, 36.7, 36.8, 36.9, 38, 38, 37.9, 
                 36.6, 37.8, 36.9, 36.7, 36.8, 36.9, 37.5, 36.5, 36.8, 36.9,
                 36.5, 36.7, 36.9, 36.3, 37.7, 36.5, 36.6, 37.7, 37.9, 36.8,
                 37.6, 37.7, 37.9, 37.8, 36.7, 36.6, 37.8, 36.7, 36.9,
                 36.5, 36.3, 36.7, 37.7, 36.8, 36.6, 36.8, 38, 37.9)
length(temperatura)
length(gripe)
enfermedad <- data.frame(gripe, temperatura)
head(enfermedad, 4)
enfermedad

# Representación gráfica
library(ggplot2)
ggplot(data = enfermedad, aes(x = gripe, y = temperatura, color = gripe))+
  geom_boxplot(otlier.shape = NA)+
  geom_jitter(width = 0.3)+
  theme_bw() +
  theme(legend.position = 'null')

#Generar modelo lógistico
logistic <- glm(gripe~temperatura, data = enfermedad, family = 'binomial')
summary(logistic)

# Logistic graphic
# MEDIANTE BASE GRAPHICS SIN INTERVALOS DE CONFIANZA

# Codificación 0,1 de la variable respuesta
enfermedad$gripe <- as.character(enfermedad$gripe)
enfermedad$temperatura <- as.numeric(enfermedad$temperatura)

plot(gripe ~ temperatura, enfermedad, col = "darkblue",
     main = "Modelo regresión logística",
     ylab = "P(Gripe=1|Temperatura)",
     xlab = "Temperatura", pch = "I")

# type = "response" devuelve las predicciones en forma de 
#probabilidad en lugar de en log_ODDs
curve(predict(logistic, data.frame(temperatura = x), type = "response"),
      col = "firebrick", lwd = 2.5, add = TRUE)
