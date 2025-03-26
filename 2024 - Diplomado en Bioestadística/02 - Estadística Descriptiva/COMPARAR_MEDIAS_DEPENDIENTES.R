##Comparar muestras dependientes

#Creamos los dataframes con los datos

datos <- data.frame(
  Corredor = c(1:10),
  antes = c(12.9, 13.5, 12.8, 15.6, 17.2, 19.2, 12.6, 15.3, 14.4, 11.3),
  despues = c(12.7, 13.6, 12.0, 15.2, 16.8, 20.0, 12.0, 15.9, 16.0, 11.1)
)
datos

#Diferencia de cada par de observaciones.

diferencia <- datos$antes - datos$despues
datos      <- cbind(datos,diferencia)
head(datos,3)

#Calcular las medias
colMeans(datos)

#Condiciones para comparar dos medias independientes mediante t-test
#Método gráfico:

par(mar = c(2, 2, 2, 2))
par(mfrow = c(1, 2))
qqnorm(datos$antes, xlab = "", ylab = "", main = "antes")
qqline(datos$antes)
qqnorm(datos$despues, xlab = "", ylab = "", main = "despues")
qqline(datos$despues)


#Analíticamente
shapiro.test(datos$antes)

shapiro.test(datos$despues)


#Los gráficos qqnorm indican que las muestras se asemejan a los esperado en una 
#población normal y los test de Saphiro-Wilk no muestran evidencias para descartar
#que las muestras procedan poblaciones sean normales (para un alpha de 0.05).


#Prueba T:

#t.test(x = datos$antes, y = datos$despues, alternative = "less",
#      mu = 0, paired = TRUE, conf.level = 0.95)

t.test(x = datos$antes, y = datos$despues, alternative = "two.sided",
       mu = 0, paired = TRUE, conf.level = 0.95)

#Conclusión
#p-value > alfa, no hay evidencias significativas para rechazar H0
#en favor de H1. No se pude considerar que el rendimiento de los atletas haya cambiado.



muestraX <- c( 1.1, 3.4, 4.3, 2.1, 7.0 , 2.5 )
muestraY <- c( 7.0, 8.0, 3.0, 5.0, 6.2 , 4.4 )

fligner.test(x = list(muestraX,muestraY))


#No hay evidencias en contra de la igualdad de varianzas.

#Una vez comprobadas las condiciones necesarias para que el test de Mann–Whitney–Wilcoxon
#sea válido se procede a calcular su estadístico, p-value asociado y tamaño de efecto.

# unión de las muestras y ordenación
observaciones  <- sort(c(muestraX, muestraY)) 
# La función rank() de R calcula las posiciones automáticamente,
# solucionando las ligaduras en caso de que las haya.
rango_observaciones <- rank(observaciones) 
observaciones  <- cbind(observaciones, rango_observaciones)
observaciones


R1 <- sum(1,5,6,2,10.5,3)
R2 <- sum(10.5,12,4,8,9,7)
n1 <- length(muestraX)
n2 <- length(muestraY)
U1 <- (n1 * n2) + (n1*(n1 + 1) / 2) - R1
U2 <- (n1 * n2) + (n2*(n2 + 1) / 2) - R2
U  <- min(c(U1,U2))
U



wilcox.test(x = muestraX, y = muestraY, alternative = "two.sided", mu = 0,
            paired = FALSE, conf.int = 0.95)


install.packages("coin")
library(coin)
# La función wilcox.test() del paquete coin requiere pasarle los argumentos en
# forma de función (~), por lo que los datos tienen que estar almacenados en
# forma de data frame.
datos <- data.frame(
  grupo   = as.factor(rep(c("A", "B"), c(6, 6))),
  valores = c(muestraX, muestraY))

wilcox_test(valores ~ grupo, data = datos, distribution = "exact", conf.int=0.95)


# La función devuelve el valor de Z, por lo que se puede calcular fácilmente el
# tamaño del efecto
tamanyo_efecto <- 1.845/sqrt(n1 + n2)
tamanyo_efecto



posicion <- c("OF", "IF", "IF", "OF", "IF", "IF", "OF", "OF", "IF", "IF", "OF", "OF", "IF", "OF", "IF", "IF", "IF", "OF", "IF", "OF", "IF", "OF", "IF", "OF", "IF", "DH", "IF", "IF", "IF", "OF", "IF", "IF", "IF", "IF", "OF", "IF", "OF", "IF", "IF", "IF", "IF", "OF", "OF", "IF", "OF", "OF", "IF", "IF", "OF", "OF", "IF", "OF", "OF", "OF", "IF", "DH", "OF", "OF", "OF", "IF", "IF", "IF", "IF", "OF", "IF", "IF", "OF", "IF", "IF", "IF", "OF", "IF", "IF", "OF", "IF", "IF", "IF", "IF", "IF", "IF", "OF", "DH", "OF", "OF", "IF", "IF", "IF", "OF", "IF", "OF", "IF", "IF", "IF", "IF", "OF", "OF", "OF", "DH", "OF", "IF", "IF", "OF", "OF", "C", "IF", "OF", "OF", "IF", "OF", "IF", "IF", "IF", "OF", "C", "OF", "IF", "C", "OF", "IF", "DH", "C", "OF", "OF", "IF", "C", "IF", "IF", "IF", "IF", "IF", "IF", "OF", "C", "IF", "OF", "OF", "IF", "OF", "IF", "OF", "DH", "C", "IF", "OF", "IF", "IF", "OF", "IF", "OF", "IF", "C", "IF", "IF", "OF", "IF", "IF", "IF", "OF", "OF", "OF", "IF", "IF", "C", "IF", "C", "C", "OF", "OF", "OF", "IF", "OF", "IF", "C", "DH", "DH", "C", "OF", "IF", "OF", "IF", "IF", "IF", "C", "IF", "OF", "DH", "IF", "IF", "IF", "OF", "OF", "C", "OF", "OF", "IF", "IF", "OF", "OF", "OF", "OF", "OF", "OF", "IF", "IF", "DH", "OF", "IF", "IF", "OF", "IF", "IF", "IF", "IF", "OF", "IF", "C", "IF", "IF", "C", "IF", "OF", "IF", "DH", "C", "OF", "C", "IF", "IF", "OF", "C", "IF", "IF", "IF", "C", "C", "C", "OF", "OF", "IF", "IF", "IF", "IF", "OF", "OF", "C", "IF", "IF", "OF", "C", "OF", "OF", "OF", "OF", "OF", "OF", "OF", "OF", "OF", "OF", "OF", "C", "IF", "DH", "IF", "C", "DH", "C", "IF", "C", "OF", "C", "C", "IF", "OF", "IF", "IF", "IF", "IF", "IF", "IF", "IF", "IF", "OF", "OF", "OF", "IF", "OF", "OF", "IF", "IF", "IF", "OF", "C", "IF", "IF", "IF", "IF", "OF", "OF", "IF", "OF", "IF", "OF", "OF", "OF", "IF", "OF", "OF", "IF", "OF", "IF", "C", "IF", "IF", "C", "DH", "OF", "IF", "C", "C", "IF", "C", "IF", "OF", "C", "C", "OF")
bateo <- c(0.359, 0.34, 0.33, 0.341, 0.366, 0.333, 0.37, 0.331, 0.381, 0.332, 0.365, 0.345, 0.313, 0.325, 0.327, 0.337, 0.336, 0.291, 0.34, 0.31, 0.365, 0.356, 0.35, 0.39, 0.388, 0.345, 0.27, 0.306, 0.393, 0.331, 0.365, 0.369, 0.342, 0.329, 0.376, 0.414, 0.327, 0.354, 0.321, 0.37, 0.313, 0.341, 0.325, 0.312, 0.346, 0.34, 0.401, 0.372, 0.352, 0.354, 0.341, 0.365, 0.333, 0.378, 0.385, 0.287, 0.303, 0.334, 0.359, 0.352, 0.321, 0.323, 0.302, 0.349, 0.32, 0.356, 0.34, 0.393, 0.288, 0.339, 0.388, 0.283, 0.311, 0.401, 0.353, 0.42, 0.393, 0.347, 0.424, 0.378, 0.346, 0.355, 0.322, 0.341, 0.306, 0.329, 0.271, 0.32, 0.308, 0.322, 0.388, 0.351, 0.341, 0.31, 0.393, 0.411, 0.323, 0.37, 0.364, 0.321, 0.351, 0.329, 0.327, 0.402, 0.32, 0.353, 0.319, 0.319, 0.343, 0.288, 0.32, 0.338, 0.322, 0.303, 0.356, 0.303, 0.351, 0.325, 0.325, 0.361, 0.375, 0.341, 0.383, 0.328, 0.3, 0.277, 0.359, 0.358, 0.381, 0.324, 0.293, 0.324, 0.329, 0.294, 0.32, 0.361, 0.347, 0.317, 0.316, 0.342, 0.368, 0.319, 0.317, 0.302, 0.321, 0.336, 0.347, 0.279, 0.309, 0.358, 0.318, 0.342, 0.299, 0.332, 0.349, 0.387, 0.335, 0.358, 0.312, 0.307, 0.28, 0.344, 0.314, 0.24, 0.331, 0.357, 0.346, 0.351, 0.293, 0.308, 0.374, 0.362, 0.294, 0.314, 0.374, 0.315, 0.324, 0.382, 0.353, 0.305, 0.338, 0.366, 0.357, 0.326, 0.332, 0.323, 0.306, 0.31, 0.31, 0.333, 0.34, 0.4, 0.389, 0.308, 0.411, 0.278, 0.326, 0.335, 0.316, 0.371, 0.314, 0.384, 0.379, 0.32, 0.395, 0.347, 0.307, 0.326, 0.316, 0.341, 0.308, 0.327, 0.337, 0.36, 0.32, 0.372, 0.306, 0.305, 0.347, 0.281, 0.281, 0.296, 0.306, 0.343, 0.378, 0.393, 0.337, 0.327, 0.336, 0.32, 0.381, 0.306, 0.358, 0.311, 0.284, 0.364, 0.315, 0.342, 0.367, 0.307, 0.351, 0.372, 0.304, 0.296, 0.332, 0.312, 0.437, 0.295, 0.316, 0.298, 0.302, 0.342, 0.364, 0.304, 0.295, 0.305, 0.359, 0.335, 0.338, 0.341, 0.3, 0.378, 0.412, 0.273, 0.308, 0.309, 0.263, 0.291, 0.359, 0.352, 0.262, 0.274, 0.334, 0.343, 0.267, 0.321, 0.3, 0.327, 0.313, 0.316, 0.337, 0.268, 0.342, 0.292, 0.39, 0.332, 0.315, 0.298, 0.298, 0.331, 0.361, 0.272, 0.287, 0.34, 0.317, 0.327, 0.354, 0.317, 0.311, 0.174, 0.302, 0.302, 0.291, 0.29, 0.268, 0.352, 0.341, 0.265, 0.307, 0.36, 0.305, 0.254, 0.279, 0.321, 0.305, 0.35, 0.308, 0.326, 0.219, 0.23, 0.322, 0.405, 0.321, 0.291, 0.312, 0.357, 0.324)

datos <- data.frame(posicion = posicion, bateo = bateo)
datos


table(datos$posicion)

aggregate(bateo ~ posicion, data = datos, FUN = mean)

aggregate(bateo ~ posicion, data = datos, FUN = sd)


ggplot(data = datos, aes(x = posicion, y = bateo, color = posicion)) +
  geom_boxplot() +
  theme_bw()


par(mfrow = c(2,2))
qqnorm(datos[datos$posicion == "C","bateo"], main = "C")
qqline(datos[datos$posicion == "C","bateo"])
qqnorm(datos[datos$posicion == "DH","bateo"], main = "DH")
qqline(datos[datos$posicion == "DH","bateo"])
qqnorm(datos[datos$posicion == "IF","bateo"], main = "IF")
qqline(datos[datos$posicion == "IF","bateo"])
qqnorm(datos[datos$posicion == "OF","bateo"], main = "OF")
qqline(datos[datos$posicion == "OF","bateo"])


#Normalidad
install.packages("nortest")
#require(nortest)
by(data = datos,INDICES = datos$posicion,FUN = function(x){ lillie.test(x$bateo)})


#Igualdad de varianzas
fligner.test(bateo ~ posicion,datos)


require(car)
leveneTest(bateo ~ posicion,datos,center = "median")


#ANALISIS ANOVA

anova <- aov(datos$bateo ~ datos$posicion)
summary(anova)

plot(anova)


#Calcular el tamaño del efecto de un ANOVA
eta_cuadrado <- 0.0076/(0.0076 + 0.4080)
eta_cuadrado

#Comparaciones múltiples

pairwise.t.test(x = datos$bateo, g = datos$posicion, p.adjust.method = "holm",
                pool.sd = TRUE, paired = FALSE, alternative = "two.sided")

TukeyHSD(anova)

plot(TukeyHSD(anova))
