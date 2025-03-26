install.packages("corrplot")
library(corrplot)
#DISTRIBUCIONES DE PROBABILIDAD
#BINOMIAL 
#Suponga que un grupo de agentes de tránsito sale
#a una vía principal para revisar el estado de los
#buses de transporte intermunicipal. De datos 
#históricos se sabe que un 10% de los buses generan
#una mayor cantidad de humo de la permitida. 
#En cada jornada los agentes revisan siempre 20 
#buses, asuma que el estado de un bus es independiente
#del estado de los otros buses
#EJEMPLO
#1. Calcular la probabilidad de que se encuentren 
#exactamente 2 buses que generan una mayor 
#cantidad de humo de la permitida.
dbinom(x=2, size=20, prob=0.10)
# 2. Calcular la probabilidad de que el número 
#de buses que sobrepasan el límite de generación 
#de gases sea al menos 4.
1-(pbinom(q=3, size=20, prob=0.10))
#sum(dbinom(x=4:20, size=20, prob=0.10))
# 3. Calcular la probabilidad de que tres o 
#menos buses emitan gases por encima de lo 
#permitido en la norma.
pbinom(q=3, size=20, prob=0.10)
#Dibujar la función de masa de probabilidad
xb <- 0:20  # Soporte (dominio) de la variable
Probabilidad <- dbinom(x=xb, size=20, prob=0.1)
plot(x=xb, y=Probabilidad, 
     type='h', las=1, lwd=6)
#POISSON 
#ejemplo
#En una editorial se asume que todo libro de 250 
#páginas tiene en promedio 50 errores.
#Encuentre la probabilidad de que en una página 
#cualquiera no se encuentren errores.
#Este es un problema de distribución Poisson con 
#tasa promedio de éxitos dada por:
#λ=0.2
#El objetivo es calcular  P(X=0), para obtener 
#esta probabilidad de usa la siguiente instrucción.
dpois(x=0, lambda=0.2)
#Encuentre la probabilidad de que en una página se encuentren máximo 2 errores
#El objetivo es calcular p(X<=2)
ppois(2,            # Cuantil o vector de cuantiles
      0.2,          # Media o vector de medias
      lower.tail = TRUE, # Si TRUE, las probabilidades son P(X <= x), o P(X > x) en otro caso
      log.p = FALSE)
dpois(x=0, lambda=0.2)+dpois(x=1, lambda=0.2)+dpois(x=2, lambda=0.2)
#Encuentre la probabilidad de que en una página 
#cualquiera se encuentren exactamente 2 errores
dpois(x=2, lambda = 0.2)
#GRÁFICA
# Rejilla de valores del eje X
xp <- 0:50
#-----------
# lambda: 0.5
#-----------
lambda <- 12
plot(dpois(xp, lambda), type = "h", lwd = 2,
     main = "Función de masa de probabilidad",
     ylab = "P(X = x)", xlab = "Número de eventos")
