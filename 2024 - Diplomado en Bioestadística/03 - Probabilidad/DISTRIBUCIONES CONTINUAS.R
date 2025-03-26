#DISTRIBUCIÓN NORMAL 
#dnorm(x,           # Valores del eje X (rejilla)
#     mean = 0,    # Número o vector representando la/s media/s
#    sd = 1,      # Número o vector representando la/s desviación/es típica/s
#   log = FALSE) # Si TRUE, las probabilidades se devuelven como logaritmos
#ejemplo
#Considera, por ejemplo, que quieres crear
#la función de densidad de probabilidad normal 
#para x∈(−5,5), con media 1 y desviación típica
#de 3. Para calcularla, puedes escribir:
x <- -5:5
xx <- seq(-5, 5, length = 20) # Más observaciones
dnorm(x, mean = 1, sd = 3) 
#dibujar la distribución normal 
par(mfrow = c(1, 2))
#REPRESENTAR NORMAL 
# Rejilla de valores para el eje X
x <- seq(-4, 8, 0.1)

#-----------------------------------------
# Misma desviación típica, distinta media
#-----------------------------------------
# Media 0, desviación típica 1
plot(x, dnorm(x, mean = 0, sd = 1), type = "l",
     ylim = c(0, 0.6), ylab = "", lwd = 2, col = "red")
# Media 3, desviación típica 1
lines(x, dnorm(x, mean = 3, sd = 1), col = "blue", lty = 1, lwd = 2)

# Añadimos una leyenda
#legend("topright", c(expression(paste(, mu, " ", sigma)), "0 1", "3 1"),
#    lty = c(0, 1, 1), col = c("blue", "red"), box.lty = 0, lwd = 2,cex = 0.8)

#-----------------------------------------
# Misma media, distinta desviación típica
#-----------------------------------------
# Media 1, desviación típica 1
plot(x, dnorm(x, mean = 1, sd = 1), type = "l",
     ylim = c(0, 1), ylab = "", lwd = 2, col = "red")
# Media 1, desviación típica 0.5
lines(x, dnorm(x, mean = 1, sd = 0.5), col = "blue", lty = 1, lwd = 2)

# Añadimos una leyenda
#legend("topright", c(expression(paste(, mu, " ", sigma)), "1 1", "1 0.5"),
#     lty = c(0, 1, 1), col = c("blue", "red"), box.lty = 0, lwd = 2,cex = 0.8)
par(mfrow = c(1, 1))
#EJEMPLO
#Ejemplo de la función pnorm
#Ahora, supón que tienes una máquina que empaqueta
#arroz dentro de cajas. El proceso sigue una 
#distribución normal y se sabe que la media del
#peso de cada caja es de 1000 gramos y la desviación típica es 10 gramos. 
#Puedes dibujar la función de densidad normal en
#R escribiendo:
# Media y desviación típica
mu <- 1000
sigma <- 10
# Grid para una distribución normal no estándar
x <- seq(-3, 3, length = 100) * sigma + mu
# Función de densidad
f <- dnorm(x, mu, sigma)
plot(x, f, type = "l", lwd = 2, col = "blue", ylab = "", xlab = "Weight")
abline(v = mu) # Línea vertical en la media
#a)Primero, si quieres calcular la probabilidad 
#de que una caja pese menos de 1010 gramos (
#P(X<1010)=P(X≤1010)), puedes escribir lo que
#sigue:
pnorm(1010, mu, sigma) # 0.8413447 o 84.13%
1 - pnorm(1010, mu, sigma, lower.tail = FALSE) # Equivalente
#Por lo que la probabilidad de que la caja pese 
#menos de 1010 gramos es 0.8413 o 84.13%, 
#que corresponde a el área de la siguiente 
#ilustración:
lb <- min(x) # Límite inferior
ub <- 1010   # Límite superior
x2 <- seq(min(x), ub, length = 100) # Nueva rejilla
y <- dnorm(x2, mu, sigma) # Densidad
plot(x, f, type = "l", lwd = 2, col = "blue", ylab = "", xlab = "Peso")
abline(v = ub) 
polygon(c(lb, x2, ub), c(0, y, 0), col = rgb(0, 0, 1, alpha = 0.5))
text(995, 0.01, "84.13%")
#OTRA FORMA DE REPRESENTAR LA PROBABILIDAD-NORMAL
# mean: media de la variable normal
# sd: desviación típica de la variable normal
# lb: límite inferior del área
# ub: límite superior del área
# acolor: color del área
# ...: argumentos adicionales para ser pasados a la función lines
normal_area <- function(mean = 0, sd = 1, lb, ub, acolor = "lightgray", ...) {
  x <- seq(mean - 3 * sd, mean + 3 * sd, length = 100) 
  if (missing(lb)) {
    lb <- min(x)
  }
  if (missing(ub)) {
    ub <- max(x)
  }
  
  x2 <- seq(lb, ub, length = 100)    
  plot(x, dnorm(x, mean, sd), type = "n", ylab = "")
  
  y <- dnorm(x2, mean, sd)
  polygon(c(lb, x2, ub), c(0, y, 0), col = acolor)
  lines(x, dnorm(x, mean, sd), type = "l", ...)
}
#segundo ejemplo 
#en caso de que quieras calcular la probabilidad de que
#una caja pese más de 980 gramos (P(X>980)=P(X≥980)) 
pnorm(980, mu, sigma, lower.tail = FALSE) # 0.9772499 o 97.72%
1 - pnorm(980, mu, sigma) # Equivalente
pnorm(1020, mu, sigma)    # Equivalente por simetría
#La probabilidad calculada corresponde al área:
normal_area(mean = mu, sd = sigma, lb = 980, acolor = rgb(0, 0, 1, alpha = 0.5))
text(1000, 0.01, "97.72%")
