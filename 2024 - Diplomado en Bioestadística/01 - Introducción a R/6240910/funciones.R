# Graficando funciones
y <- function(x) x*sqrt(x+3)
plot(y,
     main ="Función 1",
     xlab = "Variable independiente (x)",
     ylab = "Variable dependiente y(x)",
     xlim = c(-3,4),
     ylim = c(-2,5),
     type = "o",
     lwd  = 2,
     col  = "green" )

# Graficos de varias funciones
x0 <- seq(-5,5,0.01)
lon <- length(x0)
lon

#creacion de tres funciones trigonometricas
y1 <- sin(x0)
y2 <- cos(x0)
y3 <- tan(x0)

#lienzo
xrange <- c(1,lon)
yrange <- c(-6,6)

plot(xrange,yrange,type="n",xaxt="n",ylab ="")

#graficar curvas
lines(y1, type="p", lwd=1,col="red")
lines(y2, type="p", lwd=1,col="yellow")
lines(y3, type="p", lwd=1,col="blue")
abline(h=0,lwd=2)

 #leyendas
legend("bottomright",
       c("sin(x)","cos(x)","tan(x)"),
       fill = c("red","yellow","blue"),
       cex=0.5)
