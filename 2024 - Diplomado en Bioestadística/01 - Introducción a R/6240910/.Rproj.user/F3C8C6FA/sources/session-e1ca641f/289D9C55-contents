duracion <- c(30,118,120,123,125,127,130,136,139,141,155,156,158,159,
              160,160,161,168,170,171,172,174,175,178,178,177,184,185,187,191,191,
              194,197,199,201,206,208,210,211,217,248,263,265,270,289,322,388,513,
              520)
e1 <- c(30,136,160,175,191,208,270)
e2 <- c(118,139,161,178,191,210,289)
e3 <- c(120,141,168,178,194,211,322)
e4 <- c(123,155,170,177,197,217,388)
e5 <- c(125,156,171,184,199,248,513)
e6 <- c(127,158,172,185,201,263,520)
e7 <- c(130,159,174,187,206,265,160)
duracion <- cbind(e1,e2,e3,e4,e5,e6)
duracion
boxplot(duracion)
length(duracion) #Tamaño de muestra
quantile(duracion, 0.25) #Primer Cuartil Q1
quantile(duracion, 0.5) #Segundo Cuartil Q2
quantile(duracion, 0.75) #Tercer Cuartil Q3
IQR(duracion) #Rango intercuartílico
stripchart(duracion,at = 1.25,xlim=c(-10,550),method='overplot',
           pch=20,cex = 0.75, col = "blue", main="Número de huecos",xlab="No. 
Huecos")
mtext("(método overplot)",side=1,line=4,font=3)
boxplot(duracion, main="Número de huecos",horizontal = T,add=T)
points(mean(duracion),1, pch = 8,add=T)
arrows(158, 1.21, 83, 1.21, length = 0.15, angle = 10, code=2)
text(120.5, 1.225, "1.5*iqr")
arrows(83, 1.21, 8, 1.21, length = 0.15, angle = 10, code=2)
text(45, 1.225, "1.5*iqr")
arrows(208, 1.21, 283, 1.21, length = 0.15, angle = 10, code=2)
text(245, 1.225, "1.5*iqr")
arrows(283, 1.21, 358, 1.21, length = 0.15, angle = 10, code=2)
text(320, 1.225, "1.5*iqr")
segments(83, 1.3, 83, 0.75,lty=3);text(83, 0.7, "f1")
segments(8, 1.3, 8, 0.75,lty=3);text(8, 0.7, "F1")
segments(283, 1.3, 283, 0.75,lty=3);text(283, 0.7, "f3")
segments(358, 1.3, 358, 0.75,lty=3);text(358, 0.7, "F3")