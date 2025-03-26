## Lectura del archivo cartera 
## Revisar que el archivo cartera se encuentre en documentos, 
## o la ruta en donde se encuentra direccionado R

file.choose()


## Copiar los archivos descargados en la ruta que surge de correr el 
## comando anterior


bebe<-read.csv("C:\\Users\\user\\Documents\\TAREAS-R\\bebesnace.csv", header=TRUE, sep=";", dec=",")

View(bebe)

#---- paquete dplyr
install.packages("dplyr")
library(dplyr)

attach(bebe)

install.packages("openintro")
install.packages("tidyverse")
library(openintro)
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)
str(bebes)

bebes$bweight <- as.factor(bebes$weight)

#Comprobación de supuestos

#1. Diferencia entre las medias muestrales:

muestra1 <- subset(bebes, select = c(gender,bweight))
M1 <- subset(muestra1, gender=='masculino')
F1 <- subset(muestra1, gender=='femenino')
mean(M1) - mean(F1)


M1 <- bebe %>% filter(gender == 'masculino') %>% pull(bweight)
F1 <- bebe %>% filter(gender == 'femenino') %>% pull(bweight)
mean(M1) - mean(F1)

M2 <- bebe %>% filter(gender == 'masculino') %>% pull(blength)
F2 <- bebe %>% filter(gender == 'femenino') %>% pull(blength)
mean(M2) - mean(F2)

M3 <- bebe %>% filter(gender == 'masculino') %>% pull(bheadcir)
F3 <- bebe %>% filter(gender == 'femenino') %>% pull(bheadcir)
mean(M3) - mean(F3)


#2. Independencia

A <- table(gender,bweight)

B <- table(gender,blength)

C  <- table(gender,bheadcir)

addmargins(A)
addmargins(B)
addmargins(C)

#3. Normalidad


ggplot(bebe,aes(x = bweight)) + 
  geom_histogram(aes(y = ..density.., colour = gender)) +
  facet_grid(.~ gender) +
  theme_bw() + theme(legend.position = "none")
  

par(mar = c(2, 2, 2, 2))
par(mfrow = c(1, 2))
qqnorm(M1, xlab = "", ylab = "",
       main = "Masculino", col = "firebrick")
qqline(M1)

qqnorm(F1, xlab = "", ylab = "",
       main = "Femenino", col = "firebrick")
qqline(F1)


#Pruebas

shapiro.test(M1)
shapiro.test(F1)

test_ks <- ks.test(
  M1 <- bebe %>% filter(gender == 'masculino') %>% pull(bweight),
  F1 <- bebe %>% filter(gender == 'femenino') %>% pull(bweight)
  )
test_ks



#4.Homogeneidad de las varianzas

ggplot(data = bebe) +
  geom_boxplot(aes(x = gender, y = bweight, colour = gender)) +
  theme_bw() + theme(legend.position = "none")

install.packages("car")
library(car)
leveneTest(bweight ~ gender, data = bebe, center = "median")

bartlett.test(list(M1,F1))


#5. Prueba T

t.test(
  x           = M1,
  y           = F1,
  alternative = "two.sided",
  mu          = 0,
  var.equal   = TRUE,
  conf.level  = 0.95
)


t.test(
  x           = M1,
  y           = F1,
  alternative = "two.sided",
  mu          = 0,
  var.equal   = FALSE,
  conf.level  = 0.95
)


#CÁLCULO DEL TAMAÑO DEL EFECTO
install.packages("effsize")
library(effsize)
cohen.d(formula = bweight ~ gender, data = bebe, paired = FALSE)


#CONCLUSIÓN
#Dado que p-value (0.06328) es mayor que alpha (0.05) , no se dispone de evidencia 
#suficiente para considerar que existe una diferencia entre el peso promedio 
#de niñas y niños recién nacidos.  
#El tamaño de efecto medido por d-Cohen es pequeño (0.23).