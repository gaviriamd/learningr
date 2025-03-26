# En los lenguajes de programación orientados a objetos exiten tres 
# objetos indispensbles: Vectores, matrices y data frames.

###################################################################
#                   CREACIÓN DE UN VECTOR
###################################################################
# Por convención se reservan las letras minúsculas para este objeto
a <- c(1,2,3,4,5,6,7,8,9,10)
a
a <- c(1:10)
a

# El comando seq resulta útil para definir secuencias, ejecute la
# instrucción
b <- seq(-3,3,0.5)
b
# ¿Qué observa en el resultado? cree un vector en el intervalo [-6,6]
# En pasos de 2.

# Otra función es la función rep(), ejecute la instrucción
d <- rep(1:3,3)
d
# ¿Qué se obtiene en el resultado?
# La longitud de un vector se calcula por medio de la instrucción
length(a)
length(b)
length(d)
#¿A qué hace referencia la longitud de un vector?
#¿Qué observa en el vector f?
f <- c('a',1,0.3)
f
class(f)
class(a)
class(b)
class(d)
###################################################################
#                    Matrices
###################################################################
# Este objeto se invoca por medio del objeto matrix y habilita la
# opción de construir arreglos de dos dimensiones con datos del 
# mismo tipo.

# creación de matrices por medio de vectores
matriz <- matrix(c(1:9),3,3)
matriz
# Cambie el primer 3 por 2 ¿Qué sucede?
# La dimensión de la matriz se obtiene por medio de la instrucción
dim(matriz)
length(matriz)

# Elementos concretos de una matriz
matriz[2,3]
matriz[1:2,2:3]

# Explique las dos últimas líneas de código

#Producto entre matrices
matriz1 <- matriz*matriz #Producto de elemento a elemento
matriz1
matriz1 <- matriz%*%matriz
matriz1
#Explique como se obtuvo el último resultado

#################################################################
#                   Hojas de datos
#################################################################
# Los datos y el correspondiente almacenamiento son el punto de 
# partida para un estudio estadístico
# Cada fila se refiere a un elemento de la muestra
# Cada columna se conecta con la variable de estudio
# Estadísticamente: las filas son los casos y las columnas las varables
# Construya una hoja de datos con la información:

#sintomas: gripe, dolor de cabeza, vomito, fiebre
#Peso (kg): 68, 72, 50, 82
#Estatura (m): 1.70, 1.68, 1.48, 1.85
#Sexo: masculino, femenino, masculino, femenino

#Represente la información en una hoja de datos
sintomas <- c('gripe', 'dolor de cabeza', 'vomito', 'fiebre')
peso <- c(68, 72, 50, 82)
estatura <- c(1.70, 1.68, 1.48, 1.85)
sexo <- c('m','f','m','f')

#Indagando por las clases del objeto
class(sintomas)
class(peso)
class(estatura)
class(sexo)

# Podriamos definir la hoja de datos como
datos <- data.frame(sintomas, peso, estatura, sexo)
datos
summary(datos)

# Bosquejo gráfico
boxplot(datos$peso)
boxplot(datos$estatura)

# Modificando etiquetas de las variables
names(datos)
names(datos) <- c('v1','v2','v3', 'v4')
datos

#####################################################################
#                         Manejo de datos
##################################################################### 
# De forma general, se tiene la información de n individuos referida 
# a k variables 

x <- c(161, 203, 235, 176,
       201, 188, 228, 211,
       191, 178)
y <- c(159, 206, 241, 163,
       197, 193, 209, 189,
       169, 201)
genero <- factor(c('Hombre', 'Mujer', 'Hombre',
                   'Hombre', 'Hombre', 'Mujer',
                   'Mujer','Mujer','Hombre','Hombre'))
genero
Datos.Pruebas <- data.frame(Prueba.escrita = x, Prueba.oral=y,
                            Genero = genero)
Datos.Pruebas


