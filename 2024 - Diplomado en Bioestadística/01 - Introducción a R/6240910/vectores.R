# Iniciamos creando vectores
v <- c(0:5) 
v
class(v)
length(v)
t <- c('Zipaquirá','Bogotá', 'Bogotá', 'Barranquilla')
t
class(t)
# Sumar un escalar a un vector
suma_escalar <- v+5
suma_escalar

# Suma de vectores
w <- c(7:12)
length(v)
length(w)
w_mas_v <- v+w
w_mas_v
w
v
rt <- c(-2.48:2)
rt

# Creacion de matrices
A <- matrix(1:4,2,2)
A
# Suma de un escalar a una matriz
suma_escalar_matriz <- 5 + A
suma_escalar_matriz

# Suma de un vector y una matriz
v_1 <- (2*1:4)
v_1

seq(2,5, by=0.2)
length(v_1)
D <- matrix(0,2,4)
D
v_1+D
D+v_1

#Longitud y dimension
S <- matrix(1,4,3)
S
length(v_1)
v_1+S

#Multiplicacion de matrices
r = c(2*1:4)
r
r_d = r/2
r2 = c(2/1:4)
r2

# Producto escalar
a <- c(1:3)
b <- c(2,5,8)
sum(a*b)

# producto ortogonal

# Norma de un vector
norma_vector <- function(x) sqrt(sum(x*x))
h <- c(3,4)
norma_vector(h)

# Producto matricial
m_1 <- matrix(1:4,2,2)
m_1
m_2 <- matrix(1,2,2) 
m_2

m_1%*%m_2


