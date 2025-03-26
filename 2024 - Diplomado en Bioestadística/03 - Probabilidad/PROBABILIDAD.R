#ESPACIO MUESTRAL 
#EJEMPLO 1 :Lanzar una moneda
moneda <- c("C","E")
moneda 
#EJEMPLO 2:Lanzar dos monedas 
moneda2 <- c("CC","CE","EC","EE")
moneda2 
#EJEMPLO 3:Un dado
dado1 <- 1:6
dado1
#EJEMPLO 4: Dos dados 
dado01 <- as.character(1:6)
dado02 <- as.character(1:6)
#ESPACIO MUESTRAL-DATAFRAME
E.Muestral <- expand.grid(dado01,dado02)
colnames(E.Muestral) <- c("Dado-1","Dado-2")
E.Muestral
##Cambiamos la columna Dado_1 de numeric a char
#dado01<-as.character(E.Muestral$`Dado-1`) 
##Cambiamos la columna Dado_2 de numeric a char
#dado02<-as.character(E.Muestral$`Dado-2`)
## Genera el espacio muetral de pares de resultados
## "21" representa que en el dado 1 salio 2 y
## en el dado 2 salio un 1.
#E.Muestral<-paste(dado01,dado02,sep="")
#E.Muestral
#EVENTO
#ejemplo: Determinar el evento de que se observe 
#solamente una cara.  
moneda01<-c("E","C")
moneda02<-moneda01
EspMuemoneda<-expand.grid(moneda01,moneda02)
colnames(EspMuemoneda)<-c("moneda01","moneda02")
##Aqui podemos ver nuestro espacio muestral
EspMuemoneda3
#VERDADERO DONDE TENGA UNA CARA EN LA MODEDA 1
evento <- EspMuemoneda$moneda01=="C"
evento
#EJEMPLO DADOS 
#QUE LA SUMA DE LOS DOS DADOS DE SIETE
#EJEMPLO 4: Dos dados 
#data
dado01 <- as.character(1:6)
dado02 <- as.character(1:6)
#ESPACIO MUESTRAL-DATAFRAME
E.Muestral <- expand.grid(dado01,dado02)
colnames(E.Muestral) <- c("Dado-1","Dado-2")
E.Muestral
check <-as.numeric(E.Muestral$`Dado-1`)+
  as.numeric(E.Muestral$`Dado-2`)==7
evento1 <- E.Muestral[check,]
evento1
#PROBABILIDA CLÁSICA
# Total de resultados posibles (características del dado)
total_resultados <- 6
# Resultados favorables (números pares: 2, 4, 6)
resultados_favorables <- 3
# Calcular la probabilidad clásica
probabilidad <- resultados_favorables / total_resultados
# Mostrar el resultado
print(probabilidad)
#EJEMPLO 2
# Generar todas las combinaciones posibles para dos dados (6x6)
dados <- expand.grid(1:6, 1:6)
# Calcular cuántos resultados tienen al menos un número par
resultados_favorables <- sum(dados$Var1 %% 2 == 0 | dados$Var2 %% 2 == 0)
# Total de resultados posibles (36 combinaciones)
total_resultados <- 36
# Calcular la probabilidad clásica
probabilidad <- resultados_favorables / total_resultados
# Mostrar el resultado
print(probabilidad)
#PROBABILIDAD CONDICIONAL 
# Definir el espacio muestral (resultados posibles de tirar 
#un dado)
dado <- 1:6
# Evento A: Obtenemos un número mayor o igual a 4
evento_A <- dado[dado >= 4]
# Evento B: Obtenemos un número impar
evento_B <- dado[dado %% 2 != 0]
# Intersección de A y B: Números que son tanto mayores o 
#iguales a 4 como impares
interseccion_AB <- intersect(evento_A, evento_B)
# Probabilidad de A (P(A)) - Probabilidad de que salga un 
#número mayor o igual a 4
P_A <- length(evento_A) / length(dado)
# Probabilidad de B (P(B)) - Probabilidad de que salga un 
#número impar
P_B <- length(evento_B) / length(dado)
# Probabilidad de la intersección A y B- Probabilidad de 
#que salga un número mayor o igual a 4 e 
#impar
P_A_inter_B <- length(interseccion_AB) / length(dado)
# Probabilidad condicional 
P_A_given_B <- P_A_inter_B / P_B
# Imprimir resultados
cat("Probabilidad de A (P(A)): ", P_A, "\n")
cat("Probabilidad de B (P(B)): ", P_B, "\n")
cat("Probabilidad de la intersección A y B: ",
    P_A_inter_B, "\n")
cat("Probabilidad condicional P(A | B): ", P_A_given_B,
    "\n")
#EJEMPLO 2 PROBABILIDAD CONDICIONAL
#Datos de ejemplo: Un conjunto de datos sobre la presencia
#de enfermedad y fumar
set.seed(123)
# Generamos un conjunto de datos con 1000 personas
n <- 1000
enfermedad <- sample(c(0, 1), n, replace = TRUE)  # 0 = No
#tiene enfermedad, 1 = Tiene enfermedad
fuma <- sample(c(0, 1), n, replace = TRUE)  # 0 = No fuma, 
#1 = Fuma
# Evento A: Personas que tienen la enfermedad
evento_A <- enfermedad == 1
# Evento B: Personas que fuman
evento_B <- fuma == 1
#Probabilidad de A(P(A)):Personas que tienen la enfermedad
P_A <- mean(evento_A)
# Probabilidad de B (P(B)): Personas que fuman
P_B <- mean(evento_B)
#Intersección de A y B : Personas que tienen la enfermedad
#y fuman
P_A_inter_B <- mean(evento_A & evento_B)
# Probabilidad condicional P(A | B): Personas que tienen 
#la enfermedad dado que fuman
P_A_given_B <- P_A_inter_B / P_B
# Imprimir resultados
cat("Probabilidad de A (P(A)): ", P_A, "\n")
cat("Probabilidad de B (P(B)): ", P_B, "\n")
cat("Probabilidad de la intersección A y B:", P_A_inter_B,
    "\n")
cat("Probabilidad condicional P(A | B): ", P_A_given_B,
    "\n")
