#Análisis base de datos diabete
# 1) Interprete la información que se presenta en la base de datos
diabetes <- read.csv('diabetes.csv')
diabetes
#¿Dequé tipo es el objeto diabetes?
class(diabetes)
# Renombre las etiquetas en español con su resoectiva traducción
# Antes imprima las etiquetas 
names(diabetes)
names(diabetes)<- c('embarazos', 'Glucosa',
                    'presion sanguinea', 'espesor de la piel',
                    'insulina', 'IMC',
                    'Diabetes Pedigree Función', 'Edad',
                    'Resultado')
diabetes
names(diabetes)
# Obtenga los Box-Plot asociados a las variables permitidas y
# discuta los resultados
# Indague por la naturaleza de cada una de las variables
class(diabetes$embarazos)
class(diabetes$Glucosa)
class(diabetes$`presion sanguinea`)
class(diabetes$`espesor de la piel`)
class(diabetes$insulina)
class(diabetes$IMC)
class(diabetes$`Diabetes Pedigree Función`)
class(diabetes$Edad)
class(diabetes$Resultado)
boxplot(diabetes)

#Desarrolle un diagrama de dispersión para cada una de las variables 
# Primero genere los objetos para las nueve variables con su 
# respectivo nombre
embarazos <- diabetes$embarazos
glucosa <- diabetes$Glucosa
presion_s <- diabetes$`presion sanguinea`
espesor <- diabetes$`espesor de la piel`
insulina <- diabetes$insulina
imc <- diabetes$IMC
pedigree <- diabetes$`Diabetes Pedigree Función`
edad <- diabetes$Edad
pronostico <- diabetes$Resultado
plot(glucosa, embarazos)
plot(embarazos, insulina)
# Resulta tedioso graficar manualmante.
pairs(diabetes)
cor(diabetes)
# ¿Qué puede concluir es plausible aplicar una regresión lineal?
# Si su respuesta es negativa ¿Qué método se puede aplicar?


##################################################################
#                     Regresión Lineal Medios
##################################################################
mercado <- read.csv('Advertising.csv')
data
class(mercado)
names(mercado)
mercado
#data <- data[,-1]
#attach(data)

pairs(mercado)
mercado
cor(mercado)
t
tv <- mercado$TV
tv
ventas <- mercado$Sales
plot(tv, ventas)

regression <- lm(ventas~tv, data = mercado)
summary(regression)






