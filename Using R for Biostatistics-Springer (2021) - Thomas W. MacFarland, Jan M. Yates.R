########################################################################
############################### HOUSEKEEPING ###########################
########################################################################
date() # Current system time and date.
R.version.string # R version and version release date.
options(digits=6) # Confirm default digits.
options(scipen=999)# Suppress scientific notation.
options(width=60) # Confirm output width.
ls() # List all objects in the working directory.
rm(list = ls()) # CAUTION: Remove all files in the
# working directory. If this
# action is not desired, use rm()
# one-by-one to remove the objects
# that are not needed.
ls.str() # List all objects with finite detail.
getwd() # Identify the current working directory.
setwd("C:/Users/juand/iCloudDrive/Research/SeGrEMO/Proyectos de Investigación/2023 - 07 - 09 - Intento de Suicidio HSLV 2019")
# Set to a new working directory.
# Note the single forward slash and double
# quotes.
# This new directory should be the directory
# where the data file is located, otherwise
# the data file will not be found.
getwd() # Confirm the working directory.
list.files() # List files at the PC directory.
.libPaths() # Library pathname.
.Library # Library pathname.
sessionInfo() # R version, locale, and packages.
search() # Attached packages and objects.
searchpaths() # Attached packages and objects.
######################################################################

# Use the utils::read.table() function to import the
# .csv file INtSUIDATASET.csv into the current R
# session and place the contents into the object
# dat.df, which is a dataframe that: (1) has a
# header row, (2) uses a period for decimals, and (3)
# uses a comma to separate one field from another.
#
# Note how the utils package is available as one of the
# packages immediately put into use when a R session is
# first started.
dat.df <- utils::read.table (file =
                                  "INTSUIDATASET.csv",
                                header=TRUE, dec=".", sep=",")

getwd() # Identify the working directory
ls() # List objects
attach(dat.df) # Attach the data, for later use
str(dat.df) # Identify structure
head(dat.df, n=3) # Show the head, 1st 3 cases
summary(dat.df) # Summary statistics

# Confirm that the number associated with dat.df$Edad are numeric.
dat.df$Edad <-as.numeric(dat.df$Edad)
length(dat.df$Edad)
table(is.na(dat.df$Edad))

# This graphic is a quality assurance density plot of the
# object variables dat.df$Edad, Estrato, NumeroIntentosPrevios, 
# EstanciaHospitalaria, with arguments
# resulting in a thick (lwd=5) red line.
par(ask=TRUE)
plot(density(dat.df$Edad, na.rm=TRUE),
     main="Edad (Años) de los casos de Intentos de Suicidio
     Hospital Susana López de Valencia, 2019",
     col="red", lwd=5)

##LOCATION (CENTRAL TENDENCY) AND VARIABILITY STATISTICS (DISPERSION)
mean(dat.df$Edad, na.rm=TRUE)
median(dat.df$Edad, na.rm=TRUE)
mean(dat.df$Edad, tr=.1, na.rm=TRUE)
sd(dat.df$Edad, na.rm=TRUE)
IQR(dat.df$Edad, na.rm=TRUE)
mad(dat.df$Edad, na.rm=TRUE)
summary(dat.df$Edad)

# Confirm that the number associated with dat.df$Estrato are numeric.
dat.df$Estrato <-as.numeric(dat.df$Estrato)
length(dat.df$Estrato)
table(is.na(dat.df$Estrato))

par(ask=TRUE)
plot(density(dat.df$Estrato, na.rm=TRUE),
     main="Estrato Socio-Económico de los casos de Intentos de Suicidio
     Hospital Susana López de Valencia, 2019",
     col="red", lwd=5)

##LOCATION (CENTRAL TENDENCY) AND VARIABILITY STATISTICS (DISPERSION)
mean(dat.df$Estrato, na.rm=TRUE)
median(dat.df$Estrato, na.rm=TRUE)
mean(dat.df$Estrato, tr=.1, na.rm=TRUE)
sd(dat.df$Estrato, na.rm=TRUE)
IQR(dat.df$Estrato, na.rm=TRUE)
mad(dat.df$Estrato, na.rm=TRUE)
summary(dat.df$Estrato)

# Confirm that the number associated with dat.df$NumeroIntentosPrevios are numeric.
dat.df$NumeroIntentosPrevios <-as.numeric(dat.df$NumeroIntentosPrevios)
length(dat.df$NumeroIntentosPrevios)
table(is.na(dat.df$NumeroIntentosPrevios))

par(ask=TRUE)
plot(density(dat.df$NumeroIntentosPrevios, na.rm=TRUE),
     main="Número de Intentos Previos de los casos de Intentos 
     de Suicidio Hospital Susana López de Valencia, 2019",
     col="red", lwd=5)

##LOCATION (CENTRAL TENDENCY) AND VARIABILITY STATISTICS (DISPERSION)
mean(dat.df$NumeroIntentosPrevios, na.rm=TRUE)
median(dat.df$NumeroIntentosPrevios, na.rm=TRUE)
mean(dat.df$NumeroIntentosPrevios, tr=.1, na.rm=TRUE)
sd(dat.df$NumeroIntentosPrevios, na.rm=TRUE)
IQR(dat.df$NumeroIntentosPrevios, na.rm=TRUE)
mad(dat.df$NumeroIntentosPrevios, na.rm=TRUE)
summary(dat.df$NumeroIntentosPrevios)

# Confirm that the number associated with dat.df$EstanciaHospitalaria are numeric.
dat.df$EstanciaHospitalaria <-as.numeric(dat.df$EstanciaHospitalaria)
length(dat.df$EstanciaHospitalaria)
table(is.na(dat.df$EstanciaHospitalaria))

par(ask=TRUE)
plot(density(dat.df$EstanciaHospitalaria, na.rm=TRUE),
     main="Estancia Hospitalaria de los casos de Intentos de Suicidio
     Hospital Susana López de Valencia, 2019",
     col="red", lwd=5)

##LOCATION (CENTRAL TENDENCY) AND VARIABILITY STATISTICS (DISPERSION)
head(dat.df$EstanciaHospitalaria) # Show the head, first values
tail(dat.df$EstanciaHospitalaria) # Show the tail, last values
mean(dat.df$EstanciaHospitalaria, na.rm=TRUE)
median(dat.df$EstanciaHospitalaria, na.rm=TRUE)
mean(dat.df$EstanciaHospitalaria, tr=.1, na.rm=TRUE)
sd(dat.df$EstanciaHospitalaria, na.rm=TRUE)
IQR(dat.df$EstanciaHospitalaria, na.rm=TRUE)
mad(dat.df$EstanciaHospitalaria, na.rm=TRUE)
summary(dat.df$EstanciaHospitalaria)

#GENERATE A 4 HISTOGRAMS PICTURE TO SUMARIZE
par(ask=TRUE)
par(mfrow=c(2,2)) #"THIS MAKES 4 SMALL; AVOID IF IF WANT IT BIG"
hist(dat.df$Edad,
     breaks=10, # Granularity of output
     main="Edad Pacientes con  de Intentos de Suicidio
     Hospital Susana López de Valencia, 
     Popayán - Colombia, 2019.",
     col="red", # Color(s)
     xlab="Edad (Años)",
     ylab="Número de pacientes",
     font.axis=2, # Make the axis bold
     font.lab=2, # Make the labels bold
     cex.main=1, # Size - main (title)
     cex.lab=1.25, # Size - labels
     xlim=c(10,55), # Adjust X axis limits (LOOK AT MIN. - MAX. FOR THIS)
     ylim=c(0,55)) # Adjust Y axis limits (LOOK AT FREQUENCY FOR THIS)
hist(dat.df$Estrato,
     breaks=4, # Granularity of output
     main="Estrato Socio-Económico de Pacientes 
     con Intentos de Suicidio Hospital 
     Susana López de Valencia, 2019.",
     col="red", # Color(s)
     xlab="Estrato Socio-Económico",
     ylab="Número de pacientes",
     font.axis=2, # Make the axis bold
     font.lab=2, # Make the labels bold
     cex.main=1, # Size - main (title)
     cex.lab=1.25, # Size - labels
     xlim=c(1,5), # Adjust X axis limits (LOOK AT MIN. - MAX. FOR THIS)
     ylim=c(0,120)) # Adjust Y axis limits (LOOK AT FREQUENCY FOR THIS)
hist(dat.df$NumeroIntentosPrevios,
     breaks=5, # Granularity of output
     main="Número de Intentos Previos en Pacientes con  
     de Intento de Suicidio Hospital 
     Susana López de Valencia, 2019.",
     col="red", # Color(s)
     xlab="Intentos de Suicidio Previos",
     ylab="Número de pacientes",
     font.axis=2, # Make the axis bold
     font.lab=2, # Make the labels bold
     cex.main=1, # Size - main (title)
     cex.lab=1.25, # Size - labels
     xlim=c(0,5), # Adjust X axis limits (LOOK AT MIN. - MAX. FOR THIS)
     ylim=c(0,140)) # Adjust Y axis limits (LOOK AT FREQUENCY FOR THIS)
hist(dat.df$EstanciaHospitalaria,
     breaks=10, # Granularity of output
     main="Estancia Hospitalaria de Pacientes 
     con Intento de Suicidio Hospital
     Susana López de Valencia, 2019",
     col="red", # Color(s)
     xlab="Estancia Hospitalaria (Días)",
     ylab="Número de pacientes",
     font.axis=2, # Make the axis bold
     font.lab=2, # Make the labels bold
     cex.main=1, # Size - main (title)
     cex.lab=1.25, # Size - labels
     xlim=c(0,20), # Adjust X axis limits (LOOK AT MIN. - MAX. FOR THIS)
     ylim=c(0,135)) # Adjust Y axis limits (LOOK AT FREQUENCY FOR THIS)

##CREATE BEAUTIFUL GRAPHICS
install.packages("ggplot2", dependencies=TRUE)
library(ggplot2) # Load the ggplot2 package.
help(package=ggplot2) # Show the information page.
sessionInfo() # Confirm all attached packages.

install.packages("ggthemes", dependencies=TRUE)
library(ggthemes) # Load the ggthemes package.
help(package=ggthemes) # Show the information page.
sessionInfo() # Confirm all attached packages.

install.packages("ggmosaic", dependencies=TRUE)
library(ggmosaic) # Load the ggmosaic package.
help(package=ggmosaic) # Show the information page.
sessionInfo() # Confirm all attached packages.

install.packages("gridExtra", dependencies=TRUE)
library(gridExtra) # Load the gridExtra package.
help(package=gridExtra) # Show the information page.
sessionInfo() # Confirm all attached packages.

install.packages("grid", dependencies=TRUE)
library(grid) # Load the grid package.
help(package=grid) # Show the information page.
sessionInfo() # Confirm all attached packages.
  
install.packages("scales", dependencies=TRUE)
library(scales) # Load the scales package.
help(package=scales) # Show the information page.
sessionInfo() # Confirm all attached packages.

##MAKE Beautiful Graphics (REMEMBER LOAD GGPLOT AND GGTHEM) EXAMPLE
#EDAD DE ACUERDO AL SEXO
# \n is used to force a line break
ggplotEdad1 <-
  ggplot2::ggplot(dat.df,
                  aes(x=Edad)) +
  geom_density(alpha = 0.5) +
  ggtitle("Edad Pacientes con Intento de Suicidio HSLV 2019") +
  xlab("Edad (Años)") +
  ylab("Número de Pacientes") +
  theme_few() +   # Review the ggthemes package
  theme(legend.position="none") # No legend

#NÚMERO DE INTENTOS PREVIOS DE ACUERDO AL SEXO
# \n is used to force a line break
ggplotEdad2 <-
  ggplot2::ggplot(dat.df,
                  aes(x=Sexo, y=Edad, fill=Sexo)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=1, size=6,
               col="black") +
  # Add a circle to represent the mean, along with the median
  # which shows as the solid line
  facet_grid( ~ as.factor(Procedencia)) +
  ggtitle("Edad Pacientes con Intento de Suicidio Hospital 
          Susana López de Valencia, Popayán - Cauca,  2019") +
  xlab("Sexo") +
  ylab("Edad") +
  scale_y_continuous(labels=scales::comma, limits=c(0,60),
                     breaks=scales::pretty_breaks(n = 3)) +
  theme_stata() + # Review the ggthemes package
  theme(legend.position="none") # No legend

par(ask=TRUE); gridExtra::grid.arrange(
  ggplotEdad1,
  ggplotEdad2, ncol=1)

# \n is used to force a line break
par(ask=TRUE)
ggplot2::ggplot(dat.df,
                aes(x=Sexo, y=NumeroIntentosPrevios, fill=Sexo)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=1, size=6,
               col="black") +
  # Add a circle to represent the mean, along with the median
  # which shows as the solid line
  ggtitle(
    "Número de Intentos Previos de Acuerdo al Sexo\n") +
  xlab("Sexo") +
  ylab("Número de Intentos Previos\n") +
  scale_y_continuous(labels=scales::comma, limits=c(0,6),
                     breaks=scales::pretty_breaks(n = 5)) +
  theme_economist_white(base_size=12, base_family="sans",
                        gray_bg=FALSE, horizontal=TRUE) +
  theme(legend.position="none") # No legend

########################################################################
################################# PRO-TIPS ##############################
########################################################################
#Ctr + 1: Cleans the Console
ls() #To #To visualize The Number of Datasets
objects()  #To visualize The Number of Objects

################################################################################
########## Prepare to Exit, Save, and Later Retrieve This R Session ############
################################################################################

getwd() # Identify the current working directory.
ls() # List all objects in the working
# directory.
ls.str() # List all objects, with finite detail.
list.files() # List files at the PC directory.
save.image("IntentodeSuicidioHSLV2019.rdata")
getwd() # Identify the current working directory.
ls() # List all objects in the working
# directory.
ls.str() # List all objects, with finite detail.
list.files() # List files at the PC directory.
alarm() # Alarm, notice of upcoming action.
q() # Quit this session.
# Prepare for Save workspace image? query.

################################################################################
################################## GOOD-BYE ####################################
################################################################################


################################################################################
################################## GOOD-BYE ####################################
################################################################################



################################################################################
### Data Exploration, Descriptive Statistics, $ Measures of Central Tendency ###
################################################################################

getwd() # Identify the working directory
ls() # List objects
attach(dat.df) # Attach the data, for later use
str(dat.df) # Identify structure
nrow(dat.df) # List the number of rows
ncol(dat.df) # List the number of columns
dim(dat.df) # Dimensions of the data frame
names(dat.df) # Identify names
colnames(dat.df) # Show column names
rownames(dat.df) # Show row names
head(dat.df) # Show the head
tail(dat.df) # Show the tail
dat.df # Show the entire dataframe
summary(dat.df) # Summary statistics

class(dat.df) # Class
str(dat.df) # Structure

# DataFrame$ObjectName notation: Useful for IDs
duplicated(dat.df$Edad) # Duplicates

#######################################################
# Code Book for dat.df                                #
#######################################################
#                                                     #
# Sexo ........................ Factor (e.g., nominal)#
#                                 Masculino o femenino#
#                                                     #
# Edad ......................... Numeric (e.g., Ratio)#
#                                              En años#
#                                                     #
# Etnia ........................Factor (e.g., nominal)#
#                      Indígena, blanco, mestizo, afro#
#                                                     #
# Estado Civil................  Factor (e.g., nominal)#
#                         Soltero, Unión libre, casado#
#                                                     #                                                     #
# Numero de intentos Previos....Numeric (e.g., ratio) #
#           Intentos de Suicidio previos a este evento#
#######################################################

## Conduct a Visual Data Check Using Graphics NUMERIC
par(ask=TRUE)
par(mfrow=c(2,3)) # 6 figures into a 2 row by 3 column grid
hist(dat.df$Edad, main="Edad (Años): Histogram")
plot(dat.df$Edad, main="Edad (Años): Plot")
plot(density(dat.df$Edad, na.rm=TRUE), # na.rm=TRUE
     main="Edad (Años): Density Plot") # missing data
boxplot(dat.df$Edad,
        main="Edad (Años): Box Plot")
stripchart(dat.df$Edad,
           main="Edad (Años): Stripchart") # Stripchart
qqnorm(dat.df$Edad, main="Edad (Años): Q-Q Plot")
# Common figures for a numeric-type object variable

par(ask=TRUE)
par(mfrow=c(2,3)) # 6 figures into a 2 row by 3 column grid
hist(dat.df$Estrato, main="Estrato Socio-Económico: Histogram")
plot(dat.df$Estrato, main="Estrato Socio-Económico: Plot")
plot(density(dat.df$Estrato, na.rm=TRUE), # na.rm=TRUE
     main="Estrato Socio-Económico: Density Plot") # missing data
boxplot(dat.df$Estrato,
        main="Estrato Socio-Económico: Box Plot")
stripchart(dat.df$Estrato,
           main="Estrato Socio-Económico: Stripchart") # Stripchart
qqnorm(dat.df$Estrato, main="Estrato Socio-Económico: Q-Q Plot")
# Common figures for a numeric-type object variable

par(ask=TRUE)
par(mfrow=c(2,3)) # 6 figures into a 2 row by 3 column grid
hist(dat.df$NumeroIntentosPrevios, main="Intentos Previos: Histogram")
plot(dat.df$NumeroIntentosPrevios, main="Intentos Previos: Plot")
plot(density(dat.df$NumeroIntentosPrevios, na.rm=TRUE), # na.rm=TRUE
     main="Intentos Previos: Density Plot") # missing data
boxplot(dat.df$NumeroIntentosPrevios,
        main="Intentos Previos: Box Plot")
stripchart(dat.df$NumeroIntentosPrevios,
           main="Intentos Previos: Stripchart") # Stripchart
qqnorm(dat.df$NumeroIntentosPrevios, main="Intentos Previos: Q-Q Plot")
# Common figures for a numeric-type object variable

par(ask=TRUE)
par(mfrow=c(2,3)) # 6 figures into a 2 row by 3 column grid
hist(dat.df$EstanciaHospitalaria, main="Estancia Hospitalaria 
     (Días): Histogram")
plot(dat.df$EstanciaHospitalaria, main="Estancia Hospitalaria 
     (Días): Plot")
plot(density(dat.df$EstanciaHospitalaria, na.rm=TRUE), # na.rm=TRUE
     main="Estancia Hospitalaria 
     (Días): Density Plot") # missing data
boxplot(dat.df$EstanciaHospitalaria,
        main="Estancia Hospitalaria
        (Días): Box Plot")
stripchart(dat.df$EstanciaHospitalaria,
           main="Estancia Hospitalaria 
           (Días): Stripchart") # Stripchart
qqnorm(dat.df$NumeroIntentosPrevios, main="Estancia Hospitalaria 
       (Días): Q-Q Plot")
# Common figures for a numeric-type object variable

## Conduct a Visual Data Check Using Graphics CATHEGORICAL
par(ask=TRUE)
par(mfrow=c(1,2)) # 2 figures into a 1 row by 2 column grid
barplot(table(dat.df$Sexo),
        main="Sexo: Barplot Frequency Distribution",
        col=c("blue", "red"), ylim=c(0,120)) # Alter color - Y scale
barplot(table(dat.df$Etnia),
        main="Edad: Barplot Frequency Distribution",
        col=c("blue", "red"), ylim=c(0,150)) # Alter color - Y scale

par(ask=TRUE)
par(mfrow=c(1,2)) # 2 figures into a 1 row by 2 column grid
barplot(table(dat.df$EstadoCivil),
        main="Estado Civil: Barplot Frequency Distribution",
        col=c("green", "red"), ylim=c(0,120)) # Alter color - Y scale
barplot(table(dat.df$Ocupacion),
        main="Ocupacion: Barplot Frequency Distribution",
        col=c("green", "red"), ylim=c(0,120)) # Alter color - Y scale

par(ask=TRUE)
par(mfrow=c(1,2)) # 2 figures into a 1 row by 2 column grid
barplot(table(dat.df$Procedencia),
        main="Procedencia: Barplot Frequency Distribution",
        col=c("green", "Blue"), ylim=c(0,120)) # Alter color - Y scale
barplot(table(dat.df$Dia),
        main="Ocupacion: Barplot Frequency Distribution",
        col=c("green", "red"), ylim=c(0,120)) # Alter color - Y scale

par(ask=TRUE)
par(mfrow=c(1,2)) # 2 figures into a 1 row by 2 column grid
barplot(table(dat.df$Mes),
        main="Mes: Barplot Frequency Distribution",
        col=c("green", "blue"), ylim=c(0,30)) # Alter color - Y scale
barplot(table(dat.df$Escolaridad),
        main="Escolaridad: Barplot Frequency Distribution",
        col=c("gray", "white"), ylim=c(0,120)) # Alter color - Y scale

par(ask=TRUE)
par(mfrow=c(1,2)) # 2 figures into a 1 row by 2 column grid
barplot(table(dat.df$MotivoConsulta),
        main="Motivo de Consulta: Barplot Frequency Distribution",
        col=c("green", "blue"), ylim=c(0,30)) # Alter color - Y scale
barplot(table(dat.df$TieneEnfermedadFisica),
        main="Presencia de Enfermedad Física: Barplot Frequency Distribution",
        col=c("gray", "white"), ylim=c(0,140)) # Alter color - Y scale

par(ask=TRUE)
par(mfrow=c(1,2)) # 2 figures into a 1 row by 2 column grid
barplot(table(dat.df$EnfermedadFisica),
        main="Enfermedad Física: Barplot Frequency Distribution",
        col=c("green", "blue"), ylim=c(0,120)) # Alter color - Y scale
barplot(table(dat.df$TieneEnfermedadMental),
        main="Presencia de Enfermedad Mental: Barplot Frequency Distribution",
        col=c("gray", "white"), ylim=c(0,130)) # Alter color - Y scale

par(ask=TRUE)
par(mfrow=c(1,2)) # 2 figures into a 1 row by 2 column grid
barplot(table(dat.df$EnfermedadMental),
        main="Enfermedad Mental: Barplot Frequency Distribution",
        col=c("green", "blue"), ylim=c(0,120)) # Alter color - Y scale
barplot(table(dat.df$IntentosPrevios),
        main="Intentos Previos: Barplot Frequency Distribution",
        col=c("gray", "white"), ylim=c(0,100)) # Alter color - Y scale

par(ask=TRUE)
par(mfrow=c(1,2)) # 2 figures into a 1 row by 2 column grid
barplot(table(dat.df$DeterminacionRiesgo),
        main="¿Se determinó el Riesgo?: Barplot Frequency Distribution",
        col=c("green", "blue"), ylim=c(0,120)) # Alter color - Y scale
barplot(table(dat.df$Riesgo),
        main="Riesgo: Barplot Frequency Distribution",
        col=c("gray", "white"), ylim=c(0,70)) # Alter color - Y scale

par(ask=TRUE)
par(mfrow=c(1,3)) # 2 figures into a 1 row by 2 column grid
barplot(table(dat.df$Premeditacion),
        main="Premeditación",
        col=c("green", "blue"), ylim=c(0,140)) # Alter color - Y scale
barplot(table(dat.df$Planificacion),
        main="Planificación",
        col=c("gray", "black"), ylim=c(0,140)) # Alter color - Y scale
barplot(table(dat.df$GestosDespedida),
        main="Gestos de Despedida",
        col=c("gray", "pink"), ylim=c(0,140)) # Alter color - Y scale

par(ask=TRUE)
par(mfrow=c(1,2)) # 2 figures into a 1 row by 2 column grid
barplot(table(dat.df$Metodo),
        main="Método empleado",
        col=c("green", "blue"), ylim=c(0,120)) # Alter color - Y scale
barplot(table(dat.df$Motivos),
        main="Riesgo: Barplot Frequency Distribution",
        col=c("gray", "white"), ylim=c(0,70)) # Alter color - Y scale

par(ask=TRUE)
par(mfrow=c(1,2)) # 2 figures into a 1 row by 2 column grid
barplot(table(dat.df$AltaMismoDia),
        main="Alta el Mismo día de ingreso",
        col=c("green", "blue"), ylim=c(0,140)) # Alter color - Y scale
barplot(table(dat.df$AltaVoluntaria),
        main="Alta Voluntaria",
        col=c("gray", "white"), ylim=c(0,140)) # Alter color - Y scale

par(ask=TRUE)
par(mfrow=c(1,2)) # 2 figures into a 1 row by 2 column grid
barplot(table(dat.df$FueHospitalizado),
        main="Requirió Hospitalización",
        col=c("green", "blue"), ylim=c(0,140)) # Alter color - Y scale
barplot(table(dat.df$PersistenciaIdeacion),
        main="Persistencia de la Ideación Suicida",
        col=c("gray", "white"), ylim=c(0,120)) # Alter color - Y scale

par(ask=TRUE)
par(mfrow=c(1,2)) # 2 figures into a 1 row by 2 column grid
barplot(table(dat.df$Remision),
        main="Remitido a Profesiona en Salud Mental",
        col=c("green", "blue"), ylim=c(0,140)) # Alter color - Y scale
barplot(table(dat.df$ProfesionalRemitido),
        main="Profesional al que fue remitido",
        col=c("gray", "white"), ylim=c(0,120)) # Alter color - Y scale

par(ask=TRUE)
par(mfrow=c(1,2)) # 2 figures into a 1 row by 2 column grid
barplot(table(dat.df$Lugar),
        main="Lugar del Hecho",
        col=c("green", "blue"), ylim=c(0,1500)) # Alter color - Y scale
barplot(table(dat.df$Subregistro),
        main="Subregistro del hecho",
        col=c("gray", "white"), ylim=c(0,120)) # Alter color - Y scale


# Section (two breakouts) by Lbs
# Generate the figure DensityFacetSectionLbs, but it will
# not show until using the gridExtra::grid.arrange()
# function.
DensityFacetSexoEdad <-
  ggplot2::ggplot(dat.df,
                  aes(x=Edad)) +
  geom_density(col="red", lwd=2) +
  facet_grid(. ~ Sexo) +
  ggtitle("Sexo por Edad (Años)\n") +
  labs(x = "\nEdad (Años)", y = "Density\n") +
  scale_x_continuous(labels=scales::comma, limits=c(10,60),
                     breaks=seq(10,60, by=25)) +
  theme_bw()

# Procedencia (two breakouts) por Edad
DensityFacetProcedenciaEdad <-
  ggplot2::ggplot(dat.df,
                  aes(x=Edad)) +
  geom_density(col="red", lwd=2) +
  facet_grid(. ~ Procedencia) +
  ggtitle("Procedencia por Edad (Años)\n") +
  labs(x = "\nEdad (Años)", y = "Density\n") +
  scale_x_continuous(labels=scales::comma, limits=c(10,60),
                     breaks=seq(10,60, by=25)) +
  theme_bw()
    
# Sexo (two breakouts) Por Edad
BoxplotSexoEdad <-
  ggplot(dat.df,
         aes(x=Sexo, y=Edad, fill=Sexo)) +
  geom_boxplot() +
  ggtitle("Sexo por Edad (Días)\n") +
  labs(x = "\nSexo", y = "Edad (Años)\n") +
  scale_y_continuous(labels=scales::comma, limits=c(10,50),
                     breaks=seq(10,50, by=25)) +
  theme_bw()

# Procedencia (two breakouts) por Edad
BoxplotProcedenciaEdad <-
  ggplot(dat.df,
         aes(x=Procedencia, y=Edad, fill=Procedencia)) +
  geom_boxplot() +
  ggtitle("Procedencia por Edad (Años)\n") +
  labs(x = "\nProcedencia", y = "Edad (Años)\n") +
  scale_y_continuous(labels=scales::comma, limits=c(10,50),
                     breaks=seq(10,50, by=25)) +
  theme_bw()

gridExtra::grid.arrange(
  DensityFacetSexoEdad,
  DensityFacetProcedenciaEdad,
  BoxplotSexoEdad,
  BoxplotProcedenciaEdad, ncol=2)




#######################################################################
####### Descriptive Statistics for Initial Analysis of the Data #######
#######################################################################

# Length or N of a vector
length(dat.df$Estrato)

# Returns TRUE if indexed value is missing (e.g., NA) and
# FALSE if indexed value is not missing
table(is.na(dat.df$Estrato))

# Returns TRUE if indexed value is not missing (e.g., NA)
# and FALSE if indexed value is missing
table(complete.cases(dat.df$Estrato))

# Descriptive statistics, including NAs if any
summary(dat.df$Estrato)

# Mean or arithmetic average
mean(dat.df$Estrato, na.rm=TRUE)

# Standard Deviation
sd(dat.df$Estrato, na.rm=TRUE)

# Variance
var(dat.df$Estrato, na.rm=TRUE)

# Median or midpoint
median(dat.df$Estrato, na.rm=TRUE)

install.packages("modes", dependencies=TRUE)
library(modes) # Load the modes package.
elp(package=modes) # Show the information page.
sessionInfo() # Confirm all attached packages.

# Mode, or the most frequent value
# Note how this distribution is bimodal
modes::modes(dat.df$Estrato, type=1)

# Range, minimum and maximum
range(dat.df$Estrato, na.rm=TRUE)

# Minimum
min(dat.df$Estrato, na.rm = TRUE)

# Location (e.g., index) of the first occurrence of the
# minimum value
which.min(dat.df$Estrato)

# Maximun
max(dat.df$Estrato, na.rm = TRUE)

# Location (e.g., index) of the first occurrence of the
# maximum value
which.max(dat.df$Estrato)

# Quantiles, or values at: 0%, 25%, 50% 75%, and 100%
quantile(dat.df$Estrato, na.rm=TRUE)

# Arithmetic sum of all values in a vector
sum(dat.df$Estrato, na.rm = TRUE)

# Produce values for a vector related to a boxplot:
# lower whisker, lower hinge, median, upper hinge, upper
# whisker, N, and outliers
boxplot.stats(dat.df$Estrato)

# Tukey’s five number summary for a vector: minimum,
# lower-hinge, median, upper-hinge, and maximum
fivenum(dat.df$Estrato, na.rm=TRUE)

# Interquartile range of a vector (e.g., a measure of
# dispersion that is equal to the difference between the
# upper quartile and the lower quartile
IQR(dat.df$Estrato, na.rm=TRUE)

# Value-by-value contingency table (e.g., crosstab) of
# counts for each combination of numeric object variable
# values v factor levels (e.g., Weight (rows) by Section
# (columns)
table(dat.df$Estrato, dat.df$Sexo)

# Value-by-value contingency table (e.g., crosstab) of
# counts for each combination of numeric object variable
# values v factor levels (e.g., Weight (rows) by Gender
# (columns)
table(dat.df$Estrato, dat.df$Procedencia)

# Contingency table of cell sums, not individual values
# Observe Row (e.g., Gender) by Column (e.g., Section)
# placement and also note how there are no missing data
# for either Gender or Section (only for Lbs).
table(dat.df$Sexo, dat.df$Procedencia,
      useNA=c("always"))

# Contingency table of cell sums, not individual values
# Observe Row (e.g., Section) by Column (e.g., Gender)
# placement and also note how there are no missing data
# for either Section or Gender (only for Lbs).
table(dat.df$Procedencia, dat.df$Sexo,
      useNA=c("always"))

# Proportions for each breakout group, Row by Column,
# which cell-by-cell adds to 100 percent
prop.table(table(dat.df$Procedencia,
                 dat.df$Sexo, useNA=c("always")))

# Proportions for each breakout group, Row by Column,
# which cell-by-cell adds to 100 percent
prop.table(table(dat.df$Sexo,
                 dat.df$Procedencia, useNA=c("always")))

xtabs(~Procedencia+Sexo, data=dat.df)

# Common to many uses with R, note how the ftable()
# function is wrapped around the xtabs() function.
ftable(xtabs(~Procedencia+Sexo, data=dat.df))

install.packages("RcmdrMisc", dependencies=TRUE)
library(RcmdrMisc) # Load the RcmdrMisc package.
help(package=RcmdrMisc) # Show the information page.
sessionInfo() # Confirm all attached packages.

# Descriptive statistics overall, no breakout groupings
RcmdrMisc::numSummary(dat.df$Estrato)

##Higlight##
RcmdrMisc::numSummary(dat.df[,c("Estrato")],
                      groups=Procedencia) # Default printout, breakouts by Section

RcmdrMisc::numSummary(dat.df[,c("Estrato")],
                      groups=Sexo) # Default printout, breakouts by Gender

# Specialized External Packages and Functions

install.packages("asbio")
library(asbio) # Load the asbio package.
help(package=asbio) # Show the information page.
sessionInfo() # Confirm all attached packages.

asbio::Mode(dat.df$Edad)
# The modes::modes() function was demonstrated previously.
# The asbio::Mode() function is merely another way to obtain
# this measure of central tendency.

install.packages("epiDisplay")
library(epiDisplay) # Load the epiDisplay package.
help(package=epiDisplay) # Show the information page.
sessionInfo() # Confirm all attached packages.

# Generate frequency distributions and percentages
# of by breakouts. Generate a figure of frequency
# distributions.
par(ask=TRUE)
par(mfrow=c(1,2)) # 2 figures into a 1 row by 2 column grid
epiDisplay::tab1(dat.df$Sexo,
                 main="Frecuencia Dis. Procedencia",
                 col=c("red", "blue"), font.lab=2, font.axis=2)
epiDisplay::tab1(dat.df$Procedencia,
                 main="Frecuencia Dis Sexo",
                 col=c("red", "blue"), font.lab=2, font.axis=2)

# Along with the figure that appears as a stacked bar
# chart, look at the detailed frequency distribution
# statistics printed to the screen, which can be
# copied and pasted into a word-processed document.
par(ask=TRUE)
par(mfrow=c(1,2)) # 2 figures into a 1 row by 2 column grid
epiDisplay::tabpct(dat.df$Procedencia,
                   dat.df$Sexo, graph=TRUE, decimal=1,
                   main="Frecuencia de la distribución de la Procedencia por Sexo/n",
                   xlab = "Procedencia", ylab = "Sexo", cex.axis=1,
                   percent= "both", las=1, col=c("red", "blue"))
epiDisplay::tabpct(dat.df$Sexo,
                   dat.df$Procedencia, graph=TRUE, decimal=1,
                   main="Frecuencia de la Distribución del Sexo por la Procedencia",
                   xlab = "Sexo", ylab = "Procedencia", cex.axis=1,
                   percent= "both", las=1, col=c("red", "blue"))                   


# Produce a sorted dotplot and accompanying descriptive
# statistics: by=NULL (e.g., overall), by=Section, and
# by=Gender. Do not confuse a sorted dotplot with a QQ plot.
# The two figures represent different constructs.
par(ask=TRUE)
par(mfrow=c(1,3)) # 3 figures into a 1 row by 3 column grid
epiDisplay::summ(dat.df$Edad,
                 by=NULL, # No breakout statistics
                 graph=TRUE, box=TRUE, # Dotplot and boxplot
                 pch=20, ylab="auto",
                 main="Sorted Dotplot of Edad (Años), Overall",
                 cex.X.axis=1.25, # Note X axis label size.
                 cex.Y.axis=1.25, # Note Y axis label size.
                 font.lab=2, dot.col="auto")
epiDisplay::summ(dat.df$Edad,
                 by=Procedencia, # Breakout statistics
                 graph=TRUE, # Dotplot
                 pch=20, ylab="auto",
                 main="Sorted Dotplot of Edad (Años) por Procedencia",
                 cex.X.axis=1.25, # Note X axis label size.
                 cex.Y.axis=1.25, # Note Y axis label size.
                 font.lab=2, dot.col="auto")
epiDisplay::summ(dat.df$Edad,
                 by=Sexo, # Breakout statistics
                 graph=TRUE, # Dotplot
                 pch=20, ylab="auto",
                 main="Sorted Dotplot of Edad (Años) por Sexo",
                 cex.X.axis=1.25, # Note X axis label size.
                 cex.Y.axis=1.25, # Note Y axis label size.
                 font.lab=2, dot.col="auto")


install.packages("s20x", dependencies=TRUE)
library(s20x) # Load the s20x package.
help(package=s20x) # Show the information page.
sessionInfo() # Confirm all attached packages.

s20x::summaryStats(dat.df$Estrato,
                   na.rm=TRUE) # Accommodate missing values.

s20x::summaryStats(Estrato ~ Etnia, dat.df,
                   na.rm=TRUE) # Accommodate missing values

s20x::summaryStats(Estrato ~ EstadoCivil, dat.df,
                   na.rm=TRUE) # Accommodate missing values

par(ask=TRUE); s20x::rowdistr(crosstabs(~ Sexo + Edad,
                                        data=dat.df), plot=TRUE, suppressText=FALSE,
                              comp=’basic’)

par(ask=TRUE); s20x::rowdistr(crosstabs(~ Procedencia + Edad,
                                        data=dat.df), plot=TRUE, suppressText=FALSE,
                              comp=’basic’)

####################################################################
                                                                   #
install.packages("arsenal", dependencies=TRUE)                     #
library(arsenal) # Load the arsenal package.                       #
help(package=arsenal) # Show the information page.                 #
sessionInfo() # Confirm all attached packages.                     #
                                                                   #
# Produce a highly-detailed table of descriptive                   #  
# statistics, especially: mean, sd, and range. GIVES P VALUE       #
summary(arsenal::tableby(list(AltaMismoDia, Sexo) ~ NumeroIntentosPrevios,  #
                         data = dat.df), text=TRUE, total=TRUE)    #
                                                                   #
####################################################################

install.packages("pivottabler", dependencies=TRUE)
library(pivottabler) # Load the pivottabler package.
help(package=pivottabler) # Show the information page.
sessionInfo() # Confirm all attached packages.

pivottabler::qpvt(CPIIISecLbsGen.df, "Gender", "Section",
                  c("Mean Lbs"="mean(Lbs, na.rm=TRUE)",
                    "SD Lbs"="sd(Lbs, na.rm=TRUE)"),
                  formats=list("%.0f", "%.1f"))
# Row (Gender) by Column (Section) in text forma

pivottabler::qpvt(CPIIISecLbsGen.df, "Section", "Gender",
                  c("Median Lbs"="median(Lbs, na.rm=TRUE)"),
                  formats=list("%.0f", "%.1f"))
# Row (Section) by Column (Gender) in text format
# Note how the header Median Lbs does not show in output.

# summarize manypackages gained from when R is first downloaded by keying:
base::getOption("defaultPackages")  

##################################
# Descriptive Statistics of Edad #
##################################

# Summary of all object variables
base::summary(dat.df$Edad)

modes::modes(dat.df$Edad)
stats::median(dat.df$Edad)
base::mean(dat.df$Edad)
base::mean(dat.df$Edad, trim = .1)

# Trimmed mean, or the arithmetic average after
# removing 5 percent (i.e., trim=0.05) of the
# highest and lowest values
base::mean(dat.df$Edad, trim=0.05)

install.packages("psych", dependencies=TRUE)
library(psych) # Load the psych package.
help(package=psych) # Show the information page.
sessionInfo() # Confirm all attached packages.

# Geometric mean, used to limit the impact of
# extreme values
psych::geometric.mean(dat.df$Edad)

# Harmonic mean, used to address the impact of
# outliers
psych::harmonic.mean(dat.df$Edad)

# Winsorized mean, where data at the ends are not
# so much trimmed as they are replaced with
# values that provide a robust estimate of
# central tendency, accommodating the potential
# undue influence of outliers
psych::winsor.mean(dat.df$Edad, trim=0.05)


stats::var(dat.df$Edad) # Variance

stats::sd(dat.df$Edad) # Standard deviation

base::min(dat.df$Edad) # Minimum value

base::which.min(dat.df$Edad) # Minimum value location (e.g., row number)

base::max(dat.df$Edad) # Maximum value

base::which.max(dat.df$Edad) # Maximum value location (i.e., row number)

base::range(dat.df$Edad) # Range of values, minimum to maximum

base::length(dat.df$Edad) # Number of occurrences (e.g., N, datapoints)

# First few datapoints of SBPNormal, sorted
# The head() function is wrapped around the
# sort() function.
utils::head(base::sort(dat.df$Edad))

utils::tail(base::sort(dat.df$Edad)) # Last few datapoints of SBPNormal, sorted

base::sum(dat.df$Edad) # Sum of all values

stats::quantile(dat.df$Edad) # Quantile scores, 0% 25% 50% 75% 100%

# Use of stats::quantile() function to produce
# deciles, 0% 10% 20% 30% 40% 50% 60% 70% 80% 90%
# 100%
stats::quantile(dat.df$Edad,
                prob=seq(0, 1, length=11), type=5)


# Interquartile range, or a measure of dispersion
# between the 3rd quartile and the 1st quartile
stats::IQR(dat.df$Edad)

# Median Absolute Deviation (MAD), or the median
# of the absolute deviations from the median
# (compare the MAD statistic to the sd statistic)
stats::mad(dat.df$Edad)

# Boxplot Statistics: Lower-Whisker, Lower-Hinge,
# Median, Upper-Hinge, and Upper-Whisker, N, and
# Outliers
# The boxplot.stats() function is included in the
# grDevices package, which is available when R is
# first downloaded.
# Outlies and descriptive statistics are printed.
grDevices::boxplot.stats(dat.df$Edad)

# Tukey’s Five-Number Summary: Minimum,
# Lower-Hinge, Median, Upper-Hinge, and Maximum
stats::fivenum(dat.df$Edad)

install.packages("doBy", dependencies=TRUE)
library(doBy) # Load the doBy package.
help(package=doBy) # Show the information page.
sessionInfo() # Confirm all attached packages.

doBy::descStat(Edad) #Note only put Edad dued to dat.df is already attached

install.packages("tables", dependencies=TRUE)
library(tables) # Load the tables package.
help(package=tables) # Show the information page.
sessionInfo() # Confirm all attached packages.

# It is also possible to generate descriptive
# statistics for breakouts of factor-type object
# variables.
tables::tabular((Edad) ~ (n=1) + Format(digits=2)*
                  (min + mode + mean + sd + median + IQR + mad + var + max))

# Select the most local mirror site using Set CRAN mirror.
install.packages("pastecs", dependencies=TRUE)
library(pastecs) # Load the pastecs package.
help(package=pastecs) # Show the information page.
sessionInfo() # Confirm all attached packages.

# Additional descriptive statistics are available
pastecs::stat.desc(Edad,
                   basic=FALSE, desc=FALSE, norm=FALSE)

# Additional descriptive statistics are available
psych::describe(Edad, fast=TRUE)

install.packages("furniture", dependencies=TRUE)
library(furniture) # Load the furniture package.
help(package=furniture) # Show the information page.
sessionInfo() # Confirm all attached packages.

# Wrap the base::data.frame() function around the
# numeric object variable SBPNormal to temporarily
# coerce it into format as a dataframe to
# accommodate requirements for use of the
# furniture::table1() function.
furniture::table1((base::data.frame(Edad, Estrato, NumeroIntentosPrevios, EstanciaHospitalaria)))

## Table 1: TOCA SEGUIR PROBANDO, PORQUE NO FUNCIONÓ.
data %>%
  table1(Sexo, 
         splitby = ~groupvar,
         test = TRUE)

# Additional descriptive statistics are available
RcmdrMisc::numSummary(Edad,
                      statistics=c("mean", "sd", "quantiles"))

# Along with descriptive statistics, the graph=TRUE
# argument produces a sorted dot chart.
epiDisplay::summ(Edad, graph=TRUE)

#######################################################################
#### Quality Assurance, Data Distribution, and Tests forNormality #####
#######################################################################

shapiro.test(dat.df$Edad) #What a Beauty!

install.packages("RVAideMemoire", dependencies=TRUE)
library(RVAideMemoire) # Load the RVAideMemoire package.
help(package=RVAideMemoire) # Show the information page.
sessionInfo() # Confirm all attached packages.

# Normality test of Lbs overall
RVAideMemoire::mshapiro.test(dat.df$Edad)

# Normality test of Edad by Procedencia
RVAideMemoire::byf.shapiro(Edad ~ Procedencia,
                           data=dat.df)

# Normality test of Edad by Sexo
RVAideMemoire::byf.shapiro(Edad ~ Sexo,
                           data=dat.df)

# Edad (Años), Overall
QQAños <-
  ggplot2::ggplot(dat.df,
                  aes(sample=Edad)) +
  stat_qq(color="red") +
  stat_qq_line(color="blue", linewidth=1.75) +
  ggtitle("
Edad (Años) QQ-Plot and QQ-Line,\nOverall\n") +
  labs(x = "\nTheoretical", y = "Edad (Años)\n") +
  theme_bw()

# Edad (Años) by Procedencia (two breakouts)
QQFacetEdadProcedencia <-
  ggplot2::ggplot(dat.df,
                  aes(sample=Edad)) +
  stat_qq(color="red") +
  stat_qq_line(color="blue", linewidth=1.75) +
  facet_grid(. ~ Procedencia) +
  ggtitle("
Edad (Años) QQ-Plot and QQ-Line,\nby Procedencia\n") +
  labs(x = "\nTheoretical", y = "Edad (Años)\n") +
  theme_bw()

# Edad (Años) by Sexo (two breakouts)
QQFacetEdadSexo <-
  ggplot2::ggplot(dat.df,
                  aes(sample=Edad)) +
  stat_qq(color="red") +
  stat_qq_line(color="blue", size=1.75) +
  facet_grid(. ~ Sexo) +
  ggtitle("
Edad (Años) QQ-Plot and QQ-Line,\nby Sexo\n") +
  labs(x = "\nTheoretical", y = "Edad (Años)\n") +
  theme_bw()

gridExtra::grid.arrange(
  QQAños,
  QQFacetEdadProcedencia,
  QQFacetEdadSexo, ncol=3)


s20x::summaryStats(Edad) #Gives Skewness
s20x::summaryStats(NumeroIntentosPrevios, na.rm=TRUE)  #Gives Skewness

utils::head(base::sort(Edad))
utils::head(base::sort(NumeroIntentosPrevios))

utils::tail(base::sort(Edad))
utils::tail(base::sort(NumeroIntentosPrevios))

install.packages("UsingR", dependencies=TRUE)
library(UsingR) # Load the UsingR package.
help(package=UsingR) # Show the information page.
sessionInfo() # Confirm all attached packages.

par(ask=TRUE) # Pause
par(mfrow=c(2,2)) # 4 figures - 2 rows by 2 column grid
UsingR::simple.violinplot(Edad,
                          lty=4, lwd=2, col="red", ylim=c(0,60))
title("Edad")
UsingR::simple.violinplot(NumeroIntentosPrevios, na.rm=TRUE,
                          lty=4, lwd=2, col="red", ylim=c(0,16))
title("Numero de Intentos Previos")
epiDisplay::dotplot(Edad, dot.col="red",
                    main="Edad")
epiDisplay::dotplot(NumeroIntentosPrevios, dot.col="red",
                    main="Numero de Intentos Previos")

install.packages("nortest", dependencies=TRUE)
library(nortest) # Load the nortest package.
help(package=nortest) # Show the information page.
sessionInfo() # Confirm all attached packages.

nortest::ad.test(Edad) #The Anderson-Darling Test for Normal Dist.




#######################################################################
#################### Practicing Data Exploration ######################
#######################################################################

# The base::set.seed() function is commonly used
# immediately before any attempt to generate random
# numbers. Some numerical value is then used along
# with this function in an effort to set the seed
# and in turn produce a specific sequence of random
# numbers. For this addendum, the number 8 was used
# to set the seed. There is nothing special about
# the number 8 being used to set the seed and the
# sequence of random numbers generated because of
# this selection. It would have been possible to
# generate another set of random numbers by using
# 1234 or any other number to set the seed. Some
# number had to be selected and for this addendum
# the number 8 was used to set the seed. If this
# number were used by others the same set of random
# numbers would be generated, allowing reproduction
# of results in the future and/or by others.

# Set the seed
base::set.seed(8)

# Data with Normal Distribution Patterns
# Approximately +3 and -3 SDs for SBP with 120 mean,
# when data show normal distribution.
# Use the stats::rnorm() function to generate random
# numbers.
# Create a set of 100,000 random numbers that
# exhibits normal distribution, with mean = 120 and
# standard deviation = 06. To allow for some degree
# of familiarity, this distribution (mean = 120 and
# sd = 06) is equivalent to common metrics for
# Systolic Blood Pressure, SBP
SBPNormal <- stats::rnorm(100000, mean=120, sd=06)

# Data That Do Not Exhibit Normal Distribution Patterns
# Approximately +3 and -3 SDs for SBP with 120 mean,
# when data show normal distribution.
# Use the stats::runif() function to generate random
# numbers.
# Create a set of 100,000 random numbers that does
# not exhibit normal distribution. For this set of
# random numbers, the minimum value is set to 102
# and the maximum value is set to 138, which model
# to a large degree the two extreme values for SBP
# readings (mean = 120 and standard deviation = 06)
# at 3 SDs (standard deviations).
SBPNotNormal <- stats::runif(100000, min=102, max=138)

##############################################
# Code Book for SBPNormal and SBPNotNormal   #
##############################################
#                                            #
# SBPNormal ........................ Numeric #
# 100,000 SBP readings with mean =           #
# 120 and sd = 06                            #
#                                            #
# SBPNotNormal ......................Numeric #
# 100,000 SBP readings with minimum =        #
# 102 and maximum = 138                      #
##############################################

base::getwd() # Confirm working directory
base::ls() # Confirm available objects

utils::str(SBPNormal) # Identify structure
utils::head(SBPNormal, n=10) # Show the head, first 10 cases
utils::tail(SBPNormal, n=10) # Show the tail, last 10 cases
base::summary(SBPNormal) # Summary statistics

utils::str(SBPNotNormal) # Identify structure
utils::head(SBPNotNormal, n=10) # Show the head, first 10 cases
utils::tail(SBPNotNormal, n=10) # Show the tail, last 10 cases
base::summary(SBPNotNormal) # Summary statistics

# Place four separate figures (e.g., histogram, density
# plot, boxplot, and a Q-Q plot with an accompanying Q-Q
# line) into one common figure.
#
# Note how one function can be wrapped around another
# function. In this example, the graphics::plot() function
# has been wrapped around the stats::density() function. A
# requirement for the stats::density() function is that the
# na.rm=TRUE argument must be used.

par(ask=TRUE) # Pause
par(mfrow=c(2,2)) # 4 figures - 2 rows by 2 column grid
graphics::hist(SBPNormal)
graphics::plot(stats::density(SBPNormal, na.rm=TRUE))
graphics::boxplot(SBPNormal)
stats::qqnorm(SBPNormal); stats::qqline(SBPNormal)

# Place four separate figures (e.g., histogram, density
# plot, boxplot, and a Q-Q plot with an accompanying Q-Q
# line) into one common figure.
par(ask=TRUE) # Pause
par(mfrow=c(2,2)) # 4 figures - 2 rows by 2 column grid
graphics::hist(SBPNotNormal)
graphics::plot(stats::density(SBPNotNormal, na.rm=TRUE))
graphics::boxplot(SBPNotNormal)
stats::qqnorm(SBPNotNormal); stats::qqline(SBPNotNormal)

# Histogram
# Notice how both histograms have the same X axis scale
# and Y axis scale, allowing meaningful side-by-side
# comparisons.
par(ask=TRUE) # Pause
par(mfrow=c(1,2)) # 2 figures - 1 row by 2 column grid
graphics::hist(SBPNormal,
               main="SBP - Normal Distribution",
               col="red", # Add color
               breaks=50, # Increase granularity of histogram
               font.lab=2, # Bold labels
               xlim=c(0,200), # X axis scale
               ylim=c(0,7000)) # Y axis scale
axis(side=1, font=2) # X axis bold
axis(side=2, font=2) # Y axis bold
graphics::hist(SBPNotNormal,
               main="SBP - Not Normal Distribution",
               col="red", # Add color
               breaks=50, # Increase granularity of histogram
               font.lab=2, # Bold labels
               xlim=c(0,200), # X axis scale
               ylim=c(0,7000)) # Y axis scale
axis(side=1, font=2) # X axis bold
axis(side=2, font=2) # Y axis bold


# Density Plot
# Notice how both density plots have the same X axis
# scale and Y axis scale, allowing meaningful side-by-
# side comparisons.
par(ask=TRUE) # Pause
par(mfrow=c(1,2)) # 2 figures - 1 row by 2 column grid
graphics::plot(stats::density(SBPNormal, na.rm=TRUE),
               main="SBP - Normal Distribution",
               col="red", # Add color
               lwd=5, # Thick line
               font.lab=2, # Bold labels
               xlim=c(0,200), # X axis scale
               ylim=c(0,0.08)) # Y axis scale
axis(side=1, font=2) # X axis bold
axis(side=2, font=2) # Y axis bold
graphics::plot(stats::density(SBPNotNormal, na.rm=TRUE),
               main="SBP - Not Normal Distribution",
               col="red", # Add color
               lwd=5, # Thick line
               font.lab=2, # Bold labels
               xlim=c(0,200), # X axis scale
               ylim=c(0,0.08)) # Y axis scale
axis(side=1, font=2) # X axis bold
axis(side=2, font=2) # Y axis bold


# Boxplot
# Notice how both boxplots have the same Y axis scale,
# allowing meaningful side-by-side comparisons.
par(ask=TRUE) # Pause
par(mfrow=c(1,2)) # 2 figures - 1 row by 2 column grid
graphics::boxplot(SBPNormal,
                  main="SBP - Normal Distribution",
                  xlab="Boxplot", # X axis label
                  ylab="SBP", # Y axis label
                  cex.axis=1.15, # Axis size
                  cex.lab=1.15, # Label size
                  col="red", # Box color
                  lwd=2, # Line thickness
                  font.lab=2, # Bold labels
                  font=2, # Bold font
                  ylim=c(0,200)) # Y axis scale
graphics::boxplot(SBPNotNormal,
                  main="SBP - Not Normal Distribution",
                  xlab="Boxplot", # X axis label
                  ylab="SBP", # Y axis label
                  cex.axis=1.15, # Axis size
                  cex.lab=1.15, # Label size
                  col="red", # Box color
                  lwd=2, # Line thickness
                  font.lab=2, # Bold labels
                  font=2, # Bold font
                  ylim=c(0,200)) # Y axis scale

# Q-Q Plot
# Notice how both Q-Q plots have the same X axis scale
# and Y axis scale, allowing meaningful side-by-side
# comparisons.
par(ask=TRUE) # Pause
par(mfrow=c(1,2)) # 2 figures - 1 row by 2 column grid
stats::qqnorm(SBPNormal,
              main="Q-Q Plot (Blue) and Q-Q Line (Red) of SBP -
Normal Distribution",
              col="blue", xlim=c(-4,4), ylim=c(0,200), font.axis=2,
              font.lab=2)
stats::qqline(SBPNormal, # Add a Q-Q Line to the Q-Q Plot
              col="red", lwd=4, lty=2)
stats::qqnorm(SBPNotNormal,
              main="Q-Q Plot (Blue) and Q-Q Line (Red) of SBP -
Not Normal Distribution",
              col="blue", xlim=c(-4,4), ylim=c(0,200), font.axis=2,
              font.lab=2)
stats::qqline(SBPNotNormal,# Add a Q-Q Line to the Q-Q Plot
              col="red", lwd=4, lty=2)


#######################################################################
##### Descriptive Statistics of Singular Numeric Object Variables #####
#######################################################################


summary(Edad); sd(Edad)

# Create Individual Histograms Placed in a Common Figure

# Notice how the X axis and the Y axis were suppressed
# using the xaxt="n" and yaxt="n" arguments. The X axis
# and Y axis were then brought back with desired changes
# to the scale and presentation:
# side=1 X axis
# side=2 Y axis
# at=seq(min,max,break) Placement of tick marks
# font=2 Bold
# las Orientation
par(ask=TRUE) # Pause
par(mfrow=c(2,2)) # 4 figures - 2 row by 2 column grid
hist(
  Edad,
  col="red", # Add color
  breaks=20, # Increase granularity of histogram
  xaxt="n", # Suppress X axis for later manipulation
  xlab="Edad", # X axis label
  yaxt="n", # Suppress Y axis for later manipulation
  ylab="N", # Y axis label
  font.lab=2, # Bold labels
  xlim=c(10,60), # X axis scale
  ylim=c(0,30) # Y axis scale
)
axis(side=1, at=seq(10,60,5), font=2) # X axis
axis(side=2, at=seq(0,30,5), font=2, las=3)# Y axis

hist(
  Estrato,
  col="gray", # Add color
  breaks=5, # Increase granularity of histogram
  xaxt="n", # Suppress X axis for later manipulation
  xlab="Estrato", # X axis label
  yaxt="n", # Suppress Y axis for later manipulation
  ylab="N", # Y axis label
  font.lab=2, # Bold labels
  xlim=c(1,5), # X axis scale
  ylim=c(0,120) # Y axis scale
)
axis(side=1, at=seq(0,5,1), font=2) # X axis
axis(side=2, at=seq(0,120,20), font=2, las=3)# Y axis

hist(
  NumeroIntentosPrevios,
  col="green", # Add color
  breaks=5, # Increase granularity of histogram
  xaxt="n", # Suppress X axis for later manipulation
  xlab="Número de Intentos Previos", # X axis label
  yaxt="n", # Suppress Y axis for later manipulation
  ylab="N", # Y axis label
  font.lab=2, # Bold labels
  xlim=c(0,5), # X axis scale
  ylim=c(0,150) # Y axis scale
)
axis(side=1, at=seq(0,7,1), font=2) # X axis
axis(side=2, at=seq(0,125,25), font=2, las=3)# Y axis

hist(
  EstanciaHospitalaria,
  col="blue", # Add color
  breaks=10, # Increase granularity of histogram
  xaxt="n", # Suppress X axis for later manipulation
  xlab="Estancia Hospitalaria (Días)", # X axis label
  yaxt="n", # Suppress Y axis for later manipulation
  ylab="N", # Y axis label
  font.lab=2, # Bold labels
  xlim=c(0,35), # X axis scale
  ylim=c(0,150) # Y axis scale
)
axis(side=1, at=seq(0,35,5), font=2) # X axis
axis(side=2, at=seq(0,150,25), font=2, las=3)# Y axis


#######################################################################
#### Descriptive Statistics of Multiple Numeric and Factor Object #####
###################### Variables in a Dataframe #######################
#######################################################################

# Prepare Descriptive Statistics at the Summary Level
# and by Breakouts

# Wrap the base::data.frame() function around the
# numeric object variable SoilCornYield.df$BUperAcre
# to temporarily coerce it into format as a dataframe
# to accommodate requirements for use of the
# furniture::table1() function.
furniture::table1((base::data.frame(
  Edad)))

# Breakout Statistics by Factor-Type Object Variables

# NICE TABLE Examine BUperAcre (corn yield) by Soil (Clay, Sand, Silt)
# and calculate p-value to examine statistical significance.
# Note use of the tilde (e.g., ~) character.
furniture::table1(dat.df, Edad, Procedencia, TieneEnfermedadFisica, TieneEnfermedadMental, IntentosPrevios, DeterminacionRiesgo, Riesgo, Premeditacion, Planificacion, GestosDespedida, AltaMismoDia, AltaVoluntaria, FueRemitido, Lugar, Subregistro,
                  splitby=~Sexo,
                  digits=2,
                  test=TRUE,
                  format_number = TRUE,
                  na.rm = TRUE,
                  output = "markdown")

furniture::table1((base::data.frame(Edad, Sexo, Procedencia)),
                  splitby=NULL,# Here put a variable to do bivariate
                  FUN = NULL,
                  FUN2 = TRUE,
                  total = NULL,
                  second = NULL,
                  row_wise = FALSE,
                  test = FALSE,
                  param = TRUE,
                  header_labels = NULL,
                  type = "pvalues",
                  output = "markdown",
                  rounding_perc = 1,
                  digits = 2,
                  var_names = c("Edad (Años)", "Sexo", "Procedencia"),
                  format_number = TRUE,
                  NAkeep = TRUE,
                  na.rm = FALSE,
                  booktabs = TRUE,
                  caption = NULL,
                  align = NULL,
                  float = "ht",
                  export = "Tabla1",
                  label = NULL)




savelwd <- par(lwd=1) # Line thickness
savefont <- par(font=2) # Bold text
savefontaxis <- par(font.axis=2) # Bold axis
savecexlab <- par(cex.lab=1.15) # Label size
savecexaxis <- par(cex.axis=0.95) # Axis size

par(ask=TRUE)
plot(as.factor(Estrato), Edad, data=dat.df,
     main="Edad (Años) por Estrato",
     col="gold", xlab="Estrato", ylim=c(0,60),
     ylab="Edad")

par(ask=TRUE)
plot(Procedencia, Estrato, data=dat.df,
     main="Sexo por Edad",
     col="gold", xlab="Sexo", xlim=c(0,4), ylim=c(0,60),
     ylab="Edad por sexo")

# Notice how global settings were altered by using the
# par() function and then set back to default (e.g.,
# original) format by using the par() function again.
par(savelwd) # Return to default setting
par(savefont) # Return to default setting
par(savefontaxis) # Return to default setting
par(savecexlab) # Return to default setting
par(savecexaxis) # Return to default setting

# The base::summary() function is usually the best first
# choice to generate descriptive statistics of a numeric
# object variable.
base::summary(Edad)

# The base::table() function is a typical first selection
# to gain a sense of frequency distributions of a factor
# object variable.
base::table(Sexo)

base::getwd() # ID working directory
base::ls() # List objects
base::attach(dat.df) # Attach the data
utils::str(dat.df) # Identify structure
utils::head(dat.df, n=3) # Show the head
base::summary(dat.df) # Summary statistics

base::table(dat.df$Sexo)
base::table(dat.df$Procedencia)

# Adjust the axis values to accommodate the figure.
# Place the bars in either horizontal or vertical
# orientation, depending on the number of letters needed
# to display labels.
graphics::barplot(base::table(
  dat.df$Sexo),
  main="Sexo de los Casos de Intento de Suicidio HSLV 2019",
  horiz=FALSE, # Vertical bars
  las=1, # Horizontal axis labels
  col="red", # Color
  xlab="Sexo", # X axis label
  ylab="Número de Casos", # X axis label
  ylim=c(0,120), # X axis scale
  cex.axis=1.25, # Axis size
  cex.lab=1.25) # Label size

# Put the bar plot into horizontal orientation.
# Temporarily adjust the margins, generate the figure,
# and then toggle back to original margins.


# [1] 5.1 4.1 4.1 2.1
par("mar") # Confirm default margin (BLTR)

# BLTR - Bottom, Left, Top, Right
par(mar=c(5, 8, 4, 2) + 0.1) # Set a new margin
par(ask=TRUE) # Control the screen
graphics::barplot(base::table(
  dat.df$Ocupacion),
  main="Distribución de la Ocupación",
  horiz=TRUE, # Horizontal bars
  las=1, # Horizontal axis labels
  col="red", # Color
  xlab="Número de Casos", # X axis label
  xlim=c(0,100), # X axis scale
  cex.axis=1.25, # Axis size
  cex.lab=1.25) # Label size


# BLTR - Bottom, Left, Top, Right
par(mar=c(5.1, 4.1, 4.1, 2.1))# Toggle back to default margin

par("mar") # Confirm default margin (BLTR)


#ESTA TABLA RESUME CASI TODO
# Column 2 = Gender2
# Column 3 = RaceEthnic2
epiDisplay::tableStack(1:33,
                       dataFrame=dat.df,
                       by="none", count=TRUE, decimal=2,
                       percent=c("column", "row"))

#####ESTA FUE UN INTENTO MÍO DE USAR MÁS OPCIONES, PERO NO SE PUDO; AÚN...
epiDisplay::tableStack(1:10,
                       dataFrame=dat.df,
                       by="none", count=TRUE, decimal=2,
                       vars.to.factor = Estrato,
                       medians = TRUE,
                       test = TRUE,
                       assumption.p.value = TRUE,
                       percent=c("column", "row"))


epiDisplay::tableStack(
  vars=Procedencia, # Rows
  dataFrame=dat.df,
  by=Sexo, count=TRUE, decimal=2, # Columns
  percent=c("column", "row"),
  frequency=TRUE, name.test=TRUE,
  total.column=TRUE, test=TRUE)

# Generate frequency distributions and percentages
# of Gender2 breakouts: Female and Male. Then,
# generate an accompanying figure of frequency
# distributions
par(ask=TRUE)
epiDisplay::tab1(dat.df$Sexo,
                 main="Distribución del Sexo", # Title
                 col=c("red", "darkred"), # Color
                 font.lab=2, # Bold
                 font.axis=2) # Bold

# Generate frequency distributions and percentages
# of RaceEthnic2 breakouts: African-American,
# Hispanic, Other, and White. Then, generate an
# accompanying figure of frequency distributions.
# Note how the bar.values argument was used to show
par(ask=TRUE)
epiDisplay::tab1(dat.df$Ocupacion,
                 main="% Procedencia",
                 col=c("red", "blue", "seagreen", "cyan"),# Color
                 font.lab=2, # Bold
                 font.axis=2, # Bold
                 decimal=1, # Decimals
                 bar.values="percent") # Percent

par(ask=TRUE)
graphics::mosaicplot(~Sexo + Procedencia,
                     data=dat.df,
                     main="Procedencia por el sexo",
                     col=c("red", "blue", "seagreen", "cyan"),
                     xlab="Sexo", ylab="Procedencia",
                     type="pearson") # Pearson’s Chi-square

#####ESTE NO NOS SALIÓ
SexoProcedencia <- base::table(
  dat.df$sexo, # Row
  dat.df$Procedencia) # Column

base::margin.table(Edad) # Total count
base::margin.table(Edad, 1) # 1 represents rows
base::margin.table(dat.df, 2) # 2 represents columns
base::prop.table(Edad) # Sum to 100%, all cells
# Proportions, by individual cell
base::prop.table(Edad, 1) # Sum to 100%, by rows
# Proportions, by row
base::prop.table(Edad, 2) # Sum to 100%, by columns
# Proportions, by column

stats::addmargins(base::prop.table(dat.df))



install.packages("waffle")
library(waffle) # Load the waffle package.
help(package=waffle) # Show the information page.
sessionInfo() # Confirm all attached packages.

par(ask=TRUE)
waffle::waffle(Edad/10,
               rows=5, size=0.1, col=c("red", "blue", "seagreen", "cyan"),
               title="Waffle Chart of Race-Ethnicity",
               xlab="With 1,000 subjects each square =~ 10 subjects")
# Experiment, as needed, to determine the best number of
# rows, cell size, and the number of subjects for each cell
# in the waffle chart, or 1,000 overall subjects in this
# example.


#######################################################################
########################## Beautiful Images ###########################
#######################################################################

# A simple ggplot2 figure, showing a drab and generally
# colorless bar plot, absent any embellishments. Even
par(ask=TRUE)
ggplot2::ggplot(dat.df, aes(x=Sexo)) +
  geom_bar()


# Use stat="count" to count the number of cases for each bar.
par(ask=TRUE)
ggplot2::ggplot(dat.df, aes(x=Sexo)) +
  geom_bar(stat="count",
           fill=c("red", "blue")) +
  ggtitle("Sexo de los Participantes") +
  xlab("Sexo") +
  ylab("Count") +
  theme_bw()


####IMAGEN PARA PUBLICACIONES
par(ask=TRUE)
ggplot2::ggplot(dat.df, aes(x=Sexo)) +
  geom_bar(stat="count",
           fill=c("red", "darkgreen")) +
  ggtitle("Distribución del Sexo Pacientes Intento de Suicidio HSLV 2019") +
  xlab("Sexo") +
  ylab("No. de Pacientes") +  
  theme(plot.title=element_text(face="bold", size=14)) +
  theme(axis.title.x=element_text(face="bold", size=14)) +
  theme(axis.text.x=element_text(face="bold", size=14)) +
  theme(axis.title.y=element_text(face="bold", size=14)) +
  theme(axis.text.y=element_text(face="bold", size=14)) +
  theme(axis.ticks.x=element_line(size=2)) +
  theme(axis.ticks.y=element_line(size=2)) +
  theme(axis.ticks.length=unit(0.5,"cm")) +
  theme(panel.background=element_rect(fill="grey95"))



dat.df$Edad.factor <-
  base::cut(dat.df$Edad,
            breaks=c(-Inf, 12, 13, 14, 18, 20, 29, 25, 36,
                     55, Inf),
            labels=c("<= 099", "100-104", "105-109", "110-114",
                     "115-119", "120-124", "125-129", "130-134", "135-139",
                     ">= 140"),
            right=FALSE)

table(dat.df$Edad.factor)

base::getwd()
base::ls()
base::attach(dat.df)
utils::str(dat.df)
utils::head(dat.df, n=3)
base::summary(dat.df)

# Add embellishments, labels, and themes as needed.
par(ask=TRUE)
ggplot2::ggplot(dat.df) +
  geom_bar(aes(x=Edad.factor, fill=Sexo)) +
  facet_grid(. ~ Sexo) +
  theme(legend.position="none")

# Add embellishments, labels, and themes as needed.
par(ask=TRUE)
ggplot2::ggplot(dat.df) +
  geom_bar(aes(x=Edad.factor, fill=Sexo)) +
  facet_grid(. ~ Sexo) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(legend.position="none")


###### HEY HEY, HEY, HEY USE THEM
s20x::summaryStats(Edad ~ Sexo,
                   data=dat.df)
par(ask=TRUE)
ggplot2::ggplot(dat.df) +
  geom_histogram(aes(x=Edad, fill=Sexo)) +
  facet_grid(. ~ Sexo) +
  theme(legend.position="none")

s20x::summaryStats(Edad ~ Sexo,
                   data=dat.df)
par(ask=TRUE)
ggplot2::ggplot(dat.df) +
  geom_histogram(aes(x=Edad, fill=Sexo)) +
  facet_grid(. ~ Sexo) +
  theme(legend.position="none")


########
par(ask=TRUE)
ggplot2::ggplot(dat.df) +
  geom_histogram(aes(x=Edad, fill=Procedencia)) +
  facet_grid(Procedencia ~ Sexo)

par(ask=TRUE)
ggplot2::ggplot(dat.df) +
  geom_histogram(aes(x=Edad, fill=Sexo)) +
  facet_grid(Sexo ~ Procedencia)


#############################################################
############## ESTOS SON LOS GRÁFICOS BRUTALES ##############

par(ask=TRUE)
ggplot2::ggplot(dat.df,
  aes(x=Edad, y=Sexo, color=Procedencia)) +
  geom_point(size=2.5) +
  facet_wrap(~ Procedencia, ncol=1) +
  labs(x="\nEdad", y=" ",
       title="Edad por Género y Procedencia\n") +
  theme_bw() +
  theme(legend.position="none")

par(ask=TRUE)
ggplot2::ggplot(dat.df,
                aes(x=Edad, y=Procedencia, color=Sexo)) +
  geom_point(size=2.5) +
  facet_wrap(~ Sexo, ncol=1) +
  labs(
    x="\nEdad", y =" ",
    title="Edad por Procedencia y Sexo\n") +
  theme_classic() +
  theme(plot.title=element_text(face="bold", size=14)) +
  theme(axis.title.x=element_text(face="bold", size=14)) +
  theme(axis.text.x=element_text(face="bold", size=14)) +
  theme(axis.title.y=element_text(face="bold", size=14)) +
  theme(axis.text.y=element_text(face="bold", size=14)) +
  theme(plot.background = element_rect(fill="aliceblue")) +
  theme(strip.background=element_rect(color="black",
                                      fill="linen", size=2, linetype=1)) +
  theme(legend.position="none")

#######################################################################
# CHAPTER 3 Student’s t-Test for Independent Samples ####
## Housekeeping Use for All Analyses ####
date() # Current system time and date.
Sys.time() # Current system time and date (redundant).
R.version.string # R version and version release date.
options(digits=6) # Confirm default digits.
options(scipen=999)# Suppress scientific notation.
options(width=60) # Confirm output width.
ls(all.names=TRUE) # List all objects in the working
rm(list=ls()) # CAUTION: Remove all files in the working directory.
ls.str() # List all objects, with finite detail.
getwd() # Identify the current working directory.
setwd("F:/R_BiostatisticsIntroduction")# Set to a new working directory.
getwd() # Confirm the working directory.
list.files() # List files at the PC directory.
.libPaths() # Library pathname.
.Library # Library pathname.
sessionInfo() # R version, locale, and packages.
search() # Attached packages and objects.
searchpaths() # Attached packages and objects.

###### Prepare to Exit, Save, and Later Retrieve This R Session #######
#######################################################################
getwd() # Identify the current working directory.
ls() # List all objects in the working
# directory.
ls.str() # List all objects, with finite detail.
list.files() # List files at the PC directory.
save.image("R_NameOfStudy_DescriptiveStatistics.rdata")
getwd() # Identify the current working directory.
ls() # List all objects in the working
# directory.
ls.str() # List all objects, with finite detail.
list.files() # List files at the PC directory.
alarm() # Alarm, notice of upcoming action.
q() # Quit this session.
# Prepare for Save workspace image? query.

