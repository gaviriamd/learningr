# HOUSEKEEPING ------------------------------------------------------------
date() # Current system time and date.
R.version.string # R version and version release date.
options(digits=6) # Confirm default digits.
options(scipen=999)# Suppress scientific notation.
options(width=60) # Confirm output width.
ls() # List all objects in the working directory.
# working directory. If this
# action is not desired, use rm()
# one-by-one to remove the objects
# that are not needed.
ls.str() # List all objects with finite detail.
getwd() # Identify the current working directory.
setwd("C:/Users/juand/iCloudDrive/Research/2023 - 07 - 09 - Intento de Suicidio HSLV 2019")
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
par("mar") # Confirm default margin (BLTR)
par(mar = c(5.1, 4.1, 4.1, 2.1)) #adjust plot margins

# INSTALL LIBRARIES ####
install.packages("tidyverse", dependencies = TRUE)
install.packages("dplyr", dependencies = TRUE)
install.packages("Hmisc", dependencies = TRUE)
install.packages("gtsummary", dependencies = TRUE)
install.packages("lubridate", dependencies = TRUE)
install.packages("RcmdrMisc", dependencies = TRUE)
install.packages("ggplot2", dependencies = TRUE)
install.packages("ggthemes", dependencies = TRUE)
install.packages("ggmosaic", dependencies = TRUE)
install.packages("gridExtra", dependencies = TRUE)
install.packages("grid", dependencies = TRUE)
install.packages("scales", dependencies = TRUE)
install.packages("waffle", dependencies=TRUE)
install.packages("nortest", dependencies=TRUE)
install.packages("UsingR", dependencies=TRUE)
install.packages("RVAideMemoire", dependencies=TRUE)
install.packages("furniture", dependencies=TRUE)
install.packages("pastecs", dependencies=TRUE)
install.packages("tables", dependencies=TRUE)
install.packages("doBy", dependencies=TRUE)
install.packages("psych", dependencies=TRUE)
install.packages("epiDisplay", dependencies=TRUE)
install.packages("asbio", dependencies=TRUE)
install.packages("s20x", dependencies=TRUE)
install.packages("arsenal", dependencies=TRUE)
install.packages("pivottabler", dependencies=TRUE)

# OPEN LIBRARIES ####
library(tidyverse)
library(dplyr)
library(Hmisc)
library(pivottabler)
library(gtsummary)
library(arsenal)
library(s20x)
library(asbio)
library(epiDisplay)
library(psych)
library(doBy)
library(tables)
library(pastecs)
library(furniture)
library(RVAideMemoire)
library(UsingR)
library(nortest)
library(waffle)
library(scales)
library(grid)
library(gridExtra)
library(ggmosaic)
library(ggthemes)
library(ggplot2)
library(RcmdrMisc)
library(lubridate)


# 1ST VISUALIZATION OF DATA -----------------------------------------------
library(dplyr)
getwd() # Identify the working directory
ls() # List objects
attach(dat.df) # Attach the data, for later use

utils::str(dat.df) # Identify structure
utils::head(dat.df, n=10) # Show the head, first 10 cases
utils::tail(dat.df, n=10) # Show the tail, last 10 cases
base::summary(dat.df) # Summary statistics

class(dat.df) # Class
glimpse(dat.df) # Identify structure
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
Hmisc::describe(dat.df)



# CODEBOOK ----------------------------------------------------------------
#                                                     #
# Sexo ........................ Factor (e.g., nominal)#
#                                 Masculino o femenino#
#                                                     #
# Edad ...................... Numeric (e.g., Discrete)#
#                                              En años#
#                                                     #
# Etnia ........................Factor (e.g., nominal)#
#                      Indígena, blanco, mestizo, afro#
#                                                     #
# Estado Civil................  Factor (e.g., nominal)#
#                         Soltero, Unión libre, casado#
#                                                     #                                                     #
# Numero de intentos Previos.Numeric (e.g., Discrete) #
#           Intentos de Suicidio previos a este evento#


# EXPLORATION OF NUMERIC (dbl) DATA ---------------------------------------
glimpse(dat.df)

#### Simple Visual Data Check Using Graphics (Numeric) ####
par(ask=TRUE)
plot(density(dat.df$Edad, na.rm=TRUE),
     main="Edad (Años) de los casos de Intentos de Suicidio
     Hospital Susana López de Valencia, 2019",
     col="red", lwd=5)

#### Manual Visualization of Several Histograms ####

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
par(mfrow=c(1,3)) # 3 figures - 1 row by 3 column grid
hist(
  Edad,
  col="red", # Add color
  breaks=20, # Increase granularity of histogram
  xaxt="n", # Suppress X axis for later manipulation
  xlab="Edad (Años)", # X axis label
  yaxt="n", # Suppress Y axis for later manipulation
  ylab="Pacientes", # Y axis label
  font.lab=2, # Bold labels
  xlim=c(10,60), # X axis scale
  ylim=c(0,30) # Y axis scale
)
axis(side=1, at=seq(10,60,5), font=2) # X axis
axis(side=2, at=seq(0,30,5), font=2, las=3)# Y axis

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
  ylim=c(0,130) # Y axis scale
)
axis(side=1, at=seq(0,35,5), font=2) # X axis
axis(side=2, at=seq(0,150,25), font=2, las=3)# Y axis

#### Muliple (2x4) numerica data visualization ####
par(ask=TRUE)
par(mfrow=c(2,4)) # 8 figures into a 2 row by 4 column grid
hist(dat.df$Edad,
     main="Edad: Histogram")
plot(dat.df$Edad,
     main="Edad: Plot")
plot(density(dat.df$Edad,
             na.rm=TRUE), # Required: na.rm=TRUE for missing data
     main="Edad: Density Plot")
boxplot(dat.df$Edad,
        main="Edad: Box Plot")
stripchart(dat.df$Edad,
           main="Edad: Stripchart")
dotchart(dat.df$Edad,
         main="Edad: Dotchart")
qqnorm(dat.df$Edad,
       main="Edad: Q-Q Plot")
qqnorm(dat.df$Edad,
       main="Edad: Q-Q Plot\nand Q-Q Line")

qqline(dat.df$Edad)
# Common figures for a numeric-type object variable
# The \n characters force a new line.

#### Complex Visual Data Check Using Graphics (Numeric) ####
# Six Automatic Visual Data Check 
par(ask=TRUE)
par(mfrow=c(2,3)) # 6 figures into a 2 row by 3 column grid

graphics::hist(dat.df$Edad, main="Edad (Años): Histogram")
axis(side=1, font=2) # X axis bold
axis(side=2, font=2) # Y axis bold

graphics::plot(dat.df$Edad, main="Edad (Años): Plot")
axis(side=1, font=2) # X axis bold
axis(side=2, font=2) # Y axis bold

graphics::plot(density(dat.df$Edad, na.rm=TRUE), # na.rm=TRUE
     main="Edad (Años): Density Plot") # missing data
axis(side=1, font=2) # X axis bold
axis(side=2, font=2) # Y axis bold

graphics::boxplot(dat.df$Edad,
        main="Edad (Años): Box Plot")
axis(side=1, font=2) # X axis bold
axis(side=2, font=2) # Y axis bold

graphics::stripchart(dat.df$Edad,
           main="Edad (Años): Stripchart") # Stripchart
axis(side=1, font=2) # X axis bold
axis(side=2, font=2) # Y axis bold
qqnorm(dat.df$Edad, main="Edad (Años): Q-Q Plot")

#### Four Manual Visual Data Check ####

# Histogram
par(ask=TRUE) # Pause
par(mfrow=c(2,2)) # 2 figures - 1 row by 2 column grid
graphics::hist(Edad,
                 main="Histogram of Edad (Años) Pacientes \ncon Intento de Suicidio HSLV 2019",
                 col="red", # Add color
                 breaks=10, # Increase granularity of histogram
                 font.lab=2, # Bold labels
                 xlim=c(10,60), # X axis scale
                 ylim=c(0,60)) # Y axis scale
axis(side=1, font=2) # X axis bold
axis(side=2, font=2) # Y axis bold

# Plot Density
graphics::plot(stats::density(Edad, na.rm=TRUE),
                 main="Density Plot of Edad (Años) Pacientes \ncon Intento de Suicidio HSLV 2019",
                 col="red", # Add color
                 lwd=5, # Thick line
                 font.lab=2, # Bold labels
                 xlim=c(0,70), # X axis scale
                 ylim=c(0,0.08)) # Y axis scale
axis(side=1, font=2) # X axis bold
axis(side=2, font=2) # Y axis bold

# Boxplot
graphics::boxplot(Edad,
        main="Boxplot of Edad (Años) Pacientes \ncon Intento de Suicidio HSLV 2019",
        xlab="Boxplot", # X axis label
        ylab="Edad (Años)", # Y axis label
        cex.axis=1.15, # Axis size
        cex.lab=1.15, # Label size
        col="red", # Box color
        lwd=2, # Line thickness
        font.lab=2, # Bold labels
        font=2, # Bold font
        ylim=c(0,60)) # Y axis scale
axis(side=1, font=2) # X axis bold
axis(side=2, font=2) # Y axis bold

# Q-Q Plot
stats::qqnorm(Edad,
                main="Q-Q Plot (Blue) and Q-Q Line (Red) of Edad \nPacientes con Intento de Suicidio HSLV 2019",
                col="blue", xlim=c(-3,3), ylim=c(0,60), font.axis=2,
                font.lab=2)
stats::qqline(Edad, # Add a Q-Q Line to the Q-Q Plot
                col="red", lwd=4, lty=4)
axis(side=1, font=2) # X axis bold
axis(side=2, font=2) # Y axis bold



# EXPLORATION OF CATEGORICAL (fct) DATA -----------------------------------
glimpse(dat.df)
## Simple Visual Data Check Using Graphics (Categorical) ####
par(ask=TRUE)
par(mfrow=c(2,4)) # 2 figures into a 1 row by 2 column grid
barplot(table(dat.df$Sexo),
        main="Género: Barplot Frequency Distribution",
        col=c("blue", "red"), ylim=c(0,120)) # Alter color - Y scale
barplot(table(dat.df$Etnia),
        main="Etnia: Barplot Frequency Distribution",
        col=c("blue", "red"), ylim=c(0,10)) # Alter color - Y scale
barplot(table(dat.df$EstadoCivil),
        main="Estado Civil: Barplot Frequency Distribution",
        col=c("green", "red"), ylim=c(0,140)) # Alter color - Y scale
barplot(table(dat.df$Ocupacion),
        main="Ocupacion: Barplot Frequency Distribution",
        col=c("green", "red"), ylim=c(0,100)) # Alter color - Y scale
barplot(table(dat.df$Procedencia),
        main="Procedencia: Barplot Frequency Distribution",
        col=c("green", "Blue"), ylim=c(0,120)) # Alter color - Y scale
barplot(table(dat.df$Estrato),
        main="Estrato: Barplot Frequency Distribution",
        col=c("green", "red"), ylim=c(0,120)) # Alter color - Y scale
barplot(table(dat.df$Escolaridad),
        main="Escolaridad: Barplot Frequency Distribution",
        col=c("blue", "red"), ylim=c(0,80)) # Alter color - Y scale
barplot(table(dat.df$Escolaridad),
        main="Escolaridad: Barplot Frequency Distribution",
        col=c("blue", "red"), ylim=c(0,80)) # Alter color - Y scale

par(ask=TRUE)
par(mfrow=c(2,3)) # 2 figures into a 1 row by 2 column grid
barplot(table(dat.df$TieneEnfermedadFisica),
        main="Tiene Enfermedad Fisica: Barplot Frequency Distribution",
        col=c("green", "blue"), ylim=c(0,150)) # Alter color - Y scale
barplot(table(dat.df$EnfermedadFisica),
        main="Enfermedad Física: Barplot Frequency Distribution",
        col=c("gray", "white"), ylim=c(0,150)) # Alter color - Y scale
barplot(table(dat.df$TieneEnfermedadMental),
        main="Tiene Enfermedad Mental: Barplot Frequency Distribution",
        col=c("green", "blue"), ylim=c(0,150)) # Alter color - Y scale
barplot(table(dat.df$EnfermedadMental),
        main="Enfermedad Mental: Barplot Frequency Distribution",
        col=c("green", "blue"), ylim=c(0,150)) # Alter color - Y scale
barplot(table(dat.df$IntentosPrevios),
        main="Intentos Previos: Barplot Frequency Distribution",
        col=c("green", "blue"), ylim=c(0,100)) # Alter color - Y scale

### Factor-Type Barchart ####
par(ask=TRUE)
barplot(table(dat.df$Sexo),
        main="Sexo: Barplot Frequency Distribution",
        col=c("black", "burlywood4"), ylim=c(0,100))

epiDisplay::tableStack(Sexo,
                       dataFrame=dat.df,
                       by="none", count=TRUE, decimal=2,
                       percent=c("column", "row"),
                       frequency=TRUE, name.test=TRUE,
                       total.column=TRUE, test=TRUE)
par(ask=TRUE)
epiDisplay::tab1(dat.df$Sexo,
                 decimal=2, # Use the tab1() function
                 sort.group=FALSE, # from the epiDisplay
                 cum.percent=TRUE, # package to see details
                 graph=TRUE, # about the selected
                 missing=TRUE, # object variable. (The
                 bar.values=c("frequency"), # 1 of tab1 is the one
                 horiz=TRUE, # numeric character and
                 cex=1.15, # it is not the letter
                 cex.names=1.15, # lowercase l).
                 cex.lab=1.15, cex.axis=1.15,
                 main="Edad pacientes con intento de suicidio N Values",
                 col= c("darkred", "darkblue"))
# Prepare a publishable quality barplot of Ocupacion
# and have descriptive statistics printed to the screen.

# Descriptive statistics, only


# EXIT --------------------------------------------------------------------
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
