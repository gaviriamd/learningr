#OPENING THE DATASET
INTSUIDATASET <- read.csv("C:/Users/juand/iCloudDrive/Research/SeGrEMO/Proyectos de Investigación/2023 - 07 - 09 - Intento de Suicidio HSLV 2019/INTSUIDATASET.csv")

#MAKING THE DATASET READEABLE
dat.df<-INTSUIDATASET

#INSTALLING A FEW NEEDED PACKAGES
install.packages("janitor")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyverse")

#OPENING LIBRARIES
library(ggplot2)
library(tidyverse)

###CENTRAL TENDENCY STATISTICS (LOCATION) - FOR CONTINUOS AND DISCRETE VARIABLES
#Mean (non-resistant): is the base for Standard Deviation (SD)
#Median (resistant): is the base for Interquartil Range (IQR)
#Trimmed Mean (Resistant)
mean(dat.df$Edad)
median(dat.df$Edad)
mean(dat.df$Edad, tr=.1,)
mode(dat.df$Edad)

###"ADJUSTED" CENTRAL TENDENCY STATISTICS (LOCATION)
#Standard Deviation (SD) (non-resistant)
#na.rm=T tells the function to ignore missing data
mean(dat.df$Edad, na.rm=T)
median(dat.df$Edad, na.rm=T)
mean(dat.df$Edad, na.rm=T, tr=.1,)

mean(dat.df$Estrato, na.rm=T)
median(dat.df$Estrato, na.rm=T)
mean(dat.df$Estrato, na.rm=T, tr=.1,)

mean(dat.df$NumeroIntentosPrevios, na.rm=T)

mean(dat.df$EstanciaHospitalaria)


##DISPERSION STATISTICS (VARIABILITY)
#Standard Deviation
sd(dat.df$Edad)

#Adjusting SD: it was made with 50 value after revising the boxplot and finding the value
sd(dat.df$Edad[dat.df$Edad  <  50])

#Median Absolute Deviation
mad(dat.df$Edad)

#Interquartile Range
IQR(dat.df$Edad)

#explore multi-modality
#create a frequency table, sort it by frequency, then print just the first 
#(continued) 6 rows of the table (i.e., the most frequent values) 
library(dplyr)
tab<-table(dat.df$Edad)
sorted_tab <- tab %>%
  as.dat.dfa.frame() %>%
  arrange(desc(Freq))
head(sorted_tab)

###LET US GRAPH histogram, density plot, and boxplot.
##Histogram:  bin interval is a smoothing parameter:
#Few bins oversmooth the histogram and can obscure, or even eliminate, characteristics of the variable’s distribution that are important to see and explore. 
#Many bins undersmooth histograms and allows the plot to display random idiosyncrasies (sometimes called noise) in the sample data that can, again, obscure the true shape of the variable’s distribution.
hist(dat.df$NumeroIntentosPrevios)
h=hist(dat.df$Edad,plot=F)
h$counts
h$breaks

h1<-hist(dat.df$Edad, breaks=5)
h1=hist(dat.df$Edad,plot=F)
h1$breaks
h1$counts

h2<-hist(dat.df$Edad, breaks=50)
h2=hist(dat.df$Edad,plot=F)
h2$counts
h2$breaks

#Making an equilibrated histogram and better presented
h<-hist(dat.df$Edad, breaks=20, 
        prob=T,
        main = "Edad de los participantes",
        xlab = "Edad (Años)")

#Adding a density curve over a histogram
X<-na.omit(dat.df$Edad)
h<-hist(X, breaks=20, na.omit=T,
        prob=T,
        main = "Edad de los Participantes",
        xlab = "Edad (años)")
lines(density(X))

##Boxplot (box-and-whisker plot):
#A boxplot does not display the mean and SD (common location and variability statistics) 
#On the other hand, a boxplot is built on resistant location (median) and variability (IQR) statistics.
#It´s an excellent tool for identifying potentially infuential values(descriptive analysis task).
b<-boxplot(dat.df$Edad,horizontal=T)

#5-number summary: LAV,Q1,Q2,Q3,UAV
b$stats

#generate boxplot rule-defined outliers, list in ascending order, and count the number of scores
sort(b$out)
length(b$out)

###FREQUENCY STATISTICS (PROPORTIONS) - FOR CATEGORICAL VARIABLES
library(janitor)
tabyl(dat.df$Genero)
tabyl(dat.df$Edad)
tabyl(dat.df$Etnia, na.rm=T)
tabyl(dat.df$EstadoCivil)
tabyl(dat.df$Ocupacion)
tabyl(dat.df$Procedencia)
tabyl(dat.df$Dia)
tabyl(dat.df$Mes)
tabyl(dat.df$Escolaridad)
tabyl(dat.df$TieneEnfermedadFisica)
tabyl(dat.df$EnfermedadFisica)
tabyl(dat.df$TieneEnfermedadMental)
tabyl(dat.df$EnfermedadMental)
tabyl(dat.df$IntentosPrevios)
tabyl(dat.df$DeterminacionRiesgo)
tabyl(dat.df$Riesgo)
tabyl(dat.df$Premeditacion)
tabyl(dat.df$Planificacion)
tabyl(dat.df$GestosDespedida)
tabyl(dat.df$Metodo)
tabyl(dat.df$Motivos)
tabyl(dat.df$AltaMismoDia)
tabyl(dat.df$AltaVoluntaria)
tabyl(dat.df$FueHospitalizado)
tabyl(dat.df$PersistenciaIdeacion)
tabyl(dat.df$Remision)
tabyl(dat.df$ProfesionalRemitido)
tabyl(dat.df$Lugar)
tabyl(dat.df$Subregistro)

#create table of category percentages for plotting, then plot (note that with missing data, the table is not created)
tab<-prop.table(table(dat.df$Ocupacion))*100
barplot(tab,ylim=c(0,100),ylab = "Percentage(%)",las=2)

##EXPLORING BIVARIATE DATA (DIRECTION [+, -], & MAGNITUD)
#Please take into account that the Y axis= Outcome (dependant/Outcome Variable)
#Please take into account that the X axis= Predictor (independant/predictor Variable)
library(lattice)
bwplot(Riesgo~NumeroIntentosPrevios,data=dat)
tapply(dat.df$NumeroIntentosPrevios,dat.df$Riesgo,mean,na.rm=TRUE)
tapply(dat.df$NumeroIntentosPrevios,dat.df$Riesgo,median,na.rm=TRUE)

histogram(~NumeroIntentosPrevios|Riesgo,data=dat,
          type="density",
          breaks=seq(0,80,by=2.5),
          layout=c(1,2))
tapply(dat.df$NumeroIntentosPrevios,dat.df$Riesgo,sd,na.rm=TRUE)
tapply(dat.df$NumeroIntentosPrevios,dat.df$Riesgo,mad,na.rm=TRUE)
#Please Note there were no differences in the results of mean, median, sd and mad but the idea is to find differences.

###MEASUREMENTS LEVELS
##Nominal: Name (Sex, Status)
##Ordinal: Name + Order (Level of Satisfaction)
##Interval: Name + Order + Proportionate Interval Between Var (Temperature)
##Ratio: Name + Order + Proportionate Interval Between Var + True Absolute Zero (Height)

##Pie Chart,Bar Chart : Nominal & Ordinal
##Boxplot, Histogram: Interval $ Ratio

### General Linear Model
## Numeric Y = β0 + β1categorical X + ε: ANOVA model
## categorical Y = β0 + β1categorical X + ε: Proportions model
## numeric Y = β0 + β1numeric X + ε: Regression model
## categorical Y = β0 + β1numeric X + ε: Logistic model

### STATISTICAL LENSES
## Comparative Statistics
# Difference Statistics (Mean difference
# Ratio Statistics (odds ratio)

