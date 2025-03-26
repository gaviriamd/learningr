# HOUSEKEEPING ------------------------------------------------------------
date() # Current system time and date.
R.version.string # R version and version release date.
options(digits=6) # Confirm default digits.
options(scipen=999)# Suppress scientific notation.
options(width=60) # Confirm output width.
ls() # List all objects in the working directory.
rm(list = ls()) # CAUTION: Remove all files in the
ls.str() # List all objects with finite detail.
getwd() # Identify the current working directory.
setwd("C:/Users/juand/iCloudDrive/Epidemiology/CMD/2024 - Diplomado en Bioestadística/01 - Introducción a R/DBS") #Copy & Paste the 
getwd() # Confirm the working directory.
load("DBS.rdata")
list.files() # List files at the PC directory.
.libPaths() # Library pathname.
.Library # Library pathname.
sessionInfo() # R version, locale, and packages.
search() # Attached packages and objects.
searchpaths() # Attached packages and objects.sto
par("mar") # Confirm default margin (BLTR)
par(mar = c(5.1, 4.1, 4.1, 2.1)) #adjust plot margins

# OPEN LIBRARIES ####
library(asbio)
library(tidyverse)
library(dplyr)
library(effsize)
library(Hmisc)
library(pivottabler)
library(arsenal)
library(s20x)
library(lattice)
library(effsize)
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
library(gtsummary)
library(lubridate)
library(RcmdrMisc)


# Introducción ####
v <- c(0:5)
v
class (v)
length(v)
t <- c("Zipaquirá", "Bogotá", "Bogotá", "Barranquilla")
class(t)
length(t)
s <- matrix(1, 2, 3, 4)
rt <- c()
help(Pima.tr)

suma_escalar <- v+5
# EXIT --------------------------------------------------------------------
getwd() # Identify the current working directory.
ls() # List all objects in the working
# directory.
ls.str() # List all objects, with finite detail.
list.files() # List files at the PC directory.
save.image("DBS.rdata")
getwd() # Identify the current working directory.
ls() # List all objects in the working
# directory.
ls.str() # List all objects, with finite detail.
list.files() # List files at the PC directory.
alarm() # Alarm, notice of upcoming action.
q() # Quit this session.
# Prepare for Save workspace image? query.