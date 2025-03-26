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
setwd("C:/Users/juand/iCloudDrive/Research/2023 - 07 - 09 - Intento de Suicidio HSLV 2019") #Copy & Paste the 
getwd() # Confirm the working directory.
list.files() # List files at the PC directory.
.libPaths() # Library pathname.
.Library # Library pathname.
sessionInfo() # R version, locale, and packages.
search() # Attached packages and objects.
searchpaths() # Attached packages and objects.
par("mar") # Confirm default margin (BLTR)
par(mar = c(5.1, 4.1, 4.1, 2.1)) #adjust plot margins

# INSTALL LIBRARIES -------------------------------------------------------
install.packages("tidyverse", dependencies = TRUE)
library(tidyverse) # Load the arsenal package.
help(package=tidyverse) # Show the information page.
sessionInfo() # Confirm all attached packages.
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


# 33 ####
## 33.2 Results of Screening for Vertebral Fracture (VFx) Based on ≥ 4 cm Height Loss (N = 151) ####
summary(T332.df)
str(T332.df)
VFxHLT.df <- table(test = T332.df$HLTest, ref = T332.df$VFx)
print(VFxHLT.df)

tp <- VFxHLT.df[2, 2]
fp <- VFxHLT.df[1, 2]
fn <- VFxHLT.df[2, 1]
tn <- VFxHLT.df[1, 1]

print(tp)
print(fp)
print(fn)
print(tn)

sens <- tp / (tp + fn)
spec <- tn / (tn + fp)
ppv <- tp / (tp + fp)
npv <- tn / (tn + fn)

print(sens)
print(spec)
print(ppv)
print(npv)
