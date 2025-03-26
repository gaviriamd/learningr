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
setwd("C:/Users/juand/iCloudDrive/Epidemiology/R Projects") #Copy & Paste the
getwd() # Confirm the working directory.
load("Statistics_Using R.rdata") #Copy & Paste the 
list.files() # List files at the PC directory.
.libPaths() # Library pathname.
.Library # Library pathname.
sessionInfo() # R version, locale, and packages.
search() # Attached packages and objects.
searchpaths() # Attached packages and objects.
par("mar") # Confirm default margin (BLTR)
par(mar = c(5.1, 4.1, 4.1, 2.1)) #adjust plot margins


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
library(sur)

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
install.packages("sur", dependencies=TRUE)

# OPEN ISHSLV2019 ####
dat.df <- read.csv("~/Work/2020 - Intento de Suicidio HSLV 2019/INTSUIDATASET.csv", stringsAsFactors=TRUE)
# Chapter 1: Introduction ####
## R TUTORIAL ####
learnr::run_tutorial("R_Basics", package = "sur")

## SETTING A CONTEXT WITH REAL DATA ####
?NELS
NELS <- as.data.frame(NELS)
str(NELS)
glimpse(NELS)
dim(NELS)

## 1.11 ####
NELS$famsize[NELS$id == 1] # to know the famsize of id = 1.

# Did the first student in the NELS dataset (id =1) take advanced math in eighth
# grade (advmath8)?
NELS$advmath8[NELS$id == 1]

# Did the second student
NELS$advmath8[NELS$id == 2]

dim(NELS)
str(NELS)
glimpse(NELS)

class(NELS$expinc30)
# CHAPTER TWO: EXAMINING UNIVARIATE DISTRIBUTIONS ####
## WHEN VARIABLES ARE MEASURED AT THE NOMINAL LEVEL ####
### FREQUENCY AND PERCENT DISTRIBUTION TABLES ####
# tabular format
table(NELS$region)
percent.table(NELS$region) #To create a percent table

#### bar charts ####
barplot(table(NELS$region), ylab = "Frequency", xlab = "Region")

ggplot(NELS,
       mapping = aes(x = region, fill = gender))+
  labs(x = "Region", y = "Frequency", fill = "Gender")+
  geom_bar()

percent.table(NELS$region):
  barplot(percent.table(NELS$region), ylab = "Percent")

# To orient the bars horizontally, instead of vertically 
# as has been shown, we use the command:
  barplot(table(NELS$region), xlab = "Frequency", horiz = TRUE)

#### Pie Charts ####
# To create a pie chart of the variable region from the NELS dataset 
pie(table(NELS$region))

pie(table(dat.df$Motivos))

pie(table(Figure2_4$causes), main = "Annual Number of Deaths, NYC")

## WHEN VARIABLES ARE MEASURED AT THE ORDINAL, INTERVAL, OR RATIO LEVEL ####
### FREQUENCY AND PERCENT DISTRIBUTION TABLES ####

# construct a frequency distribution
table(NELS$ses)
table(dat.df$Edad)

# To obtain the grouped frequency distribution
NELS$categories.ses = cut(NELS$ses, breaks = c(0,5,10,15,20,25,30,35,40))
table(NELS$categories.ses)
percent.table(NELS$categories.ses)

#### STEM-AND-LEAF DISPLAYS ####
stem(States$pertak)
stem(States$satm)
stem(States$satm,
     scale = 2) #To have all the values appear as stems (not just the even values)
#### HISTROGRAMS ####
# EXIT --------------------------------------------------------------------

getwd() # Identify the current working directory.
ls() # List all objects in the working
# directory.
ls.str() # List all objects, with finite detail.
list.files() # List files at the PC directory.
save.image("Statistics_Using R.rdata")
getwd() # Identify the current working directory.
ls() # List all objects in the working
# directory.
ls.str() # List all objects, with finite detail.
list.files() # List files at the PC directory.
alarm() # Alarm, notice of upcoming action.
q() # Quit this session.
# Prepare for Save workspace image? query.