# Housekeeping ####
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
setwd("C:/Users/juand/iCloudDrive/Epidemiology/R Projects")
getwd() # Identify the current working directory.
load("Hitchhiker’s.rdata")
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

pi
sqrt(2)
print(matrix(c(1, 2, 3, 4), 2, 2))
print(list("a", "b", "c"))
print(matrix(c(1, 2, 3, 4), 2, 2))
cat("The zero occurs at", 2 * pi, "radians.")
fib <- c(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
cat("The first few Fibonacci numbers are:", fib, "...\n")
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

# Chapter 1 Plaint Plots ####

## 1.2. Basic graph ####
# The first thing to do is load in the data and the libraries, as below:
  if (!require("pacman")) install.packages("pacman")
p_load(ggplot2, ggthemes, dplyr, readr)
chilean_exports <- "year,product,export,percentage
2006,copper,4335009500,81
2006,others,1016726518,19
2007,copper,9005361914,86
2007,others,1523085299,14
2008,copper,6907056354,80
2008,others,1762684216,20
2009,copper,10529811075,81
2009,others,2464094241,19
2010,copper,14828284450,85
2010,others,2543015596,15
2011,copper,15291679086,82
2011,others,3447972354,18
2012,copper,14630686732,80
2012,others,3583968218,20
2013,copper,15244038840,79
2013,others,4051281128,21
2014,copper,14703374241,78
2014,others,4251484600,22
2015,copper,13155922363,78
2015,others,3667286912,22
"
exports_data <- read_csv(chilean_exports)

# Graphics since here
p1 <- ggplot(aes(y = export, x = year, colour = product), data = exports_data) +
  geom_line()
p1

ggplot(data = exports_data,
       aes(x = year, y = export, colour = product))+
  geom_line()

# Read a data set
dat.df <- read.csv("~/Research/2020 - Intento de Suicidio HSLV 2019/INTSUIDATASET.csv", stringsAsFactors=TRUE)

ggplot(data = dat.df,
       aes(x = Edad, y = NumeroIntentosPrevios, colour = Sexo)) +
         geom_line()

## 1.3. Adjusting line width ####
p1 <- ggplot(aes(y = export, x = year, colour = product), data = exports_data) +
  geom_line(size = 1.5)
p1

ggplot(data = exports_data,
       aes(x = year, y = export, colour = product)) +
  geom_line(linewidth = 1.5)

ggplot(data = dat.df,
       aes(x = Edad, y = EstanciaHospitalaria, colour = Sexo)) +
  geom_line(linewidth = 1.5)

## 1.4. Changing variables display ####
# re-factor our data labels in exports_data tibble. Then
# we move the legend to the bottom using the theme command.
exports_data <- exports_data %>%
  mutate(product = factor(product, levels = c("copper","others"),
                          labels = c("Copper ","Pulp wood, Fruit, Salmon & Others")))
p1 <- ggplot(aes(y = export, x = year, colour = product),
             data = exports_data) +
  geom_line(size = 1.5) +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.title = element_blank())
p1

ggplot(data = dat.df,
       aes(x = Edad, y = EstanciaHospitalaria, colour = Sexo)) +
  geom_line(linewidth = 1.5) +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.title = element_blank())

## 1.5. Adjusting x-axis scale ####
# To change the axis tick marks, we use the scale_x_continuous and/or
# scale_y_continuous commands.
p1 <- p1 + scale_x_continuous(breaks = seq(2006,2015,1))
p1

ggplot(data = dat.df,
       aes(x = Edad, y = EstanciaHospitalaria, colour = Sexo)) +
  geom_line(linewidth = 1) +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.title = element_blank()) +
  scale_x_continuous(breaks = seq(10, 60, 5))


## 1.6. Adjusting axis labels & adding title ####
# To add a title, we include the option labs and include the name of the graph as a string argument, and to
# change the axis names we use the labs command too.
p1 <- p1 +
  labs(title = "Composition of Exports to China ($)",
       subtitle = "Source: The Observatory of Economic Complexity") +
  labs(x = "Year", y = "USD million")
p1

ggplot(data = dat.df,
       aes(x = Edad, y = EstanciaHospitalaria, colour = Sexo)) +
  geom_line(linewidth = 1) +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.title = element_blank()) +
  scale_x_continuous(breaks = seq(10, 60, 10)) +
  labs(title = "Días de Estancia Hospitalaria por Edad",
       subtitle = "Intento de Suicidio Hospital Susana López de Valencia, 2019") +
         labs(x = "Edad", y = "Estancia Hospitalaria (Días)")

## 1.7. Adjusting color palette ####
colour <- c("#5F9EA0", "#E1B378")
p1 <- p1 + scale_colour_manual(values = colour)
p1

colour <- c("#5F9EA0", "#E1B378")

ggplot(data = dat.df,
       aes(x = Edad, y = EstanciaHospitalaria, colour = Sexo)) +
  geom_line(linewidth = 1) +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.title = element_blank()) +
  scale_x_continuous(breaks = seq(10, 60, 10)) +
  labs(title = "Días de Estancia Hospitalaria por Edad",
       subtitle = "Intento de Suicidio Hospital Susana López de Valencia, 2019") +
  labs(x = "Edad", y = "Estancia Hospitalaria (Días)") +
  scale_colour_manual(values = colour)

## 1.8. Using the white theme ####
p1 <- ggplot(aes(y = export, x = year, colour = product),
             data = exports_data) +
  geom_line(size = 1.5) +
  scale_x_continuous(breaks = seq(2006,2015,1)) +
  labs(title = "Composition of Exports to China ($)",
       subtitle = "Source: The Observatory of Economic Complexity") +
  labs(x = "Year", y = "USD million") +
  scale_colour_manual(values = colour) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_blank())
p1

ggplot(data = dat.df,
       aes(x = Edad, y = EstanciaHospitalaria, colour = Sexo)) +
  geom_line(linewidth = 1) +
    scale_x_continuous(breaks = seq(10, 60, 10)) +
  labs(title = "Días de Estancia Hospitalaria por Edad",
       subtitle = "Intento de Suicidio Hospital Susana López de Valencia, 2019") +
  labs(x = "Edad", y = "Estancia Hospitalaria (Días)") +
  scale_colour_manual(values = colour) + 
  theme_bw() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.title = element_blank())
  
## 1.9. Using system fonts ####
p_load(showtext)

font_add("Tahoma","Tahoma.ttf")
font_add("XKCD","xkcd-Regular.otf")
font_add("Roboto Condensed", "RobotoCondensed-Regular.ttf")
font_add("Decima Mono Pro","DecimaMonoPro.otf")
font_add("Atlas Grotesk Regular","AtlasGrotesk-Regular.otf")
font_add("Atlas Grotesk Medium","AtlasGrotesk-Medium.otf")
showtext_auto()

## 1.10. Creating an XKCD style chart ####
fill <- c("#40b8d0","#b2d183")
p1 <- ggplot(aes(y = export, x = year, colour = product),
             data = exports_data) +
  geom_line(size = 1.5) +
  scale_x_continuous(breaks = seq(2006,2015,1)) +
  labs(title = "Composition of Exports to China ($)",
       subtitle = "Source: The Observatory of Economic Complexity") +
  labs(x = "Year", y = "USD million") +
  scale_fill_manual(values = fill) +
  theme(axis.line.x = element_line(size = .5, colour = "black"),
        axis.line.y = element_line(size = .5, colour = "black"),
        axis.text.x = element_text(colour = "black", size = 10),
        axis.text.y = element_text(colour = "black", size = 10),
        legend.key = element_rect(fill = "white", colour = "white"),
        legend.position = "bottom", legend.direction = "horizontal",
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(family = "XKCD"),
        text = element_text(family = "XKCD"))
p1



## 1.11. Using ‘The Economist’ theme ####
p1 <- ggplot(aes(y = export, x = year, colour = product),
             data = exports_data) +
  geom_line(size = 1.5) +
  scale_x_continuous(breaks = seq(2006,2015,1)) +
  labs(title = "Composition of Exports to China ($)",
       subtitle = "Source: The Observatory of Economic Complexity") +
  labs(x = "Year", y = "USD million") +
  theme_economist() + scale_colour_economist() +
  theme(axis.line.x = element_line(linewidth = .5, colour = "black"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        plot.title = element_text(family = "Roboto Condensed"),
        text = element_text(family = "Roboto Condensed"))
p1

## 1.12. Using ‘Five Thirty Eight’ theme ####
p1 <- ggplot(aes(y = export, x = year, colour = product),
             data = exports_data) +
  geom_line(size = 1.5) +
  scale_x_continuous(breaks = seq(2006,2015,1)) +
  labs(title = "Composition of Exports to China ($)",
       subtitle = "Source: The Observatory of Economic Complexity") +
  labs(x = "Year", y = "USD million") +
  theme_fivethirtyeight() + scale_colour_fivethirtyeight() +
  theme(axis.title = element_text(family = "Atlas Grotesk Regular"),
        legend.position = "bottom", legend.direction = "horizontal",
        legend.title = element_blank(),
        plot.title = element_text(family = "Atlas Grotesk Medium"),
        legend.text = element_text(family = "Atlas Grotesk Regular"),
        text = element_text(family = "Decima Mono Pro"))
p1

## 1.13. Creating your own theme ####
p1 <- ggplot(aes(y = export, x = year, colour = product),
             data = exports_data) +
  geom_line(size = 1.5) +
  scale_x_continuous(breaks = seq(2006,2015,1)) +
  labs(title = "Composition of Exports to China ($)",
       subtitle = "Source: The Observatory of Economic Complexity") +
  labs(x = "Year", y = "USD million") +
  scale_colour_manual(values = colour) +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = .5),
        axis.text.x = element_text(colour = "black", size = 10),
        axis.text.y = element_text(colour = "black", size = 10),
        legend.key = element_rect(fill = "white", colour = "white"),
        legend.position = "bottom", legend.direction = "horizontal",
        legend.title = element_blank(),
        panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(family = "Tahoma"))
p1

# Finally, to add points to create a marked line we use geom_point.
colour <- c("#40b8d0", "#b2d183")
p1 <- ggplot(aes(y = export, x = year, colour = product),
             data = exports_data) +
  geom_line(size = 1.5) +
  scale_x_continuous(breaks = seq(2006,2015,1)) +
  labs(title = "Composition of Exports to China ($)",
       subtitle = "Source: The Observatory of Economic Complexity") +
  labs(x = "Year", y = "USD million") +
  scale_colour_manual(values = colour) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = .5),
        axis.text.x = element_text(colour = "black", size = 10),
        axis.text.y = element_text(colour = "black", size = 10),
        legend.key = element_rect(fill = "white", colour = "white"),
        legend.position = "bottom", legend.direction = "horizontal",
        legend.title = element_blank(),
        panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(family = "Tahoma"),
        legend.key.size = unit(2, 'lines')) +
  geom_point(size = 3)
p1

# Chapter 2 Area Plots ####
## 2.2. Basic graph ####
# The first thing to do is load in the data and the libraries, as below:
if (!require("pacman")) install.packages("pacman")
p_load(ggplot2, ggthemes, dplyr, readr, forcats)
chilean_exports <- "year,product,export,percentage
2006,copper,4335009500,81
2006,others,1016726518,19
2007,copper,9005361914,86
2007,others,1523085299,14
2008,copper,6907056354,80
2008,others,1762684216,20
2009,copper,10529811075,81
2009,others,2464094241,19
2010,copper,14828284450,85
2010,others,2543015596,15
2011,copper,15291679086,82
2011,others,3447972354,18
2012,copper,14630686732,80
2012,others,3583968218,20
2013,copper,15244038840,79
2013,others,4051281128,21
2014,copper,14703374241,78
2014,others,4251484600,22
2015,copper,13155922363,78
2015,others,3667286912,22
"
exports_data <- read_csv(chilean_exports)


# To render an areplot by ading the geom_area command
p2 <- ggplot(
  aes(y = export, x = year, fill = product),
  data = exports_data) +
  geom_area()
p2

p2b <- ggplot(
  aes(y = EstanciaHospitalaria, x = Edad, fill = Procedencia),
  data = dat.df) +
  geom_area()
p2b

# stack in the opposite order.
p2 <- ggplot(
  aes(y = export, x = year, fill = fct_rev(product)), 
  data = exports_data) +
  geom_area()
p2

p2b <- ggplot(
  aes(y = EstanciaHospitalaria, x = Edad, fill = fct_rev(Procedencia)), 
  data = dat.df) +
  geom_area()
p2b

## 2.3. Adjusting legend position ####
p2 <- ggplot(
  aes(y = export, x = year, fill = fct_rev(product)), 
  data = exports_data) +
  geom_area() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.title = element_blank())
p2

p2b <- ggplot(
  aes(y = EstanciaHospitalaria, x = Edad, fill = fct_rev(Procedencia)), 
  data = dat.df) +
  geom_area() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.title = element_blank())
p2b
## 2.4. Changing variables display ####
exports_data <- exports_data %>%
  mutate(product = factor(product, levels = c("copper","others"),
                          labels = c("Copper ","Pulp wood, Fruit, Salmon & Others")))

p2 <- ggplot(aes(y = export, x = year, fill = fct_rev(product)), data = exports_data) +
  geom_area() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.title = element_blank()) +
  guides(fill = guide_legend(reverse = T))
p2

dat.df <- dat.df %>%
  mutate(Procedencia = factor(Procedencia, levels = c("Rural","Urbano"),
                          labels = c("Zona Rural","Zona Urbana")))

p2b <- ggplot(aes(y = EstanciaHospitalaria, x = Edad, fill = fct_rev(Procedencia)), data = dat.df) +
  geom_area() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.title = element_blank()) +
  guides(fill = guide_legend(reverse = T))
p2b


# Save & Exit ####
getwd() # Identify the current working directory.
ls() # List all objects in the working
# directory.
ls.str() # List all objects, with finite detail.
list.files() # List files at the PC directory.
save.image("Hitchhiker’s.rdata")
getwd() # Identify the current working directory.
ls() # List all objects in the working
# directory.
ls.str() # List all objects, with finite detail.
list.files() # List files at the PC directory.
alarm() # Alarm, notice of upcoming action.
q() # Quit this session.
# Prepare for Save workspace image? query.
