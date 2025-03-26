# WELCOME ------------------------------------------------------------
date() # Current system time and date.
R.version.string # R version and version release date.
options(digits=6) # Confirm default digits.
options(scipen=999)# Suppress scientific notation.
options(width=60) # Confirm output width.
ls() # List all objects in the working directory.
rm(list = ls()) # CAUTION: Remove all files in the
ls.str() # List all objects with finite detail.
getwd() # Identify the current working directory.
setwd("C:/Users/juand/iCloudDrive/Research/2023 - 07 - 09 - Intento de Suicidio HSLV 2019")
getwd() # Confirm the working directory.
list.files() # List files at the PC directory.
.libPaths() # Library pathname.
.Library # Library pathname.
sessionInfo() # R version, locale, and packages.
search() # Attached packages and objects.
searchpaths() # Attached packages and objects.
par("mar") # Confirm default margin (BLTR)
par(mar = c(5.1, 4.1, 4.1, 2.1)) #adjust plot margins

dat.df <- utils::read.table (file =
                               "INTSUIDATASET.csv",
                             header=TRUE, dec=".", sep=",")

getwd() # Identify the working directory
ls() # List objects
attach(dat.df) # Attach the data, for later use

# INSTALL & ACTIVATE LIBRARIES -------------------------------------------------------
# Install ´em!
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

# Activate Libraries
library(tidyverse)
library(dplyr)
library(Hmisc)
library(gtsummary)
library(lubridate)
library(RcmdrMisc)
library(ggplot2)
library(ggthemes)
library(ggmosaic)
library(gridExtra)
library(grid)
library(scales)
library(waffle)
library(nortest)
library(UsingR)
library(RVAideMemoire)
library(furniture)
library(pastecs)
library(tables)
library(doBy)
library(psych)
library(epiDisplay)
library(asbio)
library(s20x)
library(arsenal)
library(pivottabler)

# REVIEW OF DATA FRAME ----------------------------------------------------
library(tidyverse)
view(dat.df) # To wacht on screen the dataset
class(dat.df) #class of dat.df object.
dim(dat.df) #Shows the size Rows and Columns
utils::str(dat.df) # Identify structure
glimpse(dat.df) # Same as above, but better printed.
utils::head(dat.df, n=3) # Show the head, 1st 3 cases #Standard is 6
utils::tail(dat.df, n=3) # Show the head, 1st 3 cases #Standard is 6
dat.df #To see de 1st 30 values of each variable
base::summary(dat.df) # Summary statistics

# DATA DISTRIBUTION AND TEST FOR NORMALITY --------------------------------
shapiro.test(dat.df$Edad) #What a Beauty! # Shapiro Test
RVAideMemoire::mshapiro.test(dat.df$Edad) # Normality test of Lbs overall
RVAideMemoire::byf.shapiro(Edad ~ Sexo,
                           data=dat.df) # Normality test of Edad by Sexo
nortest::ad.test(Edad) #The Anderson-Darling Test for Normal Dist.

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

# LOCATION (CENTRAL TENDENCY) & VARIABILITY STATISTICS (DISPERSION) -------
#### Single Statistics ####
utils::head(base::sort(Edad)) # Show the head, first values
utils::tail(base::sort(Edad))
base::sum(Edad) # Sum of all values
base::min(Edad) # Minimum value
base::which.min(Edad) # Minimum value location (e.g., row number)
base::max(Edad) # Maximum value
base::which.max(Edad) # Maximum value location (i.e., row number)
base::range(Edad) # Range of values, minimum to maximum
base::length(Edad) # Number of occurrences (e.g., N, datapoints)
stats::quantile(Edad) # Quantile scores, 0% 25% 50% 75% 100%
stats::quantile(dat.df$Edad, # Use of stats::quantile() function to produce
                prob=seq(0, 1, length=11), type=5) # deciles, 0% 10% 20% 30% 40% 50% 60% 70% 80% 90%

base::mean(Edad, na.rm=TRUE)
base::mean(Edad, trim = .1, na.rm=TRUE)
base::mean(Edad, trim = .05, na.rm=TRUE)
psych::geometric.mean(Edad)
psych::harmonic.mean(Edad)
psych::winsor.mean(Edad, trim=0.05)
stats::median(Edad, na.rm=TRUE)
asbio::Mode(Edad)

stats::sd(Edad, na.rm=TRUE) # Standard deviation
stats::IQR(Edad, na.rm=TRUE)
mad(Edad)
stats::var(Edad, na.rm=TRUE) # Variance
stats::var(dat.df$Edad) # Variance

#### Multiple Statistics ####
# Boxplot Statistics: Lower-Whisker, Lower-Hinge, Mean, Median, Upper-Hinge, and Upper-Whisker, N, and Outliers
grDevices::boxplot.stats(Edad)
RcmdrMisc::numSummary(Edad,
                      statistics=c("mean", "mode", "sd", "quantiles", "CV", "var"))
pastecs::stat.desc(Edad,
                   basic=FALSE, desc=FALSE, norm=FALSE)
psych::describe(Edad, fast=TRUE)
base::summary(Edad)
doBy::descStat(Edad) #Note only put Edad dued to dat.df is already attached
tables::tabular((Edad) ~ (n=1) + Format(digits=2)*
                  (min + mode + mean + sd + median + IQR + mad + var + max))

s20x::summaryStats(Edad ~ Sexo,
                   data=dat.df,
                   na.rm=TRUE)

#### Multiple Group Statistics ####
s20x::summaryStats(Edad, ~ Sexo, dat.df,
                   na.rm=TRUE) # Accommodate missing values
RcmdrMisc::numSummary(dat.df[,c("Edad")],
                      groups=Sexo) # Default printout, breakouts by Section
pivottabler::qpvt(dat.df, "Sexo", "Procedencia",
                  c("Mean Edad"="mean(Edad, na.rm=TRUE)",
                    "Median Edad"="median(Edad, na.rm=TRUE)",
                    "SD Edad"="sd(Edad, na.rm=TRUE)"),
                  formats=list("%.0f", "%.1f"))
epiDisplay::summ(dat.df$Edad,
                 by=Sexo, # Breakout statistics
                 graph=TRUE, # Dotplot
                 pch=20, ylab="auto",
                 main="Sorted Dotplot of Edad (Años) por Procedencia",
                 cex.X.axis=1.25, # Note X axis label size.
                 cex.Y.axis=1.25, # Note Y axis label size.
                 font.lab=2, dot.col="auto")





# TABLES -----------------------------------------------------------------

#### Table 1 ####
dat.df %>% select(c(Sexo, Edad, Etnia, EstadoCivil, Procedencia)) %>%
  tbl_summary(
    by = NULL,
    label = list(Edad ~ "Edad (Años)", Sexo ~ "Género", Etnia ~ "Etnia", EstadoCivil ~ "Estado Civil", Procedencia ~ "Procedencia"),
    statistic = NULL,
    digits = NULL,
    type = NULL,
    value = NULL,
    missing = "ifany",
    missing_text = "Sin Información",
    sort = list(everything() ~ "frequency",
    percent = NULL,
    include = everything()
  )%>%
  modify_caption("**Tabla 1. Características Sociodemográficas de los Pacientes con Intento de Suicidio HSLV 2019**"))
  
#### Table with Categories ####
dat.df %>% select(c(Sexo, Edad, Etnia, EstadoCivil)) %>%
  tbl_summary(
    by = Sexo,
    label = NULL,
    statistic = NULL,
    digits = NULL,
    type = NULL,
    value = NULL,
    missing = "ifany",
    missing_text = "Sin Información",
    sort = NULL,
    percent = NULL,
    include = everything()
  )%>%
  add_p(
    test = NULL,
    pvalue_fun = NULL,
    group = NULL,
    include = everything(),
    test.args = NULL,
  ) %>%
add_overall(
  last = FALSE,
  col_label = "**Total**, N = {N}",
  statistic = NULL,
  digits = NULL,
) %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Sexo**") %>%
  modify_caption("**Tabla 1. Características Sociodemográficas de los Pacientes con Intento de Suicidio HSLV 2019**")

# CONTINGENCY TABLES ------------------------------------------------------

Riesgo x Intentos Previos
Riesgo x Tiene Enfermedad Mental
Riesgo x Mes
Riesgo x Premeditación
Riesgo x Planificación
Persistencia Ideación x Intentos Previos

par(ask=TRUE)
s20x::rowdistr(crosstabs(~ Sexo + Edad,
                                        data=dat.df), plot=TRUE, suppressText=FALSE,
                              comp="basic")

# crosstab with p-Value by Fisher´s exact test
dat.df %>%
  tbl_cross(
    row = Mes,
    col = Riesgo,
    percent = "cell",
    missing = "no"
  ) %>%
  add_p() %>%
  modify_caption("**Relación entre el sexo y el riesgo de Intento Suicida**")

# Numeric Crosstab
summary(arsenal::tableby(list(Edad, Riesgo) ~ NumeroIntentosPrevios,
                         data = dat.df), text=TRUE, total=TRUE)

epiDisplay::tableStack(
  vars=Edad, # Rows
  dataFrame=dat.df,
  by=Riesgo, count=TRUE, decimal=2, # Columns
  percent=c("column", "row"),
  frequency=TRUE, name.test=TRUE,
  na.rm = TRUE,
  total.column=TRUE, test=TRUE)

# GRAPHICS -----------------------------------------------------------------

# Create data sets with complete information
cdat.df <- dat.df[complete.cases(dat.df$Riesgo), ] 

#### Scatter Plot ####
# aes x (Numeric), y (numeric), col (Categorical)
# Note each category has its shape
par(ask=TRUE)
par(mfrow=c(1,1)) # To put several graphics on the same image
ggplot2::ggplot(dat.df) + # DATA
  aes(x=Edad, y=EstanciaHospitalaria, col=Sexo, shape=Sexo) + # AESTHETICS
  geom_point() + # GEOMETRICS
  geom_smooth (position = "identity") + # GEOMETRICS
      ggtitle("Ocupación de los Pacientes que consultaron por Intento de Suicidio al Hospital Susana López de Valencia 2019", subtitle = "Rojo (Femenino) - Azul (Masculino)") +
      xlab("Edad") +
      ylab("Estancia Hospitalaria") +
  scale_alpha() + # SCALES
  scale_fill_brewer(palette = "Set1")+
  stat_smooth(NULL) + # STATISTICAL TRANSFORMATIONS
  facet_grid(Estrato ~ Procedencia,labeller = "label_both") + #FACETS
  theme_grey() + #VISUAL THEMES
    theme(panel.background = element_rect(fill = "#72efdd"))
    
    theme(panel.border = element_rect(fill = "transparent", color = "#72efdd",linewidth = 4))+
    theme(panel.grid = element_line(color = "#3a0ca3",size = 1,linetype = 3))+
    theme(plot.background = element_rect(color = 1,size = 1),
      plot.margin = margin(t = 20,r = 50,b = 40,l = 30))+
    theme(plot.title = element_text(face = "bold", hjust = NULL, size = 16, color = "black")) +
    theme(plot.subtitle = element_text(face="italic", size = 14, color = "black")) +
    theme(axis.title.x=element_text(size=12)) +
    theme(axis.text.x=element_text(size=10)) +
    theme(axis.title.y=element_text(size=12)) +
    theme(axis.text.y=element_text(size=10)) +
    theme(axis.ticks.x=element_line(linewidth = 0.5)) +
    theme(axis.ticks.y=element_line(linewidth = 0.5)) +
    theme(axis.ticks.length=unit(0.1,"cm"))

    gridExtra::grid.arrange(
      BarSexo,
      BarEstadoCivil,
      BarProcedencia,
      BarEstrato,
      ncol=2, nrow=2,
      top = textGrob("Imagen 2: Sexo, Estado Civil, Procedencia y \nEstrato Socio-Económico de los Pacientes con Intento de Suicidio HSLV 2019",
                     gp=gpar(fontsize=15,
                             font=7)))
    
# Create subplots using gridExtra    
# Remember to create the 4 grids by first, Second, Third, Fourth <- 
library(gridExtra)
grid.arrange(first,second,third,fourth, nrow = 2)

#### Q-Q Plot ####
par(ask=TRUE)
par(mfrow=c(1,1)) # To put several graphics on the same image
ggplot2::ggplot(dat.df) + # DATA
  aes(sample=Edad) + # AESTHETICS
  geom_qq(geom="path", position="identity")+ # GEOMETRICS
  geom_qq_line(geom="path", position="identity")+ # GEOMETRICS
  ggtitle("Imagen 1: Distribución de la Edad de los Pacientes que consultaron por Intento de Suicidio", subtitle = "Hospital Susana López de Valencia 2019")+
  xlab("Theoretical Quantile") +
  ylab("Edad (Años)") +
  scale_fill_continuous(palette = "Set1")+
  stat_qq(color="red4")+
  stat_qq_line(color="blue", linewidth=1.50)+ # STATISTICAL TRANSFORMATIONS
  facet_grid(Estrato ~ Procedencia,labeller = "label_both") + #FACETS
  theme_grey() #VISUAL THEMES
theme(panel.background = element_rect(fill = "#72efdd"))+
  
#### Barplot ####
# Remember to erase the coord_flip, fill and facet_grid.
par(ask=TRUE)
par(mfrow=c(1,1)) # To put several graphics on the same image
ggplot2::ggplot(dat.df) + #DATA
  aes(x=Sexo, fill=IntentosPrevios) + #AESTHETICS
  geom_bar(stat="count",
           position = "stack") + # GEOMETRICS
    coord_flip()+
    ggtitle("Ocupación de los Pacientes que consultaron por Intento", subtitle = "Hospital Susana López de Valencia 2019") +
    xlab("Sexo") +
    ylab("Número de Pacientes") +
    scale_alpha()
scale_fill_brewer(palette = "Set1")+
  facet_grid(Estrato ~ Procedencia,labeller = "label_both") + #FACETS
  theme_grey() + #VISUAL THEMES
    theme(panel.background = element_rect(fill = "#72efdd"))+
    theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))
    
#### Histogram ####
# Remember to erase the coord_flip, fill and facet_grid.
par(ask=TRUE)
par(mfrow=c(1,1)) # To put several graphics on the same image
ggplot2::ggplot(dat.df) + #DATA
  aes(x=Edad, fill=Sexo, after_stat(count)) + #AESTHETICS
  geom_histogram(binwidth = 2,
                 position = "identity",
                 alpha = 0.75,
                 na.rm = TRUE)+
      ggtitle("Edad de los Pacientes que consultaron por Intento de Suicidio", subtitle = "Hospital Susana López de Valencia 2019") +
  xlab("Edad") +
  ylab("Número de Pacientes") +
  scale_x_continuous(name = "Edad (Años)",
                     breaks = seq(10, 60, 5),
                     limits = c(10, 60))+
  scale_y_continuous(name = "Número de Pacientes")+
  scale_fill_brewer(palette = "Set1")
  !facet_grid(Estrato ~ Procedencia,labeller = "label_both") + #FACETS
  theme_grey() + #VISUAL THEMES
  theme(panel.background = element_rect(fill = "#72efdd"))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))

#### BoxPlot ####
  library(ggplot2) # Load the ggplot2 package.
  library(ggthemes) # Load the ggthemes package.
  library(ggmosaic) # Load the ggmosaic package.
  library(gridExtra) # Load the gridExtra package.
  library(grid) # Load the grid package.
  library(scales) # Load the scales package.
  
  par(ask=TRUE)
  par(mfrow=c(1,1)) # To put several graphics on the same image
  ggplot2::ggplot(dat.df, #DATA
                  aes(x=Sexo, y=NumeroIntentosPrevios, fill=Sexo)) + #AESTHETICS
    geom_boxplot(position = "dodge") +
    ggtitle("Número de Intentos Previos de Acuerdo al Sexo", subtitle = "Hospital Susana López de Valencia 2019") +
    xlab("Sexo") +
    ylab("Número de Intentos Previos") +
    scale_alpha() + # SCALES
    scale_fill_brewer(palette = "Set1")+
    stat_summary(fun.y=mean, geom="point", shape=1, size=6, col="black") +
    theme_grey()+  #VISUAL THEMES
    theme(legend.position="none")+ # No legend
  facet_grid(Estrato ~ Procedencia,labeller = "label_both") + #FACETS
    

#### Complex Plots 1 var. Num * 2 var. Cat ####
ggplotEdad1 <-
  ggplot2::ggplot(dat.df,
                  aes(x=Edad)) +
  geom_density(alpha = 0.5) +
  ggtitle("Edad Pacientes con Intento de Suicidio HSLV 2019") +
  xlab("Edad (Años)") +
  ylab("Número de Pacientes") +
  theme_few() +   # Review the ggthemes package
  theme(legend.position="none") # No legend
ggplotEdad2 <-
  ggplot2::ggplot(dat.df,
                  aes(x=Sexo, y=Edad, fill=Sexo)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=1, size=6,
               col="black") +
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

#### Very Complex Density Facet 1 var. Numer * 2 var. Cat ####
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
DensityFacetProcedenciaEdad <- # Procedencia (two breakouts) por Edad
  ggplot2::ggplot(dat.df,
                  aes(x=Edad)) +
  geom_density(col="red", lwd=2) +
  facet_grid(. ~ Procedencia) +
  ggtitle("Procedencia por Edad (Años)\n") +
  labs(x = "\nEdad (Años)", y = "Density\n") +
  scale_x_continuous(labels=scales::comma, limits=c(10,60),
                     breaks=seq(10,60, by=25)) +
  theme_bw()
BoxplotSexoEdad <- # Sexo (two breakouts) Por Edad
  ggplot(dat.df,
         aes(x=Sexo, y=Edad, fill=Sexo)) +
  geom_boxplot() +
  ggtitle("Sexo por Edad (Años)\n") +
  labs(x = "\nSexo", y = "Edad (Años)\n") +
  scale_y_continuous(labels=scales::comma, limits=c(10,50),
                     breaks=seq(10,50, by=25)) +
  theme_bw()
BoxplotProcedenciaEdad <- # Procedencia (two breakouts) por Edad
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