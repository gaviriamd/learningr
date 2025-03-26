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
load("Usi_R_for.rdata")
list.files() # List files at the PC directory.
.libPaths() # Library pathname.
.Library # Library pathname.
sessionInfo() # R version, locale, and packages.
search() # Attached packages and objects.
searchpaths() # Attached packages and objects.
par("mar") # Confirm default margin (BLTR)
par(mar = c(5.1, 4.1, 4.1, 2.1)) #adjust plot margins

# OPEN LIBRARIES ####
library(Hmisc)
library(tidyverse)
library(dplyr)
library(effsize)
library(Hmisc)
library(pivottabler)
library(arsenal)
library(s20x)
library(asbio)
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

# OPEN DATASET % MAKE IT READEABLE #######
dat.df <- utils::read.table (file =
                               "INTSUIDATASET.csv",
                             header=TRUE, dec=".", sep=",")
getwd() # Identify the working directory
ls() # List objects
attach(dat.df) # Attach the data, for later use

# INSTALL LIBRARIES -------------------------------------------------------
install.packages("tidyverse", dependencies = TRUE)
library(tidyverse) # Load the arsenal package.
help(package=tidyverse) # Show the information page.
sessionInfo() # Confirm all attached packages.
install.packages("dplyr", dependencies = TRUE)
install.packages("effsize", dependencies = TRUE)
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

# 3.0 Student’s t-Test for Independent Samples ####
MilkBreedFatProt.df <- read.table (file ="MilkBreedButterfatProtein.csv",
                                   header = TRUE, dec = ".", sep = ",") # Import the .csv file.
getwd() # Identify the working directory
ls() # List objects
attach(MilkBreedFatProt.df) # Attach the data, for later use
str(MilkBreedFatProt.df) # Identify structure
nrow(MilkBreedFatProt.df) # List the number of rows
ncol(MilkBreedFatProt.df) # List the number of columns
dim(MilkBreedFatProt.df) # Dimensions of the data frame
names(MilkBreedFatProt.df) # Identify names
colnames(MilkBreedFatProt.df) # Show column names
rownames(MilkBreedFatProt.df) # Show row names
head(MilkBreedFatProt.df) # Show the head
tail(MilkBreedFatProt.df) # Show the tail
MilkBreedFatProt.df # Show the entire dataframe
summary(MilkBreedFatProt.df) # Summary statistics 



## 3.3 Organize the Data and Display the Code Book (includes Code Book) ####
class(MilkBreedFatProt.df) # Class
str(MilkBreedFatProt.df) # Structure
duplicated(MilkBreedFatProt.df$Subject) # Duplicates

# Code Book for MilkBreedFatProt.df
# Subject .................... Factor (e.g., nominal) #
# A unique ID assigned to each cow #
# #
# Breed ...................... Factor (e.g., nominal) #
# 1 = Holstein and 2 = Jersey (alpha order) #
# #
# PctButterfat ............ Numeric (e.g., interval) #
# Percent Butterfat that can reach #
# 5.000000 or more #
# #
# PctProtein ............... Numeric (e.g., interval) #
# Percent Protein that can reach #
# 4.000000 or more #

MilkBreedFatProt.df$Subject <- as.factor(
  MilkBreedFatProt.df$Subject)
MilkBreedFatProt.df$Breed.recode <- factor(
  MilkBreedFatProt.df$Breed,
  labels=c("Holstein", "Jersey"))
# Use factor() and not as.factor().
MilkBreedFatProt.df$PctButterfat <- as.numeric(
  MilkBreedFatProt.df$PctButterfat)
MilkBreedFatProt.df$PctProtein <- as.numeric(
  MilkBreedFatProt.df$PctProtein)

getwd() # Identify the working directory
ls() # List objects
attach(MilkBreedFatProt.df) # Attach the data, for later use
str(MilkBreedFatProt.df) # Identify structure
nrow(MilkBreedFatProt.df) # List the number of rows
ncol(MilkBreedFatProt.df) # List the number of columns
dim(MilkBreedFatProt.df) # Dimensions of the data frame
names(MilkBreedFatProt.df) # Identify names
colnames(MilkBreedFatProt.df) # Show column names
rownames(MilkBreedFatProt.df) # Show row names
head(MilkBreedFatProt.df) # Show the head
tail(MilkBreedFatProt.df) # Show the tail
MilkBreedFatProt.df # Show the entire dataframe
summary(MilkBreedFatProt.df) # Summary statistics

## 3.4 Conduct a Visual Data Check Using Graphics (e.g.Figures) ####
### Factor-Type Barchart of Breed.recode/Sexo ####
par(ask=TRUE)
barplot(table(MilkBreedFatProt.df$Breed.recode),
        main="Breed: Barplot Frequency Distribution",
        col=c("black", "burlywood4"), ylim=c(0,25))

epiDisplay::tableStack(Breed.recode,
                       dataFrame=MilkBreedFatProt.df,
                       by="none", count=TRUE, decimal=2,
                       percent=c("column", "row"),
                       frequency=TRUE, name.test=TRUE,
                       total.column=TRUE, test=TRUE)
par(ask=TRUE)
epiDisplay::tab1(MilkBreedFatProt.df$Breed.recode,
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
                 main="Dairy Cow Breeds (Holstein v Jersey) N Values",
                 col= c("darkred", "darkblue"))
# Prepare a publishable quality barplot of Breed.recode
# and have descriptive statistics printed to the screen.

# Descriptive statistics, only

### Numeric-Type Graphics of PctButterfat and PctProtein ####
par(ask=TRUE)
par(mfrow=c(2,4)) # 8 figures into a 2 row by 4 column grid
hist(MilkBreedFatProt.df$PctButterfat,
     main="Percent Butterfat: Histogram")
plot(MilkBreedFatProt.df$PctButterfat,
     main="Percent Butterfat: Plot")
plot(density(MilkBreedFatProt.df$PctButterfat,
             na.rm=TRUE), # Required: na.rm=TRUE for missing data
     main="Percent Butterfat: Density Plot")
boxplot(MilkBreedFatProt.df$PctButterfat,
        main="Percent Butterfat: Box Plot")
stripchart(MilkBreedFatProt.df$PctButterfat,
           main="Percent Butterfat: Stripchart")
dotchart(MilkBreedFatProt.df$PctButterfat,
         main="Percent Butterfat: Dotchart")
qqnorm(MilkBreedFatProt.df$PctButterfat,
       main="Percent Butterfat: Q-Q Plot")
qqnorm(MilkBreedFatProt.df$PctButterfat,
       main="Percent Butterfat: Q-Q Plot\nand Q-Q Line")
qqline(MilkBreedFatProt.df$PctButterfat)
# Common figures for a numeric-type object variable
# The \n characters force a new line.

### Creating a theme for numeric-type graphics####
### Figure 3.5: Distribution of percent butterfat—2 ####
theme_MacYates <- function(base_size=12, base_family="sans")
  theme(
    plot.title=element_text(face="bold", size=14, hjust=0),
    axis.title.x=element_text(face="bold", size=14,
                              hjust=0.5),
    axis.text.x=element_text(face="bold", size=10),
    axis.title.y=element_text(face="bold", size=14, vjust=1,
                              angle=90),
    axis.text.y=element_text(face="bold", size=12, hjust=1),
    legend.title=element_text(face="bold", size=14),
    legend.text=element_text(face="bold", size=14),
    axis.ticks.x=element_line(size=1.2),
    axis.ticks.y=element_line(size=1.2),
    axis.ticks.length=unit(0.25,"cm"),
    panel.background=element_rect(fill="whitesmoke")
  )
# hjust - horizonal justification; 0 = left edge to 1 = right
# edge, with 0.5 the default
# vjust - vertical justification; 0 = bottom edge to 1 = top
# edge, with 0.5 the default
# angle - rotation; generally 1 to 90 degrees, with 0 the
# default

class(theme_MacYates)
# Confirm the class of the enumerated theme.

HistogramPctButterfatOverall <-
  ggplot2::ggplot(MilkBreedFatProt.df,
                  aes(x=PctButterfat)) +
  geom_histogram(binwidth=0.30, color="black", lwd=1.25,
                 fill="cornsilk2") +
  geom_vline(aes(xintercept=mean(PctButterfat, na.rm=TRUE)),
             color="darkred", linetype="dashed", size=1.25) +
  geom_vline(aes(xintercept=median(PctButterfat, na.rm=TRUE)),
             color="dodgerblue", linetype="dotted", size=1.25) +
  ggtitle(
    "Percent Butterfat Produced by Holstein and Jersey
Dairy Cows, Mean - Red and Median - Blue") +
  scale_x_continuous(name="Percent Butterfat", limits=c(0,6),
                     breaks=seq(0,6,1.0)) +
  scale_y_continuous(name="Count", limits=c(0,10),
                     breaks=seq(0,10,1)) +
  theme_MacYates()
# Generate the figure, but it will not show until using the
# gridExtra::grid.arrange() function.
# Regarding the scales used for this and other figures, it is
# often best to first generate the figure with no attention
# to the later axis scales, to see what the default shows.
# Then, determine individual needs for presentation and
# practice with scale_x_continuous(), scale_y_continuous(),
# and associated options such as name, limits, and breaks.
# Using name, the axis label can be placed within
# scale_x_continuous() as well as scale_y_continuous().
# Notice how limits is used to set the scale that shows
# on the axis, from minimum and maximum.
# See how breaks is used to determine the placement of
# tick marks.
HistogramPctButterfatBreed.recode <-
  ggplot2::ggplot(MilkBreedFatProt.df,
                  aes(x=PctButterfat)) +
  geom_histogram(binwidth=0.30, color="black", lwd=1.25,
                 fill="cornsilk2") +
  facet_grid(. ~ Breed.recode) +
  ggtitle(
    "Percent Butterfat Produced by Holstein and Jersey
Dairy Cows by Breed: Holstein v Jersey") +
  scale_x_continuous(name="Percent Butterfat", limits=c(0,6),
                     breaks=seq(0,6,1.0)) +
  scale_y_continuous(name="Count", limits=c(0,10),
                     breaks=seq(0,10,1)) +
  theme(strip.text.x=element_text(face="bold", size=12,
                                  color="navyblue")) +
  theme(strip.background=element_rect(fill="wheat1")) +
  theme_MacYates()
# Note how theme(strip.text) and theme(strip.background) are
# placed before theme_MacYates()
par(ask=TRUE)
gridExtra::grid.arrange(
  HistogramPctButterfatOverall,
  HistogramPctButterfatBreed.recode, ncol=2)



### Figure 3.6: Distribution of percent butterfat—3 ####
BoxplotPctButterfatOverall <-
  ggplot2::ggplot(MilkBreedFatProt.df,
                  aes(x=factor(0), y=PctButterfat)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=1, size=12,
               col="red") +
  ggtitle(
    "Percent Butterfat Produced by Holstein and Jersey
Dairy Cows, Mean - Red Circle") +
  xlab("Both Breeds: Holstein and Jersey") +
  scale_x_discrete(breaks=NULL) +
  scale_y_continuous(name="Percent Butterfat",
                     limits=c(3.0,6.0), breaks=seq(3,6,0.5)) +
  theme_MacYates()
# Note the creation of a dummy variable, x=factor(0).
BoxplotPctButterfatBreed.recode <-
  ggplot2::ggplot(MilkBreedFatProt.df,
                  aes(x=factor(0), y=PctButterfat)) +
  geom_boxplot() +
  facet_grid(. ~ Breed.recode) +
  ggtitle(
    "Percent Butterfat Produced by Holstein and Jersey
Dairy Cows by Breed: Holstein v Jersey") +
  scale_x_discrete(name="Breed", breaks=NULL) +
  scale_y_continuous(name="Percent Butterfat",
                     limits=c(3.0,6.0), breaks=seq(3,6,0.5)) +
  theme(strip.text.x=element_text(face="bold", size=12,
                                  color="navyblue")) +
  theme(strip.background=element_rect(fill="wheat1")) +
  theme_MacYates()
par(ask=TRUE)
gridExtra::grid.arrange(
  BoxplotPctButterfatOverall,
  BoxplotPctButterfatBreed.recode, ncol=2)
                  

### Figure 3.7: Distribution of percent butterfat—4 ####
DensityCurvePctButterfatOverall <-
ggplot2::ggplot(MilkBreedFatProt.df,
                aes(x=PctButterfat)) +
  geom_density(size=1.5, col=("red")) +
  ggtitle(
    "Percent Butterfat Produced by Holstein and
Jersey Dairy Cows") +
  scale_x_continuous(name="Percent Butterfat", limits=c(0,6),
                     breaks=seq(0,6,1.0)) +
  scale_y_continuous(name="Density", limits=c(0,0.60),
                     breaks=seq(0,1,0.1)) +
  theme_MacYates()
# There is no easy way to know the best Y axis scale to use
# with a density curve. It is usually best to generate the
# density curve using default settings first, with no use of
# arguments with scale_y_continuous. Then, experiment to see
# the best values for limits and breaks.
DensityCurvePctButterfatBreed.recode <-
  ggplot2::ggplot(MilkBreedFatProt.df,
                  aes(x=PctButterfat)) +
  geom_density(size=1.5, col=("red")) +
  facet_grid(. ~ Breed.recode) +
  ggtitle(
    "Percent Butterfat Produced by Holstein and Jersey
Dairy Cows by Breed: Holstein v Jersey") +
  scale_x_continuous(name="Percent Butterfat", limits=c(0,6),
                     breaks=seq(0,6,1.0)) +
  scale_y_continuous(name="Density", limits=c(0,2),
                     breaks=seq(0,2,0.5)) +
  theme(strip.text.x=element_text(face="bold", size=12,
                                  color="navyblue")) +
  theme(strip.background=element_rect(fill="wheat1")) +
  theme_MacYates()
# Generate the figure, but it will not show until using the
# gridExtra::grid.arrange() function.
par(ask=TRUE)
gridExtra::grid.arrange(
  DensityCurvePctButterfatOverall,
  DensityCurvePctButterfatBreed.recode, ncol=2)

### Figure 3.8: Distribution of percent protein—2 ####
HistogramPctProteinOverall <-
  ggplot2::ggplot(MilkBreedFatProt.df,
                  aes(x=PctProtein)) +
  geom_histogram(binwidth=0.30, color="black", lwd=1.25,
                 fill="cornsilk2") +
  geom_vline(aes(xintercept=mean(PctProtein, na.rm=TRUE)),
             color="darkred", linetype="dashed", size=0.75) +
  geom_vline(aes(xintercept=median(PctProtein, na.rm=TRUE)),
             color="dodgerblue", linetype="dotted", size=1.25) +
  ggtitle(
    "Percent Protein Produced by Holstein and Jersey
Dairy Cows, Mean - Red and Median - Blue") +
  scale_x_continuous(name="Percent Protein", limits=c(0,5),
                     breaks=seq(0,5,1.0)) +
  scale_y_continuous(name="Count", limits=c(0,20),
                     breaks=seq(0,20,5)) +
  theme_MacYates()
# Generate the figure, but it will not show until using the
# gridExtra::grid.arrange() function.
# Practice with scale_x_continuous and scale_y_continuous to
# determine the best selections for limits and breaks.
# The red dashed line (PctProtein Mean = 3.48) and the blue
# dotted line (PctProtein Median = 3.47) are quite close to
# each other, given the similar values for mean and median.
HistogramPctProteinBreed.recode <-
  ggplot2::ggplot(MilkBreedFatProt.df,
                  aes(x=PctProtein)) +
  geom_histogram(binwidth=0.30, color="black", lwd=1.25,
                 fill="cornsilk2") +
  facet_grid(. ~ Breed.recode) +
  ggtitle(
    "Percent Protein Produced by Holstein and Jersey
Dairy Cows by Breed: Holstein v Jersey") +
  scale_x_continuous(name="Percent Protein", limits=c(0,5),
                     breaks=seq(0,5,1.0)) +
  scale_y_continuous(name="Count", limits=c(0,20),
                     breaks=seq(0,20,5)) +
  theme(strip.text.x=element_text(face="bold", size=12,
                                  color="navyblue")) +
  theme(strip.background=element_rect(fill="wheat1")) +
  theme_MacYates()
# Note how theme(strip.text) and theme(strip.background) are
# placed before theme_MacYates()
par(ask=TRUE)
gridExtra::grid.arrange(
  HistogramPctProteinOverall,
  HistogramPctProteinBreed.recode, ncol=2)
### Figure 3.9: Distribution of percent protein—3 ####
BoxplotPctProteinOverall <-
  ggplot2::ggplot(MilkBreedFatProt.df,
                  aes(x=factor(0), y=PctProtein)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=1, size=12,
               col="red") +
  ggtitle(
    "Percent Protein Produced by Holstein and Jersey
Dairy Cows, Mean - Red Circle") +
  scale_x_discrete(name="Both Breeds: Holstein and Jersey",
                   breaks=NULL) +
  scale_y_continuous(name="Percent Protein",
                     limits=c(3.0,4.25), breaks=seq(3.0,4.25,0.5)) +
  theme_MacYates()
# Note the creation of a dummy variable, x=factor(0). Give
# attention, also, to scale_x_discrete() and how it is set to
# present the X axis.
BoxplotPctProteinBreed.recode <-
  ggplot2::ggplot(MilkBreedFatProt.df,
                  aes(x=factor(0), y=PctProtein)) +
  geom_boxplot() +
  facet_grid(. ~ Breed.recode) +
  ggtitle(
    "Percent Protein Produced by Holstein and Jersey
Dairy Cows by Breed: Holstein v Jersey") +
  scale_x_discrete(name="Breed", breaks=NULL) +
  scale_y_continuous(name="Percent Protein",
                     limits=c(3.0,4.25), breaks=seq(3.0,4.25,0.5)) +
  theme(strip.text.x=element_text(face="bold", size=12,
                                  color="navyblue")) +
  theme(strip.background=element_rect(fill="wheat1")) +
  theme_MacYates()
par(ask=TRUE)
gridExtra::grid.arrange(
  BoxplotPctProteinOverall,
  BoxplotPctProteinBreed.recode, ncol=2)
### Figure 3.10: Distribution of percent protein—4 ####
DensityCurvePctProteinOverall <-
  ggplot2::ggplot(MilkBreedFatProt.df,
                  aes(x=PctProtein)) +
  geom_density(size=1.5, col=("red")) +
  ggtitle(
    "Percent Protein Produced by Holstein and
Jersey Dairy Cows") +
  scale_x_continuous(name="Percent Protein", limits=c(0,6),
                     breaks=seq(0,6,1.0)) +
  scale_y_continuous(name="Density", limits=c(0.0,2.5),
                     breaks=seq(0.0,2.5,0.50)) +
  theme_MacYates()
# There is no easy way to know the best Y axis scale to use
# with a density curve. It is usually best to generate the
# density curve using default settings first, with no use of
# arguments with scale_y_continuous. Then, experiment to see
# the best values for limits and breaks.
DensityCurvePctProteinBreed.recode <-
  ggplot2::ggplot(MilkBreedFatProt.df,
                  aes(x=PctProtein)) +
  geom_density(size=1.5, col=("red")) +
  facet_grid(. ~ Breed.recode) +
  ggtitle(
    "Percent Protein Produced by Holstein and Jersey
Dairy Cows by Breed: Holstein v Jersey") +
  scale_x_continuous(name="Percent Protein", limits=c(0,6),
                     breaks=seq(0,6,1.0)) +
  scale_y_continuous(name="Density", limits=c(0.0,2.5),
                     breaks=seq(0.0,2.5,0.50)) +
  theme(strip.text.x=element_text(face="bold", size=12,
                                  color="navyblue")) +
  theme(strip.background=element_rect(fill="wheat1")) +
  theme_MacYates()
# Generate the figure, but it will not show until using the
# gridExtra::grid.arrange() function.
par(ask=TRUE)
gridExtra::grid.arrange(
  DensityCurvePctProteinOverall,
  DensityCurvePctProteinBreed.recode, ncol=2)


## 3.5 Descriptive Statistics for Initial Analysis of the Data ####
# To wrap the table() function around the is.na() function and the 
# complete.cases() function. To make the presentation to the screen compact
table(is.na(MilkBreedFatProt.df))
# Check for missing data

table(complete.cases(MilkBreedFatProt.df))
# Check for complete cases

summary(MilkBreedFatProt.df[, 3:5])
# Apply the summary() function for columns 3 to 5, or
# PctButterfat - PctProtein - Breed.recode

table(MilkBreedFatProt.df$Breed.recode)

epiDisplay::tab1(MilkBreedFatProt.df$Breed.recode,
                 decimal=2, sort.group=FALSE, cum.percent=TRUE,
                 graph=FALSE, missing=TRUE)
# Frequency distribution shows at the screen.

par(mfrow=c(1,2)) # 2 figures into a 1 row by 2 column grid
with(MilkBreedFatProt.df, epiDisplay::summ(PctButterfat,
by=Breed.recode, main="Percent Butterfat by Breed",
graph=TRUE))
with(MilkBreedFatProt.df, epiDisplay::summ(PctProtein,
by=Breed.recode, main="Percent Protein by Breed",
graph=TRUE))
# Display a dotplot of PctButterfat and then a side-by-side
# dotplot of PctProtein, Breed.recode.
# Look at descriptive statistics as screen output, too.

RcmdrMisc::numSummary(MilkBreedFatProt.df$PctButterfat,
                      statistics=c("mean", "sd", "quantiles"))
# Show descriptive statistics of Percent Butterfat, overall.

RcmdrMisc::numSummary(MilkBreedFatProt.df[,c("PctButterfat")],
                      groups=Breed.recode, statistics=c("mean", "sd", "quantiles"))
# Show descriptive statistics of Percent Butterfat, by Breed.

RcmdrMisc::numSummary(MilkBreedFatProt.df$PctProtein,
                      statistics=c("mean", "sd", "quantiles"))
# Show descriptive statistics of Percent Protein, overall.

RcmdrMisc::numSummary(MilkBreedFatProt.df[,c("PctProtein")],
                      groups=Breed.recode, statistics=c("mean", "sd", "quantiles"))
# Show descriptive statistics of Percent Protein, by Breed.

Length <- function(x) base::length(x)
Min <- function(x) base::min(x, na.rm=TRUE)
Max <- function(x) base::max(x, na.rm=TRUE)
Mean <- function(x) base::mean(x, na.rm=TRUE)
SD <- function(x) stats::sd(x, na.rm=TRUE)
Median <- function(x) stats::median(x, na.rm=TRUE)
# Create new functions that can accommodate missing
# data, using na.rm=TRUE in these examples.
tables::tabular((Breed.recode + 1) ~ (n=1) + Format(digits=2)*
                  (PctButterfat)*(Length + Min + Max + Mean + SD + Median),
                data=MilkBreedFatProt.df)
# Breed.recode (rows) by PctButterfat (columns)

tables::tabular((Breed.recode + 1) ~ (n=1) + Format(digits=2)*
                  (PctProtein)*(Length + Min + Max + Mean + SD + Median),
                data=MilkBreedFatProt.df)
# Breed.recode (rows) by PctProtein (columns)

Hmisc::latex(
  tables::tabular((Breed.recode + 1) ~ (n=1) + Format(digits=2)*
                    (PctButterfat)*(Length + Min + Max + Mean + SD + Median),
                  data=MilkBreedFatProt.df)
)
# Wrap Hmisc::latex() around tables::tabular() to put the
# output into a LaTeX-ready format, for those who use LaTeX.
# Use LaTeX syntax, as desired, to edit the table and include
# centering, color, altered alignment, etc.

Hmisc::latex(
  tables::tabular((Breed.recode + 1) ~ (n=1) + Format(digits=2)*
                    (PctProtein)*(Length + Min + Max + Mean + SD + Median),
                  data=MilkBreedFatProt.df)
  # Breed.recode (rows) by PctProtein (columns)
)


## 3.6 Quality Assurance, Data Distribution, and Tests for Normality ####
par(ask=TRUE)
epiDisplay::tab1(MilkBreedFatProt.df$Breed.recode,
                 main="Representation of Dairy Cows by Breed",
                 col= c("black", "burlywood4"), cex.main=1.5,
                 cex.name=1.2, cex.axis=1.3, cex.lab=1.3)
# Redundant confirmation of subjects, dairy cows

ggplot(MilkBreedFatProt.df,
       mapping = aes(x = Breed.recode))+
  geom_bar() +
    labs(title = "Representation of Dairy Cows by Breed", subtitle = "Test", x = "Race", y = "Count")
# Visualization by ggplot2
  
with(MilkBreedFatProt.df, epiDisplay::summ(PctButterfat,
                                           by=Breed.recode, graph=FALSE))
# Redundant confirmation of PctButterfat by breed

with(MilkBreedFatProt.df, epiDisplay::summ(PctProtein,
                                           by=Breed.recode, graph=FALSE))
# Redundant confirmation of PctProtein by bree

install.packages("nortest", dependencies=TRUE)
library(nortest) # Load the nortest package.
help(package=nortest) # Show the information page.
sessionInfo() # Confirm all attached packages.

# Normality test for PctButterfat
nortest::ad.test(MilkBreedFatProt.df$PctButterfat)

nortest::cvm.test(MilkBreedFatProt.df$PctButterfat)

nortest::lillie.test(MilkBreedFatProt.df$PctButterfat)

nortest::pearson.test(MilkBreedFatProt.df$PctButterfat)

nortest::sf.test(MilkBreedFatProt.df$PctButterfat)

# To address this concern, prepare a simple density curve of PctButterfat to once
# again gain a sense of the distribution pattern (Fig. 3.13):
# R Input
plot(density(MilkBreedFatProt.df$PctButterfat, na.rm=TRUE),
     main="Percent Butterfat: Density Plot", col="red",
     lwd=3, xlim=c(0,6), font=2)

ggplot(
  data = MilkBreedFatProt.df,
  aes(x = PctButterfat)) +
  geom_density()


install.packages("RVAideMemoire", dependencies=TRUE)
library(RVAideMemoire) # Load the RVAideMemoire package.
help(package=RVAideMemoire) # Show the information page.
sessionInfo() # Confirm all attached packages.
RVAideMemoire::mshapiro.test(MilkBreedFatProt.df$PctButterfat)
# Normality test of Percent Butterfat, overall

RVAideMemoire::byf.shapiro(PctButterfat ~ Breed.recode,
                           data=MilkBreedFatProt.df)
# Normality test of Percent Butterfat by Dairy Breed

RVAideMemoire::byf.shapiro(Edad ~ Sexo,
                           data=dat.df)
# Normality test of Percent Butterfat by Dairy Breed

# Apply the same process against PctProtein, to determine if there is normal
# distribution overall and by breed:

# Normality tests for PctProtein
nortest::ad.test(MilkBreedFatProt.df$PctProtein)

nortest::cvm.test(MilkBreedFatProt.df$PctProtein)

nortest::lillie.test(MilkBreedFatProt.df$PctProtein)

nortest::pearson.test(MilkBreedFatProt.df$PctProtein)

nortest::sf.test(MilkBreedFatProt.df$PctProtein)

# To add assurance to this finding, prepare a simple density curve of PctProtein
#to once again gain a sense of the distribution pattern for PctProtein (Fig. 3.14):

plot(density(MilkBreedFatProt.df$PctProtein, na.rm=TRUE),
     main="Percent Protein: Density Plot", col="red",
     lwd=3, xlim=c(0,6), font=2)

ggplot(
  data = MilkBreedFatProt.df,
  aes(x = PctProtein)) +
  geom_density()

RVAideMemoire::mshapiro.test(MilkBreedFatProt.df$PctProtein)
# Normality test of Percent Protein, overall

RVAideMemoire::byf.shapiro(PctProtein ~ Breed.recode,
                           data=MilkBreedFatProt.df)
# Normality test of Percent Protein by Dairy Breed


# Use these figures as a visual reinforcement of outcomes from application of 
# the Anderson–Darling test  and the Shapiro test

# Percent Butterfat, Overall
QQPctFatOverall <-
  ggplot2::ggplot(MilkBreedFatProt.df,
                  aes(sample=PctButterfat)) +
  stat_qq(color="red") +
  stat_qq_line(color="blue", size=1.25) +
  ggtitle("
% Butterfat QQ-Plot and QQ-Line, Overall") +
  labs(x = "\nTheoretical", y = "Percent\n") +
  theme_MacYates()
# Percent Butterfat by Breed (two breakouts)
QQPctFatBreed.Recode <-
  ggplot2::ggplot(MilkBreedFatProt.df,
                  aes(sample=PctButterfat)) +
  stat_qq(color="red") +
  stat_qq_line(color="blue", size=1.25) +
  facet_grid(. ~ Breed.recode) +
  ggtitle("
% Butterfat QQ-Plot and QQ-Line, by Dairy Breed") +
  labs(x = "\nTheoretical", y = "Percent\n") +
  theme(strip.text.x=element_text(face="bold", size=12,
                                  color="navyblue")) +
  theme(strip.background=element_rect(fill="wheat1")) +
  theme_MacYates()
# Percent Protein, Overall
QQPctProteinOverall <-
  ggplot2::ggplot(MilkBreedFatProt.df,
                  aes(sample=PctProtein)) +
  stat_qq(color="red") +
  stat_qq_line(color="blue", size=1.25) +
  ggtitle("
% Protein QQ-Plot and QQ-Line, Overall") +
  labs(x = "\nTheoretical", y = "Percent\n") +
  theme_MacYates()

# Percent Protein by Breed (two breakouts)
QQPctProteinBreed.Recode <-
  ggplot2::ggplot(MilkBreedFatProt.df,
                  aes(sample=PctProtein)) +
  stat_qq(color="red") +
  stat_qq_line(color="blue", size=1.25) +
  facet_grid(. ~ Breed.recode) +
  ggtitle("
% Protein QQ-Plot and QQ-Line, by Dairy Breed") +
  labs(x = "\nTheoretical", y = "Percent\n") +
  theme(strip.text.x=element_text(face="bold", size=12,
                                  color="navyblue")) +
  theme(strip.background=element_rect(fill="wheat1")) +
  theme_MacYates()
par(ask=TRUE)
gridExtra::grid.arrange(
  QQPctFatOverall,
  QQPctFatBreed.Recode,
  QQPctProteinOverall,
  QQPctProteinBreed.Recode, ncol=2)

##
## 3.7 Statistical Test(s) ####

t.test(MilkBreedFatProt.df$PctButterfat ~ # Measured variable
         MilkBreedFatProt.df$Breed.recode, # Grouping variable
       alternative="two.sided", # Two-sided t-Test
       paired=FALSE, # Independent samples
       na.rm=TRUE, # Missing data
       var.equal=TRUE) # Equal variance

t.test(dat.df$Edad ~ # Measured variable
         dat.df$Procedencia, # Grouping variable
       alternative="two.sided", # Two-sided t-Test
       paired=FALSE, # Independent samples
       na.rm=TRUE, # Missing data
       var.equal=TRUE) # Equal variance

# Confirmation of outcomes for Percent Butterfat by Breed
onewaytests::describe(PctButterfat ~ Breed.recode,
                      data=MilkBreedFatProt.df)

onewaytests::describe(Edad ~ Procedencia,
                      data=dat.df)
# Generate descriptive statistics of PctButterfat by
# Breed.recode and confirm that the results are in close
# parity to prior outcomes.

onewaytests::st.test(PctButterfat ~ Breed.recode,
                     data=MilkBreedFatProt.df, alpha=0.05, na.rm=TRUE,
                     verbose=TRUE)

onewaytests::st.test(Edad ~ Procedencia,
                     data=dat.df, alpha=0.05, na.rm=TRUE,
                     verbose=TRUE)
# Perform a Student’s t-Test for two samples and confirm
# that the results are in close parity to prior outcomes

onewaytests::wt.test(PctButterfat ~ Breed.recode,
                     data=MilkBreedFatProt.df, alpha=0.05, na.rm=TRUE,
                     verbose=TRUE)

onewaytests::wt.test(Edad ~ Procedencia,
                     data=dat.df, alpha=0.05, na.rm=TRUE,
                     verbose=TRUE)
# Perform a Welch’s Unequal Variances t-Test for two
# samples and confirm that the results are in close
# parity to prior outcomes.

# Confirmation of outcomes for Percent Protein by Breed
onewaytests::describe(PctProtein ~ Breed.recode,
                      data=MilkBreedFatProt.df)

# Generate descriptive statistics of PctProtein by
# Breed.recode and confirm that the results are in close
# parity to prior outcomes.

onewaytests::st.test(PctProtein ~ Breed.recode,
                     data=MilkBreedFatProt.df, alpha=0.05, na.rm=TRUE,
                     verbose=TRUE)
# Perform a Student’s t-Test for two samples and confirm
# that the results are in close parity to prior outcomes.

onewaytests::wt.test(PctProtein ~ Breed.recode,
                     data=MilkBreedFatProt.df, alpha=0.05, na.rm=TRUE,
                     verbose=TRUE)
# Perform a Welch’s Unequal Variances t-Test for two
# samples and confirm that the results are in close
# parity to prior outcomes

## 3.8 Summary of Outcomes####
latticedensityButterfatBreed <-
  lattice::densityplot(~ MilkBreedFatProt.df$PctButterfat |
                         MilkBreedFatProt.df$Breed.recode, type="density", lwd=6,
                       par.strip.text=list(cex=1.15, font=2),
                       scales=list(cex=1.15),
                       main="Percent Butterfat by Dairy Cow Breed",
                       xlab=list("Percent Butterfat by Dairy Cow Breed:
Student’s t-Test p-value = 0.00000000000000201",
                                 cex=1.15, font=2),
                       xlim=c(0,6), ylim=c(0.00,2.75), # Note ranges
                       ylab=list("Density", cex=1.15, font=2),
                       aspect=0.25,
                       layout = c(1,2), # Note: 1 Column by 2 Rows.
                       col="dodgerblue")
latticedensityProteinBreed <-
  lattice::densityplot(~ MilkBreedFatProt.df$PctProtein |
                         MilkBreedFatProt.df$Breed.recode, type="density", lwd=6,
                       par.strip.text=list(cex=1.15, font=2),
                       scales=list(cex=1.15),
                       main="Percent Protein by Dairy Cow Breed",
                       xlab=list("Percent Protein by Dairy Cow Breed:
Student’s t-Test p-value = 0.0394",
                                 cex=1.15, font=2),
                       xlim=c(0,6), ylim=c(0.00,2.75), # Note ranges
                       ylab=list("Density", cex=1.15, font=2),
                       aspect=0.25,
                       layout = c(1,2), # Note: 1 Column by 2 Row.
                       col="dodgerblue")
par(ask=TRUE)
gridExtra:: grid.arrange(
  latticedensityButterfatBreed,
  latticedensityProteinBreed, ncol=2)

######################################################3

ggplotdensityButterfatBreed <-
ggplot(data = MilkBreedFatProt.df,
       aes(x = PctButterfat)) +
       geom_density() +
    facet_wrap(MilkBreedFatProt.df$Breed.recode) +
  labs(title = "Percent Butterfat by Dairy Cow Breed") +
  xlab("Percent Butterfat by Dairy Cow Breed:Student’s t-Test p-value = 0.00000000000000201")

ggplotdensityProtein <-
ggplot(data = MilkBreedFatProt.df,
       aes(x = PctProtein)) +
  geom_density() +
  facet_wrap(MilkBreedFatProt.df$Breed.recode) +
  labs(title = "Percent Protein by Dairy Cow Breed") +
  xlab("Percent Protein by Dairy Cow Breed:Student’s t-Test p-value = 0.0394")


par(ask=TRUE)
gridExtra:: grid.arrange(
  ggplotdensityButterfatBreed,
  ggplotdensityProtein, ncol=2)
# Combine 
# Combine different lattice plots.
                       
## 3.9 Addendum 1: t-Statistic v z-Statistic ####
Ad1BloodPressure.df <- read.table(textConnection("
Ad1Subject Ad1Gender Ad1Systolic
S01 Male 147
S02 Female 121
S03 Female 116
S04 Female 77
S05 Male 79
S06 Female 107
S07 Male 119
S08 Female 105
S09 Female 128
S10 Male 155
S11 Male 79
S12 Female 111
S13 Male 122
S14 Male 130
S15 Female 150
S16 Male 170
S17 Male 146
S18 Male 171
S19 Male 153
S20 Female 113
S21 Male 106
S22 Male 147
S23 Female 158
S24 Male 143
S25 Male 132
S26 Female 131
S27 Male 84
S28 Male 133
S29 Male 165
S30 Female 123
S31 Female 163
S32 Male 119
S33 Female 125
S34 Male 143
S35 Male 106
S36 Male 96
S37 Female 78
S38 Female 125
S39 Male 150
S40 Male 80"), header=TRUE)
getwd() # Identify the working directory
ls() # List objects
attach(Ad1BloodPressure.df) # Attach the data, for later use
str(Ad1BloodPressure.df) # Identify structure
glimpse(Ad1BloodPressure.df)
head(Ad1BloodPressure.df, n=3) # Show the head, 1st 3 cases
summary(Ad1BloodPressure.df) # Summary statistics

### 3.9.2 Calculate the t-Statistic ####
t.test(Ad1BloodPressure.df$Ad1Systolic ~ # Measured variable
         Ad1BloodPressure.df$Ad1Gender, # Grouping variable
       alternative="two.sided", # Two-sided t-Test
       paired=FALSE, # Independent
       na.rm=TRUE, # Missing data
       var.equal=TRUE) # Equal variance

### 3.9.3 Calculate the z-Statistic ####
coin::independence_test(Ad1Systolic ~ Ad1Gender,
                        data=Ad1BloodPressure.df)


## 3.10 Addendum 2: Parametric v Nonparametric Mann-Whitney U Test ####
onewaytests::mw.test(PctButterfat ~ Breed.recode,
                     data=MilkBreedFatProt.df, alpha=0.05, na.rm=TRUE,
                     verbose=TRUE)

onewaytests::mw.test(PctProtein ~ Breed.recode,
                     data=MilkBreedFatProt.df, alpha=0.05, na.rm=TRUE,
                     verbose=TRUE)


onewaytests::mw.test(NumeroIntentosPrevios ~ Sexo,
                     data=dat.df, alpha=0.05, na.rm=TRUE,
                     verbose=TRUE)

onewaytests::mw.test(NumeroIntentosPrevios ~ Sexo,
                     data=dat.df, alpha=0.05, na.rm=TRUE,
                     verbose=TRUE)


onewaytests::mw.test(NumeroIntentosPrevios ~ TieneEnfermedadMental,
                     data=dat.df, alpha=0.05, na.rm=TRUE,
                     verbose=TRUE)

## 3.11 Addendum 3: Additional Practice Datasets for Data ####
# with Normal Distribution Patterns and Data That
# Do Not Exhibit Normal Distribution Patterns
### 3.11.1 Data with Normal Distribution Patterns ####

base::set.seed(8) # Set the seed.
# See prior lessons why the set.seed() function
# is used to provide consistent outcomes that
# can be reproduced when constructing data that
# are user-created.

#  create a dataframe of Systolic Blood Pressure data for 480
# Females, SBPF.df
SBPF.df <- data.frame(
  Gender <- replicate(480, "F"),
  SBP <- round(rnorm(480, mean=118, sd=02))
)
colnames(SBPF.df) <- c("Gender","SBP")
str(SBPF.df)
names

attach(SBPF.df) # Attach the dataset
head(SBPF.df, 5) # Look at the first 5 rows of data
tail(SBPF.df, 5) # Look at the last 5 rows of data
summary(SBPF.df) # Summary descriptive statistics
glimpse(SBPF.df)

# Remove the individual object variables Gender and SBP since they are no longer
# needed and then use the ls() function to confirm that they no longer show in
# the working directory:

ls()
rm(Gender)
rm(SBP)
ls()

# create a dataframe of Systolic Blood Pressure data for
# 500 Males, SBPM.df.
SBPM.df <- data.frame(
  Gender <- replicate(500, "M"),
  SBP <- round(rnorm(500, mean=120, sd=16))
)
colnames(SBPM.df) <- c("Gender","SBP")
str(SBPM.df)
names(SBPM.df)

attach(SBPM.df) # Attach the dataset
head(SBPM.df, 5) # Look at the first 5 rows of data
tail(SBPM.df, 5) # Look at the last 5 rows of data
summary(SBPM.df) # Summary descriptive statistics
glimpse(SBPM.df)

ls()
rm(Gender)
rm(SBP)
ls()

# rbind function mixed the 2 datsets
SBPFM.df <- rbind(SBPF.df, SBPM.df)
attach(SBPFM.df) # Attach the dataset
head(SBPFM.df, 5) # Look at the first 5 rows of data
tail(SBPFM.df, 5) # Look at the last 5 rows of data
summary(SBPFM.df) # Summary descriptive statistics
glimpse(SBPFM.df)

# When viewing the data from the two original datasets (SBPF.df and SBPM.df),
# notice differences in length, mean, and sd:
length(SBPF.df$SBP); mean(SBPF.df$SBP); sd(SBPF.df$SBP)
length(SBPM.df$SBP); mean(SBPM.df$SBP); sd(SBPM.df$SBP)

#  Then, as a quality assurance check, look at the combined dataset of SBP values
# for both genders, to be sure that the descriptive statistics are in range:
length(SBPFM.df$SBP); mean(SBPFM.df$SBP); sd(SBPFM.df$SBP)

# EXIT --------------------------------------------------------------------

getwd() # Identify the current working directory.
ls() # List all objects in the working
# directory.
ls.str() # List all objects, with finite detail.
list.files() # List files at the PC directory.
save.image("Usi_R_for.rdata")
getwd() # Identify the current working directory.
ls() # List all objects in the working
# directory.
ls.str() # List all objects, with finite detail.
list.files() # List files at the PC directory.
alarm() # Alarm, notice of upcoming action.
q() # Quit this session.
# Prepare for Save workspace image? query.