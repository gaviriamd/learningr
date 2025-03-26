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
setwd("C:/Users/juand/iCloudDrive/Documents/Codes & Projects") #Copy & Paste the 
load("Sta_The_Art.rdata")
getwd() # Confirm the working directory.
list.files() # List files at the PC directory.
.libPaths() # Library pathname.
.Library # Library pathname.
sessionInfo() # R version, locale, and packages.
search() # Attached packages and objects.
searchpaths() # Attached packages and objects.
par("mar") # Confirm default margin (BLTR)
par(mar = c(5.1, 4.1, 4.1, 2.1)) #adjust plot margins

# OPEN DATASET % MAKE IT READEABLE #######
dat.df <- utils::read.table (file =
                               "INTSUIDATASET.csv",
                             header=TRUE, dec=".", sep=",")

# Read a data set
dat.df <- read.csv("~/Research/2020 - Intento de Suicidio HSLV 2019/INTSUIDATASET.csv", stringsAsFactors=TRUE)

getwd() # Identify the working directory
ls() # List objects
attach(dat.df) # Attach the data, for later use

##  Import a .csv File of  from an Online Source into R ####
height.df <- data.table::fread(
  "https://img1.wsimg.com/blobby/go/bbca5dba-4947-4587-b40a-db346c01b1b3/downloads/heights.csv?ver=1709965708065"
)
# Section 2.3 Measuring the Center of Quantitative Data ####
## 2.34 More on CO2 emissions ####
countries <- c("China", "United States", "India", "Russia", "Japan", "Germany", "Korea", "Canada", "Iran", "Saudi Arabia")
emissions <- c(8, 5.3, 1.8, 1.7, 1.2, 0.8, 0.6, 0.5, 0.4, 0.4)
CO2_emissions <- data.frame(countries, emissions)
mean(CO2_emissions$emissions)
median(CO2_emissions$emissions)

## 2.35 Resistance to an outlier####
set_1 <- c(8, 9, 10, 11, 12)
set_2 <- c(8, 9, 19, 11, 100)
set_3 <- c(8, 9, 10, 11, 1000)
mean(set_1)
mean(set_2)
mean(set_3)
median(set_1)
median(set_2)
median(set_3)

## 2.36 Weekly earnings and gender ####

## 2.37 Labour dispute ####

## 2.38 Cereal sodium ####

## 2.39 Center of plots ####
Set_1 <- c(4,10,9,6,2,2,1,1,1)
Set_a <- c("1","2","3","4","5","6","7","8","9")

Set_2 <- c(2,9,5,2,1,2,5,9,2)
Set_b <- c("1","2","3","4","5","6","7","8","9")

Set_3 <- c(1,2,5,8,11,8,5,2,1)
Set_c <- c("1","2","3","4","5","6","7","8","9")

Set_1a <- as.data.frame(Set_1, Set_a)
Set_2b <- as.data.frame(Set_2, Set_b)
Set_3c <- as.data.frame(Set_3, Set_c)

mean(Set_1a$Set_1)
mean(Set_2b$Set_2)
mean(Set_3c$Set_3)

median(Set_1a$Set_1)
median(Set_2b$Set_2)
median(Set_3c$Set_3)

## 2.40 Public transportation-center ####
mile <- c(0, 0, 4, 0, 0, 0, 10, 0, 6, 0)
employee <- c("1", "2", "3", "4", "5","6","7","8","9","10")
mile_employee <- as.data.frame(mile, employee)

mean(mile_employee$mile)
median(mile_employee$mile)

mile90 <- c(0, 0, 4, 0, 0, 0, 10, 0, 6, 0, 90)
employee90 <- c("1", "2", "3", "4", "5","6","7","8","9","10", "11")
mile90_employee90 <- as.data.frame(mile90, employee90)

mean(mile90_employee90$mile90)
median(mile90_employee90$mile90)
## 2.41 Public transportation-outlier ####
mile9 <- c(0, 0, 4, 0, 0, 0, 10, 0, 6, 0)
mean(mile9)
median(mile9)

mile90 <- c(0, 0, 4, 0, 0, 0, 10, 0, 6, 0, 90)
mean(mile90)
median(mile90)

mile901 <- c(0, 0, 4, 0, 0, 0, 10, 0, 6, 0, 90, 0, 0, 4, 0, 0, 0, 10, 0, 6, 0)
mean(mile901)
median(mile901)

## 2.42 Baseball salaries ####
mean(NY_Sal$salary)
median(NY_Sal$salary)

head(NY_Sal$salary)
tail(NY_Sal$salary)

## 2.43 More baseball salaries ####
## 2.44 Fertility statistics ####
mean(Fert$V4)
median(Fert$V4)

fertility_rates <- c("Austria" = 1.5, "Belgium" = 1.7, "Denmark" = 1.8, 
                     "Findland" = 1.6, "France" = 1.9, "Germany" = 1.6,
                     "Greece" = 1.4, "Ireland" = 1.8, "Italy" = 1.3, 
                     "Netherlands" = 1.7, "Norway" = 1.7, "Spain" = 1.3,
                     "Sweden" = 1.6, "Switzerland" = 1.5, 
                     "United Kingdom" = 1.8, "United States" = 1.8,
                     "Canada" = 1.5, "Mexico" = 2.2)

fertility_rates
median(fertility_rates)
mean(fertility_rates)

## 2.45 Dining out ####

# I coudn´t do it

## 2.46 Milk drinking habits of 12- to 48-month-olds ####
Num_of_kid <- data.frame(Num_of_tim = c(1, 2, 3), twe_to_twe = c(48, 281, 373), For_mon_old = c(66, 74, 32) )
summary(Num_of_kid)

## 2.47 Accidents ####

###  Example 12: Standard Deviation ####

# Read in values:
men <- c(0, 0, 0, 2, 4, 4, 4)
women <- c(0, 2, 2, 2, 2, 2, 4)

# Dotplot for values for men
library(ggplot2)
ggplot(data.frame(men), aes(x = men)) + 
  geom_dotplot() +
  labs(x = '',
       title = 'Dotplot', subtitle = 'Men') +
  theme_classic() +
  theme(axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank()
  )

# Dotplot for values for women
ggplot(data.frame(women), aes(x = women)) + 
  geom_dotplot() +
  labs(x = '',
       title = 'Dotplot', subtitle = 'Women') +
  theme_classic() +
  theme(axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank()
  )

# To find the Standard Deviation
sd(men)
sd(women)


### Example 14: Female Student Heights ####
Heights.df <- read.csv("https://img1.wsimg.com/blobby/go/bbca5dba-4947-4587-b40a-db346c01b1b3/downloads/heights.csv?ver=1709965708065")


# Section 2.4 Measuring the Variability of Quantitative Data ####
## Figure 2.13 Histogram of Female Student Height Data. ###
ggplot(height.df) +
       geom_histogram(aes(x = HEIGHT))
       
# EXIT --------------------------------------------------------------------

getwd() # Identify the current working directory.
ls() # List all objects in the working
# directory.
ls.str() # List all objects, with finite detail.
list.files() # List files at the PC directory.
save.image("Sta_The_Art.rdata")
getwd() # Identify the current working directory.
ls() # List all objects in the working
# directory.
ls.str() # List all objects, with finite detail.
list.files() # List files at the PC directory.
alarm() # Alarm, notice of upcoming action.
q() # Quit this session.
# Prepare for Save workspace image? query.