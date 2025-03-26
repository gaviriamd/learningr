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
setwd("C:/Users/juand/iCloudDrive/Epidemiology/R Codes") #Copy & Paste the 
getwd() # Confirm the working directory.
load("C:/Users/juand/iCloudDrive/Epidemiology/R Codes/Data_Science.RData")
list.files() # List files at the PC directory.
.libPaths() # Library pathname.
.Library # Library pathname.
sessionInfo() # R version, locale, and packages.
search() # Attached packages and objects.
searchpaths() # Attached packages and objects.
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

# EXIT --------------------------------------------------------------------

getwd() # Identify the current working directory.
ls() # List all objects in the working
# directory.
ls.str() # List all objects, with finite detail.
list.files() # List files at the PC directory.
save.image("Data_Science.RData")
getwd() # Identify the current working directory.
ls() # List all objects in the working
# directory.
ls.str() # List all objects, with finite detail.
list.files() # List files at the PC directory.
alarm() # Alarm, notice of upcoming action.
q() # Quit this session.
# Prepare for Save workplace image? query.
# R ####
## 2.2 The very basics ####
### 2.2.1 Objects ####
coef_a <- 1
coef_b <- 1
coef_c <- -1

coef_a
print(coef_a)

### 2.2.2 The workspace ####
ls()

(-coef_b + sqrt(coef_b^2 - 4*coef_a*coef_c))/(2*coef_a)
(-coef_b - sqrt(coef_b^2 - 4*coef_a*coef_c))/(2*coef_a)

### 2.2.3 Prebuilt functions ####
log(8)
log(coef_a)

help("log")
?log

args(log)

log(8, base = 2)
log(x = 8, base = 2)

log(8, 2)

# If using the arguments’ names, then we can include them in whatever order we want:
# To specify arguments, we must use =, and cannot use <-.
log(base = 2, x = 8)

2^3

# You can see the arithmetic operators by typing:
help("+") 

# or
?"+"

# and the relational operators by typing:
help(">") 

# or
?">"

### 2.2.4 Prebuilt objects ####
data()

# R will show you Mauna Loa atmospheric CO2 concentration data.
co2

# Other prebuilt objects are mathematical quantities
pi
Inf + 1

### 2.2.5 Variable names ####
r_1 <- (-coef_b + sqrt(coef_b^2 - 4*coef_a*coef_c))/(2*coef_a)
r_2 <- (-coef_b - sqrt(coef_b^2 - 4*coef_a*coef_c))/(2*coef_a)

### 2.2.6 Saving your work space ####
save.image("Data_Science.RData")
load("Data_Science.RData")
### 2.2.6 Saving your workspace ####
coef_a <- 3
coef_b <- 2
coef_c <- -1
(-coef_b + sqrt(coef_b^2 - 4*coef_a*coef_c))/(2*coef_a)
(-coef_b - sqrt(coef_b^2 - 4*coef_a*coef_c))/(2*coef_a)

### 2.2.8 Commenting your code ####
## Code to compute solution to quadratic equation

## Define the variables
coef_a <- 3 
coef_b <- 2
coef_c <- -1

## Now compute the solution
(-coef_b + sqrt(coef_b^2 - 4*coef_a*coef_c))/(2*coef_a)
(-coef_b - sqrt(coef_b^2 - 4*coef_a*coef_c))/(2*coef_a)
## 2.3 Data types ####
a <- 2
class(a)

### 2.3.1 Data frames ####
class(murders)

### 2.3.2 Examining objects ####
str(murders)
glimpse(murders)
head(murders)

### 2.3.3 The accessor: $ ####
murders$population
names(murders)

### 2.3.4 Vectors ####
pop <- murders$population
length(pop)
class(pop) # NUMERIC
mean(pop)
median(pop)

class(murders$state) # CHARACTERS

# Another important type of vectors are logical vectors. These must be either TRUE or FALSE.
z <- 3 == 2
z
class(z) # LOGIC

# converting "1" in integer vector
class(1)
class(1L)
as.integer(1)
class(as.integer(1))

### 2.3.5 Factors ####
class(murders$region)

levels(murders$region)

# In the background, R stores these levels as integers and keeps a map to 
# keep track of the labels. This is more memory efficient than storing 
# all the characters.

# he levels of region ordered by the total number of murders

region <- murders$region
value <- murders$total
region <- reorder(region, value, FUN = sum)
levels(region)

region <- murders$region
value <- murders$total
region <- reorder(region, value, FUN = sum)
levels(region)

### 2.3.6 Lists ####
# Create a list.
record <- list(name = "John Doe",
               student_id = 1234,
               grades = c(95, 82, 91, 97, 93),
               final_grade = "A")
record

record$student_id #Or
record[["student_id"]]

# You might also encounter lists without variable names.

record2 <- list("John Doe", 1234)
record2

record2[[1]]

### 2.3.7 Matrices ####
mat <- matrix(1:12, 4, 3)
mat
mat[2, 3]
mat[2, ]
mat[ ,3]

# You can access more than one column or more than one row if you like. 
# This will give you a new matrix.
mat[, 2:3]

# You can subset both rows and columns:
mat[1:2, 2:3]

as.data.frame(mat)

murders[25, 1]

## 2.4 Vectors ####
### 2.4.1 Creating vectors ####
codes <- c(380, 124, 818)
codes

# character vectors
country <- c("italy", "canada", "egypt")

### 2.4.2. Names ####
codes <- c(italy = 380, canada = 124, egypt = 818)
codes

# If the use of strings without quotes looks confusing, know 
# that you can use the quotes as well:
codes <- c("italy" = 380, "canada" = 124, "egypt" = 818)
codes

# We can also assign names using the names functions:
codes <- c(380, 124, 818)
country <- c("italy","canada","egypt")
names(codes) <- country
codes

### 2.4.3 Sequences ####
# Another useful function for creating vectors generates sequences:
seq(1, 10)

# a third argument lets us tell it how much to jump by:
seq(1, 10, 2)

class(seq(1, 10, 0.5))

### 2.4.4 Subsetting ####
codes[2]

# You can get more than one entry by using a multi-entry vector as an index:
codes[c(1,3)]
codes[1:2]

codes["canada"]
codes[c("egypt","italy")]

### 2.5 Coercion ####
# you can turn numbers into characters with:
x <- 1:5
y <- as.character(x)
y

# You can turn it back with as.numeric:
as.numeric(y)

## 2.6 Not availables (NA) ####
x <- c("1", "b", "3")
as.numeric(x)

## 2.7 Sorting ####
### 2.7.1 sort ####
sort(murders$total)

### 2.7.2 order ####
x <- c(31, 4, 15, 92, 65)
sort(x)

index <- order(x)
x[index]

x
order(x)

murders$state[1:6]
murders$abb[1:6]

ind <- order(murders$total) 
murders$abb[ind]

### 2.7.3 max and which.max ####
library(dslabs)
max(murders$total)

# and which.max for the index of the largest value:
i_max <- which.max(murders$total)
murders$state[i_max]

nip_max <- which.max(dat.df$NumeroIntentosPrevios)
dat.df$NumeroIntentosPrevios[nip_max]

### 2.7.4 rank ####
x <- c(31, 4, 15, 92, 65)
rank(x)

## 2.8 Vector arithmetics ####
library(dslabs)
# We can quickly confirm that California indeed has the largest population:
murders$state[which.max(murders$population)]

### 2.8.1 Rescaling a vector ####
inches <- c(69, 62, 66, 70, 70, 73, 67, 73, 67, 70)

#  to convert to centimeters
inches * 2.54

# compute how many inches taller or shorter than 69 inches
inches - 69

### 2.8.2 Two vectors ####
murder_rate <- murders$total / murders$population * 100000
murders$abb[order(murder_rate)]

### 2.8.3 Beware of recycling ####
x <- c(1, 2, 3)
y <- c(10, 20, 30, 40, 50, 60, 70)
x + y

## 2.9 Indexing ####
library(dslabs)

### 2.9.1 Subsetting with logicals ####
murder_rate <- murders$total / murders$population * 100000 
ind <- murder_rate < 0.71
ind

murders$state[ind]
sum(ind)

### 2.9.2 Logical operators ####
TRUE & TRUE
TRUE & FALSE
FALSE & FALSE

# For our example, we can form two logicals:
west <- murders$region == "West"
safe <- murder_rate <= 1

ind <- safe & west
murders$state[ind]

### 2.9.3 which ####

# to look up California’s murder 
# rate it is convenient to convert vectors of logicals into indexes
ind <- which(murders$state == "California")
murder_rate[ind]

### 2.9.4 match ####
# find out the murder rates for several states
ind <- match(c("New York", "Florida", "Texas"), murders$state)
ind

# Now we can look at the murder rates:
murder_rate[ind]
