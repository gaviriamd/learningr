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
setwd("C:/Users/juand/iCloudDrive/Documents/Codes & Projects")
getwd() # Identify the current working directory.
load("R_Cookbook.rdata")
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

# OPEN DATASET ####
dat.df <- read.csv("C:/Users/juand/OneDrive - unicauca.edu.co/2020 - Intento de Suicidio HSLV 2019/Informe Final/INTSUIDATASET.csv", stringsAsFactors=TRUE)
View(dat.df)
# 2.02 Setting variables ####
x <- 3
y <- 4
z <- sqrt(x^2 + y^2)
print(z)

x <- c("fee", "fie", "foe", "fum")

x <<- 3

foo <- 3
5 -> fum

# 2.03 Listing Variables ####
x <- 10
y <- 50
z <- c("three", "blind", "mice")
f <- function(n, p) sqrt(p * (1 - p) / n)
ls.str()
ls(all.names = TRUE)

# 2.04 Deleting Variables ####
rm(x)
x
rm(x, y, z)
rm(list = ls()) #Removes all the enviroment

# 2.05 Creating a Vector ####
c(1, 1, 2, 3, 5, 8, 13, 21)
c(1 * pi, 2 * pi, 3 * pi, 4 * pi)
c("My", "twitter", "handle", "is", "@cmastication")
c(TRUE, TRUE, FALSE, TRUE)

v1 <- c(1, 2, 3)
v2 <- c(4, 5, 6)
c(v1, v2)

mode(3.1415)
mode("foot")

c(3.1415, "foo")
mode(c(3.1415, "foo"))

# 2.06 Computing Basic Statistics ####
x <- c(0, 1, 1, 2, 3, 5, 8, 13, 21, NA)
mean(x, na.rm = TRUE) #mean
median(x, na.rm = TRUE) #median
sd(x, na.rm = TRUE) #standard deviation
var(x, na.rm = TRUE) #variance
IQR(x, na.rm = TRUE)

x <- c(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
y <- log(x + 1)
cor(x, y) #correlation
cov(x, y) #covariance

library(tidyverse)
data(cars)
map_dbl(cars, mean) #map_dbl applies the function "mean" to each column of a data frame
map_dbl(cars, sd)
map_dbl(cars, median)
map_dbl(cars, IQR)

var(cars)
cor(cars)
cov(cars)

# 2.07 Creating Sequences ####
1:5
seq(from = 1, to = 5, by = 2)
rep(1, times = 5)
0:9 # colon ":" creates a vector containig the sequence n, n+1, n+2...
10:19
9:0

10:20 %>% mean() #This pass data to another function

## sequences increment by integers ####
seq(from = 0, to = 20)
seq(from = 0, to = 20, by = 2)
seq(from = 0, to = 20, by = 5)

seq(from = 0, to = 20, length.out = 5)
seq(from = 0, to = 100, length.out = 5)

## sequences increment by fractions ####
seq(from = 1.0, to = 2.0, length.out = 5)

## Repeated value sequence ####
rep(pi, times = 5)

# 2.08 Comparing Vectors ####
a <- 3
a == pi # Test for equality
a != pi # Test for inequality
a < pi
a > pi
a <= pi
a >= pi

## compare entire vectors at once ####
v <- c(3, pi, 4)
w <- c(pi, pi, pi)
v == w # Compare two 3-element vectors
v != w
v < w
v <= w
v > w
v >= w

## compare a vector against a single scalar ####
v <- c(3, pi, 4)
v == pi # Compare a 3-element vector against one number
v != pi

## to know whether any of the comparisons were true or whether all the comparisons were true ####
v <- c(3, pi, 4)
any(v == pi) # Return TRUE if any element of v equals pi
all(v == 0) # Return TRUE if all elements of v are zero

# 2.09 Selecting Vector Elements ####
## Use a bracket to select element by their position ####
fib <- c(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
fib
fib[1] # show 1st element of fib
fib[2]
fib[3]
fib[4]
fib[5]

fib[1:3] # Select elements 1 through 3
fib[4:9] # Select elements 4 through 9

fib[c(1, 2, 4, 8)]

fib[-1] # Ignore first element

fib[-(1:3)] # Invert sign of index to exclude instead of select

fib < 10 # This vector is TRUE wherever fib is less than 10
fib[fib < 10] # Use that vector to select elements less than 10
fib %% 2 == 0 # This vector is TRUE wherever fib is even
fib[fib %% 2 == 0] # Use that vector to select the even elements

dat.df$Edad < 29 # This vector is TRUE wherever fib is less than 10
dat.df$Edad [dat.df$Edad < 29] # Use that vector to select elements less than 10
dat.df$Edad %% 2 == 0 # This vector is TRUE wherever fib is even
dat.df$Edad [dat.df$Edad %% 2 == 0] # Use that vector to select the even elements

dat.df$Edad [dat.df$Edad %% 5 == 0] # Use that vector to select elements by 5

## select all elements greater than the median: ####
v <- c(3, 6, 1, 9, 11, 16, 0, 3, 1, 45, 2, 8, 9, 6, -4)
v[ v > median(v)] # select all elements greater than the median:

dat.df$Edad [ dat.df$Edad > median(dat.df$Edad)] # select all elements greater than the median:

## select all elements in the lower and upper 5%: ####
# "|" operator means "or" when indexing, "&" means "and"
# | = or
# & = and
dat.df$Edad[ (dat.df$Edad < quantile(dat.df$Edad, 0.05)) | (dat.df$Edad > quantile(dat.df$Edad, 0.95)) ]

## select all elements that exceed ±1 standard deviations from the mean: ####
dat.df$Edad [ abs(dat.df$Edad - mean(dat.df$Edad)) > sd(dat.df$Edad)]

## select all elements that are neither NA nor NULL: ####
dat.df$Etnia [!is.na(dat.df$Etnia) & !is.null(dat.df$Etnia)]

## define the names by assigning a vector of character strings to the attribute: ####
years <- c(1960, 1964, 1976, 1994)
names(years) <- c("Kennedy", "Johnson", "Carter", "Clinton")
years

## refer to individual elements by name: ####
years["Carter"]
years["Clinton"]

## generalizes to allow indexing by vectors of names ####
years[c("Carter", "Clinton")]

# 2.10 Performing Vector Arithmetic ####
dat.df$Edad + dat.df$NumeroIntentosPrevios
dat.df$Edad - dat.df$NumeroIntentosPrevios
dat.df$Edad * dat.df$NumeroIntentosPrevios
dat.df$Edad / dat.df$NumeroIntentosPrevios
dat.df$Edad ^ dat.df$NumeroIntentosPrevios

dat.df$Edad + 2
dat.df$Edad - 2
dat.df$Edad * 2
dat.df$Edad / 2
dat.df$Edad ^ 2

## recenter an entire vector in one expression by subtracting the mean of its content ####
mean(dat.df$Edad)
dat.df$Edad - mean(dat.df$Edad)

## calculate the z-sore: subtract the mean and divide by the standard deviation:####
sd(dat.df$Edad)
(dat.df$Edad - mean(dat.df$Edad)) / sd(dat.df$Edad)

## Functions sqr, sin and log ####
sqrt(dat.df$Edad) #computes the (principal) square root of 
log(dat.df$Edad) #og computes logarithms, by default natural logarithm
sin(dat.df$Edad)


## 2.11 Getting Operator Precedente Right ####
%%
 # Modulo operator
%/%
  #Integer division
%*%
  #Matrix multiplication
%in%
  #Returns TRUE if the left operand occurs in its right operand; FALSE otherwise
%>%
  #Pipe that passes results from the left to a function on the right
## 2.12 Typing Less and Accomplishing More ####
## 2.13 Creating a Pipeline of Function Calls ####

# Use the pipe operator to combine multiple functions together into a 
# “pipeline” of functions without intermediate variables:
  
dat.df %>%
  filter(NumeroIntentosPrevios > 3) %>%
  head(3) %>%
  print()

# Using the pipe is much cleaner and easier to read than
# using intermediate temporary variables:
temp1 <- filter(dat.df, Edad > 45)
temp2 <- head(temp1, 3)
print(temp2)

# filter to limit our data to values, then select to keep only certain 
# variables, followed by ggplot to create a simple plot

dat.df %>%
  filter(Edad > 21) %>%
  select(Edad, EstanciaHospitalaria) %>%
  ggplot(aes(Edad, EstanciaHospitalaria)) + geom_point()


# the argument going into your target (righthand side) function to be
# somewhere other than the first argument, use the dot (.) operator. 
dat.df %>% head(3, x = .)

## 2.14 Avoiding some Common Mistakes ####
# using = instead of ==
v = 2024 # this assigns 2024 to v
# Installing a package but not loading it with library or require
library(MASS) # Load the MASS package into R
x <- rnorm(100)
n <- 5
truehist(x, n)
# Writing lst[n] when you mean lst[[n]] or vice versa
lst[[x]] #is the nth element of the list
lst[x] #is a list whose only element is the nth element of lst.
# Passing multiple arguments to a single-argument function
mean(9, 10, 11) # 2nd & 3rd argument are being interpreted as other positional argumentx
mean(c(9,10,11)) # Use "c" operator to pass multiple items into a single one

## 3.1 Getting and Setting the Working Directory ####
getwd()
setwd("~/Documents/MyDirectory")
## 3.2 Creating a New RStudio Project ####
## 3.3 Saving Your Workspace ####
save.image()
## 3.4 Viewing Your Command History ####
history()
history(100) # Show 100 most recent lines of history
history(Inf) # Show entire saved history
## 3.5 Saving the Result of the Previous Command ####
aVeryLongRunningFunction() # Oops! Forgot to save the result!
x <- .Last.value # Capture the result now
mean(dat.df$Edad)
## 3.6 Displaying Loaded Packages via the Search Path ####
search()
## 3.7 Viewing the List of Installed Packages ####
library()
installed.packages()[1:5, c("Package", "Version")]
## 3.8 Accessing the Functions in a Package ####
library(packagename)
lda(x)
my_model <-
  lda(cty ~ displ + year, data = mpg)
library(MASS) 

# The detach function will unload a package that is currently loaded:
detach(package:MASS) # use it to unhide functions

## 3.9 Accessing Built-in Datasets ####
# To access datasets in other packages, use the data function while giving the dataset
# name and package name:
data(dsname, package = "pkgname")

head(pressure) # this is native in R
help(pressure) # Bring up help page for pressure dataset
data() # Bring up a list of dataset

data(Cars93, package = "MASS")
head(Cars93)
summary(Cars93)
glimpse(Cars93)

# see a list of available datasets in MASS, or any other package, by using the data
# function with a package argument and no dataset name:
data(package = "MASS")


## 3.10 Installing Packages from CRAN ####
install.packages("packagename")

# to control the path for your library location, you can use the lib argument 
# of the install.pack ages function:
install.packages("packagename", lib = "~/lib/R")

## 3.11 Installing a Package from GitHub ####
install.packages("devtools")
library(devtools)

install_github("thomasp85/tidygraph") #to install Thomas Lin Pederson’s tidygraph package

## 3.12 Setting or Changing a Default CRAN Mirror ####
# running R without RStudio
# 1. Call the chooseCRANmirror function:
  chooseCRANmirror()
# R will present a list of CRAN mirrors.
# 2. Select a CRAN mirror from the list and press OK.
# 3. To get the URL of the mirror, look at the first element of the repos option:
  options("repos")[[1]][1]
# 4. Add this line to your .Rprofile file. If you want the RStudio CRAN mirror, you
# would do the following:
options(repos = c(CRAN = "http://cran.rstudio.com"))
# Or you could use the URL of another CRAN mirror.
  



## 3.13 Running a Script ####
source("myScript.R")
print("Hello, World!")
source("hello.R")
source("hello.R", echo = TRUE)
## 3.14 Running a Batch Scrip ####
R CMD BATCH scriptfile outputfile
Rscript scriptfile arg1 arg2 arg3

#  silences the startup messages that would other‐
# wise clutter the output:
R CMD BATCH --quiet myScript.R results.out

# Like --quiet, but it makes R even more silent by inhibiting echo of the input.
--slav

# At startup, do not restore the R workspace. This is important if your script
# expects R to begin with an empty workspace.
--no-restore

# At exit, do not save the R workspace. Otherwise, R will save its workspace and
# overwrite the .RData file in the working directory
--no-save

# Do not read either the .Rprofile or the ~/.Rprofile file.
--no-init-file

# NO TERMINÉ DE LLENARLO, NO ENTIENDO PARA QUÉ SIRVE

## 3.15 Locating the R Home Directory ####
Sys.getenv("R_HOME")
# R creates an environment variable called R_HOME

## 3.16 Customizing R Startup ####
# Create a script called .Rprofile that customizes your R session. R will execute
# the .Rprofile script when it starts. The placement of .Rprofile depends upon your plat‐
# form
.Rprofile
## 3.17 Using R and RStudio in the Cloud ####


# 4 Input and Output ####
## 4.1 Entering Data from the Keyboard ####
# For very small datasets, enter the data as literals using the c constructor for vectors:
scores <- c(61, 66, 90, 88, 100)

# You can use this approach for data frames, too, by entering each variable (column) as
# a vector:
points <- data.frame(
  label = c("Low", "Mid", "High"),
  lbound = c(0, 0.67, 1.64),
  ubound = c(0.67, 1.64, 2.33)
)

## 4.2 Printing Fewer Digits (or More Digits)####
print(pi, digits = 4)
print(100 * pi, digits = 4)

# use the format function to format your numbers before calling cat:
cat(pi, "\n")
cat(format(pi, digits = 4), "\n")

# This is R, so both print and format will format entire vectors at once:
print(pnorm(-3:3), digits = 2)
format(pnorm(-3:3), digits = 2)

q <- seq(from = 0, to = 3, by = 0.5)
tbl <- data.frame(Quant = q,
                  Lower = pnorm(-q),
                  Upper = pnorm(q))
tbl # Unformatted print

print(tbl, digits = 2)

# alter the format of all output by using the options function to change
# the default for digits:
options(digits = 15)  


## 4.3 Redirecting Output to a File ####
# redirect the output of the cat function by using its file argument:
cat("The answer is", answer, "\n", file = "filename.txt")

sink("filename") # Begin writing output to file
# ... other session work ...
sink() # Resume writing output to console

sink("script_output.txt") # Redirect output to file
source("script.R") # Run the script, capturing its output
sink() # Resume writing output to console

cat(data, file = "analysisReport.out")
cat(results, file = "analysisRepart.out", append = TRUE)
cat(conclusion, file = "analysisReport.out", append = TRUE)

con <- file("analysisReport.out", "w")
cat(data, file = con)
cat(results, file = con)
cat(conclusion, file = con)
close(con)

## 4.4 Listing ####
list.files()
list.files(path = 'data/') # show files in a directory

# To see all the files in your subdirectories, too, use:
list.files(recursive = T)

list.files(path = 'data/', all.files = TRUE)

## 4.5 Dealing with “Cannot Open File” in Windows ####
samp <- read_csv("C:\Data\sample-data.csv")
samp <- read_csv("C:/Data/sample-data.csv")
samp <- read_csv("C:\\Data\\sample-data.csv")

## 4.6 Reading Fixed-Width Records ####
# DIDN´T WORK AT ALL

## 4.8 Reading from CSV Files ####
library(tidyverse)
tbl <- read_csv("datafile.csv")
# If your CSV file does not contain a header line, set the col_names option to FALSE:
  tbl <- read_csv("datafile.csv", col_names = FALSE)
  
na = c("", "NA") #  Indicates what values represent missing or NA values
comment = "" #  Indicates which lines to ignore as comments or metadata
trim_ws = TRUE # Indicates whether to drop whitespace at the beginning and/or end of fields
skip = 0  # Indicates the number of rows to skip at the beginning of the file
guess_max = min(1000, n_max) # Indicates the number of rows to consider when imputing column types
  
# If you have a data file that uses semicolons (;) for separators and commas for the dec‐
# imal mark, as is common outside of North America, you should use the function
read_csv2
  
## 4.9 Writing to CSV Files ####
write_csv(dat.df, path = "outfile.csv")

write_csv(tab1, "./data/tab1.csv", 
          col_names = TRUE, 
          col_types = NULL, 
          na = c("", "NA"), 
          comment = "", 
          trim_ws = TRUE,
          skip = 0, 
          guess_max = min(1000, n_max))

# I DIDN´T WORK FOR ME

## 4.10 Reading Tabular or CSV Data from the Web ####
berkley <- read_csv('http://bit.ly/barkley18', comment = '#')
tbl <- read_table("ftp://ftp.example.com/download/data.txt")

## 4.11 Reading Data from Excel ####
df1 <- read.xlsx(xlsxFile = "out_file.xlsx",
                 sheet = "some_sheet")

df1 <- read.xlsx(xlsxFile = "data/iris_excel.xlsx",
                 sheet = 'iris_data')
head(df1, 3)

# First we load the entire workbook into R:
  library(openxlsx)
wb <- loadWorkbook("data/excel_table_data.xlsx")

tables <- getTables(wb, 'input_data')
table_range_str <- names(tables[tables == 'example_table'])
table_range_refs <- strsplit(table_range_str, ':')[[1]]

# use a regex to extract out the row numbers
table_range_row_num <- gsub("[^0-9.]", "", table_range_refs)
# extract out the column numbers
table_range_col_num <- convertFromExcelRef(table_range_refs)

df <- read.xlsx(
  xlsxFile = "data/excel_table_data.xlsx",
  sheet = 'input_data',
  cols = table_range_col_num[1]:table_range_col_num[2],
  rows = table_range_row_num[1]:table_range_row_num[2]
)

## 4.12 Writing a Data Frame to Excel ####
write.xlsx(dat.df,
           sheetName = "some_sheet",
           file = "out_file.xlsx")

wb <- loadWorkbook("out_file.xlsx")

# Before we delete the table, we want to extract the table’s starting row and column:
tables <- getTables(wb, 'input_data')
table_range_str <- names(tables[tables == "example_table"])
table_range_refs <- strsplit(table_range_str, ":")[[1]]

# use a regex to extract out the starting row number
table_row_num <- gsub("[^0-9.]", "", table_range_refs)[[1]]

# extract out the starting column number
table_col_num <- convertFromExcelRef(table_range_refs)[[1]]

#Then we can use the removeTable function to remove the existing named Excel table:
  removeTable(wb = wb,
              sheet = 'input_data',
              table = 'example_table')

# Now we can use writeDataTable to write the iris data frame (which comes with R)
# back into our workbook object in R:
  writeDataTable(
    wb = wb,
    sheet = 'input_data',
    x = iris,
    startCol = table_col_num,
    startRow = table_row_num,
    tableStyle = "TableStyleLight9",
    tableName = 'example_table'
    )
  
# save some metadata in the workbook to let others know exactly
# when the data was refreshed.
  writeData(
    wb = wb,
    sheet = 'input_data',
    x = paste('example_table data refreshed on:', Sys.time()),
    startCol = 2,
    startRow = 5
  )
# then save the workbook
saveWorkbook(wb = wb,
             file = "data/excel_table_data.xlsx",
             overwrite = TRUE)

#
## 4.13 Reading Data from a SAS file ####
library(haven)
sas_movie_data <- read_sas("data/movies.sas7bdat")

sapply(sas_movie_data, attributes)

## 4.14 Reading Data from HTML Tables ####

library(rvest)
library(tidyverse)

all_tables <-
  read_html("URL") %>%
  html_table(fill = TRUE, header = TRUE)

# Here we can extract all tables from a Wikipedia article:
all_tables <-
  read_html("https://en.wikipedia.org/wiki/Aviation_accidents_and_incidents") %>%
  html_table(fill = TRUE, header = TRUE)

# To pull a single table from that list, you can use the function extract2 from the magrittr package:
out_table <-
all_tables %>%
magrittr::extract2(2)

head(out_table)

url <- 'http://en.wikipedia.org/wiki/World_population'
tbls <- read_html(url) %>%
  html_table(fill = TRUE, header = TRUE)

length(tbls)

library(magrittr)
tbl <- tbls %>%
  extract2(6)


## 4.15 Reading Files with a Complex Structure ####
lines <- readLines("input.txt")
lines <- readLines("input.txt", n = 10) # Read 10 lines and stop

what=numeric(0)
# Interprets the next token as a number

what=integer(0)
# Interprets the next token as an integer

what=complex(0)
# Interprets the next token as a complex number

what=character(0)
# Interprets the next token as a character string

what=logical(0)
# Interprets the next token as a logical value

# Example 2355.09 2246.73 1738.74 1841.01 2027.85
singles <- scan("./data/singles.txt", what = numeric(0))
singles

# Example:
# 15-Oct-87 2439.78 2345.63 16-Oct-87 2396.21 2207.73
# 19-Oct-87 2164.16 1677.55 20-Oct-87 2067.47 1616.21
# 21-Oct-87 2081.07 1951.76
triples <-
  scan("./data/triples.txt",
       what = list(character(0), numeric(0), numeric(0)))
triples

#Give names to the list elements, and scan will assign those names to the data:
triples <- scan("./data/triples.txt",
                what = list(
                  date = character(0),
                  high = numeric(0),
                  low = numeric(0)
                ))
triples

df_triples <- data.frame(triples)
df_triples

n=number # Stop after reading this many tokens. (Default: stop at end of file.)

nlines=number #Stop after reading this many input lines. (Default: stop at end of file.)

skip=number #Number of input lines to skip before reading data.

na.strings=list #A list of strings to be interpreted as NA.

world.series <- scan(
  "http://lib.stat.cmu.edu/datasets/wseries",
  skip = 35,
  nlines = 23,
  what = list(year = integer(0),
              pattern = character(0)),
)

world.series$year #dataset is organized by columns and so the years appear in a strange order:

# We can fix that by sorting the list elements according to year:
perm <- order(world.series$year)
world.series <- list(year = world.series$year[perm],
                     pattern = world.series$pattern[perm])

# Now the data appears in chronological order:
world.series$year

world.series$pattern

## 4.16 Reading from MySQL Databases ####
# I didn´t study it. 

## 4.17 Accessing a Database with dbplyr####
# I didn´t study it. 

## 4.18 Saving and Transporting Objects ####
save(tbl, t, file = "myData.RData")
load("myData.RData")

# To save in an ASCII format, use dput or dump instead:
dput(tbl, file = "myData.txt")
dump("tbl", file = "myData.txt")

myData <- load("myData.RData") # Achtung! Might not do what you think

# Let’s look at what myData is:
myData
str(myData)


# 5 Data Structures ####
## Vectors ####
# All elements of a vector must have the same type or, in R terminology, the same mode.
v <- c(10, 20, 30)
names(v) <- c("Moe", "Larry", "Curly")
print(v)
v[["Larry"]]

dat.df[["Edad"]] # or
dat.df$Edad

## List ####
# list elements may have different modes.

## Mode: Physical Type ####
# gives us this information:
mode(3.1415) # Mode of a number
mode(c(2.7182, 3.1415)) # Mode of a vector of numbers
mode("Moe") # Mode of a character string
mode(list("Moe", "Larry", "Curly")) # Mode of a list

# In a vector, all elements must have the same mode.
# In a list, the elements can have different mode

d <- as.Date("2010-03-15")
mode(d)
length(d)
class(d)
## Class: Abstract Type ####
d <- as.Date("2010-03-15")
mode(d)
length(d)
class(d)

## Scalars ####
# Since a scalar is a one-element vector, you can use vector functions on 
pi
length(pi)
pi[1]
pi[2]

## Matrices ####
# In R, a matrix is just a vector that has dimensions.
A <- 1:6
dim(A)
print(A)

# Watch what happens when we set our vector dimensions to 2 × 3 and print it:
dim(A) <- c(2, 3)
print(A)

B <- list(1, 2, 3, 4, 5, 6)
dim(B)
dim(B) <- c(2, 3)
print(B)

## Arrays ####
# The following example creates a three-dimensional array with dimensions 2 × 3 × 2:
D <- 1:12
dim(D) <- c(2, 3, 2)
print(D)

# This code snippet creates a matrix that is a mix of numeric and character data:
C <- list(1, 2, 3, "X", "Y", "Z")
dim(C) <- c(2, 3)
print(C)

## Factors ####
# There was no code. 

## Data Frames ####
dat.df$Edad
dat.df["Edad"]
dat.df[["Edad"]]

# You can use matrix-like notation
dat.df["Edad", "Procedencia"]
dat.df["Edad",]
dat.df[, "Procedencia"]

## Tibbles ####
## 5.1 Appending Data to a Vector ####
v <- c(1, 2, 3)
newItems <- c(6, 7, 8)
c(v, newItems)

# For a single item, you can also assign the new item to the next vector element. 
# R will automatically extend the vector:
v <- c(1, 2, 3)
v[length(v) + 1] <- 42
v

# create a new vector using the vector constructor (c) to join the old and new data.
v <- c(1, 2, 3)
v <- c(v, 4) # Append a single value to v
v
#> [1] 1 2 3 4

w <- c(5, 6, 7, 8)
v <- c(v, w) # Append an entire vector to v
v

# append an item by assigning it to the position past the end of the vector
v <- c(1, 2, 3)   # Create a vector of three elements
v[10] <- 10       # Assign to the 10th element
v                 # R extends the vector automatically

## 5.2 Inserting Data into a Vector ####
# append(vec,newvalues, after =n)
append(1:10, 99, after = 5)

# The special value of after=0 means insert the new items at the head of the vector:
append(1:10, 99, after = 0)

## 5.3 Understanding the Recycling Rule ####
# in operations
(1:6) + (1:3)

# Functions
cbind(1:6)
cbind(1:3)
cbind(1:6, 1:3)

(1:6) + (1:5)

(1:6) + 10

## 5.4 Creating a Factor (Categorical Variable) ####
f <- factor(v) # v can be a vector of strings or integers
f <- factor(v, levels)

f <- factor(c("Win", "Win", "Lose", "Tie", "Win", "Lose"))
f

wday <- c("Wed", "Thu", "Mon", "Wed", "Thu",
          "Thu", "Thu", "Tue", "Thu", "Tue")
f <- factor(wday)
f

f <- factor(wday, levels=c("Mon", "Tue", "Wed", "Thu", "Fri"))
f

## 5.5 Combining Multiple Vectors into One Vector and a Factor ####
v1 <- c(1, 2, 3)
v2 <- c(11, 12, 13)
v3 <- c(21, 22, 23)

comb <- stack(list(v1 = v1, v2 = v2, v3 = v3)) # Combine 3 vectors
comb

freshmen <- c(1, 2, 1, 1, 5)
sophomores <- c(3, 2, 3, 3, 5)
juniors <- c(5, 3, 4, 3, 3)

comb <- stack(list(fresh = freshmen, soph = sophomores, jrs = juniors))
print(comb)

aov(values ~ ind, data = comb) #perform the ANOVA on the two columns:

## 5.6 Creating a List ####
x <- 0.5
y <- 0.841
z <- 0.977
lst <- list(x, y, z)
lst

lst <- list(3.14, "Moe", c(1, 1, 2, 3), mean)
lst

# build a list by creating an empty list and populating it
lst <- list()
lst[[1]] <- 3.14
lst[[2]] <- "Moe"
lst[[3]] <- c(1, 1, 2, 3)
lst[[4]] <- mean
lst

#List elements can be named. The list function lets you supply a name for every element:
lst <- list(mid = 0.5, right = 0.841, far.right = 0.977)
lst

## 5.7 Selecting List Elements by Position ####
years <- list(1960, 1964, 1976, 1994)
years

years[[1]] # access single elements using the double-square-bracket syntax: (Element)
years[c(1, 2)] # extract sublists using the single-square-bracket syntax: (List)

class(years[[1]])
class(years[1])

cat(years[[1]], "\n")
cat(years[1], "\n") #> Error in cat(years[1], "\n"): argument 1 (type 'list') cannot be handled by 'cat'

## 5.8 Selecting List Elements by Name ####
years <- list(Kennedy = 1960, Johnson = 1964,
              Carter = 1976, Clinton = 1994)

years[["Kennedy"]]
years$Kennedy
years[c("Kennedy", "Johnson")]
## 5.9 Building a Name/Value Association List ####
lst <- list(mid = 0.5, right = 0.841, far.right = 0.977)
lst

values <- c(1, 2, 3)
names <- c("a", "b", "c")
lst <- list()
lst[names] <- values
lst

lst <- list(
  far.left = 0.023,
  left = 0.159,
  mid = 0.500,
  right = 0.841,
  far.right = 0.977
)
lst

lst <- list()
lst$far.left <- 0.023
lst$left <- 0.159
lst$mid <- 0.500
lst$right <- 0.841
lst$far.right <- 0.977
lst

values <- -2:2
names <- c("far.left", "left", "mid", "right", "far.right")
lst <- list()
lst[names] <- values
lst

cat("The left limit is", lst[["left"]], "\n")
cat("The right limit is", lst[["right"]], "\n")
for (nm in names(lst)) cat("The", nm, "limit is", lst[[nm]], "\n")

## 5.10 Removing an Element from a List ####
years <- list(Kennedy = 1960, Johnson = 1964,
              Carter = 1976, Clinton = 1994)
years

years[["Johnson"]] <- NULL # Remove the element labeled "Johnson"
years

# You can remove multiple elements this way, too:
years[c("Carter", "Clinton")] <- NULL # Remove two elements
years

## 5.11 Flatten a List into a Vector usiing Unlist ####
iq.scores <- list(100, 120, 103, 80, 99)
mean(unlist(iq.scores))

cat("IQ Scores:", unlist(iq.scores), "\n")

## 5.12 Removing NULL Elements from a List ####
lst <- list("Moe", NULL, "Curly")
lst

compact(lst)   # Remove NULL element

## 5.13 Removing List Elements Using a Condition ####
lst <- list(NA, 0, NA, 1, 2)

lst %>%
  discard(is.na)
lst

lst <- list(3, "dog", 2, "cat", 1)

lst %>%
  discard(is.character)

#  define your own predicate and use it with discard
is_na_or_null <- function(x) {
  is.na(x) || is.null(x)
}

lst <- list(1, NA, 2, NULL, 3)

lst %>%
  discard(is_na_or_null)

# Lists can hold complex objects
mods <- list(lm(x ~ y1),
             lm(x ~ y2),
             lm(x ~ y3))

# Define a predicate, filter_r2, to identify models whose R2 values are less than 0.70, then use the predicate to remove models from mods:
filter_r2 <- function(model) {
  summary(model)$r.squared < 0.7
}

mods %>%
  discard(filter_r2)

## 5.14 Initializing a Matrix ####
# shapes a vector into a 2 × 3 matrix (i.e., two rows and three columns):
vec <- 1:6
matrix(vec, 2, 3)

class(vec)
View(vec)

matrix(0, 2, 3) # Create an all-zeros matrix

matrix(NA, 2, 3) # Create a matrix populated with NA

#  typing the data itself in a rectangular shape that reveals the matrix structure:
theData <- c(
  1.1, 1.2, 1.3,
  2.1, 2.2, 2.3
)
mat <- matrix(theData, 2, 3, byrow = TRUE)
mat

# byrow=TRUE tells matrix that the data is row-by-row and not column-by-column 
# (which is the default). In condensed form, that becomes:
mat <- matrix(c(1.1, 1.2, 1.3,
                2.1, 2.2, 2.3),
              2, 3,
              byrow = TRUE)
mat

#  turn a vector into a matrix: just assign dimensions to the vector.
v <- c(1.1, 1.2, 1.3, 2.1, 2.2, 2.3)
dim(v) <- c(2, 3)
v

## 5.15 Performing Matrix Operations ####
t(A) # Matrix transposition of A
solve(A) # Matrix inverse of A
A %*% B # Matrix multiplication of A and B
diag (*n*) # An n-by-n diagonal (identity) matrix

## 5.16 Giving Descriptive Names to the Rows and Columns of a Matrix ####
rownames(mat) <- c("rowname1", "rowname2")
colnames(mat) <- c("colname1", "colname2", "colname3")
mat

corr_mat <- c(1.000, 0.556, 0.390, 0.556, 1.000, 0.444, 0.390, 0.444, 1.000)
dim(corr_mat) <- c(3, 3)
corr_mat

colnames(corr_mat) <- c("AAPL", "MSFT", "GOOG")
rownames(corr_mat) <- c("AAPL", "MSFT", "GOOG")

print(corr_mat)

# What is the correlation between MSFT and GOOG?
corr_mat["MSFT", "GOOG"]

## 5.17 Selecting One Row or Column from a Matrix ####
mat[1, ]     # First row
mat[, 3]     # Third column

mat[1, , drop=FALSE]   # First row, one-row matrix
mat[, 3, drop=FALSE]   # Third column, one-column matrix

## 5.18 Initializing a Data Frame from Column Data ####
df <- data.frame(v1, v2, v3)
df

# If your data is captured in a list that contains vectors and/or factors, use
df <- as.data.frame(list.of.vectors)

data.frame(pred1, pred2, pred3, resp)

# To put names on variables
data.frame(p1 = pred1, p2 = pred2, p3 = pred3, r = resp)

df <- data.frame(var1 = v1, var2 = v2, var3 = v3)
df

# If you’d rather have a tibble than a data frame, use the tibble function from the tidyverse:
tibble(p1 = pred1, p2 = pred2, p3 = pred3, r = resp)
tibble(var1 = v1, var2 = v2, var3 = v3)

# Sometimes, your data may indeed be organized into vectors, but those vectors are held in a list, not individual program variables.
list.of.vectors <- list(p1=pred1, p2=pred2, p3=pred3, r=resp)
list.of.vectors <- list(var1 = v1, var2 = v2, var3 = v3)

# In that case, use the as.data.frame function to create a data frame from the list:
as.data.frame(list.of.vectors)

# or use as_tibble to create a tibble:
as_tibble(list.of.vectors)


## 5.19 Initializing a Data Frame from Row Data ####
row1 <- c(1, 2, 3, 4)
row2 <- c(11, 12, 13, 14)
row3 <- c(21, 22, 23, 24)

rbind(row1, row2,row3)

r1 <- data.frame(a = 1, b = 2, c = "X")
r2 <- data.frame(a = 3, b = 4, c = "Y")
r3 <- data.frame(a = 5, b = 6, c = "Z")
rbind(r1, r2, r3)

list.of.rows <- list(r1, r2, r3)
bind_rows(list.of.rows)

# Same toy data, but rows stored in lists
l1 <- list(a = 1, b = 2, c = "X")
l2 <- list(a = 3, b = 4, c = "Y")
l3 <- list(a = 5, b = 6, c = "Z")
list.of.lists <- list(l1, l2, l3)

bind_rows(list.of.lists)

data.frame(a = 1, b = 2, c = "a", stringsAsFactors = FALSE)

## same set up as in the previous examples
l1 <- list( a=1, b=2, c='X' )
l2 <- list( a=3, b=4, c='Y' )
l3 <- list( a=5, b=6, c='Z' )
obs <- list(l1, l2, l3)
df <- do.call(rbind, Map(as.data.frame, obs)) 

df

i <- sapply(df, is.factor)             ## determine which columns are factors
df[i] <- lapply(df[i], as.character)   ## turn only the factors to characters
df 

## 5.20 Appending Rows to a Data Frame ####
suburbs <- read.csv("./suburbs.txt", stringsAsFactors=TRUE)
suburbs

newRow <- data.frame(city = "West Dundee", county = "Kane",
                     state = "IL", pop = 7352)

rbind(suburbs, newRow)

# We can combine these two steps into one, of course:
rbind(suburbs,
      data.frame(city = "West Dundee", county = "Kane",
                   state = "IL", pop = 7352))


# extend this technique to multiple new rows because rbind allows multiple arguments:

rbind(suburbs,
      data.frame(city = "West Dundee", county = "Kane",
                 state = "IL", pop = 7352),
      data.frame(city = "East Dundee", county = "Kane",
                 state = "IL", pop = 3192)
      )

str(newRow)  # a data.frame
str(suburbs) # a tibble

# this would produce a tibble:
rbind(some_tibble, some_data.frame)

# while this would produce a data frame:
rbind(some_data.frame, some_tibble)

## 5.21 Selecting Data Frame Columns by Position ####

# select columns from a data frame according to their position.

suburbs <- read_csv("suburbs.txt") %>% head(3)
suburbs

# Extract the first column (and only the first column):
suburbs %>%
  dplyr::select(1)

# Extract multiple columns:
suburbs %>%
  dplyr::select(1, 3, 4)

suburbs %>%
  dplyr::select(2:4) # Notice : 

dat.df %>%
  dplyr::select(1, 3, 4)

# Use list notation to select the first column from suburbs, the city column:
suburbs[[1]]

# Returns the first column as a one-column data frame:
suburbs[1]

# Returns the first and third columns as a data frame:
suburbs[c(1, 3)]
# (The expression suburbs[1] is actually a shortened form of suburbs[c(1)]

# suburbs is a tibble so we convert for this example
suburbs_df <- as.data.frame(suburbs)
suburbs_df[, 1]

# using the same matrix-style syntax with multiple indexes returns a data frame:
suburbs_df[, c(1, 4)]

## 5.22 Selecting Data Frame Columns by Name ####
dat.df[["Sexo"]]
dat.df$Sexo

## 5.23 Changing the Names of Data Frame Columns ####
df <- data.frame(V1 = 1:3, V2 = 4:6, V3 = 7:9)
df %>% rename(tom = V1, dick = V2)

colnames(df) <- c("tom", "dick", "V2")
df

# select individual columns, you can rename those columns at the same time:
df <- data.frame(V1 = 1:3, V2 = 4:6, V3 = 7:9)
df %>% select(tom = V1, V2)

# TO CHANGHE A SINGLE VARIABLE
dat1.df <- dat.df %>% rename(Género = Sexo)

dat.df <- dat.df %>% rename("Sexo o Género" = Género)

# TO CHANGHE MANY VARIABLES
dat.df <- dat.df %>% rename(Sexo = "Sexo o Género",  "Estado Civil" = EstadoCivil)

## 5.24 Removing NAs from a Data Frame ####
## Remember that na.omit will remove entire rows, not just the NA values, which could eliminate useful information.


# remove rows that contain any NA values.
dat.df <- na.omit(dat.df)

df <- data.frame(
  x = c(1, NA, 3, 4, 5),
  y = c(1, 2, NA, 4, 5)
)
df

# calculate cumulative sums, but it stumbles on the NA values:
colSums(df)

# If we remove rows with NA values, cumsum can complete its summations:
cumsum(na.omit(df))

## 5.25 Excluding Columns by Name ####
# Use the select function from the dplyr package with a dash 
# (minus sign) in front of the name of the column to exclude:
select(dat.df, -Edad)  # Select all columns from df except bad

dat.df$Edad <- as.numeric(dat.df$Edad)
dat.df$NumeroIntentosPrevios <- as.numeric(dat.df$NumeroIntentosPrevios)
dat.df$EstanciaHospitalaria <- as.numeric(dat.df$EstanciaHospitalaria)

dat.df %>%
  select(-Género, -Etnia, -EstadoCivil, -Ocupacion, -Procedencia, -Estrato,
         -Escolaridad, -TieneEnfermedadFisica, -EnfermedadFisica, 
         -TieneEnfermedadMental, -EnfermedadMental, -IntentosPrevios, 
         -Dia, -Mes, -Lugar, -Metodo, -Motivos, -Premeditacion, -Planificacion,
         -GestosDespedida, -DeterminacionRiesgo, -Riesgo, -AltaMismoDia, 
         -AltaVoluntaria, -Hospitalizacion, -PersistenciaIdeacion,
         -FueRemitido, -ProfesionalRemitido, -Subregistro) %>%
  cor()



## 5.26 Combining Two Data Frames####
# To combine the columns of two data frames side by side
all.cols <- cbind(dat2.df, dat3.df)

# To “stack” the rows of two data frames
all.rows <- rbind(dat2.df, dat3.df)

## 5.27 Merging Data Frames by Common Column ####
inner_join(df1, df2, by = "x")

# If you want all rows that appear in either data frame, use full_join instead.
full_join(df1, df2, by = "x")

# If you want all rows from df1 and only those from df2 that match, use left_join:
left_join(df1, df2, by = "x")

# Or to get all records from df2 and only the matching ones from df1, use right_join:
right_join(df1, df2, by = "x")

# Suppose you have two data frames, born and died, that each contain a column called name:
born <- tibble(
  name = c("Moe", "Larry", "Curly", "Harry"),
  year.born = c(1887, 1902, 1903, 1964),
  place.born = c("Bensonhurst", "Philadelphia", "Brooklyn", "Moscow")
)

died <- tibble(
  name = c("Curly", "Moe", "Larry"),
  year.died = c(1952, 1975, 1975)
)

# We can merge them into one data frame by using name to combine matched rows:
inner_join(born, died, by="name")

# A full_join of these data frames includes every row of both, even rows with no matching values.
full_join(born, died, by="name")

# If we don’t supply the join function with a field to join by, then it will 
# attempt to join by any field with matching names in both data frames and 
# will return an informational response stating which field it is joining on:
full_join(born, died)

# If we want to join two data frames on a field that does not have the same 
# name in both data frames, we need our by paramter to be a vector of 
# equalities:
df1 <- data.frame(key1 = 1:3, value=2)
df2 <- data.frame(key2 = 1:3, value=3)

inner_join(df1, df2, by = c("key1" = "key2"))

## 5.28 Converting One Atomic Value into Another ####
as.numeric(" 3.14 ")
as.integer(3.14)
as.numeric("foo")
as.character(101)

as.numeric(c("1", "2.718", "7.389", "20.086"))
as.numeric(c("1", "2.718", "7.389", "20.086", "etc."))
as.character(101:105)

# When converting logical values into numeric values, R converts FALSE to 0 
# and TRUE to 1:
as.numeric(FALSE)
as.numeric(TRUE)

# This behavior is useful when you are counting occurrences of TRUE in 
# vectors of logical values. If logvec is a vector of logical values, 
# then sum(logvec) does an implicit conversion from logical to integer 
# and returns the number of TRUEs:
logvec <- c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE)
sum(logvec) ## num true
length(logvec) - sum(logvec) ## num not true

## 5.29 Converting One Structured Data Type into Another ####

# 6 Data Transformations ####
## 6.1 Applying a Function to Each List Element ####
library(tidyverse)

lst %>%
  map(fun)

lst <- list(
  a = c(1,2,3),
  b = c(4,5,6)
)
lst %>%
  map(mean)

# Examples with dat.df
dat.df$Edad %>%
  map(mean)

map() # Always returns a list.
map_chr() # Returns a character vector.
map_int() # Returns an integer vector.
map_dbl() # Returns a floating-point numeric vector.

# A function that could result in a character or an integer result:

fun <- function(x) {
  if (x > 1) {
    1
  } else {
    "Less Than 1"
  }
}

fun(5)
fun(0.5)


# Examples
lst <- list(.5, 1.5, .9, 2)
map(lst, fun)

# Example with dat.df to convert age into a dicotomyc variable

BEdad <- function(x) {
  if (x < 18) {
    "Menor de 18 años"
  } else {
    "Mayor o Igual de 18 años"
  }
}

map(dat.df$Edad, BEdad)

dat.df$BEdad <- map(dat.df$Edad, DEdad)
dat.df$BEdad <- as.character(dat.df$DEdad)
dat.df$BEdad <- as.factor(dat.df$DEdad)

# look at how some of the map variants behave:
map(dat.df$Edad, BEdad)

# Produce a character vector and coerce the numbers into characters:
map_chr(dat.df$Edad, BEdad)

## or using pipes
dat.df$Edad %>%
  map_chr(BEdad)

# Try to coerce a character string into a double and die trying.
map_dbl(dat.df$Edad, BEdad)



## 6.2 Applying a Function to Every Row of a Data Frame ####
df %>%
  rowwise() %>%
  row_by_row_function()
# calculate the sum of a sequence from a to b by c:
fun <- function(a, b, c) {
  sum(seq(a, b, c))
}

# Let’s create some data to apply this function to, then use rowwise to 
# apply our function, fun, to it:
df <- data.frame(mn = c(1, 2, 3),
                 mx = c(8, 13, 18),
                 rng = c(1, 2, 3))

df %>%
  rowwise %>%
  mutate(output = fun(a = mn, b = mx, c = rng))

## 6.3 Applying a Function to Every Row of a Matrix
results <- apply(mat, 1, fun)    # mat is a matrix, fun is a function

long <- matrix(1:15, 3, 5)
long

# Calculate the average observation for each subject by applying the mean 
# function to each row. The result is a vector:
apply(long, 1, mean)

# If your matrix has row names, apply uses them to identify the elements 
# of the resulting vector, which is handy.
rownames(long) <- c("Moe", "Larry", "Curly")
apply(long, 1, mean)

# The range function returns a vector of two elements, the minimum and 
# the maximum, so applying it to long produces a matrix:
apply(long, 1, range)
# You can employ this recipe on data frames as well. It works if the data frame is homogeneous

## 6.4 Applying a Function to Every Column ####
# apply a function named fun to every column
apply(mat, 2, fun)

# For a data frame, use the map_df function from purrr:
df2 <- map_df(df, fun) 

# Let’s look at an example with real numbers and apply the mean function to every column of a matrix:
mat <- matrix(c(1, 3, 2, 5, 4, 6), 2, 3)
colnames(mat) <- c("t1", "t2", "t3")

mat

apply(mat, 2, mean)  # Compute the mean of every column
apply(mat, 2, sd)  # Compute the mean of every column
apply(mat, 2, median)  # Compute the mean of every column
apply(mat, 2, IQR)  # Compute the mean of every column

# map_df, which returns a data.frame
df2 <- map_df(df, fun) # Returns a data.frame

# using map_df to print out the class of each column reveals the column batch to be a factor instead:
map_df(dat.df, class)

## 6.5 Applying a Function to Parallel Vectors or Lists ####
lst <- list(v1, v2, v3)
pmap(lst, fun)

# the typed variants (map2_chr, map2_dbl, etc.) return vectors of the type their name implies:
map2(v1, v2, fun)

# or if fun returns only a double, then use the typed variant of map2:
map2_dbl(v1, v2, fun)

# Consider the gcd function, “Defining a Function”, which takes two arguments:
  
  gcd <- function(a, b) {
    if (b == 0) {
      return(a)
    } else {
      return(gcd(b, a %% b))
    }
  }

# use map to “vectorize” it. 
a <- c(1, 2, 3)
b <- c(9, 6, 3)
my_gcds <- map2(a, b, gcd)
my_gcds  
  
# For an output in a vector,  use unlist or use one of the 
# typed variants, such as map2_dbl:
unlist(my_gcds)

# ask R to coerce the results into character output or map2_dbl to 
# ensure the results are doubles:
map2_chr(a, b, gcd)
map2_dbl(a, b, gcd)

# >2 vectors, or the data is already in a list, we can use the pmap family of functions, which take a list as an input.
lst <- list(a,b)
pmap(lst, gcd)

# if we want a typed vector as output:
lst <- list(a,b)
pmap_dbl(lst, gcd)

# EXIT --------------------------------------------------------------------
getwd() # Identify the current working directory.
ls() # List all objects in the working
# directory.
ls.str() # List all objects, with finite detail.
list.files() # List files at the PC directory.
save.image("R_Cookbook.rdata")
getwd() # Identify the current working directory.
ls() # List all objects in the working
# directory.
ls.str() # List all objects, with finite detail.
list.files() # List files at the PC directory.
alarm() # Alarm, notice of upcoming action.
q() # Quit this session.
# Prepare for Save workspace image? query.
 