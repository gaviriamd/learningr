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


# OPEN DATASET % MAKE IT READEABLE ----------------------------------------
## Open a CSV ####
dat.df <- utils::read.table (file =
                               "INTSUIDATASET.csv",
                             header=TRUE, dec=".", sep=",")
getwd() # Identify the working directory
ls() # List objects
attach(dat.df) # Attach the data, for later use

##  Import a .csv File of  from an Online Source into R ####
height.df <- data.table::fread(
  "https://img1.wsimg.com/blobby/go/bbca5dba-4947-4587-b40a-db346c01b1b3/downloads/heights.csv?ver=1709965708065"
)
# REVIEW OF DATA FRAME ----------------------------------------------------
library(tidyverse)
view(dat.df) # To wacht on screen the dataset
class(dat.df) #class of dat.df object.
dim(dat.df) #Shows the size Rows and Columns
str(dat.df) # Identify structure
glimpse(dat.df) # Same as above, but better printed.
head(dat.df, n=3) # Show the head, 1st 3 cases #Standard is 6
tail(dat.df, n=3) # Show the head, 1st 3 cases #Standard is 6
dat.df #To see de 1st 30 values of each variable
summary(dat.df) # Summary statistics

#### Watch a Data Frame as a Tibble for better printing ####

# dplyr function group_by() takes a data frame and outputs a
# grouped tibble, which allows grouped operations 
dat.df_by_Edad <- group_by(dat.df, Edad) # Edad could be any var.
class(dat.df_by_Edad) # tbl_df refers to class tibble
dat.df_by_Edad # prints only rows and columns that fit to screen
class(as.data.frame(dat.df)) # now just a data.frame

#### See by Columns & Rows ####
dat.df[3,2] # row 3 (Participant 3) column 2 (Var 2)
dat.df[c(2,4), "Edad"] # rows 2 and 4 of columns Edad (x)
dat.df[10,] # all columns for row 10
dat.df[, "Sexo"] # all rows of column "Sexo"
dat.df$Edad[2:3] # Gives the Edad of 2 and 3 Rows
dat.df[["Edad"]][2:3] # same as above

#### Watching by selected < or > arrange ####
dat.df$Edad > 25 # logical comparison results in a logical vector 
dat.df[dat.df$Edad > 25, c("Edad", "Sexo")] # observations where age > 25
seq(from=1, to=5, by=1) # specifying arguments by name
seq(10, 0, -2) # specifying arguments by position

# DATA CLEANING AND MISSING VALUES ----------------------------------------
glimpse(dat.df) # I believe it´s better than str
str(dat.df) # base R function
library(Hmisc)
describe(dat.df) # detailed summaries of variables
describe(dat.df[,c( "Sexo", "Edad", "Etnia")]) # Explore 1 by 1 if needed
plot(describe(dat.df), which = "categorical")
plot(describe(dat.df), which = "continuous")
hist(dat.df$Edad, breaks = 50) # histogram of Edad with a suspicious extreme value
hist(dat.df$NumeroIntentosPrevios, breaks = 50) # histogram of Estancia Hospitalaria with a suspicious extreme value
dat.df$age[dat.df$age<18 | dat$age>120] <- NA # change all impossible age values to NA 
dat.df$sex[dat.df$sex == "12.2"] <- NA # remember to use quotes for character variables

duplicated(dat.df$Edad) # Duplicates very useful to IDs

#### Missing Values ####
describe(dat.df) # detailed summaries of variables
dat.df$Etnia[dat.df$Etnia == ""] <- NA # 1 by 1, or
dat.df[dat.df == ""] <- NA #all At once
view(dat.df) # Confirm

#### Change all of the following values in the data set to NA ####
dat[dat == -99] <- NA
dat[dat == -98] <- NA
dat[dat == "not assessed"] <- NA
dat[dat == "-99"] <- NA

#### Correcting misspelling ####
glimpse (dat.df)

class(dat.df$Ingresos)
dat.df$Ingresos <- as.character(dat.df$Ingresos)
unique(dat.df$Ingresos) # Seek or Confirm
dat.df$Ingresos[dat.df$Ingresos == "Estudiante"] <- "Sin ingresos" # and Correct
dat.df$Ingresos[dat.df$Ingresos == "Hogar"] <- "Sin ingresos" # and Correct
dat.df$Ingresos[dat.df$Ingresos == "Desempleado"] <- "Sin ingresos" # and Correct
dat.df$Ingresos[dat.df$Ingresos == "Empleado"] <- "Con ingresos" # and Correct
dat.df$Ingresos[dat.df$Ingresos == "Independiente"] <- "Con ingresos" # and Correct
unique(dat.df$Ingresos) # Confirm
dat.df$Ingresos <- as.factor(dat.df$Ingresos)
class(dat.df$Ingresos)
summary(dat.df$Ingresos)


#### Transform a Politomyc Variable a dicotomyc variable ####
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
#### Lowercase, Upper Case, Sentence Case & Title Case ####
tolower(dat.df$Ocupacion)
touper(dat.df$Ocupacion)

dat.df$Ocupacion <- str_to_lower()
dat.df$Ocupacion <- str_to_upper()
dat.df$Ocupacion <- str_to_sentence(Ocupacion)
dat.df$Ocupacion <- str_to_title(Ocupacion)


#### Now colored by missing values ####
plot(describe(dat.df))
plot(describe(dat.df), which = "categorical")
plot(describe(dat.df), which = "continuous") + scale_color_gradient(low="blue", high="red") # scale_color_gradient() changes the colors used to denote missing
describe(dat)
hist(dat.df$NumeroIntentosPrevios, breaks = 50) # histogram of "NumeroIntentos" variable after cleaning

#### Look which cases are complete ####
complete.cases(dat.df) #returns a logical vector of whether each row of x has no missing (TRUE) or at least one NA (FALSE).
sum(complete.cases(dat.df)) # number of complete cases
datcc.df <- dat.df[complete.cases(dat.df),] # create a dataset of complete cases



# FACTORS -----------------------------------------------------------------
glimpse(dat.df) # Explore how many and which character vectors there are
dat.df$Ocupacion # Example of character vector

dat.df$Ocupacion_fct <- factor(dat.df$Ocupacion) # convert single to factor
dat.df$Ocupacion_fct # categories in alphabetical order

#### Convert all character to Factors ####
dat.df[sapply(dat.df, is.character)] <- lapply(dat.df[sapply(dat.df, is.character)], as.factor)

#### specifying the order of levels ####
glimpse(dat.df) # Explore
unique(dat.df$Estrato) # See uniqueness
dat.df$Estrato <- factor(dat.df$Estrato, levels = c("Uno","Dos", "Tres", "Cuatro"))
dat.df$Estrato # See Distributions and Levels
as.numeric(dat.df$Estrato) # See underlying integer values 

dex4$honors # honors: 0="enrolled", 1="not enrolled"

# convert integer vector into a factor and assign levels
# labels become the levels
factor(dex4$honors, labels=c("enrolled","not enrolled"))

#### recoding ####
fct_recode(dex4$program_fct, general = "general", other = "academic", other = "vocation")
dex4$program_fct # factor variable
fct_relevel(dex4$program_fct, "academic") # put academic at the first position
fct_relevel(dex4$program_fct, "academic", after=1) # put "academic" at the second position
fct_relevel(dex4$program_fct, rev) # reverse


#### Create a factor vector ####
dex4$state_fct <- factor( dex4$state ) # Creates a Vector
dex4$state_fct # Shows vector
fct_expand(dex4$state_fct, c("TX", "IL")) # add level to a factor

# specify where new factors should be added
dex4$state_fct <- fct_expand(dex4$state_fct, c("TX","IL"), after=1)
dex4$state_fct 
fct_drop(dex4$state_fct) # drop unused levels
fct_drop(dex4$state_fct, only="TX") # drop only specified levels

# NUMBERS -----------------------------------------------------------------
glimpse(dat.df)

#### Convert all integers (int) to numerics (dbl) ####
dat.df[sapply(dat.df, is.integer)] <- lapply(dat.df[sapply(dat.df, is.integer)], as.numeric)

### # Confirm that the number associated with dat.df$Edad are numeric.####
dat.df$Edad <-as.numeric(dat.df$Edad)
length(dat.df$Edad)
table(is.na(dat.df$Edad))


# PIPE --------------------------------------------------------------------
glimpse(dat.df)

# a dataset of age and transfer status for females only
f25 <- filter(d, sex == "female" & age<25)
f25_small <- select(f25, age, transfer)
f25_small

# start with d, then filter rows, then select variables
f25_small <- d |>   
  filter(sex == "female" & age<25) |>
  select(age, transfer)
f25_small

# create a dataset of mean scores of midterm and final
# by teaching assistant, for teaching assistants with more than 15 students
g1 <- d |>   
  summarise(n_students = n(),   
            avg_midterm = mean(midterm), 
            avg_final = mean(final),
            .by = teaching_assistant) |>  
  filter(n_students>15)

# In base R uses "_" represents the input data set
d |>
  filter(age<30) |>
  lm(total_proportion ~ age, data=_ ) 

# NOTE: in tidyverse "." represents the input data set instead of "_"  
d %>%
  filter(age<30) %>%
  lm(total_proportion ~ age, data=.) # the . is the filtered dataset


# DPLYR -------------------------------------------------------------------
glimpse(dat.df)

# select 4 variables
d_small <- d |>
  select(universityid, student, age, class)
head(d_small, n = 3)

# get university variables
duniversity <- d |>
  select(starts_with("university"))
names(duniversity)

# get "homework1" and "homework2" variables using num_range()
homeworks <- d |>
  select(num_range("homework", 1:2))
names(homeworks)
names(dat.df) # original names

#### Renaming Variables (Columns) #####
names(dat.df) #Visualize Names

# rename "TieneEnfermedadFisica" variable as "AntecedentesMedicos"
dat.df <- dat.df |>
  rename(Genero = Sexo) 

# capitalize all column names using toupper() inside of rename_with()
dat.df <- dat.df |>
  rename_with(toupper)

# add a prefix to each column
dat.df <- dat.df |>
  rename_with(~paste0("d1_", .x)) # .x place holder for all columns

# add a suffix to only columns that start with "univsersity"
dat.df <- dat.df |>
  rename_with(~paste0(.x, "d1_GENERO"), starts_with("Femenino") )

# make column "ProfesionalRemitido" and "Subregistro" to first columns in the dataset
dat.df <- dat.df |>
  relocate(ProfesionalRemitido, Subregistro)

# same as above
first <- d |>
  relocate(starts_with("total"))

# make PersistenciaIdeacion first column and rename it to h1
dat.df <- dat.df |>
  relocate(PersistenciaRiesgo = PersistenciaIdeacion)

# relocate d1_ESTANCIA HOSPITALARIA 1 after homework2
dat.df <- dat.df |>
  relocate(d1_ESTANCIAHOSPITALARIA, .after=d1_MES)

# relocate d1_LUGAR before last column
dat.df <- dat.df |>
  relocate(d1_LUGAR, .before=last_col())

names(dat.df) # Confirm Changes

# create age category variable, and highprop binary variable
d <- d |>
  mutate(agecat = cut(age, breaks=c(18,20,24,28,32,36)),
         highprop = total_proportion > mean(total_proportion))
table(d$agecat, d$highprop)

# all variables for Patients with total Edad 40 or more
Patients_H40 <- dat.df[dat.df$d1_EDAD>=40,]
view(Patients_H40)

# subset to Participants from rural a. with Edad >= 30
RuralH30.df <- dat.df |>
  filter(d1_PROCEDENCIA=="Rural", d1_EDAD>=30)
RuralH30.df

# select those not in Rural a, who are either younger than 25 or older than 30
dat.df |>
  filter(d1_PROCEDENCIA!="Rural", d1_EDAD<25 | d1_EDAD>30)

# organizados por género (opg) where females are before males, then by age, oldest first
opg <- dat.df |>
  arrange(d1_GENERO, desc(d1_EDAD)) 
head(opg, n=20)

# Create summaries of students by teaching assistant using .by
student_summ2 <- d |>
  summarise(n_students = n(), # number of students
            total_pts = max(total_points), # total points 
            avg_age = mean(age), # average age
            n_seniors = sum(class=="senior"), # number of seniors students 
            .by = teaching_assistant) 
student_summ2

# add a variable that is the mean final score by teaching assistant
d_with_ta_summ <- d |>
  mutate(avg_final = mean(final),
         .by = teaching_assistant)
# first 20 rows of relevant columns
head(select(d_with_ta_summ, student, teaching_assistant, final, avg_final), n=10)

# create a pass/fail (0/1) variable
d <- d |>
  mutate(pass_final = if_else(final > 60, true=1, false=0))

head(select(d, final, pass_final))

# use highest score for best_test
d <- d |>
  mutate(best_test = if_else(midterm>final, true=midterm, false=final))

head(select(d, midterm, final, best_test))

d <- d |>
  mutate(letter_grade = case_when(
    total_points < 120 ~ "F",
    between(total_points, 120, 130) ~ "D", # between(x, a, b) return TRUE if a<=x<=b, FALSE otherwise
    between(total_points, 130.01, 140) ~ "C",
    between(total_points, 140.01, 150) ~ "B",
    total_points > 150 ~ "A"
  )
  )

head(select(d, total_points, letter_grade))

# creat an "upperclassman" variable
d <- d |>
  mutate(upper = case_when(
    class %in% c("freshman", "sophomore") ~ 0,
    class == "senior" ~ 1 # oops, forgot juniors
  ))

head(select(d, class, upper))

# create a recommend variable based on grade
d <- d |>
  mutate(recommend = case_when(
    letter_grade == "A" ~ "yes",
    letter_grade == "B" ~ "maybe",
    is.na(letter_grade) ~ NA, # without this, NA will match to .default
    .default = "no"  # C, D, and F will be no
  ))

head(select(d, letter_grade, recommend), n=20)



# COMBINING DATASETS ------------------------------------------------------
glimpse(dat.df)

# new data set that contains the same variables as d, except is missing those created during the workshop
d2 <- read_csv("https://stats.oarc.ucla.edu/wp-content/uploads/2023/08/student_data2.csv") 

# rows and columns of d2 and d
dim(d)
dim(d2)

# a new variable called source is added to the beginning of the dataset
d3 <- bind_rows(d, d2, .id="source")

# these are the rows where the datasets meet
#  final and total_points are in both datasets, letter_grade and recommend only in d
select(d3[95:105,], final, total_points, letter_grade, recommend)

# this will work because we restrict d to only variables common to both
drbind <- rbind(d[,1:14], d2)

# load data set for merging example
d_ta <- read_csv("https://stats.oarc.ucla.edu/wp-content/uploads/2023/08/teaching_assistant_data.csv")
d_ta

# select one non-matching and one matching teaching assistant from each to demo joins
#   just a few variables from d3
d3_small <- d3 |>
  filter(teaching_assistant == "T2" | teaching_assistant == "T7") |>
  select(teaching_assistant, student,midterm, final)

d_ta_small <- d_ta |>
  filter(teaching_assistant == "T2" | teaching_assistant == "T35")

# first a look at the two datasets to be merged
d3_small 
d_ta_small

# only matching rows returned
# T2 from d_ta_small matched four times to T2 in d3_small
inner_join(d3_small, d_ta_small)

# all rows from d3_small returned
left_join(d3_small, d_ta_small)

# all rows from both returned
full_join(d3_small, d_ta_small)


# TIDYR -------------------------------------------------------------------
dept <- read_csv("https://stats.idre.ucla.edu/stat/data/rdm/dept1.csv")
dept

# the new column "year" uses the column headings as values,
#  the new column "graduates" will be the collapsed values
#  we do not want to collapse id
dept_by_year <- dept |>
  pivot_longer(cols=-id, names_to="year", values_to="grad")
dept_by_year

worms <- read_csv("https://stats.idre.ucla.edu/stat/data/rdm/worms.csv")
worms

# create new columns with names from feature variable and values from measure variable
by_worm <- worms |>
  pivot_wider(names_from=feature, values_from=measure)
by_worm
table5

# remember that new variable names must be quoted, while old variables do not need to be
table5 |>
  separate(col=rate, into=c("cases", "population"), sep="/") |> 
  unite(col="year", century, year, sep="")

dat.dfse <- dat.df
rm(dat.dfse$Etnia)
# DATES -------------------------------------------------------------------
glimpse(dat.df)

# string dates have no numeric value, so this errors
"2018-03-05" + 1

# as.Date only accepts a couple of formats by default
# good
as.Date("2015-02-14")

as.Date("02/14/2014") # bad

# specify a format to fix
as.Date("02/14/2014", format="%m/%d/%Y")

a <- as.Date("1971-01-01")
class(a)

as.numeric(a) # days since 1970-01-01

a - as.Date("1971-12-31") # date arithmetic

a + 2

library(lubridate)

d <- read_csv("https://stats.idre.ucla.edu/stat/data/rdm/dates.csv")
d

# no format specifications needed
# just ordering y,m, and d
dates <- data.frame(f1=mdy(d$fmt1), f2=mdy(d$fmt2),
                    f3=ymd(d$fmt3), f4=ymd(d$fmt4))
dates

# month, day, year, hour and minute with PM
mdy_hm("7/19/23 11:30PM")

# year, month, day, hour, minute, second
ymd_hms("2023-4-19 08:25:30")

# time difference between those 2 date-times
mdy_hm("7/19/23 1:30PM") - ymd_hms("2023-4-19 21:25:30") 

# class POSIXct is a standard way of representing calendar time
class(mdy_hm("7/19/23 11:30PM"))

# month day year hour minute second
mdy_hms(d$decision_time)

# Assign a time zone to a time variable
dates$decision_time <- mdy_hms(d$decision_time, tz="US/Pacific")
dates$decision_time

head(OlsonNames(),  n=20) # first 20 valid time zones

dates$f1 # we'll use the first column of our dates dataset

day(dates$f1) # day of the month

yday(dates$f1) # day of the year

wday(dates$f1) # weekday as numbers

wday(dates$f1, label=TRUE) # weekday with labels

month(dates$f1) # month of the year

#break up the time variable decision time
#display as a data.frame with 4 columns
with(dates,  # with() tells R to look for variables in object "dates"
     data.frame(time=decision_time, h=hour(decision_time),
                m=minute(decision_time), s=second(decision_time)))

#2016 is a leap year
# the intuitive result
ymd("2015-02-14") + years(2) 

ymd("2015-02-14") + dyears(2) # the exact result


# STRINGS -----------------------------------------------------------------
library(tidyverse) #Loads tidyverse Library

view(dat.df) #Shows de DataSet

# element-by-element concatenation
paste(centers$ROOM, centers$BUILDING)

# separate with comma and space
paste(centers$CITY, centers$STATE, sep=", ")

# separate with comma and space
paste(1:4, centers$UNIVERSITY)

# separate with comma and space
paste(centers$CITY, centers$STATE, c("USA", "USA","USA", NA), sep=", ")

# put everything into a single string with collapse
paste(1:4, centers$UNIVERSITY, collapse="; ")

# default is no separating character
str_c(centers$CITY, centers$STATE)

# separate with comma and space
str_c(centers$CITY, centers$STATE, c("USA", "USA", "USA", NA), sep=", ")

# start at first, end at third character
substr(centers$BUILDING, 1, 3)

# specifying an end longer than the entirety of the string
#   results in truncation
str_sub(centers$UNIVERSITY, 2, 6)

# start from third to last, end at last
str_sub(centers$UNIVERSITY, -3, -1)

# change the first character of the ROOM variable to "R"
substr(centers$ROOM, 1, 1) <- "R"
centers$ROOM

# lower case city names
tolower(centers$CITY)
str_to_lower()
str_to_upper()
str_to_sentence()
str_to_title()

##### Change variable names to lower case ####
names(dat.df) <- tolower(names(dat.df))
names(dat.df)

centers$name # stat center names

# row numbers of names with "STAT"
grep(pattern="STAT", centers$name)

# we can use those indices to subset
centers$name[grep(pattern="STAT", centers$name)]

# or just use value=TRUE to get the words directly
grep(pattern="STAT", centers$name, value=TRUE)

# stringr version returns TRUE/FALSE, first argument is the string vector
str_detect(centers$name, "STAT")

# can be used to subset like grep
centers$name[str_detect(centers$name, "STAT")]

centers$name <- sub("STAT", "STATISTICAL", centers$name) # change STAT to STATISTICAL
centers$name 

str_replace(centers$name, "STAT", "STATISTICAL") # stringr substitution

# EXIT --------------------------------------------------------------------
getwd() # Identify the current working directory.
ls() # List all objects in the working
# directory.
ls.str() # List all objects, with finite detail.
list.files() # List files at the PC directory.
save.image("R_Project.rdata")
getwd() # Identify the current working directory.
ls() # List all objects in the working
# directory.
ls.str() # List all objects, with finite detail.
list.files() # List files at the PC directory.
alarm() # Alarm, notice of upcoming action.
q() # Quit this session.
# Prepare for Save workspace image? query.