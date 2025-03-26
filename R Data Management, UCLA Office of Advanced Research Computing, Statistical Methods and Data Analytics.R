##############################################
# UCLA Office of Advanced Research Computing #
# Statistical Methods and Data Analytics     #    
# R data management workshop 2023            #
##############################################

## install.packages("tidyverse", dependencies=TRUE)
## install.packages("Hmisc", dependencies=TRUE)

library(tidyverse)


# tidyverse package readxl is used to import Excel files
library(readxl)

# tidyverse package haven is used to import Stata, SAS, and SPSS files
library(haven)

library(Hmisc)

######################################
### Data frames and Tibbles ##########
######################################

# read in data over internet
dex1 <- read.csv("https://stats.oarc.ucla.edu/wp-content/uploads/2023/08/excercise_1.csv")
# class of dex1 object
class(dex1) 
# first few rows
head(dex1)

# create data frame manually
dt <- data.frame(
  x = runif(5), # 5 random numbers from uniform(0,1)
  y = rnorm(5)  # 5 random numbers from normal(0,1)
)

dt

# dplyr function group_by() takes a data frame and outputs a grouped tibble,
#  which allows grouped operations 
dex1_by_ses <- group_by(dex1, ses)

# tbl_df refers to class tibble
class(dex1_by_ses)

# prints only rows and columns that fit to screen
dex1_by_ses

# now just a data.frame
class(as.data.frame(dt))

### First workshop dataset
dat <- read_csv("https://stats.oarc.ucla.edu/wp-content/uploads/2023/08/student_data.csv")

dat

## View(dat)
view(dat)

# row 3 column 2
dat[3,2]

# rows 2 and 4 of columns age
dat[c(2,4), "age"]

# all columns for row 10
dat[10,]

# all rows of column "sex"
dat[, "sex"]

# subsetting with $ creates a numeric vector, which itself can be subset
dat$age[2:3]

# same as above
dat[["age"]][2:3]

# logical comparison results in a logical vector 
dat$age > 25

# when placed in [] before the comma, rows with TRUE are selected
#  this returns columns age and sex for observations where age > 25
dat[dat$age > 25, c("age", "sex")]

# specifying arguments by name
seq(from=1, to=5, by=1)

# specifying arguments by position
seq(10, 0, -2)

########################################
### Data cleaning and missing values ##
########################################


# tidyverse function
glimpse(dat)

# base R function
str(dat)

library(Hmisc)

# detailed summaries of variables
describe(dat)

describe(dat[,c( "sex", "age", "homework1")])

plot(describe(dat), which = "categorical")

plot(describe(dat), which = "continuous")

# histogram of age with a suspicious extreme value
hist(dat$age, breaks = 50)

# histogram of homework1 with a suspicious extreme value
hist(dat$homework1, breaks = 50)

# change all impossible age values to NA 
# assume all adults in this dataset
dat$age[dat$age<18 | dat$age>120] <- NA

# remember to use quotes for character variables
dat$sex[dat$sex == "12.2"] <- NA

# change all of the following values in the data set to NA 
dat[dat == -99] <- NA
dat[dat == -98] <- NA
dat[dat == "not assessed"] <- NA
dat[dat == "-99"] <- NA

# now colored by missing values
plot(describe(dat), which = "categorical")

# scale_color_gradient() changes the colors used to denote missing
plot(describe(dat), which = "continuous") + scale_color_gradient(low="blue", high="red")

describe(dat)

# histogram of "age" variable after cleaning
hist(dat$age, breaks = 50)

# histogram of "age" variable after cleaning
hist(dat$homework1, breaks = 50)

########################################
### EXCERCISE 1   ######################
########################################


# Import a dataset for exercise 1
dex1 <- read.csv("https://stats.oarc.ucla.edu/wp-content/uploads/2023/08/excercise_1.csv")
head(dex1, n=3)

# Exercise 1. 1.1 Identifying suspicious categorical values
plot(describe(dex1), which = "categorical")


# Exercise 1. 1.1 Identifying suspicious continuous values
plot(describe(dex1), which = "continuous")


# Exercise 1. 1.1 Checking suspicious values
describe(dex1[,c( "read", "write")])


# Exercise 1. 1.2 Changing suspicious values


# there is "ffemale value and "female " (an extra space)
unique(dex1$sex)

# correcting misspelling
dex1$sex[dex1$sex=="ffemale"] <- "female"
dex1$sex[dex1$sex=="female "] <- "female"

# changing ses "not assessed" to NA
dex1$ses[dex1$ses=="not assessed"] <- NA

# changing read and write to NA
dex1[dex1==-99] <- NA

# Exercise 1. 1.3 Plotting after cleaning
plot(describe(dex1))

########################################
### End of exercise 1  #################
########################################

# which cases are complete
complete.cases(dat)

# create a dataset of complete cases
d <- dat[complete.cases(dat),]

# number of complete cases
sum(complete.cases(dat))

########################################
### Pipe  ##############################
########################################


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

g1

# In base R uses "_" represents the input data set
d |>
  filter(age<30) |>
  lm(total_proportion ~ age, data=_ ) 

# NOTE: in tidyverse "." represents the input data set instead of "_"  
d %>%
  filter(age<30) %>%
  lm(total_proportion ~ age, data=.) # the . is the filtered dataset

########################################
### Dplyr  #############################
########################################

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

# original names
names(d)

# rename "teaching assistant" variable as "TA"
ta <- d |>
  rename(TA = teaching_assistant) 
names(ta)

# capitalize all column names using toupper() inside of rename_with()
cap <- d |>
  rename_with(toupper)
names(cap)

# add a prefix to each column
pre <- d |>
  rename_with(~paste0("d1_", .x)) # .x place holder for all columns
names(pre)

# add a suffix to only columns that start with "univsersity"
p_sub <- d |>
  rename_with(~paste0(.x, "_2023"), starts_with("university") )
names(p_sub)

# make column "total_points" and "total_proportion" to first columns in the dataset
first <- d |>
  relocate(total_points, total_proportion)
names(first)

# same as above
first <- d |>
  relocate(starts_with("total"))
names(first)

# make homework1 first column and rename it to h1
h1 <- d |>
  relocate(h1 = homework1)
names(h1)

# relocate homework1 after homework2
h2 <- d |>
  relocate(homework1, .after=homework2)
names(h2)

# relocate homework1 before last column
hl <- d |>
  relocate(homework1, .before=last_col())
names(hl)

# create age category variable, and highprop binary variable
d <- d |>
  mutate(agecat = cut(age, breaks=c(18,20,24,28,32,36)),
         highprop = total_proportion > mean(total_proportion))
table(d$agecat, d$highprop)

# all variables for students with total points 180 or more
total_points180 <- d[d$total_points>=180,]
total_points180

# subset to UCLA students with total_points >= 180
dfhp <- d |>
  filter(university=="UCLA", total_points>=180)
dfhp

# select those not in UCLA, who are either younger than 25 or older than 30
d |>
  filter(university!="UCLA", age<25 | age>30)

# sort by sex where females are before males, then by age, oldest first
d_arrange <- d |>
  arrange(sex, desc(age)) 
head(d_arrange, n=20)

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

########################################
### EXCERCISE 2   ######################
########################################


# Exercise 2. 2.1 Dropping columns
dex1 |>
  select(-read, -write, -awards, -school_prog) |>
  glimpse() 

# Exercise 2. 2.2 Dropping rows
dex1 |>
  select(-read, -write, -awards, -school_prog) |>
  filter(math > 50) |>
  distinct(math) |>
  arrange(math)

# Exercise 2. 2.3 Summarising
dex1 |>
  select(-read, -write, -awards, -school_prog) |> # dropping columns
  filter(math > 50) |> # dropping rows
  summarise(mean_math = mean(math), # summarizing by honors
            mean_science = mean(science),
            .by = honors) 

########################################
### End of exercise 2  #################
########################################

########################################
### Combining datasets  ################
########################################


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

########################################
### EXCERCISE 3   ######################
########################################

# Excercise 3. Import dataset dex2
dex2 <- read_csv("https://stats.oarc.ucla.edu/wp-content/uploads/2023/08/excercise_2.csv")
dex2

# Excercise 3. 3.1 Keeping a subset of columns
dex1 |>
  select(id, ses, school_prog, math, science, honors)
head(dex1)

#Exercise 3. 3.2 dropping rows
dex1 |>
  select(id, ses, school_prog, math, science, honors) |>
  filter(id %in% 1:8)

# Exercise 3. 3.3 creating a new dataset dex3
dex3 <- dex1 |>
  select(id, ses, school_prog, math, science, honors) |> # dropping columns
  filter(id %in% c(1:8)) |> # dropping rows
  left_join(dex2) # merging two datasets

dex3


########################################
### End of exercise 3  #################
########################################

########################################
### Tidyr  #############################
########################################


dept <- read_csv("https://stats.idre.ucla.edu/stat/data/rdm/dept1.csv")
dept

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

########################################
### Dates  #############################
########################################


# string dates have no numeric value, so this errors
"2018-03-05" + 1

# as.Date only accepts a couple of formats by default
# good
as.Date("2015-02-14")

# bad
as.Date("02/14/2014")

# specify a format to fix
as.Date("02/14/2014", format="%m/%d/%Y")

a <- as.Date("1971-01-01")
class(a)

# days since 1970-01-01
as.numeric(a)

# date arithmetic
a - as.Date("1971-12-31")

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

# first 20 valid time zones
head(OlsonNames(),  n=20)

# we'll use the first column of our dates dataset
dates$f1

# day of the month
day(dates$f1)

# day of the year
yday(dates$f1)

# weekday as numbers
wday(dates$f1)

# weekday with labels
wday(dates$f1, label=TRUE)

# month of the year
month(dates$f1)

#break up the time variable decision time
#display as a data.frame with 4 columns
with(dates,  # with() tells R to look for variables in object "dates"
     data.frame(time=decision_time, h=hour(decision_time),
                m=minute(decision_time), s=second(decision_time)))

#2016 is a leap year
# the intuitive result
ymd("2015-02-14") + years(2) 

# the exact result
ymd("2015-02-14") + dyears(2)

########################################
### Strings  ###########################
########################################


centers <- read_csv("https://stats.idre.ucla.edu/stat/data/rdm/stats_centers.csv")
centers

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

# change variable names to lower case
names(centers) <- tolower(names(centers))
names(centers)

# stat center names
centers$name

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

# change STAT to STATISTICAL
centers$name <- sub("STAT", "STATISTICAL", centers$name)
centers$name 

# stringr substitution
str_replace(centers$name, "STAT", "STATISTICAL")

########################################
### EXCERCISE 4   ######################
########################################


# Exercise 4. 4.1 keeping rows with student ids 1, 2 and 3. 
dex3 <- dex3 |>
  filter(id %in% 1:3)


# Exercise 4. 4.2 creating total_pts variable
dex3 |>
  mutate( total_pts = sum(math, science), .by=id) |>
  relocate(total_pts, .after = id)


# Exercise 4. 4.3 recoding total_pts
dex3 |>
  mutate( total_pts = sum(math, science), .by=id) |>
  mutate( total_pts_recoded = case_when(
    total_pts >= 100 ~ "high",
    total_pts >= 80 & total_pts < 100 ~ "medium",
    total_pts < 80 ~ "low" ) ) |>
  relocate(total_pts, total_pts_recoded, .after = id) 


# Excercise 4. 4.4 splitting school_prog into school and program
dex3 |>
  mutate( total_pts = sum(math,science), .by = id) |>
  mutate(total_pts_recoded = case_when(
    total_pts >= 100 ~ "high",
    total_pts >= 80 & total_pts < 100 ~ "medium",
    total_pts < 80 ~ "low" ),
    state = str_sub(city_state, 1, 2) ) |>
  relocate(city_state, state, state, .after = id)

# Excercise 4. 4.5 Extracting state from state_city
dex4 <- dex3 |>
  mutate( total_pts = sum(math,science), .by = id) |> # creating a variable total_pts
  mutate(total_pts_recoded = case_when( # recoding total_pts variable
    total_pts >= 100 ~ "high",
    total_pts >= 80 & total_pts < 100 ~ "medium",
    total_pts < 80 ~ "low" ),
    state = str_sub(city_state, 1, 2) ) |> # extracting characters from the city_state variable
  separate(school_prog, into=c("school", "program"), sep="/") |> # separating school_prog variable
  relocate(school, program, state, total_pts, total_pts_recoded, .after = id)
dex4

########################################
### End of exercise 4  #################
########################################

########################################
### Factors  ###########################
########################################



# character vector
dex4$program

# convert to factor
dex4$program_fct <- factor(dex4$program)

# categories in alphabetical order
dex4$program_fct

# specifying the order of levels
dex4$program_fct <- factor(dex4$program_fct, levels = c("vocation","academic", "general"))
dex4$program_fct

# underlying integer values for factors
as.numeric(dex4$program_fct)


# honors: 0="enrolled", 1="not enrolled"
dex4$honors

# convert integer vector into a factor and assign levels
# labels become the levels
factor(dex4$honors, labels=c("enrolled","not enrolled"))


# recode
fct_recode(dex4$program_fct, general = "general", other = "academic", other = "vocation")


# factor variable
dex4$program_fct

# put academic at the first position
fct_relevel(dex4$program_fct, "academic")

# put "academic" at the second position
fct_relevel(dex4$program_fct, "academic", after=1)


# reverse
fct_relevel(dex4$program_fct, rev)


# create a factor vector
dex4$state_fct <- factor( dex4$state )
dex4$state_fct

# add level to a factor
fct_expand(dex4$state_fct, c("TX", "IL"))


# specify where new factors should be added
dex4$state_fct <- fct_expand(dex4$state_fct, c("TX","IL"), after=1)
dex4$state_fct 

# drop unused levels
fct_drop(dex4$state_fct)


# drop only specified levels
fct_drop(dex4$state_fct, only="TX")

########################################
### The end  ###########################
########################################

