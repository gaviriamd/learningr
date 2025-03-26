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
load("R_for_Data_Science.rdata")
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
library(effsize)
library(Hmisc)
library(pivottabler)
library(arsenal)
library(s20x)
library(asbio)
library(epiDisplay)
library(gt)
library(psych)
library(doBy)
library(tables)
library(pastecs)
library(furniture)
library(RVAideMemoire)
library(UsingR)
library(nycflights13)
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
library(palmerpenguins)
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
install.packages("nycflights13", dependencies=TRUE)
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

# Introduction ####
install.packages("tidyverse")
library(tidyverse)
tidyverse_update() # To see if updates are available.

#other packages
install.packages(c("arrow", "babynames", "curl", "duckdb", "gapminder", "ggrepel",
                   "ggridges", "ggthemes", "hexbin", "janitor", "Lahman", "leaflet", "maps",
                   "nycflights13", "openxlsx", "palmerpenguins", "repurrrsive", "tidiverse", "tidymodels", "writexl"))

library(tidyverse)
library(palmerpenguins)
library(ggthemes)
palmerpenguins::penguins #this is a data frame
penguins
glimpse(penguins)
glimpse(dat.df)
view(penguins)
view(dat.df)
?penguins #to learn more about the dataframe
#
## Creating a ggplot ####
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = bill_depth_mm)
) +
  geom_point() +
  geom_smooth(method = "lm")


ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)", y = "Body mass (g)",
    color = "Species", shape = "Species"
  ) +
  scale_color_colorblind()

### Exercises ####
# 1. How many rows are in penguins? How many columns?
glimpse(penguins)

# 2. What does the bill_depth_mm variable in the penguins data frame describe?
?penguins

# 3. Make a scatterplot of bill_depth_mm versus bill_length_mm. That is, make a 
    # scatterplot with bill_depth_mm on the y-axis and bill_length_mm on the x-axis.
    # Describe the relationship between these two variables.
ggplot(
  data = penguins,
  mapping = aes(x = bill_length_mm, y = bill_depth_mm)
) +
  geom_point(aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Bill lenght and bill dpth",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "bill_length_mm", y = "bill_length_mm",
    color = "Species", shape = "Species"
  ) +
  scale_color_colorblind()

# 4. What happens if you make a scatterplot of species versus bill_depth_mm? 
# What might be a better choice of geom?

# 5. Why does the following give an error, and how would you fix it?
 

# 6. What does the na.rm argument do in geom_point()? What is the default 
  # value of the argument? Create a scatterplot where you successfully 
  # use this argument set  to TRUE.  
  ggplot(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  ) +
    geom_point(aes(color = species, shape = species), na.rm = TRUE)+
    geom_smooth(method = "lm") +
    labs(
      title = "Flipper Length and Body mass",
      subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
      x = "Flipper Legth (mm)", y = "Body Mass (g)",
      color = "Species", shape = "Species"
    ) +
    scale_color_colorblind()

# 7. Add the following caption to the plot you made in the previous exercise: “Data
# come from the palmerpenguins package.” Hint: Take a look at the documentation
# for labs().
  ggplot(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  ) +
    geom_point(aes(color = species, shape = species), na.rm = TRUE)+
    geom_smooth(method = "lm") +
    labs(
      title = "Data come from the palmerpenguins package.",
      subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
      x = "Flipper Legth (mm)", y = "Body Mass (g)",
      color = "Species", shape = "Species"
    ) +
    scale_color_colorblind()
# 8. Re-create the following visualization. What aesthetic should bill_depth_mm be
#  mapped to? And should it be mapped at the global level or at the geom level?
  ggplot(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g, color = bill_depth_mm)
  ) +
    geom_point() +
    geom_smooth(method = "lm")
  
# 9. Run this code in your head and predict what the output will look like. Then, run
  #the code in R and check your predictions.
  ggplot(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g, color = island)
  ) +
    geom_point() +
    geom_smooth(se = FALSE)
  
# 10. Will these two graphs look different? Why/why not?
    ggplot(
      data = penguins,
      mapping = aes(x = flipper_length_mm, y = body_mass_g)
    ) +
    geom_point() +
    geom_smooth()
  
    ggplot() +
    geom_point(
      data = penguins,
          mapping = aes(x = flipper_length_mm, y = body_mass_g)
    ) +
    geom_smooth(
      data = penguins,
      mapping = aes(x = flipper_length_mm, y = body_mass_g)
    )
  
  
## ggplot2 Calls ####
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point()

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()

penguins |>
  ggplot(aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()

dat.df |>
  ggplot(aes(x = Edad, y = EstanciaHospitalaria))+
  geom_point()

## Visualizing Distributions ####
### A Categorical Variable ####
ggplot(penguins, aes(x = species)) +
  geom_bar()

# reorder the bars based on their frequencies.
ggplot(penguins, aes(x = fct_infreq(species))) + #  transforming the variable to a factor
  geom_bar()

### A Numerical Variable ####
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 200)

ggplot(penguins, aes(x = body_mass_g)) +
  geom_density()

### Exercises ####
# 1. Make a bar plot of species of penguins, where you assign species to the y
# aesthetic. How is this plot different?
ggplot(penguins, aes(y = species)) +
  geom_bar()

# 2. How are the following two plots different? Which aesthetic, color or fill, is
# more useful for changing the color of bars?
ggplot(penguins, aes(x = species)) +
  geom_bar(color = "red")
  
ggplot(penguins, aes(x = species)) +
  geom_bar(fill = "red") # This one

# 3. What does the bins argument in geom_histogram() do?

# 4. Make a histogram of the carat variable in the diamonds dataset that is available
# when you load the tidyverse package. Experiment with different binwidth values.
# What value reveals the most interesting patterns?
diamonds <- as.data.frame(diamonds)  

diamonds |>
  ggplot(aes(x = carat))+
  geom_histogram(bins = 30, binwidth = 0.2)

## VISUALIZING RELATIONSHIPS ####

### A Numerical and a Categorical Variable ####
ggplot(penguins, aes(x = species, y = body_mass_g)) +
  geom_boxplot()

ggplot(penguins, aes(x = body_mass_g, color = species)) +
  geom_density(linewidth = 0.75)

ggplot(penguins, aes(x = body_mass_g, color = species, fill = species)) +
  geom_density(alpha = 0.5) #alpha es para que quede "transparante"

### Two Categorical Variables ####
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar()

# Relative frequency plot
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill")

### Two Numerical Variables ####
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()

### Three or More Variables ####
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = island))

# Facets
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = species)) +
  facet_wrap(~island)

### 2.5.5 Exercises ####
# 1. The mpg data frame that is bundled with the ggplot2 package contains 234
# observations collected by the US Environmental Protection Agency on 38 car
# models. Which variables in mpg are categorical? Which variables are numerical?
# (Hint: Type ?mpg to read the documentation for the dataset.) How can you see
# this information when you run mpg?
data(mpg)
mpg <- as.data.frame(mpg)
glimpse(mpg)
help(mpg)

#2. Make a scatterplot of hwy versus displ using the mpg data frame. Next, map a
# third, numerical variable to color, then size, then both color and size, and
# then shape. How do these aesthetics behave differently for categorical versus
# numerical variables?
ggplot(
  mpg,
  aes(x = hwy, y = displ, color = cty)
) +
  geom_point()

ggplot(
  mpg,
  aes(x = hwy, y = displ, size = cty)
) +
  geom_point()

ggplot(
  dat.df,
  aes(x = Edad, y = NumeroIntentosPrevios, color = EstanciaHospitalaria, size = EstanciaHospitalaria, shape = Sexo)
) +
  geom_point()

ggplot(
  mpg, 
  aes(x = hwy, y = displ, size = cty, color = cty, shape = drv)
) + 
  geom_point()

# 3. In the scatterplot of hwy versus displ, what happens if you map a third 
# variable to linewidth?

ggplot(mpg, aes(x = hwy, y = displ, linewidth = cty)) + 
  geom_point()
# Since there is no line to alter the width of, nothing happens. The code
# runs as though that aesthetic was not specified.

# 4 What happens if you map the same variable to multiple aesthetics?

ggplot(mpg, aes(x = hwy, y = hwy, color = hwy)) + 
  geom_point()
# Plotting that maps hwy to x, yy, and color aesthetics. ggplot2 will allow 
# you to map the same variable to multiple aesthetics, but the resulting 
# plot is not useful.

# 5. Make a scatterplot of bill_depth_mm versus bill_length_mm and color the
# points by species. What does adding coloring by species reveal about the rela‐
# tionship between these two variables? What about faceting by species?

ggplot(
  penguins,
  aes(x = bill_depth_mm, y = bill_length_mm, colour = species)
) +
  geom_point() +
  facet_grid(~species)

ggplot(
  penguins,
  aes(x = bill_depth_mm, y = bill_length_mm, colour = species)
) +
  geom_point() +
  facet_wrap(~species)
# Adelies tend to have higher bill depth while Gentoo have longer bills 
# and Chinstrap have deeper and longer bills.

# 6. Why does the following yield two separate legends? How would you fix it to
# combine the two legends?
ggplot(
  data = penguins,
  mapping = aes(
    x = bill_length_mm, y = bill_depth_mm,
    color = species, shape = species
  )
) +
  geom_point() +
  labs(color = "Species")


ggplot(
  data = penguins,
  mapping = aes(
    x = bill_length_mm, y = bill_depth_mm,
    color = species, shape = species
  )
) +
  geom_point() +
  labs(color = "Species",
       shape = "Species"
       )

# 7. Create the two following stacked bar plots. Which question can you answer 
# with the first one? Which question can you answer with the second one?
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill")

ggplot(penguins, aes(x = species, fill = island)) +
  geom_bar(position = "fill")


### Saving Your Plots ####
ggplot(
  data = penguins,
  aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()
ggsave(filename = "penguin.plot.png")

### 2.6.1 Exercises ####
# 1. Run the following lines of code. Which of the two plots is saved as
# mpg-plot.png? Why?
ggplot(mpg, aes(x = class)) +
  geom_bar()
ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point()
ggsave("mpg-plot.png")

# 2. What do you need to change in the previous code to save the plot as a PDF
# instead of a PNG? How could you find out what types of image files would work
# in ggsave()?
ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point()
ggsave("mpg-plot.pdf")


## Workflow:Basics
### Coding Basics ####
1 / 220 * 30
(59 + 73 + 2) / 3
sin(pi /2)

# Create new objects 
x <- 2 * 4

#combine multiple elements into a vector with c():
primes <- c(2, 3, 5, 7, 11, 13)

#  basic arithmetic on vectors is applied to every element of the vector:
primes * 2

primes - 1

# All R statements where you create objects, assignment statements, 
# have the same form:
  object_name <- value


this_is_a_really_long_name <- 2.5  

seq(from = 1, to = 10)
seq(1, 10)

x <- "hello world" 

### Exercise ####
# 1. Why does this code not work?
my_variable <- 10
my_varıable

# 2. Tweak each of the following R commands so that they run correctly:
libary(tidyverse)
ggplot(mpg, aes(x = displ, y = hwy))+
  geom_point()+
  geom_smooth(method = "lm")

# 3. Press Option+Shift+K/Alt+Shift+K. What happens? How can you get to the
# same place using the menus?

# 4. Let’s revisit an exercise from “Saving Your Plots” on page 30. Run the following
# lines of code. Which of the two plots is saved as mpg-plot.png? Why?
my_bar_plot <- ggplot(mpg, aes(x = class)) +
 geom_bar()
my_scatter_plot <- ggplot(mpg, aes(x = cty, y = hwy)) +
 geom_point()
ggsave(filename = "mpg-plot.png", plot = my_bar_plot)


## 3 Data Transformation ####
library(nycflights13)
flights
glimpse(flights)

### dplyr Basics ####
flights |>
  filter(dest == "IAH") |>
  group_by(year, month, day) |>
  summarize(
    arr_delay = mean(arr_delay, na.rm = TRUE)
  )

### Rows ####
#### filter() ####
flights |>
  filter(dep_delay > 120)

# > (greater than), 
# >= (greater than or equal to)
# < (less than)
# <= (less than or equal to)
# == (equal to)
# != (not equal to). 

# Flights that departed on January 1
flights |>
  filter(month == 1 & day == 1)

dat.df |>
  filter(Mes == "Diciembre" & Dia == "Miércoles")


# Flights that departed in January or February
flights |>
  filter(month == 1 | month == 2)

dat.df |>
  filter(Mes == "Diciembre" | Mes == "Junio")

# A shorter way to select flights that departed in January or February
flights |>
  filter(month %in% c(1, 2))

dat.df |>
  filter(NumeroIntentosPrevios %in% c(4, 5))


# save the result, you need to use the assignment operator, <-:
jan1 <- flights |>
  filter(month == 1 & day == 1)


#### arrange() ####
flights |>
  arrange(year, month, day, dep_time)

dat.df |>
  arrange(Mes, Dia, Lugar, sort = TRUE)

# orders flights from most to least delayed:
flights |>
  arrange(desc(dep_delay))

dat.df |>
  arrange(desc(Edad))


#### distinct() ####
# Remove duplicate rows, if any
flights |>
  distinct()

dat.df |>
  distinct()

# Find all unique origin and destination pairs
flights |>
  distinct(origin, dest)

dat.df |>
  distinct(Procedencia, Sexo)

dat.df |>
  distinct(Edad, Sexo)

dat.df |>
  distinct(AltaVoluntaria, EstanciaHospitalaria)


# to keep the other columns when filtering for unique rows
flights |>
  distinct(origin, dest, .keep_all = TRUE)

#  to find the number of occurrences
flights |>
  count(origin, dest, sort = TRUE)

dat.df |>
  count(Edad, Sexo, sort = TRUE)

dat.df |>
  count(AltaVoluntaria, PersistenciaIdeacion, sort = TRUE)

dat.df |>
  count(AltaVoluntaria, EstanciaHospitalaria, sort = TRUE)

dat.df |>
  count(AltaVoluntaria, Subregistro, sort = TRUE)

#### Exercises ####
# 1. In a single pipeline for each condition, find all flights that meet the condition: 
# Had an arrival delay of two or more hours
flights |>
  filter(arr_delay >= 2)

# Flew to Houston (IAH or HOU)
fth <- flights |>
  filter(dest %in% c("IAH", "HOU"))

# Were operated by United, American, or Delta

# Departed in summer (July, August, and September)
flights |>
  filter(month %in% c(7, 8, 9))

# Arrived more than two hours late, but didn’t leave late
flights |>
  filter(arr_delay > 2 & dep_delay == 0)

# Were delayed by at least an hour, but made up more than 30 minutes in flight
flights |>
  filter(dep_delay >= 1 & air_time > 30)

# 2. Sort flights to find the flights with the longest departure delays. 
flights |>
  filter(dep_delay > 800)

# Find the flights that left earliest in the morning.
flights |>
  filter(dep_time <500)

# 3. Sort flights to find the fastest flights. (Hint: Try including 
# a math calculation inside of your function.)
flights |>
  filter( air_time <30)

# 4. Was there a flight on every day of 2013?
flights |>
  distinct(year, month, day) |>
  count() |>
  as.numeric()

# 5. Which flights traveled the farthest distance? Which traveled the least distance?
flights |>
  arrange(desc(distance)) |>
  select(origin, dest, distance, air_time, carrier) |>
  # Distinct added to remove same flight (on different days) repeating in top 5
  distinct(origin, dest, .keep_all = TRUE) |>
  slice_head(n = 5) |>
  gt()

# EXIT --------------------------------------------------------------------
getwd() # Identify the current working directory.
ls() # List all objects in the working
# directory.
ls.str() # List all objects, with finite detail.
list.files() # List files at the PC directory.
save.image("R_for_Data_Science.rdata")
getwd() # Identify the current working directory.
ls() # List all objects in the working
# directory.
ls.str() # List all objects, with finite detail.
list.files() # List files at the PC directory.
alarm() # Alarm, notice of upcoming action.
q() # Quit this session.
# Prepare for Save workspace image? query.