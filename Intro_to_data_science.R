# Start ####
install.packages("tidyverse", "dslabs")
library(tidyverse)
library(dslabs)
data(murders)
installed.packages()


# 3.14.1 Subsetting with logicals ####
murder_rate <- murders$total / murders$population * 100000
ind <- murder_rate < 0.71 #It compares a vector to a single number, performing the test for each entry.
ind

ind <- murder_rate <= 0.71 #it compares if a value is less or equal:
ind

murders$state[ind]

# 3.14.2 Logical operators ####
TRUE & TRUE
TRUE & FALSE
FALSE & FALSE

# Form two logicals
west <- murders$region == "West"
safe <- murder_rate <= 1

# use the & to get a vector of logicals that tells us which states satisfy 
# both conditions:
ind <- safe & west
murders$state[ind]
safewest<-murders$state[ind]

# 3.14.3 which ####
# Look up California’s murder rate. Convert vectors of
# logicals into indexes instead of logicals. 
ind <- which(murders$state == "California")
murder_rate[ind]

# 3.14.4 match ####
# Find out the murder rates for several states using
# the function match (Tells which indexes of a second
# vector match each of the entries of a first vector
ind <- match(c("New York", "Florida", "Texas"), murders$state)
ind
murder_rate[ind] #Look at the murder rates:

# 3.14.5 %in% ####
# A logical that tells us whether or not each element 
# of a first vector is in a second using the  %in%.
# Fin if Boston, Dakota and Washington are states:
c("Boston", "Dakota", "Washington") %in% murders$state

# This lines produce the same index in anothert order:
match(c("New York", "Florida", "Texas"), murders$state)
which(murders$state%in%c("New York", "Florida", "Texas"))

# 3.15 Exercises ####
# Compute the per 100,000 murder rate for each state and store it as 
#  murder_rate (object). Then use logical operators to create 
# "low" (logical vector) telling which entries of murder_rate are <1.
murder_rate <- murders$total / murders$population * 100000
low <- murder_rate < 1 #It compares a vector to a single number, performing the test for each entry.
low

# Now use the results from the previous exercise and the function which to
# determine the indices of murder_rate associated with values lower than 1.
which(low)

# Use the results from the previous exercise to report the names of the states
# with murder rates lower than 1.
