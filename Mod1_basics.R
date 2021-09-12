?"+" #documentation about operators

data() # show all availabale data

log #without () shows the code for log() function

args(log) # shows all arguments of log()

class(ls) #class of the object

require(dslabs)
str(murders)
head(murders)
names(murders)
#murders$state = murders[["state"]]
identical(murders$state,murders[["state"]])
# no of unique regions in region variable
length(levels(murders$region))
nlevels(murders$region)

# no of states per region
table(murders$region)


#---
a <-2
b <--1  
c <--4 
(-b+sqrt((b^2)-4*a*c))/(2*a)
(-b-sqrt((b^2)-4*a*c))/(2*a)
log(1024,base=4)

data(movielens)
str(movielens)
class(movielens$title)
class(movielens$genres)
nlevels(movielens$genres)
#------------


# We can also name the elements of a numeric vector
# Note that the two lines of code below have the same result
codes <- c(italy = 380, canada = 124, egypt = 818)
codes <- c("italy" = 380, "canada" = 124, "egypt" = 818)

#-------- We can also name the elements of a numeric vector using the names() function
codes <- c(380, 124, 818)
country <- c("italy","canada","egypt")
names(codes) <- country

# Using square brackets is useful for subsetting to access specific elements of a vector
codes[2]
codes[c(1,3)]
codes[1:2]

# If the entries of a vector are named, they may be accessed by referring to their name
codes["canada"]
codes[c("egypt","italy")]


x <- seq(0, 100, length.out = 5)
#answer 0, 25, 50, 75, 100

a <- seq(1, 10)
class(a) #integer

a <- seq(1, 10, length.out = 100)
class(a) #numeric

class(1)
class(1L)


#------------- sort

sort(murders$total)

x <- c(31, 4, 15, 92, 65)
x
sort(x)    # puts elements in order--- Ans : 4,15,31,65,92

index <- order(x)    # returns index that will put x in order
x[index]    # rearranging by this index puts elements in order
order(x) #---- Ans : 2,3,1,5,4

murders$state[1:10]
murders$abb[1:10]

index <- order(murders$total)
murders$abb[index]    # order abbreviations by total murders

max(murders$total)    # highest number of total murders
i_max <- which.max(murders$total)    # index with highest number of murders
murders$state[i_max]    # state name with highest number of total murders

x <- c(31, 4, 15, 92, 65)
x
rank(x)    # returns ranks (smallest to largest)---Ans : 3 1 2 5 4


x <- c(1, 2, 3)
ind <- c(FALSE, TRUE, FALSE)
x[!ind]

library(dslabs)
data(na_example)
ind <- is.na(na_example)

# We saw that this gives an NA
mean(na_example)

# Compute the average, for entries of na_example that are not NA 
mean(na_example[!ind])


# Define an object `x` with the numbers 1 through 100
x <- seq(1:100)
# Compute the sum Ans : 1 + 1/2^2 + 1/3^2 + .... + 1/100^2
sum(1/x^2)

#----- ex
name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)

time_o <- time/60
time_o[4]

speed <- distance/time_o
speed[1]
which.max(speed)

#------------which / match
index <- which(murders$state == "Massachusetts")
index
murder_rate[index]


index <- match(c("New York", "Florida", "Texas"), murders$state)
index
murders$state[index]
murder_rate[index]

# to see if Boston, Dakota, and Washington are states
c("Boston", "Dakota", "Washington") %in% murders$state

#------filter/select/ %>% 
install.packages("dplyr")
library(dplyr)

# adding a column with mutate
library(dslabs)
data("murders")
murders <- mutate(murders, rate = total / population * 100000)

# subsetting with filter
filter(murders, rate <= 0.71)

# selecting columns with select
new_table <- select(murders, state, region, rate)

# using the pipe
murders %>% select(state, region, rate) %>% filter(rate <= 0.71)

#----stringsAsFactors = FALSE
# creating a data frame with stringAsFactors = FALSE
grades <- data.frame(names = c("John", "Juan", "Jean", "Yao"), 
                     exam_1 = c(95, 80, 90, 85), 
                     exam_2 = c(90, 85, 85, 90),
                     stringsAsFactors = FALSE)

#------rank decending
rate <-  murders$total/ murders$population * 100000

# Redefine murders to include a column named rank
# with the ranks of rate from highest to lowest
murders <- mutate(murders,rank(-rate))

#-----show the top 5

# Add the necessary columns
murders <- mutate(murders, rate = total/population * 100000, rank = rank(-rate))

# Filter to show the top 5 states with the highest murder rates
filter(murders,rank<=5)

#------ !(not operator)
# Use filter to create a new data frame no_south
no_south <- filter(murders,!region=="South")

#----------

library(dplyr)
library(dslabs)
data("murders")

# a simple scatterplot of total murders versus population
x <- murders$population /10^6
y <- murders$total
plot(x, y)

# a histogram of murder rates
murders <- mutate(murders, rate = total / population * 100000)
hist(murders$rate)

# boxplots of murder rates by region
boxplot(rate~region, data = murders)

#---assighn
library(dplyr)
library(dslabs)
data(heights)
options(digits = 3)    # report 3 significant digits for all answers

str(heights)
ind <-  heights$height >  mean(heights$height)
sum(ind & heights$sex=="Female")

mean(heights$sex=="Female")

min(heights$height)

match(min(heights$height),heights$height)
heights$sex[1032]


max(heights$height)

x <- 50:82

sum(!x %in% heights$height)

heights2 <- mutate(heights,ht_cm=height*2.54)
heights2
heights2$ht_cm[18]

mean(heights2$ht_cm)

females <- filter(heights2,sex=="Female")
nrow(females)

mean(females$ht_cm)

data(olive)
head(olive)
str(olive)

plot(olive$palmitic,olive$palmitoleic)
hist(olive$eicosenoic)

boxplot(palmitic~region,data = olive)

#----- If else old way
library(dslabs)
data(murders)
murder_rate <- murders$total / murders$population*100000
ind <- which.min(murder_rate)
if(murder_rate[ind] < 0.5){
  print(murders$state[ind]) 
} else{
  print("No state has murder rate that low")
}

#---ifelse funcyion
data(na_example)
no_nas <- ifelse(is.na(na_example), 0, na_example) 
sum(is.na(no_nas))

#---- the any() and all() functions evaluate logical vectors
z <- c(TRUE, TRUE, FALSE)
any(z)
all(z)

#--- functions
avg <- function(x, arithmetic = TRUE){
  n <- length(x)
  ifelse(arithmetic, sum(x)/n, prod(x)^(1/n))
}


#------- a for-loop for our summation
m <- 25
s_n <- vector(length = m) # create an empty vector
for(n in 1:m){
  s_n[n] <- compute_s_n(n)
}

#-- apply/sapply/tapply/mapply
#-- split/cut/identical/reduce/uantile/unique


#----assighnment
library(dslabs)
data(heights)
library(dplyr)

sum(ifelse(heights$sex=="Female",1,2))


mean(ifelse(heights$height>72,heights$heigh,0))

inches_to_ft <- function(n) n/12
inches_to_ft(144)
sum(ifelse(inches_to_ft(heights$height)<5,1,0))




