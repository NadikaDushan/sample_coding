#getwd()
#setwd("D:\R_projects\Jumble")

library(tidyverse)
library(dslabs)
take_poll(25)    # draw 25 beads

# The vector `p` contains 100 proportions of Democrats ranging from 0 to 1 using the `seq` function
p <- seq(0, 1, length = 100)

# The vector `sample_sizes` contains the three sample sizes
sample_sizes <- c(25, 100, 1000)

# Write a for-loop that calculates the standard error `se` for every value of `p` for each of the three samples sizes `N` in the vector `sample_sizes`. Plot the three graphs, using the `ylim` argument to standardize the y-axis across all three plots.

for (N in sample_sizes){
  se <- sqrt(p*(1-p)/N)
  plot(p,se,ylim=c(0,0.1))
}

#Computing the probability of X_bar being within .01 of p
X_hat <- 0.48
se <- sqrt(X_hat*(1-X_hat)/25)
pnorm(0.01/se) - pnorm(-0.01/se)


p <- 0.45
N <- 1000
B <- 10000


x_hat <- replicate(B,{
 x <- sample(c(0,1),N,replace = TRUE,prob = c(1-p,p))
 mean(x)
 })

mean(x_hat)
sd(x_hat)

library(tidyverse)
library(gridExtra)

p1 <- data.frame(x_hat = x_hat) %>%
  ggplot(aes(x_hat)) +
  geom_histogram(binwidth = 0.005, color = "black")

p2 <- data.frame(x_hat = x_hat) %>%
  ggplot(aes(sample = x_hat)) +
  stat_qq(dparams = list(mean = mean(x_hat), sd = sd(x_hat))) +
  geom_abline() +
  ylab("X_hat") +
  xlab("Theoretical normal")

grid.arrange(p1, p2, nrow=1)

#Plotting margin of error in an extremely large poll over a range of values of p
N <- 100000
p <- seq(0.35, 0.65, length = 100)
SE <- sapply(p, function(x) 2*sqrt(x*(1-x)/N))
data.frame(p = p, SE = SE) %>%
  ggplot(aes(p, SE)) +
  geom_line() 

#qqplot without ggplot
qqnorm(errors)
qqline(errors)

#----------------------
data("nhtemp")
head(nhtemp)
# handling ts data type.

data.frame(year=as.numeric(time(nhtemp)),temperature = as.numeric(nhtemp)) %>%
  ggplot(aes(year,temperature)) + geom_point() + geom_smooth() + ggtitle("Average Yearly Temperatures in New Haven")
#stat_smooth has more options
data.frame(year=as.numeric(time(nhtemp)),temperature = as.numeric(nhtemp)) %>%
  ggplot(aes(year,temperature)) + geom_point() + stat_smooth() + ggtitle("Average Yearly Temperatures in New Haven")

#-----
z <- qnorm(0.995)    # calculate z to solve for 99% confidence interval
pnorm(qnorm(0.995))    # demonstrating that qnorm gives the z value for a given probability
pnorm(qnorm(1-0.995))    # demonstrating symmetry of 1-qnorm
pnorm(z) - pno
rm(-z)    # demonstrating that this z value gives correct probability for interval

#--- monte carlo for CI
p <- 0.45
N <- 1000
X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))    # generate N observations
X_hat <- mean(X)    # calculate X_hat
SE_hat <- sqrt(X_hat*(1-X_hat)/N)    # calculate SE_hat, SE of the mean of N observations
c(X_hat - 2*SE_hat, X_hat + 2*SE_hat)    # build interval of 2*SE above and below mean

B <- 10000
inside <- replicate(B,{
  x <- sample(c(0,1), size = N , replace = TRUE , prob = c(1-p,p))
  x_hat <- mean(x)
  SE_hat <- sqrt(x_hat*(1-x_hat)/N)
  between(p,x_hat-2*SE_hat,x_hat+2*SE_hat)
})
mean(inside)

# Confidence interval for the spread with sample size of 25 - power of the test???
N <- 25
x_hat <- 0.48
(2 * x_hat - 1) + c(-1.96, 1.96) * 2 * sqrt(x_hat * (1 - x_hat) / N)

require(dslabs)
require(tidyverse)
data(polls_us_election_2016)
polls <- polls_us_election_2016 %>% filter(enddate>=as.Date("2016-10-31") & state=="U.S.")
polls <- polls_us_election_2016 %>% filter(enddate>="2016-10-31" & state=="U.S.")


#--- How to create plot for only the count of groupbu variable is five or more
#data %>% group_by(variable_for_grouping) %>% filter(n() >= 5)


#----Simulating polls to aggregate the poll results
d <- 0.039
Ns <- c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516)
p <- (d+1)/2

confidence_intervals <- sapply(Ns,function(N){
  X <- sample(c(0,1),size = N,replace = TRUE,prob = c(1-p,p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  2*c(X_hat, X_hat - 2*SE_hat, X_hat + 2*SE_hat) - 1 #CI of spread d=2p-1| 2*X_hat-1 == d_hat same goes with CI
})

polls <- data.frame(poll = 1:ncol(confidence_intervals), #poll <- seq(1:ncol(CI))
                    t(confidence_intervals), #transpose of the vector it has 3 variables "estimate", "low", "high"
                    sample_size = Ns)
names(polls) <- c("poll", "estimate", "low", "high", "sample_size")
polls

# Calculating the spread of combined polls

d_hat <- polls %>%
  summarize(avg = sum(estimate*sample_size) / sum(sample_size)) %>%
  .$avg

p_hat <- (1+d_hat)/2
moe <- 2*1.96*sqrt(p_hat*(1-p_hat)/sum(polls$sample_size))   
round(d_hat*100,1)
round(moe*100, 1)

#-------------------------

library(dslabs)
library(tidyverse)
data(polls_us_election_2016)
names(polls_us_election_2016)

levels(polls_us_election_2016$grade)

polls <- polls_us_election_2016 %>% 
  filter(state=="U.S." & enddate>="2016-10-31" & (grade %in% c("A+","A","A-","B+") | is.na(grade)))

polls <- polls %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

d_hat <- polls %>% summarise(d_hat=spread*samplesize/sum(samplesize)) %>% 
  .$d_hat #with this final step covert the data.frame in to numeric vector


p_hat <- (d_hat+1)/2
moe <- 1.96 * 2 * sqrt(p_hat*(1-p_hat)/sum(polls$samplesize)) #d=2p-1 -> se(d)= 2 se(p) d_hat=2*x_hat - 1

polls %>%
  ggplot(aes(spread)) +
  geom_histogram(color="black", binwidth = .01)

polls %>% group_by(pollster) %>% #group data based on pollester
  filter(n() >= 6) %>%   # n() counts the number by grouped variable 
  ggplot(aes(pollster, spread)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# collect last result before the election for each pollster
one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%      # keep latest poll
  ungroup()

# histogram of spread estimates
one_poll_per_pollster %>%
  ggplot(aes(spread)) + geom_histogram(binwidth = 0.01)

# construct 95% confidence interval
results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - 1.96*se, end = avg + 1.96*se)
round(results*100, 1)

#--montecarlo for bayse therom

prev <- 0.00025    # disease prevalence
N <- 100000    # number of tests
outcome <- sample(c("Disease", "Healthy"), N, replace = TRUE, prob = c(prev, 1-prev))

N_D <- sum(outcome == "Disease")    # number with disease
N_H <- sum(outcome == "Healthy")    # number healthy

# for each person, randomly determine if test is + or -
accuracy <- 0.99
test <- vector("character", N) #create empty charactor vector. code works evem without this.
#testing patience with test
test[outcome == "Disease"] <- sample(c("+", "-"), N_D, replace=TRUE, prob = c(accuracy, 1-accuracy))
test[outcome == "Healthy"] <- sample(c("-", "+"), N_H, replace=TRUE, prob = c(accuracy, 1-accuracy))

table(outcome, test)

#--use of pnorm
pnorm(0,exp_value,se)
#---equals to 
pnorm((0-exp_value)/se)

####--------------ELECTION FORCASTING---------------------
require(dslabs)
require(tidyverse)
data(polls_us_election_2016)
head(polls_us_election_2016)
names(polls_us_election_2016)
levels(polls_us_election_2016$state)
levels(polls_us_election_2016$grade)

polls <- polls_us_election_2016 %>% 
  filter(state=="U.S." & enddate>="2016-10-31" & (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

one_poll_per_pollster <- polls %>% group_by(pollster) %>% filter(enddate==max(enddate)) %>% ungroup()

results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - 1.96*se, end = avg + 1.96*se)

#-----------length(spread) == n()
#results <- one_poll_per_pollster %>%
#  summarize(avg = mean(spread), se = sd(spread)/sqrt(n())) %>%
#  mutate(start = avg - 1.96*se, end = avg + 1.96*se)

#--- mu and tau probability of difference when no polls are made
#----posterior probability when some poll data are observed
mu <- 0
tau <- 0.035
sigma <- results$se # sigma and Y observed mean and sigma 
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
posterior_mean <- B*mu + (1-B)*Y # true mean given that some poll data observed before the election
posterior_se <- sqrt(1 / (1/sigma^2 + 1/tau^2))

# 95% credible interval
posterior_mean + c(-1.96, 1.96)*posterior_se

# probability of d > 0
1 - pnorm(0, posterior_mean, posterior_se)


#---Simulated data with x= d + pollster sampling error
#---the variance ~N(0,2sqrt(p(1-p)))
#--expected value is d + sampling error

J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
X <- d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))


#-- adding multiple polster
#----now have pollster sampling varibility this is not pollster specific house variability. just the random error. 

I <- 5
J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
X <- sapply(1:I, function(i){
  d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
})

# adding pollster error which is uniq to single pollster

I <- 5
J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
h <- rnorm(I, 0, 0.025)    # assume standard error of pollster-to-pollster variability is 0.025
X <- sapply(1:I, function(i){
  d + h[i] + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
})


pivot_longer(as.data.frame(X),cols = V1:V5,names_to = "Pollster") %>% 
  mutate(Pollster=as.factor(Pollster)) %>% ggplot(aes(x=value,y=Pollster)) + geom_point() + geom_line()


# worked ---data.frame(x=c(X),y=rep(1:6,5)) %>% ggplot(aes(x=x,y=y)) + geom_point() + ylim(0,6)


# ---simulation is over. simulated data match with observed pattern. theroy holds

#--now we are adding general yearly bias for entire data set
#-----adding extra 0.025(this is not pollster house effect) variablity to previously found "results$se". 
#---this result$se includes all pollster house variability + random variability

mu <- 0
tau <- 0.035
sigma <- sqrt(results$se^2 + .025^2)
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)

posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1 / (1/sigma^2 + 1/tau^2))

1 - pnorm(0, posterior_mean, posterior_se)

#-----------------------------------Top 5 states ranked by electoral votes
#---polls_us_election_2016 and results_us_election_2016 two data sets
library(tidyverse)
library(dslabs)
data("polls_us_election_2016")
head(results_us_election_2016)

#arrange /match functions should be used
results_us_election_2016 %>% arrange(desc(electoral_votes)) %>% top_n(5, electoral_votes)

#sort returns a sorted vector. this do not arrange the entire dataframe according to sorted varible as in excel.
#results_us_election_2016 %>%mutate(sort(clinton,decreasing = TRUE)) %>% top_n(5, clinton)

head(polls_us_election_2016)
names(polls_us_election_2016)
#order alphabatically. unique function do not so
levels(as.factor(polls_us_election_2016$state))
#unique(results_us_election_2016$state)

results <- polls_us_election_2016 %>% filter(state!="U.S." &
                                               !str_detect(state,"CD") &
                                               enddate >="2016-10-31" & 
                                               (grade %in% c("A+","A","A-","B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>% 
  group_by(state) %>%
  summarize(avg = mean(spread), sd = sd(spread), n = n()) %>%
  mutate(state = as.character(state)) # summarize create state as a factor. now changing back it into charactor

results %>% filter(!is.na(sd)) %>%
  add_column(y=0) %>%  # need to add dummy column to plot all dots in a line
  ggplot(aes(sd,y)) + geom_point() + geom_boxplot()

# 10 closest races = battleground states
results %>% arrange(abs(avg))

# joining electoral college votes and results
results <- left_join(results, results_us_election_2016, by="state")

# states with no polls: note Rhode Island and District of Columbia = Democrat
#(not included in results dbf,out of our study)
results_us_election_2016 %>% filter(!state %in% results$state)

#dealing with just one poll. no sd for those.
results <- results %>% mutate(sd = ifelse(is.na(sd),median(results$sd,na.rm=TRUE),sd))

#----Calculating the posterior mean and posterior standard error
mu <- 0
tau <- 0.02
results %>% mutate(sigma = sd/sqrt(n),
                   B = sigma^2/ (sigma^2 + tau^2),
                   posterior_mean = B*mu + (1-B)*avg,
                   posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2))) %>%
  arrange(abs(posterior_mean))

#----Monte Carlo simulation of Election Night results (no general bias). this calculates state wise

mu <- 0
tau <- 0.02
clinton_EV <- replicate(1000, { results %>% mutate(
  sigma = sd/sqrt(n),
  B = sigma^2/(sigma^2+tau^2),
  posterior_mean = B*mu + (1-B)*avg,
  posterior_se = sqrt(1 / (1/sigma^2 + 1/tau^2)),
  simulated_result = rnorm(length(posterior_mean),posterior_mean,posterior_se),
  clinton = ifelse(simulated_result>0,electoral_votes,0)) %>%
    summarise(clinton = sum(clinton)) %>% .$clinton + 7
 })
mean(clinton_EV > 269)    # over 269 votes wins election

# histogram of outcomes
P1 <- data.frame(clinton_EV) %>%
  ggplot(aes(clinton_EV)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 269)

#----Monte Carlo simulation of Election Night results WITH GENERAL BIAS. this calculates state wise

mu <- 0
tau <- 0.02
bias_sd <- 0.03
clinton_EV_2 <- replicate(1000, { results %>% mutate(
  sigma = sqrt((sd^2/n)+bias_sd^2),
  B = sigma^2/(sigma^2+tau^2),
  posterior_mean = B*mu + (1-B)*avg,
  posterior_se = sqrt(1 / (1/sigma^2 + 1/tau^2)),
  simulated_result = rnorm(length(posterior_mean),posterior_mean,posterior_se),
  clinton = ifelse(simulated_result>0,electoral_votes,0)) %>%
    summarise(clinton = sum(clinton)) %>% .$clinton + 7
})
mean(clinton_EV_2 > 269)    # over 269 votes wins election

# histogram of outcomes
P2 <- data.frame(clinton_EV_2) %>%
  ggplot(aes(clinton_EV_2)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 269)

require(gridExtra)
grid.arrange(P1,P2,nrow=2)

#-------------------- Forecasting -------------
one_pollster <- polls_us_election_2016 %>% filter(pollster == "Ipsos" & state == "U.S.") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

se <- one_pollster %>%
  summarize(empirical = sd(spread),
            theoretical = 2*sqrt(mean(spread)*(1-mean(spread))/min(samplesize)))
se

polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ggplot(aes(enddate, spread)) +
  geom_smooth(method = "loess", span = 0.1) +
  geom_point(aes(color = pollster), show.legend = FALSE, alpha = 0.6)

#---original function with gather() - old -------
polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  select(enddate, pollster, rawpoll_clinton, rawpoll_trump) %>%
  rename(Clinton = rawpoll_clinton, Trump = rawpoll_trump) %>%
  gather(candidate, percentage, -enddate, -pollster) %>%
  mutate(candidate = factor(candidate, levels = c("Trump", "Clinton"))) %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  ggplot(aes(enddate, percentage, color = candidate)) +
  geom_point(show.legend = FALSE, alpha = 0.4) +
  geom_smooth(method = "loess", span = 0.15) +
  scale_y_continuous(limits = c(30, 50))


#---- Recreated with pivot_longer() - New function
polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  select(enddate, pollster, rawpoll_clinton, rawpoll_trump) %>%
  rename(Clinton = rawpoll_clinton, Trump = rawpoll_trump) %>%
  pivot_longer(c(Clinton,Trump),names_to = "candidate", values_to = "percentage") %>%
  mutate(candidate = factor(candidate, levels = c("Trump", "Clinton"))) %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  ggplot(aes(enddate, percentage, color = candidate)) +
  geom_point(show.legend = FALSE, alpha = 0.4) +
  geom_smooth(method = "loess", span = 0.15) +
  scale_y_continuous(limits = c(30, 50))

#---------------data camp exercise - -
# The `cis` data have already been loaded for you
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

cis <- polls %>% mutate(X_hat = (spread+1)/2, se = 2*sqrt(X_hat*(1-X_hat)/samplesize), 
                        lower = spread - qnorm(0.975)*se, upper = spread + qnorm(0.975)*se) %>%
  select(state, startdate, enddate, pollster, grade, spread, lower, upper)

add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)

ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")



# Create an object called `p_hits` that summarizes the proportion of hits for each pollster that has at least 5 polls.
p_hits <- ci_data %>% mutate(hit=upper>actual_spread & actual_spread > lower) %>% group_by(pollster) %>%
  filter(n()>=5) %>% summarize(proportion_hits=mean(hit),n=n(),grade=grade[1]) %>% arrange(desc(proportion_hits))

p_hits <- ci_data %>% mutate(hit=upper>actual_spread & actual_spread > lower) %>% group_by(state) %>%
  filter(n()>=5) %>% summarize(proportion_hits=mean(hit),n=n()) %>% arrange(desc(proportion_hits))

p_hits %>% ggplot(aes(x=state,y=proportion_hits)) + geom_bar(stat="identity") + coord_flip()

#-----Ascending and descending ordering of states based on mean(hit) + use of weight= for proportion
#-----incited of identity - this is harder because mean(hit) is not a natural value

p_hits <- ci_data %>% mutate(hit=upper>actual_spread & actual_spread > lower) %>% group_by(state) %>%
  filter(n()>=5) %>% summarize(proportion_hits=mean(hit),n=n())

p_hits %>% mutate(state=fct_reorder(state,proportion_hits)) %>% ggplot(aes(x=state)) + geom_bar(aes(weight=proportion_hits)) + coord_flip() 
p_hits %>% mutate(state=fct_reorder(state,proportion_hits,.desc = TRUE)) %>% ggplot(aes(x=state)) + geom_bar(aes(weight=proportion_hits)) + coord_flip() 


#------------ Data Camp Example
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

cis <- polls %>% mutate(X_hat = (spread+1)/2, se = 2*sqrt(X_hat*(1-X_hat)/samplesize), 
                        lower = spread - qnorm(0.975)*se, upper = spread + qnorm(0.975)*se) %>%
  select(state, startdate, enddate, pollster, grade, spread, lower, upper)

add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)

ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")


errors <- ci_data %>% mutate(error=spread-actual_spread,hit=sign(spread)==sign(actual_spread))


totals <- errors %>%
  filter(grade %in% c("A-", "C-")) %>%
  group_by(grade,hit) %>%
  summarize(num = n()) %>%
  spread(grade, num)
# Print the proportion of hits for grade A- polls to the console
totals[[2,3]]/sum(totals[[3]])

# Print the proportion of hits for grade C- polls to the console
totals[[2,2]]/sum(totals[[2]])
 

chisq_test <- totals %>% select(-hit) %>% chisq.test()
chisq_test$p.value

odds_C <- (totals[[2,2]]/sum(totals[[2]]))/(totals[[1,2]]/sum(totals[[2]]))
odds_A <- (totals[[2,3]]/sum(totals[[3]]))/(totals[[1,3]]/sum(totals[[3]]))
odds_A/odds_C


#------example of reordering for factoring variable itself
theTable <- data.frame(
  Position= 
    c("Zoalkeeper", "Zoalkeeper", "Defense",
      "Defense", "Defense", "Striker"),
  Name=c("James", "Frank","Jean",
         "Steve","John", "Tim"))

p1 <- ggplot(theTable, aes(x = Position)) + geom_bar()
p2 <- ggplot(theTable, aes(x = fct_infreq(Position))) + geom_bar()
p3 <- ggplot(theTable, aes(x = fct_rev(fct_infreq(Position)))) + geom_bar()

gridExtra::grid.arrange(p1, p2, p3, nrow=3)             

#--if you could manually adjust the order
positions <- c("Zoalkeeper", "Defense", "Striker")
ggplot(theTable, aes(x = Position)) + scale_x_discrete(limits = positions) + geom_bar()
#---another way
ggplot(theTable,
       aes(x=reorder(Position,Position,
                     function(x)-length(x)))) + #anonymous function(onetime function) remove (-) for ascending order
  geom_bar()
#-----------------------
require(dslabs)
data("polls_us_election_2016")

polls <- polls_us_election_2016 %>% 
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+","A","A-","B+") | is.na(grade))) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

one_poll_per_pollster <- polls %>% group_by(pollster) %>% 
  filter(enddate == max(enddate)) %>%
  ungroup()

one_poll_per_pollster %>%
  ggplot(aes(sample=spread)) + stat_qq()


#-------------- T distribution

z <- qt(0.975, nrow(one_poll_per_pollster) - 1)
one_poll_per_pollster %>%
  summarize(avg = mean(spread), moe = z*sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - moe, end = avg + moe)

# quantile from t-distribution versus normal distribution
qt(0.975, 14)    # 14 = nrow(one_poll_per_pollster) - 1
qnorm(0.975)

# Calculate the probability of seeing t-distributed random variables being more than 2 in absolute value when 'df = 3'.
(1-pt(2,3)) + pt(-2,3)


#---example of creating CI from small sample using normal
data(heights)

# Use the sample code to generate 'x', a vector of male heights
x <- heights %>% filter(sex == "Male") %>%
  .$height

# Create variables for the mean height 'mu', the sample size 'N', and the number of times the simulation should run 'B'
mu <- mean(x)
N <- 15
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Generate a logical vector 'res' that contains the results of the simulations
res1 <- replicate(B,{
  s <- sample(x,N,replace=TRUE)
  interval <- mean(s) + c(-qnorm(.975),qnorm(0.975))*sd(s)/sqrt(N)
  between(mu,interval[1],interval[2])
})

#---example of creating CI from small sample using t

# Generate a logical vector 'res' that contains the results of the simulations using the t-distribution
res2 <- replicate(B,{
  s <- sample(x,N,replace=TRUE)
  interval <- mean(s) + c(-qt(.975,14),qt(0.975,14))*sd(s)/sqrt(N)
  between(mu,interval[1],interval[2])
})

mean(res1)
mean(res2) #t captures the 95%

#-----------------Association Tests

library(tidyverse)
library(dslabs)
data(research_funding_rates)
str(research_funding_rates)

#----Old version as text book. use funs()
totals1 <- research_funding_rates %>%
  select(-discipline) %>%
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men,
            no_men = applications_men - awards_men,
            yes_women = awards_women,
            no_women = applications_women - awards_women)

#----New version as text book. use list()
totals <- research_funding_rates %>%
  select(-discipline) %>% 
  summarise_all(list(sum)) %>% #--summarize across 9 different disciplines. function is (sum) other functions also b added
      summarize(yes_men = awards_men,
            no_men = applications_men - awards_men,
            yes_women = awards_women,
            no_women = applications_women - awards_women)

identical(totals1,totals)

totals %>% summarize(percent_men = yes_men/(yes_men + no_men),
                     percent_women = yes_women/(yes_women + no_women))

#----- Lady pouring Tea

#-- distribution is hypergeometric means k number of success from n draws WITHOUT replacement(Binomial with replacement)
#-- tab simulate a result with 3 success.  
tab <- matrix(c(3,1,1,3), 2, 2)
rownames(tab) <- c("Poured Before", "Poured After")
colnames(tab) <- c("Guessed Before", "Guessed After")
tab

# p-value calculation with Fisher's Exact Test
fisher.test(tab, alternative = "greater")

#---- Chi square test
totals <- research_funding_rates %>%
  select(-discipline) %>% 
  summarise_all(list(sum)) %>% #--summarize across 9 different disciplines. function is (sum) other functions also b added
  summarize(yes_men = awards_men,
            no_men = applications_men - awards_men,
            yes_women = awards_women,
            no_women = applications_women - awards_women)

# compute overall funding rate
funding_rate <- totals %>%
  summarize(percent_total = (yes_men + yes_women) / (yes_men + no_men + yes_women + no_women)) %>%
  .$percent_total
funding_rate

# construct two-by-two table for observed data
two_by_two <- tibble(awarded = c("no", "yes"),
                     men = c(totals$no_men, totals$yes_men),
                     women = c(totals$no_women, totals$yes_women))
two_by_two

# compute null hypothesis two-by-two table
tibble(awarded = c("no", "yes"),
           men = (totals$no_men + totals$yes_men) * c(1-funding_rate, funding_rate),
           women = (totals$no_women + totals$yes_women) * c(1-funding_rate, funding_rate))

# chi-squared test
chisq_test <- two_by_two %>%
    select(-awarded) %>%
 chisq.test()
chisq_test$p.value


# odds of getting funding for men
odds_men <- (two_by_two$men[2] / sum(two_by_two$men)) /
  (two_by_two$men[1] / sum(two_by_two$men))

# odds of getting funding for women
odds_women <- (two_by_two$women[2] / sum(two_by_two$women)) /
  (two_by_two$women[1] / sum(two_by_two$women))

# odds ratio - how many times larger odds are for men than women
odds_men/odds_women

# multiplying all observations by 10 decreases p-value without changing odds ratio.when it is significant. it can be incresed with sample size
two_by_two %>%
  select(-awarded) %>%
  mutate(men = men*10, women = women*10) %>%
  chisq.test()


#--------- Brexit Polls

library(tidyverse)
options(digits = 3)
library(dslabs)
data(brexit_polls)

p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread


#-----Warm up
N <- 1500
N*p
sqrt(N*p*(1-p))
sqrt(p*(1-p)/N)
2*p-1
2*sqrt(p*(1-p)/N)
#------


str(brexit_polls)
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (1+spread)/2)

mean(brexit_polls$spread)
sd(brexit_polls$spread)

mean(brexit_polls$x_hat)
sd(brexit_polls$x_hat)

ci1 <- brexit_polls[1,] %>% mutate(lower=spread - qnorm(0.975)*2*sqrt(x_hat*(1-x_hat)/samplesize),
                              upper=spread + qnorm(0.975)*2*sqrt(x_hat*(1-x_hat)/samplesize))

ci2 <- brexit_polls[1,] %>% mutate(lower=x_hat - qnorm(0.975)*sqrt(x_hat*(1-x_hat)/samplesize),
                                  upper=x_hat + qnorm(0.975)*sqrt(x_hat*(1-x_hat)/samplesize))


june_polls <- brexit_polls %>% filter(enddate>="2016-06-01") %>% 
  mutate(se_x_hat=sqrt(x_hat*(1-x_hat)/samplesize),se_spred=2*se_x_hat,
                       lower=spread - qnorm(0.975)*se_spred,
                       upper=spread + qnorm(0.975)*se_spred,
         hit=lower<(-0.038) & upper > -0.038)

mean(june_polls$lower<0 & june_polls$upper >0)
mean(june_polls$lower>0 & june_polls$upper >0)
mean(june_polls$hit)

june_polls %>% group_by(pollster) %>% summarise(proportion_hits=mean(hit),n=n()) %>% arrange(desc(proportion_hits))

june_polls %>% group_by(poll_type) %>% ggplot(aes(spread,col=poll_type)) + geom_boxplot()


combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2) %>% mutate(lower=spread - qnorm(0.975)*2*sqrt(p_hat*(1-p_hat)/N),
                                               upper=spread + qnorm(0.975)*2*sqrt(p_hat*(1-p_hat)/N),width=upper-lower)

combined_by_type

brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)

Tibble_tp <- tibble(hit=c("yes","No"),
                      telephone=c(sum(brexit_hit$poll_type=="Telephone" & brexit_hit$hit==TRUE),sum(brexit_hit$poll_type=="Telephone" & brexit_hit$hit==FALSE)),
                      online=c(sum(brexit_hit$poll_type=="Online" & brexit_hit$hit==TRUE),sum(brexit_hit$poll_type=="Online" & brexit_hit$hit==FALSE)))

Tibble_tp2  <- brexit_hit %>% group_by(poll_type) %>% summarise(yes=sum(hit),no=n()-sum(hit))


Tibble_tp %>% select(-hit) %>% chisq.test()

odd_tp <- (Tibble_tp[[1,2]]/sum(Tibble_tp[[2]]))/(Tibble_tp[[2,2]]/sum(Tibble_tp[[2]]))
odd_on <- (Tibble_tp[[1,3]]/sum(Tibble_tp[[3]]))/(Tibble_tp[[2,3]]/sum(Tibble_tp[[3]]))

odd_R <- odd_on/odd_tp

brexit_polls %>% 
  ggplot(aes(enddate, spread , col=poll_type)) +
  geom_smooth(method = "loess", span = 0.4) +
  geom_point( alpha = 0.6) + geom_line(aes(y=-0.038),col = "black")


brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote)) 
  
  brexit_long %>% ggplot(aes(enddate, proportion , col=vote)) +
  geom_smooth(method = "loess", span = 0.3) 

# +
#   geom_point( alpha = 0.6) + geom_line(aes(y=-0.038),col = "black")
