#set.seed(x, sample.kind = "Rounding")
#creating a box with 2 red beads and 3 blue beads
beads <- rep(c("red","blue"),times = c(2,3))
sample(beads,1)
# Two samples without replacement
sample(beads,2)
#with replacement
event1 <- sample(beads,10000,replace = TRUE)
# same can be written 
event2 <- replicate(10000, sample(beads, 1))
# count table
table(event1)
#prepotion table 
prop.table(table(event1))
#percentage table 
prop.table(table(event1))*100

#--setting random number generator to fix so that answers matches with the book
set.seed(1986, sample.kind="Rounding") 

#--prepotion of blue beads
mean(beads == "blue")

#paste works
paste("Hearts","Three")
paste(letters[1:5],as.character(1:5))
#expand grid working
expand.grid(pants = c("blue", "black"), shirt = c("white", "grey", "plaid"))

suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
#all possible card types in the deck
hands1 <- paste(deck$number,deck$suit)

# probability of drawing a king first
kings <- paste("King", suits)
mean(hands1 %in% kings)

require(gtools)
#Probability of drawing a second king given that one king is drawn
#first hand means one cards. second hand means also another card
hands <- permutations(52,2, v = hands1)
first_card <- hands[,1]
second_card <- hands[,2]

sum(first_card %in% kings & second_card %in% kings) / sum(first_card %in% kings)

#Probability of a natural 21(ace and a face card) in blackjack
aces <- paste("Ace", suits)
facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number = facecard, suit = suits)
facecard <- paste(facecard$number, facecard$suit)

hands2 <- combinations(52, 2, v=hands1) # all possible hands

mean(hands2[,1] %in% aces & hands2[,2] %in% facecard)
#the best code
mean((hands2[,1] %in% aces & hands2[,2] %in% facecard)|(hands2[,2] %in% aces & hands2[,1] %in% facecard))
#both gives same results 
#since the way combinations spreads the cards means(hands2[,2] %in% aces & hands2[,1] %in% facecard) =0

#-----------Monte Carlo simulation of natural 21 in blackjack
hand <- sample(hands1, 2)
B <- 10000
results <- replicate(B, {
  hand <- sample(hands1, 2)
  (hand[1] %in% aces & hand[2] %in% facecard) | (hand[2] %in% aces & hand[1] %in% facecard)
})
mean(results)

# {} are block of codes the value returned to the replicate function is the result of the second statment

#----------Mone Carlo probability for sharing the same bithday
any(duplicated(sample(1:365,50,replace = TRUE)))

#replicate 10000 times and take the probability
n <- 50
bdays <- sample(1:365,n,replace = TRUE)
B <- 10000
results <- replicate(B,{bdays <- sample(1:365,n,replace = TRUE)
any(duplicated(bdays))})
mean(results)

compu_prob <- function(n,B=10000){
  same_day <- replicate(B,{bdays <- sample(1:365,n,replace = TRUE)
  any(duplicated(bdays))})
  mean(same_day)}

prob <- sapply(1:60,compu_prob)
plot(prob)
#---no of ways having unique birthdays 1*364/365*363/365 (mathematical way)
exact_prob <- function(n){
  prob_unique <- seq(365, 365-n+1)/365   # vector of fractions for mult. rule
  1 - prod(prob_unique)    # calculate prob of no shared birthdays and subtract from 1
}

# applying function element-wise to vector of n values
eprob <- sapply(n, exact_prob)
# plotting Monte Carlo results and exact probabilities on same graph
plot(n, prob)    # plot Monte Carlo results
lines(n, eprob, col = "red")    # add line for exact prob

#----how many montecarlo simulations
#creates seq(1,5, len=100) gives 100 values in between 1 and 5
#taking power to the 10 gives 100 values ranging from 10 to 100000
B_1 <- 10^seq(1,5,len =100)

compute_prob_1 <- function(B_1, n = 22){    # function to run Monte Carlo simulation with each B
  same_day <- replicate(B_1, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

prob <- sapply(B_1, compute_prob_1)    # apply compute_prob to many values of B
plot(log10(B), prob, type = "l")    # plot a line graph of estimates 

#---------------Monty Hall
#--stick Stratergy
B <- 10000
stick <- replicate(B,{ doors <- as.character(1:3)
          prize <- sample(c("car","goat","goat"))
          prize_door <- doors[prize=="car"]
          my_pick <- sample(doors,1)
          show <- sample(doors[!doors %in% c(my_pick,prize_door)],1)
          stick<-my_pick
          stick==prize_door})
mean(stick)
#---Switch stratergy
switch <- replicate(B,{ doors <- as.character(1:3)
          prize <- sample(c("car","goat","goat"))
          prize_door <- doors[prize=="car"]
          my_pick <- sample(doors,1)
          show <- sample(doors[!doors %in% c(my_pick,prize_door)],1)
          switch<-doors[!doors %in% c(my_pick,show)]
          switch==prize_door})
mean(switch)


#-------------test
# Assign a variable 'n' as the number of remaining games.
n <- 6

# Assign a variable `outcomes` as a vector of possible game outcomes, where 0 indicates a loss and 1 indicates a win for the Cavs.
outcomes <- c(0,1)

# Assign a variable `l` to a list of all possible outcomes in all remaining games. Use the `rep` function on `list(outcomes)` to create list of length `n`. 
l <- rep(list(outcomes), n)

# Create a data frame named 'possibilities' that contains all combinations of possible outcomes for the remaining games.
possibilities <- expand.grid(l)

# Create a vector named 'results' that indicates whether each row in the data frame 'possibilities' contains enough wins for the Cavs to win the series.
results <- rowSums(possibilities)>=4

# Calculate the proportion of 'results' in which the Cavs win the series. Print the outcome to the console.
mean(results)

#-------assighnment
medal <- c("gold","silver","bronze")
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
set.seed(1)
B <- 10000

#winners<-sample(runners,3)
#all(winners %in% "Jameica")

jam_winners <- replicate(B,{winners<-sample(runners,3)
  all(winners %in% "Jamaica")})
mean(jam_winners)

entree <- 6
sides <- seq(2:12)
drinks <- 3

combinations <- function(sides){
  entree*sides*(sides-1)/2*drinks}

sapply(sides,combinations)

plot(entree,sapply(entree,combinations))

data("esoph")
require(tidyverse)
all_cases <- sum(esoph$ncases)
all_controls <- sum(esoph$ncontrols)

sum(esoph$ncases[esoph$alcgp%in%"120+"])
sum(esoph$ncontrols[esoph$alcgp%in%"120+"])

sum(esoph$ncases[as.integer(esoph$alcgp)==1])/sum(esoph$ncontrols[as.integer(esoph$alcgp)==1])

sum(esoph$ncases[as.integer(esoph$tobgp)%in% c(2,3,4)])/sum(esoph$ncases)
sum(esoph$ncontrols[as.integer(esoph$tobgp)%in% c(2,3,4)])/sum(esoph$ncontrols)


sum(esoph$ncases[as.integer(esoph$alcgp)==4])/sum(esoph$ncases)
sum(esoph$ncases[as.integer(esoph$tobgp)==4])/sum(esoph$ncases)

sum(esoph$ncases[as.integer(esoph$alcgp)==4 | as.integer(esoph$tobgp)==4])/sum(esoph$ncases)

sum(esoph$ncases[as.integer(esoph$alcgp)==4 & as.integer(esoph$tobgp)==4])/sum(esoph$ncases)

sum(esoph$ncontrols[as.integer(esoph$alcgp)==4])/sum(esoph$ncontrols)


(sum(esoph$ncases[as.integer(esoph$alcgp)==4])/sum(esoph$ncases))/(sum(esoph$ncontrols[as.integer(esoph$alcgp)==4])/sum(esoph$ncontrols))


sum(esoph$ncontrols[as.integer(esoph$tobgp)==4 & as.integer(esoph$alcgp)==4])/sum(esoph$ncontrols)

sum(esoph$ncontrols[as.integer(esoph$tobgp)==4 | as.integer(esoph$alcgp)==4])/sum(esoph$ncontrols)

sum(esoph$ncases[as.integer(esoph$tobgp)==4 | as.integer(esoph$alcgp)==4])/sum(esoph$ncontrols[as.integer(esoph$tobgp)==4 | as.integer(esoph$alcgp)==4])

(sum(esoph$ncases[as.integer(esoph$tobgp)==4 | as.integer(esoph$alcgp)==4])/sum(esoph$ncases))/(sum(esoph$ncontrols[as.integer(esoph$tobgp)==4 | as.integer(esoph$alcgp)==4])/sum(esoph$ncontrols))

#------------ continous distributions
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)

#pull(heights) is simillar to .$hights
#creating empherical distribution function
F <- function(a) mean(x <= a)

F(68.5)-F(67.5)
F(69.5)-F(68.5)
F(70.5)-F(69.5)

# probabilities in actual data over length 1 ranges containing an integer
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)

# probabilities in normal approximation match well
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))

#---------normal curve
x <- seq(-4, 4, length = 100)
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x, f)) +
  geom_line()

#---generating normal data from a given ditribution. (mean and sd given )
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)
n <- length(x)
avg <- mean(x)
s <- sd(x)
simulated_heights <- rnorm(n,avg,s)

data.frame(simulated_heights = simulated_heights) %>%
  ggplot(aes(simulated_heights)) +
  geom_histogram(color="black", binwidth = 2)

#--------Monte carlo for continous data---------
#---max hight exceeds 7 foot
B <- 10000
tallest <- replicate(B,{
  simulated_heights <- rnorm(800,avg,s)
  max(simulated_heights)})
mean(tallest >= 7*12)

#---continous assesment
set.seed(16)
act_scores <- rnorm(10000,20.9,5.7)
avg <- mean(act_scores)
s <- sd(act_scores)
sum(act_scores>=36)
mean(act_scores>=30)
mean(act_scores<=10)

x <- seq(1:36)
f_x <- dnorm(x,20.9,5.7)
plot(x,f_x)

act_z <- (act_scores-avg)/s
mean(act_z>2)

qnorm(0.975,avg,s)

act_CDF <- function(x) mean(act_scores<=x)
x <- (1:36)
mm <- data.frame(score=x,cdf=sapply(x,act_CDF))
plot(x,sapply(x,act_CDF))
mm

qnorm(0.95,20.9,5.7)
# ----------qqplot using basic codes--------------
p <- seq(0.01, 0.99, 0.01)
sample_quantiles<- qnorm(p,mean(act_scores),sd(act_scores))
theoretical_quantiles <- qnorm(p,20.9,5.7)
plot(theoretical_quantiles,sample_quantiles)

data.frame(theoretical_quantiles=theoretical_quantiles,sample_quantiles=sample_quantiles) %>% 
  ggplot(aes(theoretical_quantiles,sample_quantiles)) + geom_line() + geom_point()

#--------------SECTion 3 -----------------------
beads <- rep(c("red", "blue"), times = c(2, 3))
x <- ifelse(sample(beads,1)=="blue",1,0)
#alternatly --- sample and ifelse can be interchanged and sample can be use to draw n number of samples without loop or replicated
x <- sample(ifelse(beads=="blue",1,0),1)


color <- rep(c("Black", "Red", "Green"), c(18, 18, 2))
n <- 1000
X <- sample(ifelse(color == "Red", -1, 1), n, replace = TRUE)

# sampling model 2: define urn inside sample function by noting probabilities
x <- sample(c(-1, 1), n, replace = TRUE, prob = c(9/19, 10/19))    # 1000 independent draws
S <- sum(x)    # total winnings = sum of draws
S

B <- 10000
S <- replicate(B,{
  x <- sample(c(-1,1),n,replace = TRUE, prob = c(9/19,10/19))
  sum(x)
})

hist(S)
mean(S<0)

avg <- mean(S)
s <- sd(S)
# ---------- See how GGplot incooperate with different data sources
#ggplot(aes(S,..density..)) this ..density.. plot the prababilities of occcuring corresponding bins.
sq <- seq(min(S),max(S),length =100)
normal_density <- data.frame(sq=sq,f=dnorm(sq,avg,s))
data.frame(S=S) %>% ggplot(aes(S,..density..)) + geom_histogram(color="black",binwidth=10) + 
  ylab("Probability") + geom_line(data = normal_density, mapping = aes(sq,f), color = "blue")

# monecarlo exam---sat
(1*0.2)+(-0.25*0.8)
abs(1-(-0.25))*sqrt(0.2*0.8)*sqrt(44)
1-pnorm(8,0,3.316625)
set.seed(21)
S <- replicate(10000,{
  sum(sample(c(1,-0.25),44,replace = TRUE,prob = c(0.2,0.8)))
})
mean(S>8)

44*((0.25*1)+(0.75*0))
#----- if P is probabilities of a answering correct from student
# the lowest p that exceeds 80% chance to score 35


p <- seq(0.25, 0.95, 0.05)

compu_prob <- function(p){replicate(10000,{
  sam <- sum(sample(c(1,0),44,replace = TRUE,prob = c(p,1-p)))
  mean(sam>35)})}

S <- sapply(p,compu_prob)
data.frame(p=p,means=colMeans(S))

((6*5/38)+(-1*33/38))*500

abs(6-(-1))*(sqrt(5*33)/38)/sqrt(500)

mean(replicate(10000,{
sum(sample(c(6,-1),500,replace = TRUE,prob = c(5/38,33/38)))
}))

pnorm(0,-39.47368,52.91045)

#----expected value of x --- E(x) prob1*outcome1+prob2*outcome2...
#--standard deviation of x--- abs(outcome1-outcome2)sqrt(prob1*prob2)
#----expected value of average x(x bar) --- E(x) prob1*outcome1+prob2*outcome2...
#----standard deviation of average x (standard error of x) -- abs(outcome1-outcome2)*sqrt(prob1*prob2)/sqrt(number of trials)
# expected value of sum(x)--- numberoftrials*E(x)
# standard error of sum(x)--- abs(outcome1-outcome2)*sqrt(prob1*prob2)*sqrt(number of trials)


#------------ Big Short
# loan amount - 180,000
# loss per default 200,000
# no of loans 1000

n <- 1000
loss_per_foreclosure  <- -200000
p <- 0.02
defaults <- sample(c(0,1),n,replace = TRUE, prob = c(1-p,p))
sum(defaults*loss_per_foreclosure )

B <- 10000
losses <- replicate(B,{
  defaults <- sample(c(0,1),n,replace = TRUE, prob = c(1-p,p))
  sum(defaults*loss_per_foreclosure )
})

require(tidyverse)
data.frame(losses_in_millions=losses/10^6) %>% ggplot(aes(losses_in_millions)) + 
  geom_histogram(binwidth = 0.6, color="black")

#Expected value and standard error of the sum of 1,000 loans
n*(p*loss_per_foreclosure + (1-p)*0)    # expected value 
sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p))    # standard error
#l-loss per foreclosure
#x - profit per loan
#lp + x(1-p)=0

x = - loss_per_foreclosure*p/(1-p)

#interest rate
X/180000

#Calculating interest rate for 1% probability of losing money
#pr(s<0)=0.01
#pr(z<-mu/sigma)=0.01
#mu=(lp + x(1-p))n
#sigma=abs(x-l)sqrt(np(1-p))
#Z=-mu/sigma
#-mu/sigma=qnorm(0.05) IE Z table value
#we have to find x

l <- loss_per_foreclosure
z <- qnorm(0.01)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))

x/180000    # interest rate
loss_per_foreclosure*p + x*(1-p)    # expected value of the profit per loan
n*(loss_per_foreclosure*p + x*(1-p)) # expected value of the profit over n loans

#Monte Carlo simulation for 1% probability of losing money

B <- 100000
profit <- replicate(B , {
  draws <- sample(c(x,loss_per_foreclosure),n,replace = TRUE,prob = c(1-p,p))
  sum(draws)
})

mean(profit) # this is approximatly same as n*(loss_per_foreclosure*p + x*(1-p))
mean(profit<0)# probability of losing money this value ~1% as we set in the calculation of x


# Expected value with higher default rate and interest rate
p <- 0.04
loss_per_foreclosure <- -200000
r <- 0.05
x<- r*180000

x*(1-p)+loss_per_foreclosure*p

# Calculating number of loans for desired probability of losing money
z <- qnorm(0.01)
l <- loss_per_foreclosure

n <- ceiling((z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2)
#number of loans required
n
n*(x*(1-p)+loss_per_foreclosure*p)

#Monte Carlo simulation with known default probabilityand known number of loans to mitigate known default rate

B <- 10000
p <- 0.04
x <- 0.5*180000

n <- ceiling((z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2)

#it takes time to run this since n=22163
profit <- replicate(B , {
  Draws <- sample(c(x,loss_per_foreclosure),n,replace = TRUE,prob = c(1-p,p))
  sum(Draws)
  })

mean(profit)
mean(profit<0)

#this holds only if events re normal and independent

#think for a instance whre probability of forfeit is changing maximum 0.01 change

p <- 0.04
x <- 0.05*180000
profit <- replicate(B, {
  new_p <- 0.04 + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-new_p, new_p), replace = TRUE)
  sum(draws)
})

mean(profit)    # expected profit - No much change
mean(profit < 0)    # probability of losing money - now 34% heavily change form 1%
mean(profit < -10000000)    # probability of losing over $10 million 24% hcnce :-(

#this is no whaere near normal distribution and this cause financial crysis
data.frame(profit_in_100000=profit/10^6) %>% ggplot(aes(profit_in_100000,..count..)) + geom_histogram(color="black")

# assesment Bigshort


library(tidyverse)
library(dslabs)

data(death_prob)
head(death_prob)

levels(death_prob$sex)

p <- death_prob$prob[death_prob$age==50 & death_prob$sex=="Female"]

exp_val <- p*-150000 + (1-p)*1150
exp_val

std_error <- abs(-150000-1150)*sqrt(p*(1-p))
std_error

exp_val*1000
std_error_all <- abs(-150000-1150)*sqrt(1000*p*(1-p))
std_error_all

pnorm(0,exp_val*1000,std_error_all)


p1 <- death_prob$prob[death_prob$age==50 & death_prob$sex=="Male"]

mp <- (700 - p1*-150000)/(1-p1)

std_error_all_M <- abs(-150000-mp)*sqrt(1000*p1*(1-p1))
std_error_all_M

pnorm(0,700000,std_error_all_M)

#---------------
p3 <- 0.015

exp_val3 <- p3*-150000 + (1-p3)*1150
exp_val3*1000

std_error_all3 <- abs(-150000-1150)*sqrt(1000*p3*(1-p3))
std_error_all3

pnorm(0,exp_val3*1000,std_error_all3)
pnorm(-1000000,exp_val3*1000,std_error_all3)


#p4 <- seq(.01, .03, .001)
p4 <- seq(.01, .03, .0025)


death_P <- function(p4) {pnorm(-1000000,(p4*-150000 + (1-p4)*1150)*1000,abs(-150000-1150)*sqrt(1000*p4*(1-p4)))}

prob <- sapply(p4, death_P)

plot(p4,prob)

view(cc<-data.frame(prob=prob,seq=p4))

#------------
set.seed(27)
p_loss = .015
B <- 10000
sim_prof <- replicate(B,{
  profloss <- sample(c(-150000,1150),1000,replace = TRUE,prob = c(p_loss,(1-p_loss)))
  sum(profloss) 
})
mean(sim_prof<=-1000000)
n <- 1000
l <- -150000
z <- qnorm(0.05)
p <- 0.015
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))

(p*-150000 + (1-p)*x)

#-----
set.seed(28)
p_loss = .015
B <- 10000
sim_prof <- replicate(B,{
  profloss <- sample(c(-150000,x),1000,replace = TRUE,prob = c(p_loss,(1-p_loss)))
  sum(profloss) 
})
mean(sim_prof<0)
#-------------

p <- .015
n <- 1000
l <- -150000
z <- qnorm(.05)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x

set.seed(29, sample.kind = "Rounding")
B <- 10000
sim_prof1 <- replicate(B,{
  p_lossN <- 0.015 + sample(seq(-0.01, 0.01, length = 100), 1)
  profloss <- sample(c(-150000,x),1000,replace = TRUE,prob = c(p_lossN,(1-p_lossN)))
  sum(profloss) 
})
mean(sim_prof1)
mean(sim_prof1< 0)
mean(sim_prof1< (-1000000))


