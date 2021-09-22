library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()

#----- 1.1 
#Scatter plot of the relationship between HRs and wins
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR / G, R_per_game = R / G) %>%
  ggplot(aes(HR_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)


#Scatter plot of the relationship between stolen bases and wins
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB / G, R_per_game = R / G) %>%
  ggplot(aes(SB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

#Scatter plot of the relationship between bases on balls and runs
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB / G, R_per_game = R / G) %>%
  ggplot(aes(BB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

#---- 1.1 Exercise --
#Scatter plot of the relationship between At bat and runs
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(AB_per_game = AB / G, R_per_game = R / G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(W_per_game = W / G, E_per_game = E / G) %>%
  ggplot(aes(W_per_game, E_per_game)) + 
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(X3B_per_game = X3B / G, X2B_per_game = X2B / G) %>%
  ggplot(aes(X3B_per_game, X2B_per_game)) + 
  geom_point(alpha = 0.5)

#----- 1.2----------------------------------------------------------------------
library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1983, sample.kind = "Rounding")

galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>% #filter only male child
  group_by(family) %>%
  sample_n(1) %>% #filter one child when there is many 
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)


# alternate code with sample_select()

# galton_heights2 <- GaltonFamilies %>% filter(gender=="male") %>% group_by(family) %>% 
#   slice_sample() %>% ungroup %>% select(father, childHeight) %>%
#   rename(son = childHeight)

sumary_galF <- galton_heights %>% summarize(mean(father),sd(father),mean(son),sd(son))

galton_heights %>% ggplot(aes(father,son)) + geom_point(alpha = 0.5)

#galton_heights %>% summarize(r = cor(father, son)) %>% pull(r)
galton_heights %>% summarize(r = cor(father, son)) %>% .$r
#what scale function do???
#rho <- mean(scale(galton_heights$father)*scale(galton_heights$son))

B <- 1000
N <- 25

R <-  replicate(B,{
  sample_n(galton_heights,N,replace = TRUE) %>%
  summarize(r = cor(father,son)) %>% .$r
})
                
# expected value and standard error
mean(R)
sd(R)

# QQ-plot to evaluate whether N is large enough
data.frame(R) %>%
  ggplot(aes(sample = R)) +
  stat_qq() +
  geom_abline(intercept = mean(R), slope = sqrt((1-mean(R)^2)/(N-2)))

# p2 <- data.frame(R) %>%
#   ggplot(aes(sample = R)) +
#   geom_qq() +
#   geom_qq_line() + geom_abline()
# 
# grid.arrange(p1,p2,ncol = 2)

#---exercise 2.1 --------------------
library(Lahman)
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(W_per_game = W / G, E_per_game = E / G) %>%
  summarize(r=cor(W_per_game,E_per_game)) %>% .$r
  
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(X2B_per_game = X2B / G, X3B_per_game = X3B / G) %>%
  summarize(r=cor(X2B_per_game,X3B_per_game)) %>% .$r
#-------------------



conditional_avg <- galton_heights %>% filter(round(father)==72) %>%
  summarize(avg=mean(son)) %>% .$avg
conditional_avg
sumary_galF

tibble(measure = "heights",
father = (72 - sumary_galF[[1,1]])/sumary_galF[[1,2]], #Fathers std value
son = (conditional_avg - sumary_galF[[1,3]])/sumary_galF[[1,4]]) #son std value

#father and son deviates from center in different phases. this is obvious due to two values come from 
#Two different distributions. 
 galton_heights %>% mutate(father_stata=factor(round(father))) %>% 
   ggplot(aes(father_stata,son)) + geom_boxplot() + geom_point()

 galton_heights %>% mutate(father_stata=round(father)) %>% group_by(father_stata) %>%
   summarise(cond_avg_son=mean(son)) %>% ggplot(aes(father_stata,cond_avg_son)) + geom_point()

 
 # calculate values to plot regression line on original data
 mu_x <- mean(galton_heights$father)
 mu_y <- mean(galton_heights$son)
 s_x <- sd(galton_heights$father)
 s_y <- sd(galton_heights$son)
 r <- cor(galton_heights$father, galton_heights$son)
 m <- r * s_y/s_x
 b <- mu_y - m*mu_x 

 # add regression line to plot
 galton_heights %>%
   ggplot(aes(father, son)) +
   geom_point(alpha = 0.5) +
   geom_abline(intercept = b, slope = m) 
 
 galton_heights %>%
   ggplot(aes(scale(father),scale(son))) + #scale function do not divide by sigma. just the subtraction 
   geom_point(alpha = 0.5) +
   geom_abline(intercept = 0, slope = r) 
 
# checking for bi variate normal distribution for father and son. when x is nor mal and y is normal. 
# for any given X=x y should also should be normal

 galton_heights %>% mutate(z_father=round((father-mean(father))/sd(father))) %>% 
   filter(z_father %in% -2:2) %>% 
   ggplot(aes(sample=son)) + geom_qq() +  geom_qq_line() +  facet_wrap( ~ z_father)
 
 
 # compute a regression line to predict the son's height from the father's height
 mu_x <- mean(galton_heights$father)
 mu_y <- mean(galton_heights$son)
 s_x <- sd(galton_heights$father)
 s_y <- sd(galton_heights$son)
 r <- cor(galton_heights$father, galton_heights$son)
 m_1 <-  r * s_y / s_x
 b_1 <- mu_y - m_1*mu_x
 
 # compute a regression line to predict the father's height from the son's height
 m_2 <-  r * s_x / s_y
 b_2 <- mu_x - m_2*mu_y

 
#-----------------exercise 1.3
 
 set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
 library(HistData)
 data("GaltonFamilies")
 
 female_heights <- GaltonFamilies%>%     
   filter(gender == "female") %>%     
   group_by(family) %>%     
   sample_n(1) %>%     
   ungroup() %>%     
   select(mother, childHeight) %>%     
   rename(daughter = childHeight) 

m_m <- mean(female_heights$mother) 
sd_m <- sd(female_heights$mother) 

m_d <- mean(female_heights$daughter) 
sd_d <- sd(female_heights$daughter) 

r <- cor(female_heights$mother,female_heights$daughter)

m <- r*(sd_d/sd_m)
b <- m_d - m*m_m
m
b
#---------------------
library(tidyverse)
library(Lahman)
str(Teams)

bb_slope <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB /G , R_per_game = R /G) %>%
  lm(R_per_game ~ BB_per_game, data = .) %>% 
  .$coef %>%
  .[2]
bb_slope


singles_slope <- Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(singles_per_game = (H-HR-X2B-X3B)/G, R_per_game = R/G) %>% 
  lm(R_per_game ~ singles_per_game , data = .) %>% 
  .$coef %>% 
  .[2]
singles_slope

Teams %>% filter(yearID %in% 1961:2001) %>% mutate(singles = (H-HR-X2B-X3B)/G , BB = BB/G , HR=HR/G) %>%
  summarise(cor(singles,BB),cor(singles,HR),cor(BB,HR)) 

# stratify HR per game to nearest 10, filter out strata with few points
dat_HR_strata <- Teams %>% filter( yearID %in% 1961:2001) %>%
  mutate(HR_strata = round(HR/G,1),
         BB_per_game = BB / G,
         R_per_game = R / G) %>% group_by(HR_strata) %>% summarise(count=n()) #summarize to see number of data in each strata

dat_HR_strata <- Teams %>% filter( yearID %in% 1961:2001) %>%
  mutate(HR_strata = round(HR/G,1),
         BB_per_game = BB / G,
         R_per_game = R / G) %>%
  filter(HR_strata >= 0.4 & HR_strata <=1.2)

# scatterplot for each HR stratum
 dat_HR_strata %>% ggplot(aes(BB_per_game,R_per_game)) + geom_point() + geom_smooth(method = "lm") + 
   facet_wrap( ~ HR_strata)

# calculate slope of regression line after stratifying by HR
 dat_HR_strata %>%  
   group_by(HR_strata) %>%
   summarize(slope = cor(BB_per_game, R_per_game)*sd(R_per_game)/sd(BB_per_game))

 
 # stratify by BB
 dat_BB_strata <- Teams %>% filter(yearID %in% 1961:2001) %>%
   mutate(BB_strata = round(BB/G, 1), 
          HR_per_game = HR / G,
          R_per_game = R / G) %>%
   filter(BB_strata >= 2.8 & BB_strata <=3.9) 
 
 # scatterplot for each BB stratum
 dat_BB_strata %>% ggplot(aes(HR_per_game, R_per_game)) +  
   geom_point(alpha = 0.5) +
   geom_smooth(method = "lm") +
   facet_wrap( ~ BB_strata)
 
 # slope of regression line after stratifying by BB
 dat_BB_strata %>%  
   group_by(BB_strata) %>%
   summarize(slope = cor(HR_per_game, R_per_game)*sd(R_per_game)/sd(HR_per_game))  
 
# E[R | BB = x1, HR = x2] = β0 + β1(x2)x1 + β2(x1)x2 
# with stratification we saw when slope of BB on ball remain approximately constant when HR are fixed and 
# vise-versa in that case (x2) and (x1) can consider as constant and equation reduces to 
# E[R | BB = x1, HR = x2] = β0 + β1x1 + β2x2 
  
#------------------------ 2.1 ----------------------------------- 
  