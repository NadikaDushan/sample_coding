setwd("C:/Users/Nadika/Desktop/R/ENV")

require(dslabs)
data("gapminder")
require(ggplot2)
require(dplyr)

#----------- Variable Cahnges---------
west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")
gapminder<- mutate(gapminder,dollars_per_day=gdp/population/365)
gapminder <- gapminder %>% mutate(group_0=ifelse(region %in% west, "West", "Developing")) %>% 
  mutate(group_0=factor(group_0))

country_list_1 <- gapminder %>% filter(year==1970 & !is.na(gdp)) %>% .$country #this produce factor variable
country_list_2 <- gapminder %>% filter(year==2010 & !is.na(gdp)) %>% .$country #this produce factor variable
countrylist <- intersect(country_list_1,country_list_2)

gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "West",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    TRUE ~ "Others"))  %>% 
      mutate(group=factor(group,levels = c("Others", "Latin America", "East Asia", "Sub-Saharan Africa", "West")))

gapminder <- gapminder %>% mutate(weight = population/sum(population*2))
#-------------------------------------

#Facet used to get plots with same axes to compare
gapminder %>% filter(year %in% c(1960,2010)) %>% 
  ggplot(aes(fertility,life_expectancy,col=continent)) + 
  geom_point() + 
  facet_grid(.~year)

#facet_wrap gives plots in multiple lines
gapminder %>% filter(year %in% c(1960,1970,1980,1990,2000,2005,2010)) %>% 
  ggplot(aes(fertility,life_expectancy,col=continent)) + 
  geom_point() + 
  facet_wrap(~year)

#geom_point for time series
gapminder %>% filter(country=="United States") %>% 
  ggplot(aes(year,fertility)) + 
  geom_point()

#geom_line for time series
gapminder %>% filter(country=="United States") %>% 
  ggplot(aes(year,fertility)) + 
  geom_line()

#Two series with legend using col command
sel_country <- c("South Korea","Germany")
gapminder %>% filter(country %in% sel_country) %>% 
  ggplot(aes(year,fertility,col=country)) + 
  geom_line()

#define lables as of countries variable to be positioned in (1975,60) and (1965,62) these coordinates picked by eye
# geom_text call for data in labales array x for c(1975,1965) and y for c(60,72) not the orginal x=year,y=life_expectancy data
labels <- data.frame(country = sel_country, x = c(1975, 1965), y = c(60, 72))
gapminder %>% filter(country %in% sel_country) %>%
  ggplot(aes(year, life_expectancy, col = country)) +
  geom_line() +
  geom_text(data = labels, aes(x, y, label = country), size = 5) +
  theme(legend.position = "none")

#adding dollars_per_day to the gapminder
#alternate coding with pipe  --- gapminder <- gapminder %>% mutate(dollars_per_day = gdp/population/365)
gapminder<- mutate(gapminder,dollars_per_day=gdp/population/365)

#dollars per day original scale hard to seea pattern
gapminder %>% filter(year == 1970 & !is.na(gdp)) %>% 
  ggplot(aes(dollars_per_day)) + 
  geom_histogram(binwidth = 1, color = "black")

#log2 shrink the scale
gapminder %>% filter(year == 1970 & !is.na(gdp)) %>% 
  ggplot(aes(log2(dollars_per_day))) + 
  geom_histogram(binwidth = 1, color = "black")

#alternate scale_x_continuous usage instead of log2 transformation in ggplot()
gapminder %>% filter(year == 1970 & !is.na(gdp)) %>% 
  ggplot(aes(dollars_per_day)) + 
  geom_histogram(binwidth = 1, color = "black") + 
  scale_x_continuous(trans = "log2")

gapminder %>% filter(year == 1970 & !is.na(gdp)) %>%
  ggplot(aes(region, dollars_per_day)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

gapminder %>% filter(year == 1970 & !is.na(gdp)) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%    # reorder
  ggplot(aes(region, dollars_per_day, fill = continent)) +    # color by continent
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + scale_y_continuous(trans = "log2") + geom_point(show.legend = FALSE)

#----------------------------------------------------------------

west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")
gapminder<- mutate(gapminder,dollars_per_day=gdp/population/365)
gapminder <- gapminder %>% mutate(group_0=ifelse(region %in% west, "West", "Developing")) %>% 
  mutate(group_0=factor(group_0))

gapminder %>% filter(year==1970 & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(. ~ group_0)

gapminder %>% filter(year %in% c(1970,2010) & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) + 
  geom_histogram(binwidth = 1,color= "black") + 
  scale_x_continuous(trans="log2") +
  facet_grid(year~group_0)

#creatig a vector to compare two years data only for the countries that have data in both years
country_list_1 <- gapminder %>% filter(year==1970 & !is.na(gdp)) %>% .$country #this produce factor variable
country_list_0 <- gapminder %>% filter(year==1970 & !is.na(gdp)) %>% select(country) #this produce dataframe with one factor variable

country_list_2 <- gapminder %>% filter(year==2010 & !is.na(gdp)) %>% .$country #this produce factor variable

countrylist <- intersect(country_list_1,country_list_2)

gapminder %>% filter(year %in% c(1970,2010) & !is.na(gdp) & country %in% countrylist) %>%
  ggplot(aes(dollars_per_day)) + 
  geom_histogram(binwidth = 1,color= "black") + 
  scale_x_continuous(trans="log2") +
  facet_grid(year~group_0)

Mbox <- gapminder %>%
  filter(year %in% c(1970,2010) & country %in% countrylist) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + scale_y_continuous(trans = "log2") 

Mbox + geom_boxplot(aes(region, dollars_per_day, fill = continent)) +
  facet_grid(year ~ .)

Mbox + geom_boxplot(aes(region, dollars_per_day, fill = factor(year)))

#------------------------------------------------------

gapminder %>%
  filter(year == 1970 & country %in% countrylist) %>% group_by(group_0) %>%
  summarize(n = n()) %>% knitr::kable()

gapminder %>% filter(year %in% c(1970,2010) & !is.na(gdp) & country %in% countrylist) %>%
  ggplot(aes(dollars_per_day,fill=group_0)) + scale_x_continuous(trans="log2") + 
  geom_density(alpha=0.2,bw = 0.75) + facet_grid(.~year)

gapminder %>% filter(year %in% c(1970,2010) & !is.na(gdp) & country %in% countrylist) %>%
  ggplot(aes(dollars_per_day,y=..count..,fill=group_0)) + scale_x_continuous(trans="log2") + 
  geom_density(alpha=0.2,bw = 0.75) + facet_grid(.~year)

#---- Stacking order depend on the factor order. if you need to change the stacking order change the levels= in factor command
gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "West",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    TRUE ~ "Others")) %>% 
    mutate(group=factor(group,levels = c("Others", "Latin America", "East Asia", "Sub-Saharan Africa", "West")))

gapminder %>% filter(year %in% c(1970,2010) & !is.na(dollars_per_day) & country %in% countrylist) %>%
  ggplot(aes(dollars_per_day,y=..count..,fill=group)) + scale_x_continuous(trans = "log2") + 
  geom_density(alpha=0.2,bw=0.75,position="stack") + facet_grid(.~year)

gapminder <- gapminder %>% filter(!is.na(population)) %>% 
  mutate(weight = population/sum(population*2))

gapminder %>% filter(year %in% c(1970,2010) & !is.na(dollars_per_day) & country %in% countrylist) %>%
  ggplot(aes(dollars_per_day,y=..count..,fill=group,weight = weight)) + scale_x_continuous(trans = "log2") + 
  geom_density(alpha=0.2,bw=0.75,position="stack") + facet_grid(.~year)

gapminder %>%
  filter(year %in% c(1970, 2010) & country %in% countrylist) %>%
  mutate(weight = population/sum(population*2)) %>%
  ggplot(aes(dollars_per_day, fill = group, weight = weight)) +
  scale_x_continuous(trans = "log2") +
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") + facet_grid(year ~ .)


#--------------------------------------------------------------------

gapminder <- gapminder %>%
  mutate(group_1 = case_when(
    .$region %in% west ~ "The West",
    .$region %in% "Northern Africa" ~ "Northern Africa",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region == "Southern Asia" ~ "Southern Asia",
    .$region %in% c("Central America", "South America", "Caribbean") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    .$region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands"))

surv_income <- gapminder %>%
  filter(year %in% 2010 & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group_1)) %>%
  group_by(group_1) %>%
  summarize(income = sum(gdp)/sum(population)/365,
            infant_survival_rate = 1 - sum(infant_mortality/1000*population)/sum(population))

#--arrange orders the rows of a data frame by the values of selected columns
surv_income %>% arrange(income)


surv_income %>% ggplot(aes(income, infant_survival_rate, label = group_1, color = group_1)) +
  scale_x_continuous(trans = "log2", limit = c(0.25, 150)) +
  scale_y_continuous(trans = "logit", limit = c(0.875, .9981),
                     breaks = c(.85, .90, .95, .99, .995, .998)) +
  geom_label(size = 3, show.legend = FALSE) 

#---------------------Slope Chart-----------------------------
library(tidyverse)
library(dslabs)
data(gapminder)

west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")

dat <- gapminder %>%
  filter(year %in% c(2010, 2015) & region %in% west & !is.na(life_expectancy) & population > 10^7)

dat %>%
  mutate(location = ifelse(year == 2010, 1, 2),
         location = ifelse(year == 2015 & country %in% c("United Kingdom", "Portugal"),
                           location + 0.22, location),
         hjust = ifelse(year == 2010, 1, 0)) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(year, life_expectancy, group = country)) +
  geom_line(aes(color = country), show.legend = FALSE) +
  geom_text(aes(x = location, label = country, hjust = hjust), show.legend = FALSE) +
  xlab("") +
  ylab("Life Expectancy") 

#---------Bland-Altman plot------------------
library(ggrepel)
dat %>%
  mutate(year = paste0("life_expectancy_", year)) %>%
  select(country, year, life_expectancy) %>% spread(year, life_expectancy) %>%
  mutate(average = (life_expectancy_2015 + life_expectancy_2010)/2,
         difference = life_expectancy_2015 - life_expectancy_2010) %>%
  ggplot(aes(average, difference, label = country)) +
  geom_point() +
  geom_text_repel() +
  geom_abline(lty = 2) +
  xlab("Average of 2010 and 2015") +
  ylab("Difference between 2015 and 2010")

#------------- tile plot
# import data and inspect
library(tidyverse)
library(dslabs)
data(us_contagious_diseases)
str(us_contagious_diseases)

# assign dat to the per 10,000 rate of measles, removing Alaska and Hawaii and adjusting for weeks reporting
the_disease <- "Measles"
dat <- us_contagious_diseases %>%
  filter(!state %in% c("Hawaii", "Alaska") & disease == the_disease) %>%
  mutate(rate = count / population * 10000 * 52/weeks_reporting) %>%
  mutate(state = reorder(state, rate))

# plot disease rates per year in California
dat %>% filter(state == "California" & !is.na(rate)) %>%
  ggplot(aes(year, rate)) +
  geom_line() +
  ylab("Cases per 10,000") +
  geom_vline(xintercept=1963, col = "blue")

# tile plot of disease rate by state and year
dat %>% ggplot(aes(year, state, fill=rate)) +
  geom_tile(color = "grey50") +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"), trans = "sqrt") +
  geom_vline(xintercept = 1963, col = "blue") +
  theme_minimal() + theme(panel.grid = element_blank()) +
  ggtitle(the_disease) +
  ylab("") +
  xlab("")

# compute US average measles rate by year
avg <- us_contagious_diseases %>%
  filter(disease == the_disease) %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm = TRUE)/sum(population, na.rm = TRUE)*10000)

# make line plot of measles rate by year by state
dat %>%
  filter(!is.na(rate)) %>%
  ggplot() +
  geom_line(aes(year, rate, group = state), color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate), data = avg, size = 1, col = "black") +
  scale_y_continuous(trans = "sqrt", breaks = c(5, 25, 125, 300)) +
  ggtitle("Cases per 10,000 by state") +
  xlab("") +
  ylab("") +
  geom_text(data = data.frame(x = 1955, y = 50),
            mapping = aes(x, y, label = "US average"), color = "black") +
  geom_vline(xintercept = 1963, col = "blue")


#Reduce the number of significant digits globally by setting an option. 
#options(digits = 3) will cause all future computations that session to have 3 significant digits.
#Reduce the number of digits locally using round() or signif().

#-----qqplots
params <- titanic_train %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

titanic_train %>% filter(!is.na(Age)) %>% ggplot(aes(sample=Age))+ geom_qq(dparams=params) + geom_abline()

#--------Percentage in bar charts position_fill()
titanic_train %>% filter(!is.na(Age)) %>% ggplot(aes(Pclass,fill=factor(Survived),y=..count..))+ 
  geom_bar() 

titanic_train %>% filter(!is.na(Age)) %>% ggplot(aes(Pclass,fill=factor(Survived),y=..count..))+ 
  geom_bar(position = position_fill())

titanic_train %>% filter(!is.na(Age)) %>% ggplot(aes(Survived,fill=factor(Pclass),y=..count..))+ 
  geom_bar(position = position_fill()) 
