### Linear regression
```{r}
require(dslabs)
data("gapminder")
require(ggplot2)
require(dplyr)

gapminder %>% filter(year %in% c(1960,2010)) %>% 
  ggplot(aes(fertility,life_expectancy,col=continent)) + 
  geom_point() + 
  facet_grid(.~year)
```
