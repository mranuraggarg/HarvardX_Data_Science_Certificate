# Practice
library(tidyverse)
library(dslabs)
data("gapminder")

ds_theme_set()
gapminder %>% filter(country == "India") %>%
  ggplot(aes(year, fertility)) +
  geom_line() + 
  theme(text=element_text(size=16, family="Impact"))

countries <- c("India", "Pakistan")
label <- data.frame(country=countries, x=c(1982, 2002), y=c(4, 5))
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, fertility, col = country)) +
  geom_line() + 
  geom_text(data=label, aes(x, y, label=country), size=5, family="Impact") +
  theme(text=element_text(size=16, family="Impact"), legend.position = "none")

countries <- c("India", "Pakistan")
gapminder %>% filter(country %in% countries, year%in%seq(1982, 2012, 2)) %>%
  ggplot(aes(year, life_expectancy, col = country)) +
  geom_point() +
  facet_grid(.~year) +
  theme(text=element_text(size=16, family="Impact"))

