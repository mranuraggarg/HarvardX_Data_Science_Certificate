library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

#Question 1 and 2
greenhouse_gases %>%
  ggplot(aes(year, concentration)) +
  geom_line() +
  facet_grid(gas~., scales = "free") +
  geom_vline(xintercept = 1850, color = "blue") +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")


# Question 3 carbon_emission vs year

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  ggplot(aes(year, carbon_emissions)) +
  geom_line()


# Question 4 Historic CO2 data

data("historic_co2")
co2_time <- historic_co2 %>%
  filter(!is.na(co2)) %>%
  ggplot(aes(year, co2, color = source)) +
  geom_line()


# Question 12, rate of change of CO2

co2_time + xlim(c(-800000, -775000))

co2_time + xlim(c(-375000, -330000))

co2_time + xlim(c(-3000, 2018))

