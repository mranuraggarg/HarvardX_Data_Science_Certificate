library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

# Question 1
# Load the temp_carbon dataset from dslabs, which contains annual global 
# temperature anomalies (difference from 20th century mean temperature 
# in degrees Celsius), temperature anomalies over the land and ocean, 
# and global carbon emissions (in metric tons). Note that the date ranges 
# differ for temperature and carbon emissions.

# Which of these code blocks return the latest year for which carbon 
# emissions are reported?

temp_carbon %>%
  .$year %>%
  max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  pull(year) %>%
  max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  max(year)

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  .$year %>%
  max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  select(year) %>%
  max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  select(year) %>%
  max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  max(.$year)


# Question 2
# Inspect the difference in carbon emissions in temp_carbon from the first 
# available year to the last available year.

# What is the first year for which carbon emissions (carbon_emissions) 
# data are available?

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  select(year) %>%
  min()

comparision_carbon <- temp_carbon %>%
  filter(year %in% c(1751, 2014), !is.na(carbon_emissions)) %>%
  select(year, carbon_emissions)

comparision_carbon <- comparision_carbon %>% mutate(relative=carbon_emissions / 3)
comparision_carbon


# Question 3
# Inspect the difference in temperature in temp_carbon from the first 
# available year to the last available year.

# What is the first year for which global temperature anomaly (temp_anomaly) 
# data are available?

min_temp_year <- temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  select(year) %>%
  min()

max_temp_year <- temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  select(year) %>%
  max()

temp_year <- c(min_temp_year, max_temp_year)

temp_diff <- temp_carbon %>%
  filter(year%in%temp_year) %>%
  select(year, temp_anomaly) %>%
  mutate(temp_diff = temp_anomaly - temp_anomaly[year==min_temp_year])

temp_diff


# Question 4
# Create a time series line plot of the temperature anomaly. 
# Only include years where temperatures are reported. 
# Save this plot to the object p.

# Which command adds a blue horizontal line indicating the 20th 
# century mean temperature?

p <- temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  select(year, temp_anomaly) %>%
  ggplot(aes(year, temp_anomaly)) +
  geom_point()

p <- p + geom_hline(aes(yintercept = 0), col = "blue")
p


# Question 5
# Continue working with p, the plot created in the previous question.

# Change the y-axis label to be "Temperature anomaly (degrees C)". 
# Add a title, "Temperature anomaly relative to 20th century mean, 1880-2018". 
# Also add a text layer to the plot: the x-coordinate should be 2000, 
# the y-coordinate should be 0.05, the text should be "20th century mean", 
# and the text color should be blue.

# Which of the following code blocks is correct?

p <- p + ylab("Temperature anomaly (degrees C)")
p

p <- p + ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018")
p

p <- p + 
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")
p

p <- temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  select(year, temp_anomaly) %>%
  ggplot(aes(year, temp_anomaly)) +
  geom_point() +
  geom_hline(aes(yintercept = 0), col = "blue") +
  ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")

p
  

# Question 7
# Add layers to the previous plot to include line graphs of the temperature 
# anomaly in the ocean (ocean_anomaly) and on land (land_anomaly). 
# Assign different colors to the lines. Compare the global temperature 
# anomaly to the land temperature anomaly and ocean temperature anomaly.

# Which region has the largest 2018 temperature anomaly relative to the 
# 20th century mean?
  
p <- temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  filter(!is.na(land_anomaly)) %>%
  filter(!is.na(ocean_anomaly)) %>%
  select(year, temp_anomaly, land_anomaly, ocean_anomaly) %>%
  ggplot(aes(year)) +
  geom_point(aes(y = temp_anomaly)) +
  geom_line(aes(y = land_anomaly), col="red") +
  geom_line(aes(y = ocean_anomaly), col="lightblue") +
  geom_hline(aes(yintercept = 0), col = "blue") +
  ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")
p

