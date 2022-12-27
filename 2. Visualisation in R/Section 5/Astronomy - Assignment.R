library(tidyverse)
library(dslabs)
library(ggrepel)
data(stars)
options(digits = 3)

# Question 1
# Load the stars data frame from dslabs. This contains the name, 
# absolute magnitude, temperature in degrees Kelvin, and spectral 
# class of selected stars. Absolute magnitude (shortened in these 
# problems to simply "magnitude") is a function of star luminosity, 
# where negative values of magnitude have higher luminosity.
# Question 1a
# What is the mean magnitude?

mean(stars$magnitude)
sd(stars$magnitude)


# Question 2
# Make a density plot of the magnitude.
# How many peaks are there in the data?

stars %>% ggplot(aes(magnitude)) +
  geom_density()

# Question 3
# Examine the distribution of star temperature.
# Which of these statements best characterizes the temperature distribution?

stars %>% ggplot(aes(temp)) +
  geom_density()



# Question 4
# Make a scatter plot of the data with temperature on the x-axis 
# and magnitude on the y-axis and examine the relationship between 
# the variables. Recall that lower magnitude means a more luminous 
# (brighter) star.

# When considering the plot of magnitude vs. temperature, most stars 
# follow a _______________ trend. These are called main sequence stars.

# Fill in the blank:

stars %>%
  ggplot(aes(temp, magnitude)) +
  geom_point()


# Question 5
# For various reasons, scientists do not always follow straight conventions 
# when making plots, and astronomers usually transform values of star 
# luminosity and temperature before plotting. Flip the y-axis so that 
# lower values of magnitude are at the top of the axis (recall that more 
# luminous stars have lower magnitude) using scale_y_reverse(). 
# Take the log base 10 of temperature and then also flip the x-axis.

# Fill in the blanks in the statements below to describe the resulting plot:
  
# The brighest, highest temperature stars are in the ______________ corner 
# of the plot.

stars %>%
  ggplot(aes(temp, magnitude)) +
  geom_point() +
  scale_y_reverse() +
  scale_x_log10() +
  scale_x_reverse()


# Question 6
# The trends you see allow scientists to learn about the evolution and 
# lifetime of stars. The primary group of stars to which most stars belong 
# we will call the main sequence stars (discussed in question 4). 
# Most stars belong to this main sequence, however some of the more rare 
# stars are classified as “old” and “evolved” stars. These stars tend to 
# be hotter stars, but also have low luminosity, and are known as white dwarfs.

# How many white dwarfs are there in our sample?



# Question 7
# Consider stars which are not part of the Main Group but are not old/evolved 
# (white dwarf) stars. These stars must also be unique in certain ways and are 
# known as giants. Use the plot from Question 5 to estimate the average 
# temperature of a giant.

# Which of these temperatures is closest to the average temperature of a giant?:


# Question 8
# We can now identify whether specific stars are main sequence stars, 
# red giants or white dwarfs. Add text labels to the plot to answer 
# these questions. You may wish to plot only a selection of the labels, 
# repel the labels, or zoom in on the plot in RStudio so you can locate 
# specific stars.

# Fill in the blanks in the statements below:
  
# The least lumninous star in the sample with a surface temperature over 
# 5000K is _________.

stars %>%
  ggplot(aes(temp, magnitude)) +
  geom_text_repel(aes(label=star)) +
  geom_point() +
  scale_y_reverse() +
  scale_x_log10() +
  scale_x_reverse()

# Question 9
# Remove the text labels and color the points by star type. 
# This classification describes the properties of the star's 
# spectrum, the amount of light produced at various wavelengths.

# Which star type has the lowest temperature?

stars %>%
  ggplot(aes(temp, magnitude, color = type)) +
  geom_point() +
  scale_y_reverse() +
  scale_x_log10() +
  scale_x_reverse()
