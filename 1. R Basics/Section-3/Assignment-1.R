library(dslabs)
library(dplyr)
library("tidyverse")
data(heights)
options(digits = 3)    # report 3 significant digits for all answers

# Question 1
# First, determine the average height in this dataset. 
# Then create a logical vector ind with the indices for those 
# individuals who are above average height.
# How many individuals in the dataset are above average height?

mu <- mean(heights$height)
ind <- heights %>% dplyr::filter(height > mu)
nrow(ind)

# Question 2
# How many individuals in the dataset are above average height 
# and are female?

females <- heights %>% dplyr::filter(height > mu & sex == "Female")
head(females)
nrow(females)


# Question 3
# If you use mean() on a logical (TRUE/FALSE) vector, 
# it returns the proportion of observations that are TRUE.
# What proportion of individuals in the dataset are female?

no_of_females <- heights %>% dplyr::filter(sex == "Female")
female_prop <- nrow(no_of_females) / nrow(heights)
print(female_prop)

# Question 4
# This question takes you through three steps to determine 
# the sex of the individual with the minimum height.

# Question 4a
# Determine the minimum height in the heights dataset.

min_height <- min(heights$height)
print(min_height)


# Question 4b
# Use the match() function to determine the index of the 
# first individual with the minimum height.

index <- match(min_height, heights$height)
print(index)


# Question 4c
# Subset the sex column of the dataset by the index in 
# 4b to determine the individual’s sex.

gender <- heights$sex[index]
print(gender)


# Question 5

# This question takes you through three steps to determine 
# how many of the integer height values between the minimum 
# and maximum heights are not actual heights of individuals 
# in the heights dataset.

# Question 5a
# Determine the maximum height.

max_height <- max(heights$height)
print(max_height)

# Question 5b
# Which integer values are between the maximum and minimum heights? 
# For example, if the minimum height is 10.2 and the maximum height 
# is 20.8, your answer should be x <- 11:20 to capture the integers 
# in between those values. (If either the maximum or minimum height 
# are integers, include those values too.)

# Write code to create a vector x that includes the integers between 
# the minimum and maximum heights (as numbers).


x <- 50:82.7L
print(x)

not_heights <- !x %in% heights$height
print(not_heights)
print(sum(not_heights))


# Question 6
# Using the heights dataset, create a new column of heights 
# in centimeters named ht_cm. Recall that 1 inch = 2.54 centimeters. 
# Save the resulting dataset as heights2.

heights <- mutate(heights, heights2=height*2.54)
head(heights)

# Question 6a
# What is the height in centimeters of the 18th 
# individual (index 18)?

height <- heights$heights2[18]
print(height)

# Question 6b

# What is the mean height in centimeters?

print(mean(heights$heights2))


# Question 6c
# How many females are in the heights2 dataset?

print(nrow(filter(heights, sex=="Female")))


# Question 6d
# What is the mean height of females in cm

mean_height_cm <- heights %>% dplyr::filter(sex=="Female")
head(mean_height_cm)
print(mean(mean_height_cm$heights2))


# Question 8

# The olive dataset in dslabs contains composition in percentage 
# of eight fatty acids found in the lipid fraction of 572 Italian 
# olive oils:

data(olive)
head(olive)

x <- olive$palmitic
y <- olive$palmitoleic

plot(x, y)


#  Question 9

# Create a histogram of the percentage of eicosenoic acid in olive.

hist(olive$eicosenoic)


# Question 10

# Make a boxplot of palmitic acid percentage in olive with 
# separate distributions for each region.
# Which region has the highest median palmitic acid percentage?

boxplot(palmitic~region, data=olive)
