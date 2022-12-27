options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

str(titanic)


# Question 2: Demographics of Titanic Passengers
# Make density plots of age grouped by sex. 
# Try experimenting with combinations of faceting, alpha blending, 
# stacking and using variable counts on the y-axis to answer the 
# following questions. Some questions may be easier to answer with 
# different versions of the density plot.

# Which of the following are true?
# Select all correct answers.

titanic %>% ggplot(aes(Age, color=Sex)) +
  geom_density()

titanic %>%
  ggplot(aes(Age, fill = Sex)) +
  geom_density(alpha = 0.2) +
  facet_grid(Sex ~ .)

titanic %>%
  ggplot(aes(Age, y = ..count.., fill = Sex)) +
  geom_density(alpha = 0.2, position = "stack")

titanic %>%
  ggplot(aes(Age, fill = Sex)) +
  geom_density(alpha = 0.2)

params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

params
titanic %>% ggplot() +
  geom_qq(aes(sample=Age)) +
  geom_abline(intercept = params$mean, slope = params$sd)


# Question 4: Survival by Sex
# To answer the following questions, make barplots of the Survived and 
# Sex variables using geom_bar(). Try plotting one variable and filling 
# by the other variable. You may want to try the default plot, then try 
# adding position = position_dodge() to geom_bar() to make separate bars 
# for each group.

# Which of the following are true?

titanic %>% ggplot(aes(Sex, fill=Survived)) +
  geom_bar()

titanic %>% ggplot(aes(Survived, fill=Sex)) +
  geom_bar()

titanic %>% filter(Survived==1) %>%
  ggplot(aes(Sex)) +
  geom_bar()

titanic %>% filter(Sex=="female") %>%
  ggplot(aes(Survived)) +
  geom_bar()

# Question 5: Survival by Age
# Make a density plot of age filled by survival status. 
# Change the y-axis to count and set alpha = 0.2.

# Which age group is the only group more likely to survive than die?
titanic %>%
  ggplot(aes(Age, y = ..count.., fill = Survived)) +
  geom_histogram(alpha = 0.2, position = "stack")

titanic %>%
  ggplot(aes(Age, fill = Survived)) +
  geom_density(alpha = 0.2, position = "stack")

titanic %>%
  ggplot(aes(Age, color=Survived)) +
  geom_histogram(alpha = 0.2, position = "stack") +
  facet_grid(.~Survived)

# Question 6: Survival by Fare
# Filter the data to remove individuals who paid a fare of 0. 
# Make a boxplot of fare grouped by survival status. 
# Try a log2 transformation of fares. Add the data points with jitter 
# and alpha blending.

# Which of the following are true?

titanic %>% filter("Fare">0) %>%
  ggplot(aes(Survived, Fare)) +
  geom_boxplot() +
  geom_jitter(aes(Survived), position = position_jitter(w=0.1, h=0.1), alpha=0.1) +
  scale_y_continuous(trans = "log2")


# Question 7: Survival by Passenger Class
# The Pclass variable corresponds to the passenger class. 
# Make three barplots. For the first, make a basic barplot of passenger class 
# filled by survival. For the second, make the same barplot but use the 
# argument position = position_fill() to show relative proportions in each 
# group instead of counts. For the third, make a barplot of survival filled by 
# passenger class using position = position_fill().

# Which of the following are true?

titanic %>%
  ggplot(aes(Pclass, fill = Survived)) +
  geom_bar()

titanic %>%
  ggplot(aes(Pclass, fill = Survived)) +
  geom_bar(position = position_fill())

titanic %>%
  ggplot(aes(Survived, fill = Pclass)) +
  geom_bar(position = position_fill())


# Question 8: Survival by Age, Sex and Passenger Class
# Create a grid of density plots for age, filled by survival status, 
# with count on the y-axis, faceted by sex and passenger class.

# Which of the following are true?


titanic %>% ggplot(aes(Age, fill=Survived, alpha=0.2)) +
  geom_density() +
  facet_grid(Sex~Pclass)