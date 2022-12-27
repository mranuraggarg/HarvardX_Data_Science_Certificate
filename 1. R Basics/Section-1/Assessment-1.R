# Question 1 solve quadratic equaiton
a <- 2
b <- -1
c <- -4

solution = c((-b + sqrt(b^2 - 4*a*c))/(2*a), (-b - sqrt(b^2 - 4*a*c))/(2*a))
print(solution)

# Question 2 log base 4 of 1024
print(log(1024, base = 4))

# Library and data load for Question 3a, 3b and 3c
library(dslabs)
data(movielens)

# Question 3a How many rows are in the dataset?
str(movielens)

# Question 3b How many different variables are in the dataset?
print(length(names(movielens)))

# Question 3c What is the variable type of title ?
print(class(movielens$title))

# Question 3d What is the variable type of genres ?
print(class(movielens$genres))

# Question 4 Determine the number of levels in genres using nlevels
print(nlevels(movielens$genres))
print(nlevels(movielens$genres) == length(levels(movielens$genres)))
