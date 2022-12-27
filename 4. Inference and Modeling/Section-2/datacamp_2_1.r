## Exercise 1. Sample average

# Write function called take_sample that takes the proportion of Democrats 
# p and the sample size N as arguments and returns the sample average of 
# Democrats (1) and Republicans (0).

# Calculate the sample average if the proportion of Democrats 
# equals 0.45 and the sample size is 100.

# Instructions
# Define a function called take_sample that takes p and N as arguments.
# Use the sample function as the first statement in your function to sample N
# elements from a vector of options where Democrats are assigned the 
# value '1' and Republicans are assigned the value '0' in that order.
# Use the mean function as the second statement in your function 
# to find the average value of the random sample.

# Write a function called `take_sample` that takes `p` and `N` 
# as arguements and returns the average value of a randomly 
# sampled population.
take_sample <- function(p, N) {
    smpl <- sample(c(1, 0), prob=c(p, (1 - p)), replace=TRUE, size=100)
    mean(smpl)
}

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# Call the `take_sample` function to determine the sample average of `N` 
# randomly selected people from a population containing a proportion of 
# Democrats equal to `p`. Print this value to the console.
take_sample(p, N)


# Exercise 2. Distribution of errors - 1

# Assume the proportion of Democrats in the population p equals 0.45 and 
# that your sample size N is 100 polled voters. The take_sample function 
# you defined previously generates our estimate, X

# Replicate the random sampling 10,000 times and calculate p − X
# for each random sample. Save these differences as a vector called errors. 
# Find the average of errors and plot a histogram of the distribution.

# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# The variable `B` specifies the number of times we want the sample to 
# be replicated
B <- 10000

# Use the `set.seed` function to make sure your answer matches the 
# expected result after random sampling
set.seed(1)

# Create an objected called `errors` that replicates subtracting the 
# result of the `take_sample` function from `p` for `B` replications
errors <- replicate(B, {
    X_hat <- take_sample(p, N)
    p - X_hat
})

# Calculate the mean of the errors. Print this value to the console.
mean(errors)


# Exercise 4. Average size of error

# The error p − X is a random variable. In practice, the error is 
# not observed because we do not know the actual proportion of 
# Democratic voters, p. However, we can describe the size of the error 
# by constructing a simulation.

# What is the average size of the error if we define the size by 
# taking the absolute value ∣p − X∣?

# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# The variable `B` specifies the number of times we want the sample to be replicated
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# We generated `errors` by subtracting the estimate from the actual proportion of Democratic voters
errors <- replicate(B, p - take_sample(p, N))

# Calculate the mean of the absolute value of each simulated error. Print this value to the console.
mean(abs(errors))


# Exercise 5. Standard deviation of the spread

# The standard error is related to the typical size of the error we 
# make when predicting. We say size because, as we just saw, the errors 
# are centered around 0. In that sense, the typical error is 0. 
# For mathematical reasons related to the central limit theorem, we 
# actually use the standard deviation of errors rather than the average 
# of the absolute values.

# As we have discussed, the standard error is the square root of the 
# average squared distance 
# (X − p)^2. The standard deviation is defined as the square root of 
# the distance squared.

# Calculate the standard deviation of the spread.

# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# The variable `B` specifies the number of times we want the sample to be replicated
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# We generated `errors` by subtracting the estimate from the actual proportion of Democratic voters
errors <- replicate(B, p - take_sample(p, N))

# Calculate the standard deviation of `errors`
sqrt(mean(abs(errors)^2))


# Exercise 6. Estimating the standard error

# The theory we just learned tells us what this standard deviation 
# is going to be because it is the standard error of X.

# Estimate the standard error given an expected value of 0.45 and 
# a sample size of 100.

# Define `p` as the expected value equal to 0.45
p <- 0.45

# Define `N` as the sample size
N <- 100

# Calculate the standard error
sqrt(p * (1 - p) / N)


# Exercise 7. Standard error of the estimate

# In practice, we don't know p, so we construct an estimate of the 
# theoretical prediction based by plugging in X for p. 
# Calculate the standard error of the estimate:

# Define `p` as a proportion of Democratic voters to simulate
p <- 0.45

# Define `N` as the sample size
N <- 100

# Use the `set.seed` function to make sure your answer matches the 
# expected result after random sampling
set.seed(1)

# Define `X` as a random sample of `N` voters with a probability of 
# picking a Democrat ('1') equal to `p`
X <- sample(c(1, 0), prob=c(p, (1 -p)), replace=TRUE, size=N)

# Define `X_bar` as the average sampled proportion
X_bar <- mean(X)

# Calculate the standard error of the estimate. Print the result to the console.
sqrt(X_bar * (1 - X_bar) / N)


# Exercise 8. Plotting the standard error

# The standard error estimates obtained from the Monte Carlo simulation, 
# the theoretical prediction, and the estimate of the theoretical prediction 
# are all very close, which tells us that the theory is working. 
# This gives us a practical approach to knowing the typical error we will 
# make if we predict p with X. The theoretical result gives us an idea of 
# how large a sample size is required to obtain the precision we need. 
# Earlier we learned that the largest standard errors occur for 
# p=0.5.

# Create a plot of the largest standard error for N ranging from 100 to 5,000. 
# Based on this plot, how large does the sample size have to be to have a 
# standard error of about 1%?

N <- seq(100, 5000, len = 100)
p <- 0.5
se <- sqrt(p*(1-p)/N)

data.frame(N=N, SE=se) %>% ggplot(aes(N, SE)) +
    geom_line()


# Exercise 11. Plotting the errors

# Make a qq-plot of the errors you generated previously to see if they 
# follow a normal distribution.

# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# The variable `B` specifies the number of times we want the sample to be replicated
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Generate `errors` by subtracting the estimate from the actual proportion of Democratic voters
errors <- replicate(B, p - take_sample(p, N))

# Generate a qq-plot of `errors` with a qq-line showing a normal distribution
qqnorm(errors)
qqline(errors)


# Exercise 12. Estimating the probability of a specific value of X-bar

# If p = 0.45 and N = 100, use the central limit theorem to estimate 
# the probability that X > 0.5.

# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# Calculate the probability that the estimated proportion of Democrats 
# in the population is greater than 0.5. Print this value to the console.
1 - pnorm(0.5, mean=p, sd=sqrt(p*(1-p)/N))


# Exercise 13. Estimating the probability of a specific error size

# Assume you are in a practical situation and you don't know p. Take a sample of size 
# N=100 and obtain a sample average of X =0.51.

# What is the CLT approximation for the probability that your error size 
# is equal or larger than 0.01?
# Define `N` as the number of people polled
N <-100

# Define `X_hat` as the sample average
X_hat <- 0.51

# Define `se_hat` as the standard error of the sample average
se_hat <- sqrt(X_hat * (1 - X_hat) / N)

# Calculate the probability that the error is 0.01 or larger
pnorm(-0.01, mean=0, sd=se_hat) + (1 - pnorm(0.01, mean=0, sd=se_hat))


