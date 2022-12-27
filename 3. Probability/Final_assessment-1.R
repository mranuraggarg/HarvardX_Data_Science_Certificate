library(tidyverse)
library(dslabs)

data(death_prob)
head(death_prob)

# An insurance company offers a one-year term life insurance policy 
# that pays $150,000 in the event of death within one year. 
# The premium (annual cost) for this policy for a 50 year 
# old female is $1,150. Suppose that in the event of a claim, 
# the company forfeits the premium and loses a total of $150,000, 
# and if there is no claim the company gains the premium amount of $1,150. 
# The company plans to sell 1,000 policies to this demographic.

# Question 1a
# The death_prob data frame from the dslabs package contains information 
# about the estimated probability of death within 1 year (prob) for 
# different ages and sexes.
# Use death_prob to determine the death probability of a 50 year old female, p.

p <- death_prob %>%
    filter(sex == "Female" & age == 50) %>%
    .$prob
p

# Question 1b

# The loss in the event of the policy holder's death 
# is -$150,000 and the gain if the policy holder 
# remains alive is the premium $1,150.
# What is the expected value of the company's net profit on 
# one policy for a 50 year old female?

mu <- p * (-150000) + (1 - p) * 1150
mu

# Question 1c
# Calculate the standard error of the profit on one policy 
# for a 50 year old female.

sd <- (-150000 - mu)^2 * p + (1150 - mu)^2 * (1 - p)
se <- sqrt(sd)

# Question 1d
# What is the expected value of the company's profit over 
# all 1,000 policies for 50 year old females?

mu_1000 <- 1000 * mu
mu_1000

# Question 1e
# What is the standard error of the sum of the expected value over 
# all 1,000 policies for 50 year old females?

se_1000 <- sqrt(1000) * se
se_1000


# Question 1f
# Use the Central Limit Theorem to calculate the probability that 
# the insurance company loses money on this set of 1,000 policies.

prob <- pnorm(0, mu_1000, se_1000)
prob


# Question 2a
# Use death_prob to determine the probability of death within 
# one year for a 50 year old male.

p_male <- death_prob %>%
    filter(sex == "Male", age == 50) %>%
    .$prob

p_male



# Question 2b
# Suppose the company wants its expected profits from 
# 1,000 50 year old males with $150,000 life insurance 
# policies to be $700,000. Use the formula for expected 
# value of the sum of draws with the following values 
# and solve for the premium 𝑏:

mu_s <- 700000
n <- 1000
a <- -150000

# a * p_male + b * (1 - p) = mu_s / n
b <- (mu_s / n - a * p_male) / (1 - p_male)
b


# Question 2c
# Using the new 50 year old male premium rate, calculate the 
# standard error of the sum of 1,000 premiums.

se_s <- abs(a - b) * sqrt(p_male * (1 - p_male) * n)
se_s

# Question 2d
# What is the probability of losing money on a series of 
# 1,000 policies to 50 year old males?

pnorm(0, mu_s, se_s)

# Life insurance rates are calculated using mortality statistics 
# from the recent past. They are priced such that companies are 
# almost assured to profit as long as the probability of death 
# remains similar. If an event occurs that changes the probability 
# of death in a given age group, the company risks significant losses.
# In this 6-part question, we'll look at a scenario in which a lethal 
# pandemic disease increases the probability of death within 1 year 
# for a 50 year old to .015. Unable to predict the outbreak, the 
# company has sold 1,000 $150,000 life insurance policies for $1,150.

# Question 3a

# What is the expected value of the company's profits over 1,000 policies?
a <- -150000
b <- 1150
p_pend <- 0.015
n <- 1000

mu <- (a * p_pend + b * (1 - p_pend)) * n
mu

# Question 3b

# What is the standard error of the expected value of 
# the company's profits over 1,000 policies?

se <- abs(b - a) * sqrt(n * p_pend * (1 - p_pend))
se

#  Question 3c

# What is the probability of the company losing money?
pnorm(0, mu, se)


#  Question 3d

# Suppose the company can afford to sustain one-time losses 
# of $1 million, but larger losses will force it to go out of business.

# What is the probability of losing more than $1 million?
pnorm(-10^6, mu, se)


# Question 3e

# Investigate death probabilities p <- seq(.01, .03, .001).

# What is the lowest death probability for which the chance of 
# losing money exceeds 90%?
p <- seq(.01, .03, .001)
mu <- (a * p_pend + b * (1 - p_pend)) * n
se <- abs(b - a) * sqrt(n * p_pend * (1 - p_pend))
pnorm(0, mu, se)

count_losses_p <- function(p_pend) {
    mu <- (a * p_pend + b * (1 - p_pend)) * n
    se <- abs(b - a) * sqrt(n * p_pend * (1 - p_pend))
    loss_probability <- pnorm(0, mu, se)
    return(c(p_pend, loss_probability))
}
sapply(p, count_losses_p)

# Question 3f

# Investigate death probabilities p <- seq(.01, .03, .0025).

# What is the lowest death probability for which the chance 
# of losing over $1 million exceeds 90%?
p <- seq(.01, .03, .0025)
mu <- (a * p_pend + b * (1 - p_pend)) * n
se <- abs(b - a) * sqrt(n * p_pend * (1 - p_pend))
pnorm(0, mu, se)

count_losses_p <- function(p_pend) {
    mu <- (a * p_pend + b * (1 - p_pend)) * n
    se <- abs(b - a) * sqrt(n * p_pend * (1 - p_pend))
    loss_probability <- pnorm(-1000000, mu, se)
    return(c(p_pend, loss_probability))
}
sapply(p, count_losses_p)


# Question 4a

# Define a sampling model for simulating the total profit 
# over 1,000 loans with probability of claim p_loss = .015, 
# loss of -$150,000 on a claim, and profit of $1,150 when 
# there is no claim. Set the seed to 25, then run the model once.

# (IMPORTANT! If you use R 3.6 or later, you will need to use 
# the command set.seed(x, sample.kind = "Rounding") instead 
# of set.seed(x). Your R version will be printed at the top 
# of the Console window when you start RStudio.)

# What is the reported profit (or loss) in millions (that is, divided by 10^6)?

set.seed(25, sample.kind = "Rounding")
n <- 1000
p <- 0.015
loss <- -150000
profit <- 1150

defaults <- sample(c(profit, loss), n, c(1-p, p), replace=TRUE)
sum(defaults) / 10^6

# Question 4b

# Set the seed to 27, then run a Monte Carlo simulation of 
# your sampling model with 10,000 replicates to simulate 
# the range of profits/losses over 1,000 loans.

# (IMPORTANT! If you use R 3.6 or later, you will need to use 
# the command set.seed(x, sample.kind = "Rounding") instead 
# of set.seed(x). Your R version will be printed at the top 
# of the Console window when you start RStudio.)

# What is the observed probability of losing $1 million or more?
set.seed(27, sample.kind = "Rounding")
n <- 1000
p <- 0.015
loss <- -150000
profit <- 1150
B <- 10000
losses <- replicate(B, {
    defaults <- sample(c(profit, loss), n, c(1-p, p), replace=TRUE)
    sum(defaults)
})
length(which(losses < -10^6))/length(losses)


# Questions 5 and 6: Insurance rates, part 3

# Question 5, 
# which has 4 parts, continues the pandemic scenario from Questions 3 and 4.

# Suppose that there is a massive demand for life insurance due to the pandemic,
# and the company wants to find a premium cost for which the probability of 
# losing money is under 5%, assuming the death rate stays stable at p = 0.015.

# Question 5a

# Calculate the premium required for a 5% chance of losing money given
# loans, probability of death , and loss per claim . Save this premium 
# as x for use in further questions.
# Pr(S < 0) = 0.05
# S is normal with mu and se
profit <- 1150
loss <- -150000
p_pend <- 0.015
mu <- (loss * p_pend + profit * (1 - p_pend)) * n
se <- abs(profit - loss) * sqrt(n * p_pend * (1 - p_pend))

# Pr(Z < -{lp + x(1-p)}n / {(x - l)*sqrt(np(1-p))}) = 0.05
# -{lp + x(1-p)}n / {(x - l)*sqrt(np(1-p))} = qnorm(0.05)
z <- qnorm(0.05)
denominator <- (n*(1 - p_pend) + z*sqrt(n*p_pend*(1 - p_pend)))
x <- -loss*(n*p_pend - z*sqrt(n*p_pend*(1 - p_pend)))/denominator
x

# Question 5b

# What is the expected profit per policy at this rate?
mu_new_per_policy <- (loss * p_pend + x * (1 - p_pend))
mu_new_per_policy

# Question 5c

# What is the expected profit over 1,000 policies?
mu_new <- mu_new_per_policy * 1000
mu_new

# Question 5d

# Run a Monte Carlo simulation with B=10000to determine the 
# probability of losing money on 1,000 policies given the new 
# premium x, loss on a claim of $150,000, and probability of claim. 
# Set the seed to 28 before running your simulation.

# (IMPORTANT! If you use R 3.6 or later, you will need to use 
# the command set.seed(x, sample.kind = "Rounding") instead 
# of set.seed(x). Your R version will be printed at the top 
# of the Console window when you start RStudio.)

# What is the probability of losing money here?
set.seed(28, sample.kind = "Rounding")
n <- 1000
p <- 0.015
loss <- -150000
profit <- 3268.063
B <- 10000
losses <- replicate(B, {
    defaults <- sample(c(profit, loss), n, c(1-p, p), replace=TRUE)
    sum(defaults)
})
length(which(losses < 0))/length(losses)

    
# The company cannot predict whether the pandemic death rate will stay stable. 
# Set the seed to 29, then write a Monte Carlo simulation that for each of 
# iterations:
    
# randomly changes p by adding a value between -0.01 and 0.01 with 
# sample(seq(-0.01, 0.01, length = 100), 1)
# uses the new random p to generate a sample of n = 1000 policies with 
# premium x and loss per claim 
# returns the profit over  policies (sum of random variable)

# (IMPORTANT! If you use R 3.6 or later, you will need to use 
# the command set.seed(x, sample.kind = "Rounding") instead 
# of set.seed(x). Your R version will be printed at the top 
# of the Console window when you start RStudio.)

# The outcome should be a vector of  total profits. 
# Use the results of the Monte Carlo simulation to answer the 
# following three questions.

# (Hint: Use the process from lecture for modeling a situation for 
# loans that changes the probability of default for all borrowers 
# simultaneously.)
set.seed(29, sample.kind = "Rounding")
B <- 10000
n <- 1000
p = 0.015
profit <- 3268.063
loss <- -150000
profits <- replicate(B, {
    new_p <- p + sample(seq(-0.01, 0.01, length = 100), 1)
    draw <- sample( c(profit, loss), n,
            prob = c(1 - new_p, new_p), replace = TRUE)
    sum(draw)
})
length(profits)


# Question 6a

# What is the expected value over 1,000 policies?
profit_expected = mean(profits)
profit_expected

# Question 6b

# What is the probability of losing money?
length(which(profits < 0)) / length(profits)


# Question 6c

# What is the probability of losing more than $1 million?
length(which(profits < -10^6)) / length(profits)
