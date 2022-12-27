# Analysing ACT Standard Test

# ACT standard Test score mimic Normal Distribution with mean 20.9 
# and stadard distribution 5.7

# generating 10000 ACT scores.

set.seed(16, sample.kind = "Rounding")
act_scores <- rnorm(10000, 20.9, 5.7)
summary(act_scores)

# What is the mean of act_scores?
mean(act_scores)

# What is the standard deviation of act_scores?
sd(act_scores)

# A perfect score is 36 or greater (the maximum reported score is 36).
# In act_scores, how many perfect scores are there out of 10,000 simulated tests?

sum(act_scores >= 36)


# In act_scores, what is the probability of an ACT score greater than 30?

Pr_greater_30 <- sum(act_scores > 30) / length(act_scores)
Pr_greater_30


# In act_scores, what is the probability of an ACT score less than or equal to 10?

Pr_less_10 <- sum(act_scores <= 10) / length(act_scores)
Pr_less_10    


# Set x equal to the sequence of integers 1 to 36. 
# Use dnorm to determine the value of the probability 
# density function over x given a mean of 20.9 and 
# standard deviation of 5.7; save the result as f_x. Plot x against f_x.

x <- seq(1, 36)
f_x <- dnorm(x, 20.9, 5.7)
plot(x, f_x)

# What is the probability of a Z-score greater than 2 
# (2 standard deviations above the mean)?

# Creating Z score vector for act_scores
z_act_scores <- (act_scores - mean(act_scores)) / sd(act_scores)

Pr_Z_more_2 <- sum(z_act_scores > 2) / length(z_act_scores)
Pr_Z_more_2

# What ACT score value corresponds to 2 standard deviations above the mean (Z = 2)?

max(act_scores[(which(z_act_scores <= 2, z_act_scores))])


# A Z-score of 2 corresponds roughly to the 97.5th percentile.

# Use qnorm() to determine the 97.5th percentile of normally distributed 
# data with the mean and standard deviation observed in act_scores.
# What is the 97.5th percentile of act_scores?

qnorm(0.975, 20.9, 5.7)



# In this 4-part question, you will write a function to create a 
# CDF for ACT scores.

# Write a function that takes a value and produces the probability 
# of an ACT score less than or equal to that value (the CDF). 
# Apply this function to the range 1 to 36.

cdf_act <- function(x){
    sum(act_scores <= x) / 10000
}

Pr_act <- sapply(1:36, cdf_act)


# What is the minimum integer score such that the probability 
# of that score or lower is at least .95?

min(which(Pr_act >= 0.95, Pr_act))

# What is the expected 95th percentile of ACT scores?
qnorm(0.95, 20.9, 5.7)


# As discussed in the Data Visualization course, we can use quantile() 
# to determine sample quantiles from the data.

# Make a vector containing the quantiles for p <- seq(0.01, 0.99, 0.01), 
# the 1st through 99th percentiles of the act_scores data. 
# Save these as sample_quantiles.

p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, probs = p)

# In what percentile is a score of 26?

max(which(sample_quantiles <= 26))




# Make a corresponding set of theoretical quantiles using qnorm() 
# over the interval p <- seq(0.01, 0.99, 0.01) with mean 20.9 and 
# standard deviation 5.7. Save these as theoretical_quantiles. 
# Make a QQ-plot graphing sample_quantiles on the y-axis versus 
# theoretical_quantiles on the x-axis.
qqplot(theoretical_quantiles, sample_quantiles)
abline(0, 1)
