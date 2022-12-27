# An old version of the SAT college entrance exam had a -0.25 point 
# penalty for every incorrect answer and awarded 1 point for a 
# correct answer. 
# The quantitative test consisted of 44 multiple-choice 
# questions each with 5 answer choices. Suppose a student 
# chooses answers by guessing for all questions on the test.

# Question 1a
# What is the probability of guessing correctly for one question?

p_corrent_ans <- 1 / 5
p_wrong_ans <- 4 / 5

# Question 1b
# What is the expected value of points for guessing on one question?

E_x <- 1 * p_corrent_ans + -0.25 * p_wrong_ans
E_x


# Question 1d
# What is the standard error of guessing on all 44 questions?

se <- sqrt(44) * abs(1 - -0.25) * sqrt(p_corrent_ans * p_wrong_ans)
se
# Question 1e
# Use the Central Limit Theorem to determine the probability that 
# a guessing student scores 8 points or higher on the test.

1 - pnorm(8, E_x, se)

set.seed(21, sample.kind = "Rounding")

# Question 1f 
B <- 10000
S <- replicate(B, {
    sum(sample(c(1, -0.25), 44, replace=TRUE, p = c(1/5, 4/5)))
})

mean(S > 8)


# The SAT was recently changed to reduce the number of multiple choice 
# options from 5 to 4 and also to eliminate the penalty for guessing.

# In this two-part question, you'll explore how that affected 
# the expected values for the test.


# Question 2a 

# Suppose that the number of multiple choice options is 4 
# and that there is no penalty for guessing - that is, an 
# incorrect question gives a score of 0.
# What is the expected value of the score when guessing on this new test?

p_corrent_ans <- 1 / 4
p_wrong_ans <- 3 / 4

E_x <- 44 * (1 * p_corrent_ans + 0 * p_wrong_ans)
E_x

# Question 2b
# Consider a range of correct answer probabilities 
# p <- seq(0.25, 0.95, 0.05) representing a range 
# of student skills.
# What is the lowest p such that the probability 
# of scoring over 35 exceeds 80%?

p <- seq(0.25, 0.95, 0.05)

sam <- function(p){
    E_x <- 44 * 1 * p
    se <- sqrt(44) * sqrt(p * (1 - p))
    prob <- 1 - pnorm(35, E_x, se)
    if (E_x > 35 & prob > 0.8) {
        return(p)
    }
    else{
        return(1)
    }
}

min(sapply(p, sam))


