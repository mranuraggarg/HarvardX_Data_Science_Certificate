# A casino offers a House Special bet on roulette, which is a bet 
# on five pockets (00, 0, 1, 2, 3) out of 38 total pockets. 
# The bet pays out 6 to 1. In other words, a losing bet 
# yields -$1 and a successful bet yields $6. 
# A gambler wants to know the chance of losing money if he 
# places 500 bets on the roulette House Special.

# The following 7-part question asks you to do some 
# calculations related to this scenario.

# Question 3a
# What is the expected value of the payout for one bet?

p_win <- 5 / 38
p_loss <- 1 - p_win
win <- 6
loss <- -1

E_x <- win * p_win + loss * p_loss
E_x


# Question 3b
# What is the standard error of the payout for one bet?

se <- abs(win - loss) * sqrt(p_win * p_loss)
se

# Question 3c
# What is the expected value of the average payout over 500 bets?

E_x

# Question 3d
# What is the standard error of the average payout over 500 bets?

se / sqrt(500)

# Question 3e
# What is the expected value of the sum of 500 bets?

E_x_500 <- 500 * E_x
E_x_500

# Question 3f
# What is the standard error of the sum of 500 bets?

se_500 <- sqrt(500) * se
se_500

# Question 3g
# Use pnorm() with the expected value of the sum and 
# standard error of the sum to calculate the probability 
# of losing money over 500 bets, Pr(𝑋≤ 0).

pnorm(0, E_x_500, se_500)
