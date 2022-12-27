library(gtools)
library(tidyverse)

# Run a Monte Carlo simulation on this vector representing 
# the countries of the 8 runners in this race:

runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", 
             "Netherlands", "France", "South Africa")
set.seed(1)
B <- 10000

medal_list <- replicate(B, {
    medal <- sample(runners, 3)
    result <- all(medal == "Jamaica")
})
Pr <- mean(medal_list)
Pr

# Question 2


# Use the information below to answer the following five questions.

# A restaurant manager wants to advertise that his lunch special offers 
# enough choices to eat different meals every day of the year. He 
# doesn't think his current special actually allows that number of choices, 
# but wants to change his special if needed to allow at least 365 choices.

# A meal at the restaurant includes 1 entree, 2 sides, and 1 drink. 
# He currently offers a choice of 1 entree from a list of 6 options, 
# a choice of 2 different sides from a list of 6 options, and a choice of 
# 1 drink from a list of 2 options.

# Write a function that takes a number of entree choices and 
# returns the number of meal combinations possible given that 
# number of entree options, 3 drink choices, and a selection 
# of 2 sides from 6 options.

# - Use sapply() to apply the function to entree option counts 
# ranging from 1 to 12.

# What is the minimum number of entree options required in order 
# to generate more than 365 combinations?

entree_options <- function(N) {
    entrees <- nrow(combinations(N, 1))
    sides <- nrow(combinations(6, 2))
    drinks <- nrow(combinations(3, 1))
    total_options = entrees * sides * drinks
    if (total_options >= 365){
        return(N)
    }
        
    else{
        return(100)
    }
}

N <- seq(1:12)

min_entree_options <- min(sapply(N, entree_options))
min_entree_options

sides_options <- function(N) {
    entrees <- nrow(combinations(6, 1))
    sides <- nrow(combinations(N, 2))
    drinks <- nrow(combinations(3, 1))
    total_options = entrees * sides * drinks
    if (total_options >= 365){
        return(N)
    }
    
    else{
        return(100)
    }
}

n <- seq(2,12)

min_entree_options <- min(sapply(n, sides_options))
min_entree_options



# Case-control studies help determine whether certain exposures are 
# associated with outcomes such as developing cancer. The built-in 
# dataset esoph contains data from a case-control study in France 
# comparing people with esophageal cancer (cases, counted in ncases) 
# to people without esophageal cancer (controls, counted in ncontrols) 
# that are carefully matched on a variety of demographic and medical 
# chracteristics. The study compares alcohol intake in grams per day 
# (alcgp) and tobacco intake in grams per day (tobgp) across cases and 
# controls grouped by age range (agegp).

# Question 3a
# Each row contains one group of the experiment. Each group has a 
# different combination of age, alcohol consumption, and tobacco 
# consumption. The number of cancer cases and number of controls 
# (individuals without cancer) are reported for each group.

rows <- nrow(esoph)
rows


# Question 3b
# How many cases are there?

all_cases <- sum(esoph$ncases)
all_cases


# Question 4a
# How many controls are there?

all_controls <- sum(esoph$ncontrols)
all_controls

# What is the probability that a subject in the highest alcohol consumption 
# group is a cancer case?

max_level <- max(esoph$alcgp)

Pr_alclg <- esoph %>% 
    filter(alcgp %in% max_level) %>%
    select(ncases, ncontrols)

Pr_alclg <- sum(Pr_alclg$ncases) / (sum(Pr_alclg$ncontrols) + sum(Pr_alclg$ncases)) 
Pr_alclg


# Question 4b
# What is the probability that a subject in the lowest alcohol 
# consumption group is a cancer case?

min_level <- min(esoph$alcgp)

Pr_alclg <- esoph %>% 
    filter(alcgp %in% min_level) %>%
    select(ncases, ncontrols)

Pr_alclg <- sum(Pr_alclg$ncases) / (sum(Pr_alclg$ncontrols) + sum(Pr_alclg$ncases)) 
Pr_alclg

# Question 4c
# Given that a person is a case, what is the probability that they 
# smoke 10g or more a day?

min_level <- min(esoph$tobgp)

tobgp_cases <- esoph %>% 
    filter(!tobgp %in% min_level & ncases > 0) %>%
    select(tobgp, ncases, ncontrols)
tobgp_cases <- sum(tobgp_cases$ncases)
Pr_tobgp_cases <- tobgp_cases / sum(esoph$ncases)
Pr_tobgp_cases


# Question 4d
# Given that a person is a control, what is the probability that 
# they smoke 10g or more a day?

min_level <- min(esoph$tobgp)

tobgp_control <- esoph %>% 
    filter(!tobgp %in% min_level & ncontrols > 0) %>%
    select(tobgp, ncases, ncontrols)
tobgp_control <- sum(tobgp_control$ncontrols)
Pr_tobgp_control <- tobgp_control / sum(esoph$ncontrols)
Pr_tobgp_control



# Question 5a
# For cases, what is the probability of being in the highest alcohol group?

max_level <- max(esoph$alcgp)

alcgp_cases <- esoph %>% 
    filter(alcgp %in% max_level & ncases > 0) %>%
    select(alcgp, ncases, ncontrols)
alcgp_cases <- sum(alcgp_cases$ncases)
Pr_alcgp_cases <- alcgp_cases / sum(esoph$ncases)
Pr_alcgp_cases


# Question 5b
# For cases, what is the probability of being in the highest tobacco group?

max_level <- max(esoph$tobgp)

tobgp_cases <- esoph %>% 
    filter(tobgp %in% max_level & ncases > 0) %>%
    select(tobgp, ncases, ncontrols)
tobgp_cases <- sum(tobgp_cases$ncases)
Pr_tobgp_cases <- tobgp_cases / sum(esoph$ncases)
Pr_tobgp_cases


# Question 5c
# For cases, what is the probability of being in the highest alcohol 
# group and the highest tobacco group?


max_tob_level <- max(esoph$tobgp)
max_alc_level <- max(esoph$alcgp)

tobgp_cases <- esoph %>% 
    filter(tobgp %in% max_tob_level & alcgp %in% max_alc_level & ncases > 0) %>%
    select(tobgp, ncases, ncontrols)

tob_alc_cases <- sum(tobgp_cases$ncases)
Pr_tob_alc_cases <- tob_alc_cases / sum(esoph$ncases)
Pr_tob_alc_cases


# Question 5d
# For cases, what is the probability of being in the highest alcohol 
# group or the highest tobacco group?

max_tob_level <- max(esoph$tobgp)
max_alc_level <- max(esoph$alcgp)

tobgp_cases <- esoph %>% 
    filter(tobgp %in% max_tob_level | alcgp %in% max_alc_level) %>%
    filter(ncases > 0) %>%
    select(tobgp, ncases, ncontrols)

tob_alc_cases <- sum(tobgp_cases$ncases)
Pr_tob_alc_cases <- tob_alc_cases / sum(esoph$ncases)
Pr_tob_alc_cases

# Question 6a
# For controls, what is the probability of being in the highest alcohol group?

max_level <- max(esoph$alcgp)

alcgp_control <- esoph %>% 
    filter(alcgp %in% max_level & ncontrols > 0) %>%
    select(alcgp, ncases, ncontrols)
alcgp_control <- sum(alcgp_control$ncontrols)
Pr_alcgp_control <- alcgp_control / sum(esoph$ncontrols)
Pr_alcgp_control


# Question 6b
# How many times more likely are cases than controls to be in the 
# highest alcohol group?

Pr_alcgp_cases / Pr_alcgp_control


# Question 6c
# For controls, what is the probability of being in the highest tobacco group?


max_tob_level <- max(esoph$tobgp)

tobgp_control <- esoph %>% 
    filter(tobgp %in% max_tob_level & ncontrols > 0) %>%
    select(tobgp, ncases, ncontrols)

tob_control <- sum(tobgp_control$ncontrols)
Pr_tob_control <- tob_control / sum(esoph$ncontrols)
Pr_tob_control


# Question 6d
# For controls, what is the probability of being in the highest alcohol group 
# and the highest tobacco group?

max_tob_level <- max(esoph$tobgp)
max_alc_level <- max(esoph$alcgp)

tobgp_control <- esoph %>% 
    filter(tobgp %in% max_tob_level & alcgp %in% max_alc_level) %>%
    filter(ncontrols > 0) %>%
    select(tobgp, ncases, ncontrols)

tob_alc_control <- sum(tobgp_control$ncontrols)
Pr_tob_alc_control <- tob_alc_control / sum(esoph$ncontrols)
Pr_tob_alc_control

# Question 6e
# For controls, what is the probability of being in the highest alcohol group 
# or the highest tobacco group?

max_tob_level <- max(esoph$tobgp)
max_alc_level <- max(esoph$alcgp)

tobgp_control <- esoph %>% 
    filter(tobgp %in% max_tob_level | alcgp %in% max_alc_level) %>%
    filter(ncontrols > 0) %>%
    select(tobgp, ncases, ncontrols)

tob_alc_control <- sum(tobgp_control$ncontrols)
Pr_tob_alc_control <- tob_alc_control / sum(esoph$ncontrols)
Pr_tob_alc_control

# Question 6f
# How many times more likely are cases than controls to be in the highest 
# alcohol group or the highest tobacco group?

Pr_tob_alc_cases / Pr_tob_alc_control
