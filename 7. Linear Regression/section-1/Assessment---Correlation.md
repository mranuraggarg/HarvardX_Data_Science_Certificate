Assignment - Correlation
================
Anurag Garg
2022-12-30

## Question 7

Load the Lahman library. Filter the Teams data frame to include years
from 1961 to 2001.

``` r
library(Lahman)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.4.0      ✔ purrr   1.0.0 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.5.0 
    ## ✔ readr   2.1.3      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(dslabs)
library(ggplot2)
ds_theme_set()
```

What is the correlation coefficient between number of runs per game and
number of at bats per game?

``` r
corr <- Teams %>%
    filter(yearID >= 1961 & yearID <= 2001) %>%
    mutate(runs_per_game = R/G, at_bat_per_game = AB/G) %>%
    select(runs_per_game, at_bat_per_game) %>%
    summarise(corr = cor(runs_per_game, at_bat_per_game)) %>%
    pull(corr)
corr
```

    ## [1] 0.6580976

## Question 8

Use the filtered Teams data frame from Question 7. What is the
correlation coefficient between win rate (number of wins per game) and
number of errors per game?

``` r
corr <- Teams %>%
    filter(yearID >= 1961 & yearID <= 2001) %>%
    mutate(win_rate = W/G, errors_per_game = E/G) %>%
    select(win_rate, errors_per_game) %>%
    summarise(corr = cor(win_rate, errors_per_game))%>%
    pull(corr)
corr
```

    ## [1] -0.3396947

## Question 9

Use the filtered Teams data frame from Question 7. What is the
correlation coefficient between doubles (X2B) per game and triples (X3B)
per game?

``` r
corr <- Teams %>%
    filter(yearID >= 1961 & yearID <= 2001) %>%
    mutate(doubles_per_game = X2B/G, tripples_per_game = X3B/G) %>%
    select(doubles_per_game, tripples_per_game) %>%
    summarise(corr = cor(doubles_per_game, tripples_per_game))%>%
    pull(corr)
corr
```

    ## [1] -0.01157404
