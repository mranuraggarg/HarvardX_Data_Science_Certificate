Assessment: Advanced dplyr, part 1
================
Anurag Garg
2022-12-30

## Question 5

You want to take the tibble dat, which we used in the video on the
advanced dplyr, and run the linear model R \~ BB for each strata of HR.
Then you want to add three new columns to your grouped tibble: the
coefficient, standard error, and p-value for the BB term in the model.

You’ve already written the function get_slope(), shown below.

``` r
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
get_slope <- function(data) {
  fit <- lm(R ~ BB, data = data)
  sum.fit <- summary(fit)

  data.frame(slope = sum.fit$coefficients[2, "Estimate"], 
             se = sum.fit$coefficients[2, "Std. Error"],
             pvalue = sum.fit$coefficients[2, "Pr(>|t|)"])
}

# Reminder dat is defined as
library(Lahman)
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1), 
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR<=1.2)
```

What additional code could you write to accomplish your goal?

``` r
dat %>% 
  group_by(HR) %>% 
  summarize(get_slope(across()))
```

    ## # A tibble: 9 × 4
    ##      HR slope     se   pvalue
    ##   <dbl> <dbl>  <dbl>    <dbl>
    ## 1   0.4 0.734 0.208  1.54e- 3
    ## 2   0.5 0.566 0.110  3.02e- 6
    ## 3   0.6 0.412 0.0974 4.80e- 5
    ## 4   0.7 0.285 0.0705 7.93e- 5
    ## 5   0.8 0.365 0.0653 9.13e- 8
    ## 6   0.9 0.261 0.0751 6.85e- 4
    ## 7   1   0.512 0.0751 3.28e-10
    ## 8   1.1 0.454 0.0855 1.03e- 6
    ## 9   1.2 0.440 0.0801 1.07e- 6

## Question 7

You want to know whether the relationship between home runs and runs per
game varies by baseball league. You create the following dataset:

``` r
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = HR/G,
         R = R/G) %>%
  select(lgID, HR, BB, R)
```

What code would help you quickly answer this question?

``` r
library(broom)
dat %>% 
  group_by(lgID) %>% 
  summarize(tidy(lm(R ~ HR, data = across()), conf.int = T)) %>% 
  filter(term == "HR") 
```

    ## `summarise()` has grouped output by 'lgID'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 2 × 8
    ## # Groups:   lgID [2]
    ##   lgID  term  estimate std.error statistic  p.value conf.low conf.high
    ##   <fct> <chr>    <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl>
    ## 1 AL    HR        1.90    0.0734      25.9 1.29e-95     1.75      2.04
    ## 2 NL    HR        1.76    0.0671      26.2 1.16e-95     1.62      1.89
