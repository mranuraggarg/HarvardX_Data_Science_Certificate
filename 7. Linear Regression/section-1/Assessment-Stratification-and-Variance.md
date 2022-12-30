Assessment: Stratification and Variance
================
Anurag Garg
2022-12-30

In the second part of this assessment, you’ll analyze a set of mother
and daughter heights, also from GaltonFamilies.

Define female_heights, a set of mother and daughter heights sampled from
GaltonFamilies, as follows:

``` r
set.seed(1989) #if you are using R 3.5 or earlier
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
```

    ## Warning in set.seed(1989, sample.kind = "Rounding"): non-uniform 'Rounding'
    ## sampler used

``` r
library(HistData)
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
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
    filter(gender == "female") %>%     
    group_by(family) %>%     
    sample_n(1) %>%     
    ungroup() %>%     
    select(mother, childHeight) %>%     
    rename(daughter = childHeight)
```

## Question 8

Calculate the mean and standard deviation of mothers’ heights, the mean
and standard deviation of daughters’ heights, and the correlaton
coefficient between mother and daughter heights. \### Question 8.1 Mean
of mothers’ heights

``` r
mu_m <- female_heights %>%
    select(mother) %>%
    summarise(mom_mean = mean(mother)) %>%
    pull(mom_mean)
mu_m
```

    ## [1] 64.125

### Question 8.2

Standard deviation of mothers’ heights

``` r
sigma_m <- female_heights %>%
    summarise(mom_std = sd(mother)) %>%
    pull(mom_std)
sigma_m
```

    ## [1] 2.289292

### Question 8.3

Mean of daughters’ heights

``` r
mu_d <- female_heights %>%
    summarise(daughter_mean = mean(daughter)) %>%
    pull(daughter_mean)
mu_d
```

    ## [1] 64.28011

### Question 8.4

Standard deviation of daughters’ heights

``` r
sigma_d <- female_heights %>%
    summarise(daughter_std = sd(daughter)) %>%
    pull(daughter_std)
sigma_d
```

    ## [1] 2.39416

### Question 8.5

Correlation coefficient

``` r
corr <- female_heights %>%
    summarise(corr = cor(mother, daughter)) %>%
    pull(corr)
corr
```

    ## [1] 0.3245199

## Question 9

Calculate the slope and intercept of the regression line predicting
daughters’ heights given mothers’ heights. Given an increase in mother’s
height by 1 inch, how many inches is the daughter’s height expected to
change?

### Question 9.1

Slope of regression line predicting daughters’ height from mothers’
heights

``` r
m_d <- corr * sigma_d / sigma_m
m_d
```

    ## [1] 0.3393856

### Question 9.2

Intercept of regression line predicting daughters’ height from mothers’
heights

``` r
b_d <- mu_d - m_d * mu_m
b_d
```

    ## [1] 42.51701

### Question 9.3

Change in daughter’s height in inches given a 1 inch increase in the
mother’s height

``` r
m_d <- corr * sigma_d / sigma_m
m_d
```

    ## [1] 0.3393856

### Question 9.4

You have used 0 of 10 attempts Some problems have options such as save,
reset, hints, or show answer. These options follow the Submit button.

## Question 10

What percent of the variability in daughter heights is explained by the
mother’s height?

``` r
corr^2 * 100
```

    ## [1] 10.53132

## Question 11

A mother has a height of 60 inches.

Using the regression formula, what is the conditional expected value of
her daughter’s height given the mother’s height?

``` r
height_m <- 60
hight_d <- b_d + m_d * height_m
hight_d
```

    ## [1] 62.88015
