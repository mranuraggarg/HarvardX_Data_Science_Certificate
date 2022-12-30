Assessment: Advanced dplyr, part 2
================
Anurag Garg
2022-12-31

We have investigated the relationship between fathers’ heights and sons’
heights. But what about other parent-child relationships? Does one
parent’s height have a stronger association with child height? How does
the child’s gender affect this relationship in heights? Are any
differences that we observe statistically significant?

The galton dataset is a sample of one male and one female child from
each family in the GaltonFamilies dataset. The pair column denotes
whether the pair is father and daughter, father and son, mother and
daughter, or mother and son.

Create the galton dataset using the code below:

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
library(broom)
library(HistData)
data("GaltonFamilies")
# set.seed(1) # if you are using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
```

    ## Warning in set.seed(1, sample.kind = "Rounding"): non-uniform 'Rounding' sampler
    ## used

``` r
galton <- GaltonFamilies %>%
    group_by(family, gender) %>%
    sample_n(1) %>%
    ungroup() %>% 
    gather(parent, parentHeight, father:mother) %>%
    mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
    unite(pair, c("parent", "child"))

head(galton)
```

    ## # A tibble: 6 × 8
    ##   family midparentHeight children childNum gender childHeight pair       paren…¹
    ##   <fct>            <dbl>    <int>    <int> <fct>        <dbl> <chr>        <dbl>
    ## 1 001               75.4        4        2 female        69.2 father_da…    78.5
    ## 2 001               75.4        4        1 male          73.2 father_son    78.5
    ## 3 002               73.7        4        4 female        65.5 father_da…    75.5
    ## 4 002               73.7        4        2 male          72.5 father_son    75.5
    ## 5 003               72.1        2        2 female        68   father_da…    75  
    ## 6 003               72.1        2        1 male          71   father_son    75  
    ## # … with abbreviated variable name ¹​parentHeight

## Question 8

Group by pair and summarize the number of observations in each group.
How many father-daughter pairs are in the dataset?

``` r
galton %>%
    group_by(pair) %>%
    summarise(count = length(parentHeight)) %>%
    filter(pair == "father_daughter") %>%
    pull(count)
```

    ## [1] 176

How many mother-son pairs are in the dataset?

``` r
galton %>%
    group_by(pair) %>%
    summarise(count = length(parentHeight)) %>%
    filter(pair == "mother_son") %>%
    pull(count)
```

    ## [1] 179

## Question 9

0.0/2.0 points (graded)

Calculate the correlation coefficients for fathers and daughters,
fathers and sons, mothers and daughters and mothers and sons. For every
1-inch increase in mother’s height, how many inches does the typical
son’s height increase?

``` r
galton %>%
    select(pair, childHeight, parentHeight) %>%
    group_by(pair) %>%
    summarise(corr = cor(childHeight, parentHeight))
```

    ## # A tibble: 4 × 2
    ##   pair             corr
    ##   <chr>           <dbl>
    ## 1 father_daughter 0.401
    ## 2 father_son      0.430
    ## 3 mother_daughter 0.383
    ## 4 mother_son      0.343

Question 10 has two parts. The information here applies to both parts.

Use lm() and the broom package to fit regression lines for each
parent-child pair type. Compute the least squares estimates, standard
errors, confidence intervals and p-values for the parentHeight
coefficient for each pair.

## Question 10a

What is the estimate of the father-daughter coefficient?

``` r
galton %>%
    group_by(pair) %>%
    summarise(tidy(lm(childHeight ~ parentHeight), conf.int = TRUE))
```

    ## `summarise()` has grouped output by 'pair'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 8 × 8
    ## # Groups:   pair [4]
    ##   pair            term         estimate std.e…¹ stati…²  p.value conf.…³ conf.…⁴
    ##   <chr>           <chr>           <dbl>   <dbl>   <dbl>    <dbl>   <dbl>   <dbl>
    ## 1 father_daughter (Intercept)    40.1    4.16      9.65 6.50e-18  31.9    48.3  
    ## 2 father_daughter parentHeight    0.345  0.0599    5.77 3.56e- 8   0.227   0.464
    ## 3 father_son      (Intercept)    38.6    4.84      7.98 1.81e-13  29.1    48.2  
    ## 4 father_son      parentHeight    0.443  0.0700    6.33 1.94e- 9   0.305   0.581
    ## 5 mother_daughter (Intercept)    38.9    4.62      8.41 1.46e-14  29.7    48.0  
    ## 6 mother_daughter parentHeight    0.394  0.0720    5.47 1.56e- 7   0.252   0.536
    ## 7 mother_son      (Intercept)    44.9    5.02      8.94 4.96e-16  35.0    54.8  
    ## 8 mother_son      parentHeight    0.381  0.0784    4.86 2.59e- 6   0.226   0.535
    ## # … with abbreviated variable names ¹​std.error, ²​statistic, ³​conf.low,
    ## #   ⁴​conf.high
