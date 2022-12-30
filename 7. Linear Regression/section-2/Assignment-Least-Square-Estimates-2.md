Assignment: Least Square Estimates Part 2
================
Anurag Garg
2022-12-30

In Questions 7 and 8, you’ll look again at female heights from
GaltonFamilies.

Define female_heights, a set of mother and daughter heights sampled from
GaltonFamilies, as follows:

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
set.seed(1989) #if you are using R 3.5 or earlier
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
```

    ## Warning in set.seed(1989, sample.kind = "Rounding"): non-uniform 'Rounding'
    ## sampler used

``` r
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
    filter(gender == "female") %>%     
    group_by(family) %>%     
    sample_n(1) %>%     
    ungroup() %>%     
    select(mother, childHeight) %>%     
    rename(daughter = childHeight)
```

## Question 7

Fit a linear regression model predicting the mothers’ heights using
daughters’ heights.

What is the slope of the model? What the intercept of the model?

``` r
fit <- lm(mother ~ daughter, data = female_heights)
fit
```

    ## 
    ## Call:
    ## lm(formula = mother ~ daughter, data = female_heights)
    ## 
    ## Coefficients:
    ## (Intercept)     daughter  
    ##       44.18         0.31

## Question 8

Predict mothers’ heights using the model from Question 7 and the
predict() function. What is the predicted height of the first mother in
the dataset?

``` r
Y_hat <- predict(fit)
Y_hat[[1]][1]
```

    ## [1] 65.6

What is the actual height of the first mother in the dataset?

``` r
female_heights$mother[[1]]
```

    ## [1] 67

We have shown how BB and singles have similar predictive power for
scoring runs. Another way to compare the usefulness of these baseball
metrics is by assessing how stable they are across the years. Because we
have to pick players based on their previous performances, we will
prefer metrics that are more stable. In these exercises, we will compare
the stability of singles and BBs.

Before we get started, we want to generate two tables: one for 2002 and
another for the average of 1999-2001 seasons. We want to define per
plate appearance statistics, keeping only players with more than 100
plate appearances. Here is how we create the 2002 table:

``` r
library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
    mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
    filter(pa >= 100) %>%
    select(playerID, singles, bb)
```

## Question 9

Now compute a similar table but with rates computed over 1999-2001. Keep
only rows from 1999-2001 where players have 100 or more plate
appearances, calculate each player’s single rate and BB rate per stint
(where each row is one stint - a player can have multiple stints within
a season), then calculate the average single rate (mean_singles) and
average BB rate (mean_bb) per player over the three year period.

``` r
bat_99_01 <- Batting %>% filter(yearID <= 2001 & yearID >=1999) %>%
    mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
    filter(pa >= 100) %>%
    group_by(playerID) %>%
    summarise(mean_singles = mean(singles), mean_bb = mean(bb))
head(bat_99_01)
```

    ## # A tibble: 6 × 3
    ##   playerID  mean_singles mean_bb
    ##   <chr>            <dbl>   <dbl>
    ## 1 abbotje01        0.169  0.0890
    ## 2 abbotku01        0.143  0.0674
    ## 3 abernbr01        0.178  0.0816
    ## 4 abreubo01        0.153  0.156 
    ## 5 agbaybe01        0.162  0.115 
    ## 6 alexama02        0.166  0.0581

How many players had a single rate mean_singles of greater than 0.2 per
plate appearance over 1999-2001?

``` r
bat_99_01 %>% 
    filter(mean_singles > 0.2) %>%
    nrow
```

    ## [1] 46

How many players had a BB rate mean_bb of greater than 0.2 per plate
appearance over 1999-2001?

``` r
bat_99_01 %>% 
    filter(mean_bb > 0.2) %>%
    nrow
```

    ## [1] 3

## Question 10

Use inner_join() to combine the bat_02 table with the table of 1999-2001
rate averages you created in the previous question.

``` r
bat <- inner_join(bat_99_01, bat_02, by = "playerID")
head(bat)
```

    ## # A tibble: 6 × 5
    ##   playerID  mean_singles mean_bb singles     bb
    ##   <chr>            <dbl>   <dbl>   <dbl>  <dbl>
    ## 1 abernbr01        0.178  0.0816   0.180 0.0512
    ## 2 abreubo01        0.153  0.156    0.148 0.154 
    ## 3 agbaybe01        0.162  0.115    0.118 0.0787
    ## 4 alfoned01        0.161  0.123    0.197 0.112 
    ## 5 alicelu01        0.168  0.100    0.160 0.119 
    ## 6 alomaro01        0.186  0.122    0.182 0.0881

What is the correlation between 2002 singles rates and 1999-2001 average
singles rates?

``` r
cor(bat$mean_singles, bat$singles)
```

    ## [1] 0.551

What is the correlation between 2002 BB rates and 1999-2001 average BB
rates?

``` r
cor(bat$mean_bb, bat$bb)
```

    ## [1] 0.717

## Question 11

Make scatterplots of mean_singles versus singles and mean_bb versus bb.

``` r
bat %>% 
    ggplot(aes(mean_singles, singles)) +
    geom_point(alpha=0.5) +
    ggtitle("Average singles from 1999-2001 vs singles in 2002")
```

![](Assignment-Least-Square-Estimates-2_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
bat %>% 
    ggplot(aes(mean_bb, bb)) +
    geom_point(alpha=0.5) +
    ggtitle("Average BB from 1999-2001 vs BB in 2002")
```

![](Assignment-Least-Square-Estimates-2_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

## Question 12

Fit a linear model to predict 2002 singles given 1999-2001 mean_singles.
What is the coefficient of mean_singles, the slope of the fit?

``` r
fit <- lm(singles ~ mean_singles, data = bat)
fit
```

    ## 
    ## Call:
    ## lm(formula = singles ~ mean_singles, data = bat)
    ## 
    ## Coefficients:
    ##  (Intercept)  mean_singles  
    ##       0.0621        0.5881

Fit a linear model to predict 2002 bb given 1999-2001 mean_bb. What is
the coefficient of mean_bb, the slope of the fit?

``` r
fit <- lm(bb ~ mean_bb, data = bat)
fit
```

    ## 
    ## Call:
    ## lm(formula = bb ~ mean_bb, data = bat)
    ## 
    ## Coefficients:
    ## (Intercept)      mean_bb  
    ##      0.0155       0.8290
