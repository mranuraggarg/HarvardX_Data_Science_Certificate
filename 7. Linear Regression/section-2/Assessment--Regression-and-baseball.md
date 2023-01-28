Assessment: Regression and baseball
================
Anurag Garg
2023-01-28

## Introduction

Use the Teams data frame from the Lahman package. Fit a multivariate
linear regression model to obtain the effects of BB and HR on Runs (R)
in 1971. Use the tidy() function in the broom package to obtain the
results in a data frame.

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
library(broom)
```

## Q9a

What is the estimate for the effect of BB on runs?

``` r
fit <- Teams %>%
    filter(yearID == 1971) %>%
    lm(R ~ BB + HR, data=.) %>%
    tidy()
fit %>% filter(term == "BB") %>% pull(estimate)
```

    ## [1] 0.413974

## Q9a

What is the estimate for the effect of HR on runs?

``` r
fit %>% filter(term == "HR") %>% pull(estimate)
```

    ## [1] 1.295311

## Q9b

Interpret the p-values for the estimates using a cutoff of 0.05 and
considering the year 1971 as a sample to make inference on the
population of all baseball games across years.

Which of the following is the correct interpretation?

``` r
fit
```

    ## # A tibble: 3 × 5
    ##   term        estimate std.error statistic p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>   <dbl>
    ## 1 (Intercept)  257.      112.         2.31 0.0314 
    ## 2 BB             0.414     0.210      1.97 0.0625 
    ## 3 HR             1.30      0.431      3.01 0.00673

## Q10

Repeat the above exercise to find the effects of BB and HR on runs (R)
for every year from 1961 to 2018 using do() and the broom package.

Make a scatterplot of the estimate for the effect of BB on runs over
time and add a trend line with confidence intervals.

Fill in the blank to complete the statement:

``` r
res <- Teams %>% 
    filter(yearID %in% 1961:2018) %>% 
    group_by(yearID) %>% 
    do(tidy(lm(R~BB+HR,data=.))) %>% 
    ungroup()
res %>% 
    filter(term=="BB") %>% 
    ggplot(aes(yearID, estimate)) + 
    geom_point() + 
    geom_smooth(method ="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Assessment--Regression-and-baseball_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## Q11

Fit a linear model on the results from Question 10 to determine the
effect of year on the impact of BB. That is, determine how the estimated
coefficients of BB from the models in Question 10 can be predicted by
the year (recall that we grouped the data by year before fitting the
models, so we have different estimated coefficients for each year).

For each additional year, by what value does the impact of BB on runs
change?

``` r
res %>% 
    filter(term=="BB") %>% 
    lm(estimate~yearID,data =.) %>% 
    tidy() %>% 
    filter(term=="yearID") %>% 
    pull(estimate)
```

    ## [1] 0.003550319

What is the p-value for this effect?

``` r
res %>% filter(term=="BB") %>% lm(estimate~yearID,data =.) %>% 
    tidy() %>% 
    filter(term=="yearID") %>% 
    pull(p.value)
```

    ## [1] 0.008066173

# Assessment: Linear Models (Verified Learners only)

Game attendance in baseball varies partly as a function of how well a
team is playing.

Load the Lahman library. The Teams data frame contains an attendance
column. This is the total attendance for the season. To calculate
average attendance, divide by the number of games played, as follows:

``` r
library(tidyverse)
library(broom)
library(Lahman)
Teams_small <- Teams %>% 
    filter(yearID %in% 1961:2001) %>% 
    mutate(avg_attendance = attendance/G)
```

## Q1

Use runs (R) per game to predict average attendance.

For every 1 run scored per game, average attendance increases by how
much?

``` r
Teams_small %>% 
    mutate(avg_R=R/G, avg_HR=HR/G) %>%
    do(tidy(lm(avg_attendance~avg_R, data=.),conf.int = TRUE)) %>%
    filter(term == 'avg_R') %>%
    pull(estimate)
```

    ## [1] 4117.029

For every 1 home run hit per game, average attendance increases by how
much?

``` r
Teams_small %>% 
    mutate(avg_R=R/G, avg_HR=HR/G) %>%
    do(tidy(lm(avg_attendance~avg_HR, data=.),conf.int = TRUE)) %>%
    filter(term == 'avg_HR') %>%
    pull(estimate)
```

    ## [1] 8113.172

## Q1b

Use number of wins to predict average attendance; do not normalize for
number of games.

For every game won in a season, how much does average attendance
increase?

``` r
Teams_small %>%
    do(tidy(lm(avg_attendance ~ W, data=.))) %>%
    filter(term == "W") %>%
    pull(estimate)
```

    ## [1] 121.0901

Suppose a team won zero games in a season.

Predict the average attendance.

``` r
Teams_small %>%
    do(tidy(lm(avg_attendance ~ W, data=.))) %>%
    filter(term == "(Intercept)") %>%
    pull(estimate)
```

    ## [1] 1129.221

## Q1c

Use year to predict average attendance.

How much does average attendance increase each year?

``` r
Teams_small %>%
    do(tidy(lm(avg_attendance ~ yearID, data=.))) %>%
    filter(term == "yearID") %>%
    pull(estimate)
```

    ## [1] 244.4826

## Q2

Game wins, runs per game and home runs per game are positively
correlated with attendance. We saw in the course material that runs per
game and home runs per game are correlated with each other. Are wins and
runs per game or wins and home runs per game correlated? Use the
Teams_small data once again.

What is the correlation coefficient for runs per game and wins?

``` r
Teams_small %>%
    summarize(r = cor(W,R/G)) %>%
    pull
```

    ## [1] 0.4116491

What is the correlation coefficient for home runs per game and wins?

``` r
Teams_small %>%
    summarize(r = cor(W,HR/G)) %>%
    pull
```

    ## [1] 0.2744313

Stratify Teams_small by wins: divide number of wins by 10 and then round
to the nearest integer. Filter to keep only strata 5 through 10. (The
other strata have fewer than 20 data points, too few for our analyses).

Use the stratified dataset to answer this three-part question.

``` r
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(W_strata=round(W/10, 0), avg_attendance = attendance/G)
```

## 3a

How many observations are in the 8 win strata? (Note that due to
division and rounding, these teams have 75-85 wins.)

``` r
dat <- Teams_small %>% filter(W_strata == 8)
nrow(dat)
```

    ## [1] 338

## 3b

Calculate the slope of the regression line predicting average attendance
given runs per game for each of the win strata.

Which win stratum has the largest regression line slope?

``` r
Teams_small %>%
    mutate(runs_per_game = R/G) %>%
    group_by(W_strata) %>%
    do(tidy(lm(avg_attendance ~ runs_per_game, data=.))) %>%
    filter(term=="runs_per_game") %>%
    select(W_strata, estimate)
```

    ## # A tibble: 9 × 2
    ## # Groups:   W_strata [9]
    ##   W_strata estimate
    ##      <dbl>    <dbl>
    ## 1        4   -1318.
    ## 2        5    4362.
    ## 3        6    4343.
    ## 4        7    3888.
    ## 5        8    3128.
    ## 6        9    3701.
    ## 7       10    3107.
    ## 8       11    7175.
    ## 9       12      NA

Calculate the slope of the regression line predicting average attendance
given HR per game for each of the win strata.

Which win stratum has the largest regression line slope?

``` r
Teams_small %>%
    mutate(homeruns_per_game = HR/G) %>%
    group_by(W_strata) %>%
    do(tidy(lm(avg_attendance ~ homeruns_per_game, data=.))) %>%
    filter(term=="homeruns_per_game") %>%
    select(W_strata, estimate)
```

    ## # A tibble: 9 × 2
    ## # Groups:   W_strata [9]
    ##   W_strata estimate
    ##      <dbl>    <dbl>
    ## 1        4    2065.
    ## 2        5   10192.
    ## 3        6    7032.
    ## 4        7    8931.
    ## 5        8    6301.
    ## 6        9    5863.
    ## 7       10    4917.
    ## 8       11    1761.
    ## 9       12      NA

``` r
Teams_small %>%
    mutate(homeruns_per_game = HR/G, runs_per_game = R/G) %>%
    group_by(W_strata) %>%
    do(tidy(lm(avg_attendance ~ homeruns_per_game + runs_per_game, data=.)))
```

    ## # A tibble: 27 × 6
    ## # Groups:   W_strata [9]
    ##    W_strata term              estimate std.error statistic  p.value
    ##       <dbl> <chr>                <dbl>     <dbl>     <dbl>    <dbl>
    ##  1        4 (Intercept)        15761.      3454.    4.56   0.0103  
    ##  2        4 homeruns_per_game   5249.      1519.    3.45   0.0259  
    ##  3        4 runs_per_game      -3748.      1122.   -3.34   0.0288  
    ##  4        5 (Intercept)        -7147.      5104.   -1.40   0.171   
    ##  5        5 homeruns_per_game    849.      5279.    0.161  0.873   
    ##  6        5 runs_per_game       4127.      1844.    2.24   0.0323  
    ##  7        6 (Intercept)        -8835.      3705.   -2.38   0.0189  
    ##  8        6 homeruns_per_game    -87.4     3009.   -0.0291 0.977   
    ##  9        6 runs_per_game       4365.      1182.    3.69   0.000354
    ## 10        7 (Intercept)        -4427.      2191.   -2.02   0.0447  
    ## # … with 17 more rows

## Q4

Fit a multivariate regression determining the effects of runs per game,
home runs per game, wins, and year on average attendance. Use the
original Teams_small wins column, not the win strata from question 3.

What is the estimate of the effect of runs per game on average
attendance?

``` r
Teams_small %>%
    mutate(homeruns_per_game = HR/G, runs_per_game = R/G) %>%
    do(tidy(lm(avg_attendance ~ runs_per_game + W + homeruns_per_game + yearID, data=.)))
```

    ## # A tibble: 5 × 5
    ##   term              estimate std.error statistic  p.value
    ##   <chr>                <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)       -456674.  21815.     -20.9   3.00e-81
    ## 2 runs_per_game         322.    331.       0.972 3.31e- 1
    ## 3 W                     117.      9.88    11.8   2.79e-30
    ## 4 homeruns_per_game    1798.    690.       2.61  9.24e- 3
    ## 5 yearID                230.     11.2     20.6   7.10e-79

## Q5

Use the multivariate regression model from Question 4. Suppose a team
averaged 5 runs per game, 1.2 home runs per game, and won 80 games in a
season. Use the predict() function to generate predictions for this
team.

What would this team’s average attendance be in 2002?

``` r
Teams_small <- Teams %>% 
    filter(yearID %in% 1961:2001) %>% 
    mutate(avg_attendance = attendance/G, avg_R=R/G, avg_HR=HR/G)

model <- Teams_small %>% mutate(R_per_game = R/G, HR_per_game = HR/G) %>% 
  lm(avg_attendance ~ R_per_game + HR_per_game + W + yearID, data = .)

predict(model, data.frame(R_per_game = 5, HR_per_game = 1.2, W = 80, yearID = 2002))
```

    ##        1 
    ## 16149.29

What would this team’s average attendance be in 1960?

``` r
predict(model, data.frame(R_per_game = 5, HR_per_game = 1.2, W = 80, yearID = 1960))
```

    ##        1 
    ## 6504.751

## Q6

Use your model from Question 4 to predict average attendance for teams
in 2002 in the original Teams data frame.

What is the correlation between the predicted attendance and actual
attendance?

``` r
newdata <- Teams %>%
  filter(yearID == 2002) %>%
  mutate(avg_attendance = attendance/G,
         R_per_game = R/G,
         HR_per_game = HR/G)
preds <- predict(model, newdata)
cor(preds, newdata$avg_attendance)
```

    ## [1] 0.5191942
