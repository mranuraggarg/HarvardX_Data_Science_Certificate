Assignment-1
================
Anurag Garg
2022-12-28

## Loading library

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.4.0      ✔ purrr   1.0.0 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.5.0 
    ## ✔ readr   2.1.3      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

## Question 4

You have a data frame of monthly sales and profits in R:

    > head(dat)
    # A tibble: 5 x 3
    Month     Sales     Profit 
    <chr>     <chr>     <chr>  
    January   $128,568  $16,234
    February  $109,523  $12,876
    March     $115,468  $17,920
    April     $122,274  $15,825
    May       $117,921  $15,437
        

Data is stored in `question4_31.csv`

``` r
dat <- read_csv('question4_31.csv', col_names = TRUE)
```

    ## Warning: One or more parsing issues, call `problems()` on your data frame for details,
    ## e.g.:
    ##   dat <- vroom(...)
    ##   problems(dat)

    ## Rows: 5 Columns: 3
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): Month, Sales, Profit
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
head(dat)
```

    ## # A tibble: 5 × 3
    ##   Month    Sales Profit     
    ##   <chr>    <chr> <chr>      
    ## 1 January  $128  568,$16,234
    ## 2 February $109  523,$12,876
    ## 3 March    $115  468,$17,920
    ## 4 April    $122  274,$15,825
    ## 5 May      $117  921,$15,437

Which of the following commands could convert the sales and profits
columns to numeric? Select all that apply.

``` r
dat %>% mutate_at(2:3, parse_number)
```

    ## # A tibble: 5 × 3
    ##   Month    Sales Profit
    ##   <chr>    <dbl>  <dbl>
    ## 1 January    128    568
    ## 2 February   109    523
    ## 3 March      115    468
    ## 4 April      122    274
    ## 5 May        117    921

``` r
dat %>% mutate_at(2:3, funs(str_replace_all(., c("\\$|,"), ""))) %>% 
    mutate_at(2:3, as.numeric)
```

    ## Warning: `funs()` was deprecated in dplyr 0.8.0.
    ## ℹ Please use a list of either functions or lambdas:
    ## 
    ## # Simple named list: list(mean = mean, median = median)
    ## 
    ## # Auto named with `tibble::lst()`: tibble::lst(mean, median)
    ## 
    ## # Using lambdas list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))

    ## # A tibble: 5 × 3
    ##   Month    Sales   Profit
    ##   <chr>    <dbl>    <dbl>
    ## 1 January    128 56816234
    ## 2 February   109 52312876
    ## 3 March      115 46817920
    ## 4 April      122 27415825
    ## 5 May        117 92115437
