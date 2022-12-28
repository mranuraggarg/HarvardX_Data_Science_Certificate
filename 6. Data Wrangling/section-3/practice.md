Practice
================
Anurag Garg
2022-12-28

## Load libraries

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.4.0      ✔ purrr   1.0.0 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.5.0 
    ## ✔ readr   2.1.3      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## 
    ## Attaching package: 'rvest'
    ## 
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

## Section 3.1

read in raw murders data from Wikipedia

``` r
url <- "https://en.wikipedia.org/w/index.php?title=Gun_violence_in_the_United_States_by_state&direction=prev&oldid=810166167"
murders_raw <- read_html(url) %>% 
  html_nodes("table") %>% 
  html_table() %>%
  .[[1]] %>%
  setNames(c("state", "population", "total", "murder_rate"))
```

inspect data and column classes

``` r
head(murders_raw)
```

    ## # A tibble: 6 × 4
    ##   state      population total murder_rate
    ##   <chr>      <chr>      <chr>       <dbl>
    ## 1 Alabama    4,853,875  348           7.2
    ## 2 Alaska     737,709    59            8  
    ## 3 Arizona    6,817,565  309           4.5
    ## 4 Arkansas   2,977,853  181           6.1
    ## 5 California 38,993,940 1,861         4.8
    ## 6 Colorado   5,448,819  176           3.2

``` r
print(class(murders_raw$population))
```

    ## [1] "character"

``` r
print(class(murders_raw$total))
```

    ## [1] "character"

## String Operations

``` r
s <- "Hello!"    # double quotes define a string
s <- 'Hello!'    # single quotes define a string
```

Following will give the error

    s <- `Hello`    # backquotes do not work and gives and error

    s <- "10""    # error - unclosed quotes
    s <- '10"'    # correct

cat shows what the string actually looks like inside R

``` r
cat(s)
```

    ## Hello!

``` r
s <- "5'"
cat(s)
```

    ## 5'

If we need to include both the quotes, following emplimentations will
give the eror:

    s <- '5'10"'    # error
    s <- "5'10""    # error

to include both single and double quotes in string, escape with  

``` r
s <- '5\'10"'    # correct
cat(s)
```

    ## 5'10"

``` r
s <- "5'10\""    # correct
cat(s)
```

    ## 5'10"

## `stringr` Package

murders_raw defined in web scraping video direct conversion to numeric
fails because of commas

``` r
murders_raw$population[1:3]
```

    ## [1] "4,853,875" "737,709"   "6,817,565"

``` r
as.numeric(murders_raw$population[1:3])
```

    ## Warning: NAs introduced by coercion

    ## [1] NA NA NA

``` r
library(tidyverse)    # includes stringr
```

## String Coersing

``` r
# murders_raw was defined in the web scraping section

# detect whether there are commas
commas <- function(x) any(str_detect(x, ","))
murders_raw %>% summarize_all(funs(commas))
```

    ## Warning: `funs()` was deprecated in dplyr 0.8.0.
    ## ℹ Please use a list of either functions or lambdas:
    ## 
    ## # Simple named list: list(mean = mean, median = median)
    ## 
    ## # Auto named with `tibble::lst()`: tibble::lst(mean, median)
    ## 
    ## # Using lambdas list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))

    ## # A tibble: 1 × 4
    ##   state population total murder_rate
    ##   <lgl> <lgl>      <lgl> <lgl>      
    ## 1 FALSE TRUE       TRUE  FALSE

``` r
# replace commas with the empty string and convert to numeric
test_1 <- str_replace_all(murders_raw$population, ",", "")
test_1 <- as.numeric(test_1)
```

Same thing can be acheived through mutate function also

``` r
# parse_number also removes commas and converts to numeric
test_2 <- parse_number(murders_raw$population)
identical(test_1, test_2)
```

    ## [1] TRUE

``` r
murders_new <- murders_raw %>% mutate_at(2:3, parse_number)
murders_new %>% head
```

    ## # A tibble: 6 × 4
    ##   state      population total murder_rate
    ##   <chr>           <dbl> <dbl>       <dbl>
    ## 1 Alabama       4853875   348         7.2
    ## 2 Alaska         737709    59         8  
    ## 3 Arizona       6817565   309         4.5
    ## 4 Arkansas      2977853   181         6.1
    ## 5 California   38993940  1861         4.8
    ## 6 Colorado      5448819   176         3.2
