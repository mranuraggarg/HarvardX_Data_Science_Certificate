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

## Section 3.2 Case Study 2: Reported Heights

load raw heights data and inspect

``` r
library(dslabs)
data(reported_heights)
class(reported_heights$height)
```

    ## [1] "character"

# convert to numeric, inspect, count NAs

``` r
x <- as.numeric(reported_heights$height)
```

    ## Warning: NAs introduced by coercion

``` r
head(x)
```

    ## [1] 75 70 68 74 61 65

``` r
sum(is.na(x))
```

    ## [1] 81

keep only entries that result in NAs

``` r
reported_heights %>% mutate(new_height = as.numeric(height)) %>%
  filter(is.na(new_height)) %>% 
  head(n=10)
```

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ##             time_stamp    sex                 height new_height
    ## 1  2014-09-02 15:16:28   Male                  5' 4"         NA
    ## 2  2014-09-02 15:16:37 Female                  165cm         NA
    ## 3  2014-09-02 15:16:52   Male                    5'7         NA
    ## 4  2014-09-02 15:16:56   Male                  >9000         NA
    ## 5  2014-09-02 15:16:56   Male                   5'7"         NA
    ## 6  2014-09-02 15:17:09 Female                   5'3"         NA
    ## 7  2014-09-02 15:18:00   Male 5 feet and 8.11 inches         NA
    ## 8  2014-09-02 15:19:48   Male                   5'11         NA
    ## 9  2014-09-04 00:46:45   Male                  5'9''         NA
    ## 10 2014-09-04 10:29:44   Male                 5'10''         NA

# calculate cutoffs that cover 99.999% of human population

``` r
alpha <- 1/10^6
qnorm(1-alpha/2, 69.1, 2.9)
```

    ## [1] 83.28575

``` r
qnorm(alpha/2, 63.7, 2.7)
```

    ## [1] 50.49258

keep only entries that either result in NAs or are outside the plausible
range of heights

``` r
not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
}
```

# number of problematic entries

``` r
problems <- reported_heights %>% 
  filter(not_inches(height)) %>%
  .$height
length(problems)
```

    ## [1] 292

10 examples of `x'y or x'y" or x'y\"`

``` r
pattern <- "^\\d\\s*'\\s*\\d{1,2}\\.*\\d*'*\"*$"
str_subset(problems, pattern) %>% head(n=10) %>% cat
```

    ## 5' 4" 5'7 5'7" 5'3" 5'11 5'9'' 5'10'' 5' 10 5'5" 5'2"

10 examples of `x.y or x,y`

``` r
pattern <- "^[4-6]\\s*[\\.|,]\\s*([0-9]|10|11)$"
str_subset(problems, pattern) %>% head(n=10) %>% cat
```

    ## 5.3 5.5 6.5 5.8 5.6 5,3 5.9 6,8 5.5 6.2

10 examples of entries in cm rather than inches

``` r
ind <- which(between(suppressWarnings(as.numeric(problems))/2.54, 54, 81) )
ind <- ind[!is.na(ind)]
problems[ind] %>% head(n=10) %>% cat
```

    ## 150 175 177 178 163 175 178 165 165 180

## String RegEX

load stringr through tidyverse

Few example of RegEX in action

``` r
# detect whether a comma is present
pattern <- ","
str_detect(murders_raw$total, pattern) 
```

    ##  [1] FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE
    ## [13] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [25] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [37] FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE
    ## [49] FALSE FALSE FALSE

``` r
# show the subset of strings including "cm"
str_subset(reported_heights$height, "cm")
```

    ## [1] "165cm"  "170 cm"

``` r
# use the "or" symbol inside a regex (|)
yes <- c("180 cm", "70 inches")
no <- c("180", "70''")
s <- c(yes, no)
str_detect(s, "cm") | str_detect(s, "inches")
```

    ## [1]  TRUE  TRUE FALSE FALSE

``` r
str_detect(s, "cm|inches")
```

    ## [1]  TRUE  TRUE FALSE FALSE

``` r
# highlight the first occurrence of a pattern
str_view(s, pattern)

# highlight all instances of a pattern
str_view_all(s, pattern)
```

    ## Warning: `str_view()` was deprecated in stringr 1.5.0.
    ## ℹ Please use `str_view_all()` instead.

    ## [1] │ 180 cm
    ## [2] │ 70 inches
    ## [3] │ 180
    ## [4] │ 70''

## Testing RegEX

``` r
# s was defined in the previous video
yes <- c("5", "6", "5'10", "5 feet", "4'11")
no <- c("", ".", "Five", "six")
s <- c(yes, no)
pattern <- "\\d"

# [56] means 5 or 6
str_view(s, "[56]")
```

    ## [1] │ <5>
    ## [2] │ <6>
    ## [3] │ <5>'10
    ## [4] │ <5> feet

``` r
# [4-7] means 4, 5, 6 or 7
yes <- as.character(4:7)
no <- as.character(1:3)
s <- c(yes, no)
str_detect(s, "[4-7]")
```

    ## [1]  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE

``` r
# ^ means start of string, $ means end of string
pattern <- "^\\d$"
yes <- c("1", "5", "9")
no <- c("12", "123", " 1", "a4", "b")
s <- c(yes, no)
str_view(s, pattern)
```

    ## [1] │ <1>
    ## [2] │ <5>
    ## [3] │ <9>

``` r
# curly braces define quantifiers: 1 or 2 digits 
pattern <- "^\\d{1,2}$"
yes <- c("1", "5", "9", "12")
no <- c("123", "a4", "b")
str_view(c(yes, no), pattern)
```

    ## [1] │ <1>
    ## [2] │ <5>
    ## [3] │ <9>
    ## [4] │ <12>

``` r
# combining character class, anchors and quantifier
pattern <- "^[4-7]'\\d{1,2}\"$"
yes <- c("5'7\"", "6'2\"",  "5'12\"")
no <- c("6,2\"", "6.2\"","I am 5'11\"", "3'2\"", "64")
str_detect(yes, pattern)
```

    ## [1] TRUE TRUE TRUE

``` r
str_detect(no, pattern)
```

    ## [1] FALSE FALSE FALSE FALSE FALSE

## Recoding problems within Heights reporting in Case Study

``` r
# number of entries matching our desired pattern
pattern <- "^[4-7]'\\d{1,2}\"$"
sum(str_detect(problems, pattern))
```

    ## [1] 14

``` r
# inspect examples of entries with problems
problems[c(2, 10, 11, 12, 15)] %>% str_view(pattern)
```

    ## [2] │ <5'7">
    ## [3] │ <5'3">

``` r
str_subset(problems, "inches")
```

    ## [1] "5 feet and 8.11 inches" "Five foot eight inches" "5 feet 7inches"        
    ## [4] "5ft 9 inches"           "5 ft 9 inches"          "5 feet 6 inches"

``` r
str_subset(problems, "''")
```

    ##  [1] "5'9''"   "5'10''"  "5'10''"  "5'3''"   "5'7''"   "5'6''"   "5'7.5''"
    ##  [8] "5'7.5''" "5'10''"  "5'11''"  "5'10''"  "5'5''"

``` r
# replace or remove feet/inches words before matching
pattern <- "^[4-7]'\\d{1,2}$"
problems %>% 
  str_replace("feet|ft|foot", "'") %>% # replace feet, ft, foot with ' 
  str_replace("inches|in|''|\"", "") %>% # remove all inches symbols
  str_detect(pattern) %>% 
  sum()
```

    ## [1] 48

``` r
# R does not ignore whitespace
identical("Hi", "Hi ")
```

    ## [1] FALSE

``` r
# \\s represents whitespace
pattern_2 <- "^[4-7]'\\s\\d{1,2}\"$"
str_subset(problems, pattern_2)
```

    ## [1] "5' 4\""  "5' 11\"" "5' 7\""

``` r
# * means 0 or more instances of a character
yes <- c("AB", "A1B", "A11B", "A111B", "A1111B")
no <- c("A2B", "A21B")
str_detect(yes, "A1*B")
```

    ## [1] TRUE TRUE TRUE TRUE TRUE

``` r
str_detect(no, "A1*B")
```

    ## [1] FALSE FALSE

``` r
# test how *, ? and + differ
data.frame(string = c("AB", "A1B", "A11B", "A111B", "A1111B"),
           none_or_more = str_detect(yes, "A1*B"),
           nore_or_once = str_detect(yes, "A1?B"),
           once_or_more = str_detect(yes, "A1+B"))
```

    ##   string none_or_more nore_or_once once_or_more
    ## 1     AB         TRUE         TRUE        FALSE
    ## 2    A1B         TRUE         TRUE         TRUE
    ## 3   A11B         TRUE        FALSE         TRUE
    ## 4  A111B         TRUE        FALSE         TRUE
    ## 5 A1111B         TRUE        FALSE         TRUE

``` r
# update pattern by adding optional spaces before and after feet symbol
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
problems %>% 
  str_replace("feet|ft|foot", "'") %>% # replace feet, ft, foot with ' 
  str_replace("inches|in|''|\"", "") %>% # remove all inches symbols
  str_detect(pattern) %>% 
  sum()
```

    ## [1] 53

## Dealing with more complicated patterns in Case Study

``` r
# define regex with and without groups
pattern_without_groups <- "^[4-7],\\d*$"
pattern_with_groups <-  "^([4-7]),(\\d*)$"

# create examples
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)

# demonstrate the effect of groups
str_detect(s, pattern_without_groups)
```

    ## [1]  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE

``` r
str_detect(s, pattern_with_groups)
```

    ## [1]  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE

``` r
# demonstrate difference between str_match and str_extract
str_match(s, pattern_with_groups)
```

    ##      [,1]   [,2] [,3]
    ## [1,] "5,9"  "5"  "9" 
    ## [2,] "5,11" "5"  "11"
    ## [3,] "6,"   "6"  ""  
    ## [4,] "6,1"  "6"  "1" 
    ## [5,] NA     NA   NA  
    ## [6,] NA     NA   NA  
    ## [7,] NA     NA   NA  
    ## [8,] NA     NA   NA

``` r
str_extract(s, pattern_with_groups)
```

    ## [1] "5,9"  "5,11" "6,"   "6,1"  NA     NA     NA     NA

``` r
# improve the pattern to recognize more events
pattern_with_groups <-  "^([4-7]),(\\d*)$"
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
str_replace(s, pattern_with_groups, "\\1'\\2")
```

    ## [1] "5'9"   "5'11"  "6'"    "6'1"   "5'9"   ","     "2,8"   "6.1.1"

``` r
# final pattern
pattern_with_groups <-"^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$"

# combine stringr commands with the pipe
str_subset(problems, pattern_with_groups) %>% head
```

    ## [1] "5.3"  "5.25" "5.5"  "6.5"  "5.8"  "5.6"

``` r
str_subset(problems, pattern_with_groups) %>% 
  str_replace(pattern_with_groups, "\\1'\\2") %>% head
```

    ## [1] "5'3"  "5'25" "5'5"  "6'5"  "5'8"  "5'6"

## Testing RegEX Pattern

``` r
# function to detect entries with problems
not_inches_or_cm <- function(x, smallest = 50, tallest = 84){
    inches <- suppressWarnings(as.numeric(x))
    ind <- !is.na(inches) &
        ((inches >= smallest & inches <= tallest) |
             (inches/2.54 >= smallest & inches/2.54 <= tallest))
    !ind
}

# identify entries with problems
problems <- reported_heights %>% 
  filter(not_inches_or_cm(height)) %>%
  .$height
length(problems)
```

    ## [1] 200

``` r
converted <- problems %>% 
  str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
  str_replace("inches|in|''|\"", "") %>%  #remove inches symbols
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") ##change format

# find proportion of entries that fit the pattern after reformatting
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern)
mean(index)
```

    ## [1] 0.615

``` r
converted[!index]    # show problems
```

    ##  [1] "6"             "165cm"         "511"           "6"            
    ##  [5] "2"             ">9000"         "5 ' and 8.11 " "11111"        
    ##  [9] "6"             "103.2"         "19"            "5"            
    ## [13] "300"           "6'"            "6"             "Five ' eight "
    ## [17] "7"             "214"           "6"             "0.7"          
    ## [21] "6"             "2'33"          "612"           "1,70"         
    ## [25] "87"            "5'7.5"         "5'7.5"         "111"          
    ## [29] "5' 7.78"       "12"            "6"             "yyy"          
    ## [33] "89"            "34"            "25"            "6"            
    ## [37] "6"             "22"            "684"           "6"            
    ## [41] "1"             "1"             "6*12"          "87"           
    ## [45] "6"             "1.6"           "120"           "120"          
    ## [49] "23"            "1.7"           "6"             "5"            
    ## [53] "69"            "5' 9 "         "5 ' 9 "        "6"            
    ## [57] "6"             "86"            "708,661"       "5 ' 6 "       
    ## [61] "6"             "649,606"       "10000"         "1"            
    ## [65] "728,346"       "0"             "6"             "6"            
    ## [69] "6"             "100"           "88"            "6"            
    ## [73] "170 cm"        "7,283,465"     "5"             "5"            
    ## [77] "34"
