assignment-3
================
Anurag Garg
2022-12-28

## Loading libraries

``` r
library(dslabs)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library("tidyverse")
```

    ## ── Attaching packages
    ## ───────────────────────────────────────
    ## tidyverse 1.3.2 ──

    ## ✔ ggplot2 3.4.0     ✔ purrr   1.0.0
    ## ✔ tibble  3.1.8     ✔ stringr 1.5.0
    ## ✔ tidyr   1.2.1     ✔ forcats 0.5.2
    ## ✔ readr   2.1.3     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
data(heights)
options(digits = 3)    # report 3 significant digits for all answers
```

## Question 1

First, determine the average height in this dataset. Then create a
logical vector ind with the indices for those individuals who are above
average height. How many individuals in the dataset are above average
height?

``` r
mu <- mean(heights$height)
ind <- heights %>% dplyr::filter(height > mu)
nrow(ind)
```

    ## [1] 532

## Question 2

How many individuals in the dataset are above average height and are
female?

``` r
females <- heights %>% dplyr::filter(height > mu & sex == "Female")
head(females)
```

    ##      sex height
    ## 1 Female   72.0
    ## 2 Female   78.7
    ## 3 Female   71.0
    ## 4 Female   69.6
    ## 5 Female   69.0
    ## 6 Female   69.0

``` r
nrow(females)
```

    ## [1] 31

## Question 3

If you use mean() on a logical (TRUE/FALSE) vector, it returns the
proportion of observations that are TRUE. What proportion of individuals
in the dataset are female?

``` r
no_of_females <- heights %>% dplyr::filter(sex == "Female")
female_prop <- nrow(no_of_females) / nrow(heights)
print(female_prop)
```

    ## [1] 0.227

## Question 4

This question takes you through three steps to determine the sex of the
individual with the minimum height.

### Question 4a

Determine the minimum height in the heights dataset.

``` r
min_height <- min(heights$height)
print(min_height)
```

    ## [1] 50

### Question 4b

Use the match() function to determine the index of the first individual
with the minimum height.

``` r
index <- match(min_height, heights$height)
print(index)
```

    ## [1] 1032

### Question 4c

Subset the sex column of the dataset by the index in 4b to determine the
individual’s sex.

``` r
gender <- heights$sex[index]
print(gender)
```

    ## [1] Male
    ## Levels: Female Male

## Question 5

This question takes you through three steps to determine how many of the
integer height values between the minimum and maximum heights are not
actual heights of individuals in the heights dataset.

### Question 5a

Determine the maximum height.

``` r
max_height <- max(heights$height)
print(max_height)
```

    ## [1] 82.7

### Question 5b

Which integer values are between the maximum and minimum heights? For
example, if the minimum height is 10.2 and the maximum height is 20.8,
your answer should be x \<- 11:20 to capture the integers in between
those values. (If either the maximum or minimum height are integers,
include those values too.)

Write code to create a vector x that includes the integers between the
minimum and maximum heights (as numbers).

``` r
x <- 50:82.7L
print(x)
```

    ##  [1] 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74
    ## [26] 75 76 77 78 79 80 81 82

``` r
not_heights <- !x %in% heights$height
print(not_heights)
```

    ##  [1] FALSE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE FALSE FALSE FALSE FALSE
    ## [13] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [25] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE

``` r
print(sum(not_heights))
```

    ## [1] 3

## Question 6

Using the heights dataset, create a new column of heights in centimeters
named ht_cm. Recall that 1 inch = 2.54 centimeters. Save the resulting
dataset as heights2.

``` r
heights <- mutate(heights, heights2=height*2.54)
head(heights)
```

    ##      sex height heights2
    ## 1   Male     75      190
    ## 2   Male     70      178
    ## 3   Male     68      173
    ## 4   Male     74      188
    ## 5   Male     61      155
    ## 6 Female     65      165

### Question 6a

What is the height in centimeters of the 18th individual (index 18)?

``` r
height <- heights$heights2[18]
print(height)
```

    ## [1] 163

### Question 6b

What is the mean height in centimeters?

``` r
print(mean(heights$heights2))
```

    ## [1] 174

### Question 6c

How many females are in the heights2 dataset?

``` r
print(nrow(filter(heights, sex=="Female")))
```

    ## [1] 238

### Question 6d

What is the mean height of females in cm

``` r
mean_height_cm <- heights %>% dplyr::filter(sex=="Female")
head(mean_height_cm)
```

    ##      sex height heights2
    ## 1 Female     65      165
    ## 2 Female     66      168
    ## 3 Female     62      157
    ## 4 Female     66      168
    ## 5 Female     64      163
    ## 6 Female     60      152

``` r
print(mean(mean_height_cm$heights2))
```

    ## [1] 165

## Question 8

The olive dataset in dslabs contains composition in percentage of eight
fatty acids found in the lipid fraction of 572 Italian olive oils:

``` r
data(olive)
head(olive)
```

    ##           region         area palmitic palmitoleic stearic oleic linoleic
    ## 1 Southern Italy North-Apulia    10.75        0.75    2.26  78.2     6.72
    ## 2 Southern Italy North-Apulia    10.88        0.73    2.24  77.1     7.81
    ## 3 Southern Italy North-Apulia     9.11        0.54    2.46  81.1     5.49
    ## 4 Southern Italy North-Apulia     9.66        0.57    2.40  79.5     6.19
    ## 5 Southern Italy North-Apulia    10.51        0.67    2.59  77.7     6.72
    ## 6 Southern Italy North-Apulia     9.11        0.49    2.68  79.2     6.78
    ##   linolenic arachidic eicosenoic
    ## 1      0.36      0.60       0.29
    ## 2      0.31      0.61       0.29
    ## 3      0.31      0.63       0.29
    ## 4      0.50      0.78       0.35
    ## 5      0.50      0.80       0.46
    ## 6      0.51      0.70       0.44

``` r
x <- olive$palmitic
y <- olive$palmitoleic

plot(x, y)
```

![](Assignment-3_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

## Question 9

Create a histogram of the percentage of eicosenoic acid in olive.

``` r
hist(olive$eicosenoic)
```

![](Assignment-3_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

## Question 10

Make a boxplot of palmitic acid percentage in olive with separate
distributions for each region. Which region has the highest median
palmitic acid percentage?

``` r
boxplot(palmitic~region, data=olive)
```

![](Assignment-3_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->
