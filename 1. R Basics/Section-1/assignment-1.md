Quiz-1
================
Anurag Garg
2022-12-28

## Question 1 solve quadratic equaiton

``` r
a <- 2
b <- -1
c <- -4

solution = c((-b + sqrt(b^2 - 4*a*c))/(2*a), (-b - sqrt(b^2 - 4*a*c))/(2*a))
print(solution)
```

    ## [1]  1.686141 -1.186141

You \## Question 2 log base 4 of 1024

``` r
print(log(1024, base = 4))
```

    ## [1] 5

## Library and data load for Question 3a, 3b and 3c

Importing data and library

``` r
library(dslabs)
data(movielens)
```

## Question 3a How many rows are in the dataset?

``` r
str(movielens)
```

    ## 'data.frame':    100004 obs. of  7 variables:
    ##  $ movieId  : int  31 1029 1061 1129 1172 1263 1287 1293 1339 1343 ...
    ##  $ title    : chr  "Dangerous Minds" "Dumbo" "Sleepers" "Escape from New York" ...
    ##  $ year     : int  1995 1941 1996 1981 1989 1978 1959 1982 1992 1991 ...
    ##  $ genres   : Factor w/ 901 levels "(no genres listed)",..: 762 510 899 120 762 836 81 762 844 899 ...
    ##  $ userId   : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ rating   : num  2.5 3 3 2 4 2 2 2 3.5 2 ...
    ##  $ timestamp: int  1260759144 1260759179 1260759182 1260759185 1260759205 1260759151 1260759187 1260759148 1260759125 1260759131 ...

## Question 3b How many different variables are in the dataset?

``` r
print(length(names(movielens)))
```

    ## [1] 7

## Question 3c What is the variable type of title ?

``` r
print(class(movielens$title))
```

    ## [1] "character"

## Question 3d What is the variable type of genres ?

``` r
print(class(movielens$genres))
```

    ## [1] "factor"

## Question 4 Determine the number of levels in genres using nlevels

``` r
print(nlevels(movielens$genres))
```

    ## [1] 901

``` r
print(nlevels(movielens$genres) == length(levels(movielens$genres)))
```

    ## [1] TRUE
