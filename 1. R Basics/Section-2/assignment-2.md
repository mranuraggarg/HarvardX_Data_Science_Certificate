assignment-2
================
Anurag Garg
2022-12-28

## Question 1

Consider the vector x:

``` r
x <- c(2, 43, 27, 96, 18)
```

Match the following outputs to the function which produces that output.
Options include sort(x), order(x), rank(x) and none of these.

``` r
print(sort(x))
```

    ## [1]  2 18 27 43 96

``` r
print(order(x))
```

    ## [1] 1 5 3 2 4

``` r
print(rank(x))
```

    ## [1] 1 4 3 5 2

## Question 2

calculate min and max of x

``` r
print(min(x))
```

    ## [1] 2

``` r
print(which.min(x))
```

    ## [1] 1

``` r
print(max(x))
```

    ## [1] 96

``` r
print(which.max(x))
```

    ## [1] 4

# Question 3

Mandi, Amy, Nicole, and Olivia all ran different distances in different
time intervals. Their distances (in miles) and times (in minutes) are as
follows:

``` r
name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)
```

Write a line of code to convert time to hours. Remember there are 60
minutes in an hour. Then write a line of code to calculate the speed of
each runner in miles per hour. Speed is distance divided by time.

How many hours did Olivia run?

``` r
time <- time / 60
speed <- distance / time
print(time[4])
```

    ## [1] 0.8333333

``` r
print(speed[1])
```

    ## [1] 4.8

``` r
print(name[which.max(speed)])
```

    ## [1] "Amy"
