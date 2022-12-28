Assignment-3
================
Anurag Garg
2022-12-28

Import raw Brexit referendum polling data from Wikipedia:

``` r
library(rvest)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.4.0      ✔ purrr   1.0.0 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.5.0 
    ## ✔ readr   2.1.3      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter()         masks stats::filter()
    ## ✖ readr::guess_encoding() masks rvest::guess_encoding()
    ## ✖ dplyr::lag()            masks stats::lag()

``` r
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[6]] %>% html_table(fill = TRUE)
```

You will use a variety of string processing techniques learned in this
section to reformat these data.

## Question 5

Some rows in this table do not contain polls. You can identify these by
the lack of the percent sign (%) in the Remain column.

Update polls by changing the column names to c(“dates”, “remain”,
“leave”, “undecided”, “lead”, “samplesize”, “pollster”, “poll_type”,
“notes”) and only keeping rows that have a percent sign (%) in the
remain column. How many rows remain in the polls data frame?

## Solution

Changing the names of the columns

``` r
names(polls) <- c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes")
names(polls)
```

    ## [1] "dates"      "remain"     "leave"      "undecided"  "lead"      
    ## [6] "samplesize" "pollster"   "poll_type"  "notes"

Testing *%* sign in the data

``` r
no <- c('12%', '50.5%', '50.55%')
yes <- c('12', '50.5', '50.55')
pattern <- "^\\d+\\.?\\d*%"
str_detect(yes, pattern, negate = TRUE)
```

    ## [1] TRUE TRUE TRUE

``` r
str_detect(no, pattern, negate = TRUE)
```

    ## [1] FALSE FALSE FALSE

Applying on the `remain` column and counting the number of rows without
*%*.

``` r
pattern <- "^\\d+\\.?\\d*%"
sum(str_detect(polls$remain, pattern, negate = TRUE))
```

    ## [1] 5

``` r
updated <-polls %>% filter(str_detect(polls$remain, pattern))
updated
```

    ## # A tibble: 129 × 9
    ##    dates        remain leave undecided lead  samplesize pollster   poll_…¹ notes
    ##    <chr>        <chr>  <chr> <chr>     <chr> <chr>      <chr>      <chr>   <chr>
    ##  1 23 June 2016 48.1%  51.9% N/A       3.8%  33,577,342 Results o… UK-wid… ""   
    ##  2 23 June      52%    48%   N/A       4%    4,772      YouGov     Online  "On …
    ##  3 22 June      55%    45%   N/A       10%   4,700      Populus    Online  ""   
    ##  4 20–22 June   51%    49%   N/A       2%    3,766      YouGov     Online  "Inc…
    ##  5 20–22 June   49%    46%   1%        3%    1,592      Ipsos MORI Teleph… ""   
    ##  6 20–22 June   44%    45%   9%        1%    3,011      Opinium    Online  ""   
    ##  7 17–22 June   54%    46%   N/A       8%    1,032      ComRes     Teleph… "Tho…
    ##  8 17–22 June   48%    42%   11%       6%    1,032      ComRes     Teleph… "All…
    ##  9 16–22 June   41%    43%   16%       2%    2,320      TNS        Online  ""   
    ## 10 20 June      45%    44%   11%       1%    1,003      Survation… Teleph… ""   
    ## # … with 119 more rows, and abbreviated variable name ¹​poll_type

## Question 6

The remain and leave columns are both given in the format “48.1%”:
percentages out of 100% with a percent symbol. Which of these commands
converts the remain vector to a proportion between 0 and 1?

``` r
parse_number(polls$remain)/100
```

    ## Warning: 3 parsing failures.
    ## row col expected                                                                                                                                       actual
    ##  76  -- a number The EU referendum campaign officially begins.[31]                                                                                           
    ##  83  -- a number HM Government starts sending a pro-Remain pamphlet to 27 million UK households and begins a pro-Remain digital advertising campaign.[32][33]
    ## 112  -- a number David Cameron announces the date of UK's In/Out EU referendum after an EU summit in Brussels.[34]

    ##   [1]    NA 0.481 0.520 0.550 0.510 0.490 0.440 0.540 0.480 0.410 0.450 0.420
    ##  [13] 0.530 0.450 0.440 0.440 0.190 0.420 0.420 0.370 0.460 0.430 0.390 0.450
    ##  [25] 0.440 0.460 0.400 0.480 0.530 0.420 0.440 0.450 0.430 0.430 0.480 0.410
    ##  [37] 0.430 0.400 0.410 0.420 0.440 0.510 0.440 0.440 0.410 0.410 0.450 0.550
    ##  [49] 0.440 0.440 0.520 0.550 0.470 0.430 0.550 0.380 0.360 0.380 0.440 0.420
    ##  [61] 0.440 0.430 0.420 0.490 0.390 0.410 0.450 0.430 0.440 0.510 0.510 0.490
    ##  [73] 0.480 0.430 0.530    NA 0.380 0.400 0.390 0.350 0.450 0.420    NA 0.400
    ##  [85] 0.390 0.440 0.510 0.390 0.350 0.410 0.510 0.450 0.490 0.400 0.480 0.410
    ##  [97] 0.460 0.470 0.430 0.450 0.480 0.490 0.400 0.400 0.400 0.390 0.410 0.390
    ## [109] 0.480 0.480 0.370    NA 0.380 0.420 0.510 0.450 0.400 0.540 0.360 0.430
    ## [121] 0.490 0.410 0.360 0.420 0.380 0.550 0.440 0.540 0.410 0.520 0.420 0.380
    ## [133] 0.420 0.440
    ## attr(,"problems")
    ## # A tibble: 3 × 4
    ##     row   col expected actual                                                   
    ##   <int> <int> <chr>    <chr>                                                    
    ## 1    76    NA a number The EU referendum campaign officially begins.[31]        
    ## 2    83    NA a number HM Government starts sending a pro-Remain pamphlet to 27…
    ## 3   112    NA a number David Cameron announces the date of UK's In/Out EU refer…

## Question 7

The undecided column has some “N/A” values. These “N/A”s are only
present when the remain and leave columns total 100%, so they should
actually be zeros.

Use a function from stringr to convert “N/A” in the undecided column to
0. The format of your command should be function_name(polls\$undecided,
“arg1”, “arg2”).

What function replaces function_name?

``` r
str_replace(polls$undecided, "N/A", "0")
```

    ##   [1] "Undecided"                                                                                                                                   
    ##   [2] "0"                                                                                                                                           
    ##   [3] "0"                                                                                                                                           
    ##   [4] "0"                                                                                                                                           
    ##   [5] "0"                                                                                                                                           
    ##   [6] "1%"                                                                                                                                          
    ##   [7] "9%"                                                                                                                                          
    ##   [8] "0"                                                                                                                                           
    ##   [9] "11%"                                                                                                                                         
    ##  [10] "16%"                                                                                                                                         
    ##  [11] "11%"                                                                                                                                         
    ##  [12] "13%"                                                                                                                                         
    ##  [13] "2%"                                                                                                                                          
    ##  [14] "13%"                                                                                                                                         
    ##  [15] "9%"                                                                                                                                          
    ##  [16] "12%"                                                                                                                                         
    ##  [17] "All official campaigning suspended until 19 June after the fatal shooting of Jo Cox MP.[26]"                                                 
    ##  [18] "9%"                                                                                                                                          
    ##  [19] "13%"                                                                                                                                         
    ##  [20] "16%"                                                                                                                                         
    ##  [21] "11%"                                                                                                                                         
    ##  [22] "3%"                                                                                                                                          
    ##  [23] "15%"                                                                                                                                         
    ##  [24] "5%"                                                                                                                                          
    ##  [25] "7%"                                                                                                                                          
    ##  [26] "9%"                                                                                                                                          
    ##  [27] "13%"                                                                                                                                         
    ##  [28] "3%"                                                                                                                                          
    ##  [29] "0"                                                                                                                                           
    ##  [30] "11%"                                                                                                                                         
    ##  [31] "13%"                                                                                                                                         
    ##  [32] "0"                                                                                                                                           
    ##  [33] "11%"                                                                                                                                         
    ##  [34] "9%"                                                                                                                                          
    ##  [35] "5%"                                                                                                                                          
    ##  [36] "11%"                                                                                                                                         
    ##  [37] "16%"                                                                                                                                         
    ##  [38] "16%"                                                                                                                                         
    ##  [39] "13%"                                                                                                                                         
    ##  [40] "15%"                                                                                                                                         
    ##  [41] "9%"                                                                                                                                          
    ##  [42] "3%"                                                                                                                                          
    ##  [43] "12%"                                                                                                                                         
    ##  [44] "18%"                                                                                                                                         
    ##  [45] "13%"                                                                                                                                         
    ##  [46] "16%"                                                                                                                                         
    ##  [47] "10%"                                                                                                                                         
    ##  [48] "3%"                                                                                                                                          
    ##  [49] "14%"                                                                                                                                         
    ##  [50] "12%"                                                                                                                                         
    ##  [51] "7%"                                                                                                                                          
    ##  [52] "5%"                                                                                                                                          
    ##  [53] "14%"                                                                                                                                         
    ##  [54] "10%"                                                                                                                                         
    ##  [55] "5%"                                                                                                                                          
    ##  [56] "21%"                                                                                                                                         
    ##  [57] "22%"                                                                                                                                         
    ##  [58] "16%"                                                                                                                                         
    ##  [59] "11%"                                                                                                                                         
    ##  [60] "13%"                                                                                                                                         
    ##  [61] "11%"                                                                                                                                         
    ##  [62] "11%"                                                                                                                                         
    ##  [63] "14%"                                                                                                                                         
    ##  [64] "0"                                                                                                                                           
    ##  [65] "26%"                                                                                                                                         
    ##  [66] "13%"                                                                                                                                         
    ##  [67] "17%"                                                                                                                                         
    ##  [68] "13%"                                                                                                                                         
    ##  [69] "10%"                                                                                                                                         
    ##  [70] "6%"                                                                                                                                          
    ##  [71] "9%"                                                                                                                                          
    ##  [72] "8%"                                                                                                                                          
    ##  [73] "11%"                                                                                                                                         
    ##  [74] "13%"                                                                                                                                         
    ##  [75] "6%"                                                                                                                                          
    ##  [76] "The EU referendum campaign officially begins.[31]"                                                                                           
    ##  [77] "28%"                                                                                                                                         
    ##  [78] "16%"                                                                                                                                         
    ##  [79] "17%"                                                                                                                                         
    ##  [80] "30%"                                                                                                                                         
    ##  [81] "17%"                                                                                                                                         
    ##  [82] "12%"                                                                                                                                         
    ##  [83] "HM Government starts sending a pro-Remain pamphlet to 27 million UK households and begins a pro-Remain digital advertising campaign.[32][33]"
    ##  [84] "16%"                                                                                                                                         
    ##  [85] "18%"                                                                                                                                         
    ##  [86] "13%"                                                                                                                                         
    ##  [87] "5%"                                                                                                                                          
    ##  [88] "18%"                                                                                                                                         
    ##  [89] "30%"                                                                                                                                         
    ##  [90] "14%"                                                                                                                                         
    ##  [91] "0"                                                                                                                                           
    ##  [92] "12%"                                                                                                                                         
    ##  [93] "10%"                                                                                                                                         
    ##  [94] "19%"                                                                                                                                         
    ##  [95] "11%"                                                                                                                                         
    ##  [96] "17%"                                                                                                                                         
    ##  [97] "19%"                                                                                                                                         
    ##  [98] "4%"                                                                                                                                          
    ##  [99] "16%"                                                                                                                                         
    ## [100] "16%"                                                                                                                                         
    ## [101] "7%"                                                                                                                                          
    ## [102] "15%"                                                                                                                                         
    ## [103] "19%"                                                                                                                                         
    ## [104] "18%"                                                                                                                                         
    ## [105] "19%"                                                                                                                                         
    ## [106] "19%"                                                                                                                                         
    ## [107] "18%"                                                                                                                                         
    ## [108] "18%"                                                                                                                                         
    ## [109] "15%"                                                                                                                                         
    ## [110] "0"                                                                                                                                           
    ## [111] "25%"                                                                                                                                         
    ## [112] "David Cameron announces the date of UK's In/Out EU referendum after an EU summit in Brussels.[34]"                                           
    ## [113] "25%"                                                                                                                                         
    ## [114] "17%"                                                                                                                                         
    ## [115] "10%"                                                                                                                                         
    ## [116] "23%"                                                                                                                                         
    ## [117] "19%"                                                                                                                                         
    ## [118] "10%"                                                                                                                                         
    ## [119] "25%"                                                                                                                                         
    ## [120] "18%"                                                                                                                                         
    ## [121] "10%"                                                                                                                                         
    ## [122] "17%"                                                                                                                                         
    ## [123] "19%"                                                                                                                                         
    ## [124] "19%"                                                                                                                                         
    ## [125] "20%"                                                                                                                                         
    ## [126] "9%"                                                                                                                                          
    ## [127] "14%"                                                                                                                                         
    ## [128] "10%"                                                                                                                                         
    ## [129] "18%"                                                                                                                                         
    ## [130] "0"                                                                                                                                           
    ## [131] "17%"                                                                                                                                         
    ## [132] "22%"                                                                                                                                         
    ## [133] "12%"                                                                                                                                         
    ## [134] "18%"

## Question 8

The `dates` column contains the range of dates over which the poll was
conducted. The format is “8-10 Jan” where the poll had a start date of
2016-01-08 and end date of 2016-01-10. Some polls go across month
boundaries (16 May-12 June).

The end date of the poll will always be one or two digits, followed by a
space, followed by the month as one or more letters (either capital or
lowercase). In these data, all month abbreviations or names have 3, 4 or
5 letters.

Write a regular expression to extract the end day and month from dates.
Insert it into the skeleton code below:

``` r
temp <- str_extract_all(polls$dates, "[0-9]+\\s[a-zA-Z]+")
end_date <- sapply(temp, function(x) x[length(x)]) # take last element (handles polls that cross month boundaries)
temp
```

    ## [[1]]
    ## character(0)
    ## 
    ## [[2]]
    ## [1] "23 June"
    ## 
    ## [[3]]
    ## [1] "23 June"
    ## 
    ## [[4]]
    ## [1] "22 June"
    ## 
    ## [[5]]
    ## [1] "22 June"
    ## 
    ## [[6]]
    ## [1] "22 June"
    ## 
    ## [[7]]
    ## [1] "22 June"
    ## 
    ## [[8]]
    ## [1] "22 June"
    ## 
    ## [[9]]
    ## [1] "22 June"
    ## 
    ## [[10]]
    ## [1] "22 June"
    ## 
    ## [[11]]
    ## [1] "20 June"
    ## 
    ## [[12]]
    ## [1] "19 June"
    ## 
    ## [[13]]
    ## [1] "19 June"
    ## 
    ## [[14]]
    ## [1] "18 June"
    ## 
    ## [[15]]
    ## [1] "17 June"
    ## 
    ## [[16]]
    ## [1] "17 June"
    ## 
    ## [[17]]
    ## [1] "16 June"
    ## 
    ## [[18]]
    ## [1] "16 June"
    ## 
    ## [[19]]
    ## [1] "15 June"
    ## 
    ## [[20]]
    ## [1] "15 June"
    ## 
    ## [[21]]
    ## [1] "15 June"
    ## 
    ## [[22]]
    ## [1] "14 June"
    ## 
    ## [[23]]
    ## [1] "13 June"
    ## 
    ## [[24]]
    ## [1] "13 June"
    ## 
    ## [[25]]
    ## [1] "13 June"
    ## 
    ## [[26]]
    ## [1] "13 June"
    ## 
    ## [[27]]
    ## [1] "13 June"
    ## 
    ## [[28]]
    ## [1] "12 June"
    ## 
    ## [[29]]
    ## [1] "16 May"  "12 June"
    ## 
    ## [[30]]
    ## [1] "10 June"
    ## 
    ## [[31]]
    ## [1] "10 June"
    ## 
    ## [[32]]
    ## [1] "9 June"
    ## 
    ## [[33]]
    ## [1] "6 June"
    ## 
    ## [[34]]
    ## [1] "5 June"
    ## 
    ## [[35]]
    ## [1] "5 June"
    ## 
    ## [[36]]
    ## [1] "3 June"
    ## 
    ## [[37]]
    ## [1] "31 May" "3 June"
    ## 
    ## [[38]]
    ## [1] "31 May" "3 June"
    ## 
    ## [[39]]
    ## [1] "31 May"
    ## 
    ## [[40]]
    ## [1] "29 May"
    ## 
    ## [[41]]
    ## [1] "29 May"
    ## 
    ## [[42]]
    ## [1] "29 May"
    ## 
    ## [[43]]
    ## [1] "25 May"
    ## 
    ## [[44]]
    ## [1] "24 May"
    ## 
    ## [[45]]
    ## [1] "24 May"
    ## 
    ## [[46]]
    ## [1] "23 May"
    ## 
    ## [[47]]
    ## [1] "22 May"
    ## 
    ## [[48]]
    ## [1] "22 May"
    ## 
    ## [[49]]
    ## [1] "19 May"
    ## 
    ## [[50]]
    ## [1] "17 May"
    ## 
    ## [[51]]
    ## [1] "17 May"
    ## 
    ## [[52]]
    ## [1] "16 May"
    ## 
    ## [[53]]
    ## [1] "15 May"
    ## 
    ## [[54]]
    ## [1] "15 May"
    ## 
    ## [[55]]
    ## [1] "15 May"
    ## 
    ## [[56]]
    ## [1] "12 May"
    ## 
    ## [[57]]
    ## [1] "29 Apr" "12 May"
    ## 
    ## [[58]]
    ## [1] "29 Apr" "12 May"
    ## 
    ## [[59]]
    ## [1] "8 May"
    ## 
    ## [[60]]
    ## [1] "6 May"
    ## 
    ## [[61]]
    ## [1] "29 Apr" "3 May" 
    ## 
    ## [[62]]
    ## [1] "29 Apr"
    ## 
    ## [[63]]
    ## [1] "29 Apr"
    ## 
    ## [[64]]
    ## [1] "29 Apr"
    ## 
    ## [[65]]
    ## [1] "28 Apr"
    ## 
    ## [[66]]
    ## [1] "26 Apr"
    ## 
    ## [[67]]
    ## [1] "26 Apr"
    ## 
    ## [[68]]
    ## [1] "26 Apr"
    ## 
    ## [[69]]
    ## [1] "24 Apr"
    ## 
    ## [[70]]
    ## [1] "24 Apr"
    ## 
    ## [[71]]
    ## [1] "19 Apr"
    ## 
    ## [[72]]
    ## [1] "18 Apr"
    ## 
    ## [[73]]
    ## [1] "17 Apr"
    ## 
    ## [[74]]
    ## [1] "17 Apr"
    ## 
    ## [[75]]
    ## [1] "17 Apr"
    ## 
    ## [[76]]
    ## [1] "15 April"
    ## 
    ## [[77]]
    ## [1] "14 Apr"
    ## 
    ## [[78]]
    ## [1] "14 Apr"
    ## 
    ## [[79]]
    ## [1] "12 Apr"
    ## 
    ## [[80]]
    ## [1] "11 Apr"
    ## 
    ## [[81]]
    ## [1] "10 Apr"
    ## 
    ## [[82]]
    ## [1] "10 Apr"
    ## 
    ## [[83]]
    ## [1] "7 April"
    ## 
    ## [[84]]
    ## [1] "7 Apr"
    ## 
    ## [[85]]
    ## [1] "29 Mar" "4 Apr" 
    ## 
    ## [[86]]
    ## [1] "3 Apr"
    ## 
    ## [[87]]
    ## [1] "29 Mar" "3 Apr" 
    ## 
    ## [[88]]
    ## [1] "29 Mar" "1 Apr" 
    ## 
    ## [[89]]
    ## [1] "29 Mar"
    ## 
    ## [[90]]
    ## [1] "29 Mar"
    ## 
    ## [[91]]
    ## [1] "28 Mar"
    ## 
    ## [[92]]
    ## [1] "24 Mar"
    ## 
    ## [[93]]
    ## [1] "22 Mar"
    ## 
    ## [[94]]
    ## [1] "22 Mar"
    ## 
    ## [[95]]
    ## [1] "20 Mar"
    ## 
    ## [[96]]
    ## [1] "20 Mar"
    ## 
    ## [[97]]
    ## [1] "19 Mar"
    ## 
    ## [[98]]
    ## [1] "14 Mar"
    ## 
    ## [[99]]
    ## [1] "13 Mar"
    ## 
    ## [[100]]
    ## [1] "11 Mar"
    ## 
    ## [[101]]
    ## [1] "10 Mar"
    ## 
    ## [[102]]
    ## [1] "6 Mar"
    ## 
    ## [[103]]
    ## [1] "6 Mar"
    ## 
    ## [[104]]
    ## [1] "3 Mar"
    ## 
    ## [[105]]
    ## [1] "2 Mar"
    ## 
    ## [[106]]
    ## [1] "29 Feb" "1 Mar" 
    ## 
    ## [[107]]
    ## [1] "29 Feb"
    ## 
    ## [[108]]
    ## [1] "28 Feb"
    ## 
    ## [[109]]
    ## [1] "28 Feb"
    ## 
    ## [[110]]
    ## [1] "25 Feb"
    ## 
    ## [[111]]
    ## [1] "23 Feb"
    ## 
    ## [[112]]
    ## [1] "20 Feb"
    ## 
    ## [[113]]
    ## [1] "23 Feb"
    ## 
    ## [[114]]
    ## [1] "22 Feb"
    ## 
    ## [[115]]
    ## [1] "22 Feb"
    ## 
    ## [[116]]
    ## [1] "20 Feb"
    ## 
    ## [[117]]
    ## [1] "19 Feb"
    ## 
    ## [[118]]
    ## [1] "16 Feb"
    ## 
    ## [[119]]
    ## [1] "15 Feb"
    ## 
    ## [[120]]
    ## [1] "14 Feb"
    ## 
    ## [[121]]
    ## [1] "14 Feb"
    ## 
    ## [[122]]
    ## [1] "7 Feb"
    ## 
    ## [[123]]
    ## [1] "4 Feb"
    ## 
    ## [[124]]
    ## [1] "31 Jan"
    ## 
    ## [[125]]
    ## [1] "28 Jan"
    ## 
    ## [[126]]
    ## [1] "25 Jan"
    ## 
    ## [[127]]
    ## [1] "25 Jan"
    ## 
    ## [[128]]
    ## [1] "24 Jan"
    ## 
    ## [[129]]
    ## [1] "24 Jan"
    ## 
    ## [[130]]
    ## [1] "21 Jan"
    ## 
    ## [[131]]
    ## [1] "17 Jan"
    ## 
    ## [[132]]
    ## [1] "16 Jan"
    ## 
    ## [[133]]
    ## [1] "14 Jan"
    ## 
    ## [[134]]
    ## [1] "10 Jan"
