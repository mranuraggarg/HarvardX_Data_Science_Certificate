Practice Section 3 Part 2
================
Anurag Garg
2022-12-28

## Library loading

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

``` r
# read raw murders data line by line
filename <- system.file("extdata/murders.csv", package = "dslabs")
lines <- readLines(filename)
lines %>% head()
```

    ## [1] "state,abb,region,population,total" "Alabama,AL,South,4779736,135"     
    ## [3] "Alaska,AK,West,710231,19"          "Arizona,AZ,West,6392017,232"      
    ## [5] "Arkansas,AR,South,2915918,93"      "California,CA,West,37253956,1257"

``` r
# split at commas with str_split function, remove row of column names
x <- str_split(lines, ",") 
x %>% head()
```

    ## [[1]]
    ## [1] "state"      "abb"        "region"     "population" "total"     
    ## 
    ## [[2]]
    ## [1] "Alabama" "AL"      "South"   "4779736" "135"    
    ## 
    ## [[3]]
    ## [1] "Alaska" "AK"     "West"   "710231" "19"    
    ## 
    ## [[4]]
    ## [1] "Arizona" "AZ"      "West"    "6392017" "232"    
    ## 
    ## [[5]]
    ## [1] "Arkansas" "AR"       "South"    "2915918"  "93"      
    ## 
    ## [[6]]
    ## [1] "California" "CA"         "West"       "37253956"   "1257"

``` r
col_names <- x[[1]]
x <- x[-1]

# extract first element of each list entry
library(purrr)
map(x, function(y) y[1]) %>% head()
```

    ## [[1]]
    ## [1] "Alabama"
    ## 
    ## [[2]]
    ## [1] "Alaska"
    ## 
    ## [[3]]
    ## [1] "Arizona"
    ## 
    ## [[4]]
    ## [1] "Arkansas"
    ## 
    ## [[5]]
    ## [1] "California"
    ## 
    ## [[6]]
    ## [1] "Colorado"

``` r
map(x, 1) %>% head()
```

    ## [[1]]
    ## [1] "Alabama"
    ## 
    ## [[2]]
    ## [1] "Alaska"
    ## 
    ## [[3]]
    ## [1] "Arizona"
    ## 
    ## [[4]]
    ## [1] "Arkansas"
    ## 
    ## [[5]]
    ## [1] "California"
    ## 
    ## [[6]]
    ## [1] "Colorado"

``` r
# extract columns 1-5 as characters, then convert to proper format - NOTE: DIFFERENT FROM VIDEO
dat <- data.frame(parse_guess(map_chr(x, 1)),
                  parse_guess(map_chr(x, 2)),
                  parse_guess(map_chr(x, 3)),
                  parse_guess(map_chr(x, 4)),
                  parse_guess(map_chr(x, 5))) %>%
  setNames(col_names)
  
dat %>% head
```

    ##        state abb region population total
    ## 1    Alabama  AL  South    4779736   135
    ## 2     Alaska  AK   West     710231    19
    ## 3    Arizona  AZ   West    6392017   232
    ## 4   Arkansas  AR  South    2915918    93
    ## 5 California  CA   West   37253956  1257
    ## 6   Colorado  CO   West    5029196    65

``` r
# more efficient code for the same thing
dat <- x %>%
  transpose() %>%
  map( ~ parse_guess(unlist(.))) %>%
  setNames(col_names) %>% 
  as.data.frame() 

# the simplify argument makes str_split return a matrix instead of a list
x <- str_split(lines, ",", simplify = TRUE) 
col_names <- x[1,]
x <- x[-1,]
x %>% as_data_frame() %>%
  setNames(col_names) %>%
  mutate_all(parse_guess)
```

    ## Warning: `as_data_frame()` was deprecated in tibble 2.0.0.
    ## ℹ Please use `as_tibble()` instead.
    ## ℹ The signature and semantics have changed, see `?as_tibble`.

    ## Warning: The `x` argument of `as_tibble.matrix()` must have unique column names if
    ## `.name_repair` is omitted as of tibble 2.0.0.
    ## ℹ Using compatibility `.name_repair`.
    ## ℹ The deprecated feature was likely used in the tibble package.
    ##   Please report the issue at <]8;;https://github.com/tidyverse/tibble/issueshttps://github.com/tidyverse/tibble/issues]8;;>.

    ## # A tibble: 51 × 5
    ##    state                abb   region    population total
    ##    <chr>                <chr> <chr>          <dbl> <dbl>
    ##  1 Alabama              AL    South        4779736   135
    ##  2 Alaska               AK    West          710231    19
    ##  3 Arizona              AZ    West         6392017   232
    ##  4 Arkansas             AR    South        2915918    93
    ##  5 California           CA    West        37253956  1257
    ##  6 Colorado             CO    West         5029196    65
    ##  7 Connecticut          CT    Northeast    3574097    97
    ##  8 Delaware             DE    South         897934    38
    ##  9 District of Columbia DC    South         601723    99
    ## 10 Florida              FL    South       19687653   669
    ## # … with 41 more rows

## Case Study: Extracting a table from a PDF

One of the datasets provided in dslabs shows scientific funding rates by
gender in the Netherlands:

``` r
library(dslabs)
data("research_funding_rates")
research_funding_rates 
```

    ##            discipline applications_total applications_men applications_women
    ## 1   Chemical sciences                122               83                 39
    ## 2   Physical sciences                174              135                 39
    ## 3             Physics                 76               67                  9
    ## 4          Humanities                396              230                166
    ## 5  Technical sciences                251              189                 62
    ## 6   Interdisciplinary                183              105                 78
    ## 7 Earth/life sciences                282              156                126
    ## 8     Social sciences                834              425                409
    ## 9    Medical sciences                505              245                260
    ##   awards_total awards_men awards_women success_rates_total success_rates_men
    ## 1           32         22           10                26.2              26.5
    ## 2           35         26            9                20.1              19.3
    ## 3           20         18            2                26.3              26.9
    ## 4           65         33           32                16.4              14.3
    ## 5           43         30           13                17.1              15.9
    ## 6           29         12           17                15.8              11.4
    ## 7           56         38           18                19.9              24.4
    ## 8          112         65           47                13.4              15.3
    ## 9           75         46           29                14.9              18.8
    ##   success_rates_women
    ## 1                25.6
    ## 2                23.1
    ## 3                22.2
    ## 4                19.3
    ## 5                21.0
    ## 6                21.8
    ## 7                14.3
    ## 8                11.5
    ## 9                11.2

The data come from a paper External link published in the prestigious
journal PNAS. However, the data are not provided in a spreadsheet; they
are in a table in a PDF document. We could extract the numbers by hand,
but this could lead to human error. Instead we can try to wrangle the
data using R. Downloading the data

We start by downloading the PDF document then importing it into R using
the following code:

``` r
library("pdftools")
```

    ## Using poppler version 22.02.0

``` r
temp_file <- tempfile()
url <- "https://www.pnas.org/action/downloadSupplement?doi=10.1073%2Fpnas.1510159112&file=pnas.201510159SI.pdf"
download.file(url, temp_file)
txt <- pdf_text(temp_file)
file.remove(temp_file)
```

    ## [1] TRUE

If we examine the object text we notice that it is a character vector
with an entry for each page. So we keep the page we want using the
following code:

``` r
raw_data_research_funding_rates <- txt[2]
```

The steps above can actually be skipped because we include the raw data
in the dslabs package as well:

``` r
data("raw_data_research_funding_rates")
```

Looking at the download

Examining this object,

``` r
raw_data_research_funding_rates %>% head
```

    ## [1] "                        Table S1. Numbers of applications and awarded grants, along with success rates for male and\n                        female applicants, by scientific discipline\n                                                      Applications, n                    Awards, n                      Success rates, %\n                        Discipline              Total     Men      Women         Total    Men       Women          Total    Men      Women\n                       Total                    2,823    1,635      1,188         467      290         177          16.5    17.7a      14.9b\n                        Chemical sciences         122       83          39         32       22           10         26.2    26.5a      25.6a\n                        Physical sciences         174      135          39         35       26            9         20.1    19.3a      23.1a\n                        Physics                    76       67           9         20       18            2         26.3    26.9a      22.2a\n                        Humanities                396      230         166         65       33           32         16.4    14.3a      19.3a\n                        Technical sciences        251      189          62         43       30           13         17.1    15.9a      21.0a\n                        Interdisciplinary         183      105          78         29       12           17         15.8    11.4a      21.8a\n                        Earth/life sciences       282      156         126         56       38           18         19.9    24.4a      14.3b\n                        Social sciences           834      425         409        112       65           47         13.4    15.3a      11.5a\n                        Medical sciences          505      245         260         75       46           29         14.9    18.8a      11.2b\n                           Success rates for male and female applicants with different subscripts differ reliably from one another (P < 0.05).\n                        Table S2. Numbers of applications and awarded grants, along with success rates for male and\n                        female applicants, by first-time applications vs. reapplications in one call, 2012\n                                                         Applications, n                  Awards, n                     Success rates, %\n                        Category                    Total    Men     Women         Total   Men       Women         Total     Men     Women\n                       Total                         921     527        394         147      93          54         16.0    17.6i      13.7i\n                        First-time applications      722     417        305         100      67          33         13.9i   16.1a      10.8b\n                        Reapplications               199     110         89          47      26          21         23.6j   23.6ac     23.6c\n                           Success rates for male and female applicants with different subscripts per row and column differ reliably from\n                        one another (P < 0.05).\nvan der Lee and Ellemers www.pnas.org/cgi/content/short/1510159112                                                                             2 of 2\n"

we see that it is a long string. Each line on the page, including the
table rows, is separated by the symbol for newline:

We can therefore can create a list with the lines of the text as
elements:

``` r
tab <- str_split(raw_data_research_funding_rates, "\n")
```

Because we start off with just one element in the string, we end up with
a list with just one entry:

``` r
tab <- tab[[1]]
```

By examining this object,

``` r
tab %>% head
```

    ## [1] "                        Table S1. Numbers of applications and awarded grants, along with success rates for male and"                         
    ## [2] "                        female applicants, by scientific discipline"                                                                         
    ## [3] "                                                      Applications, n                    Awards, n                      Success rates, %"    
    ## [4] "                        Discipline              Total     Men      Women         Total    Men       Women          Total    Men      Women"  
    ## [5] "                       Total                    2,823    1,635      1,188         467      290         177          16.5    17.7a      14.9b"
    ## [6] "                        Chemical sciences         122       83          39         32       22           10         26.2    26.5a      25.6a"

we see that the information for the column names is the third and fourth
entires:

``` r
the_names_1 <- tab[3]
the_names_2 <- tab[4]
```

In the table, the column information is spread across two lines. We want
to create one vector with one name for each column. We can do this using
some of the functions we have just learned. Extracting the table data

Let’s start with the first line:

``` r
the_names_1
```

    ## [1] "                                                      Applications, n                    Awards, n                      Success rates, %"

We want to remove the leading space and everything following the comma.
We can use regex for the latter. Then we can obtain the elements by
splitting using the space. We want to split only when there are 2 or
more spaces to avoid splitting success rate. So we use the regex \s{2,}
as follows:

``` r
the_names_1 <- the_names_1 %>%
  str_trim() %>%
  str_replace_all(",\\s.", "") %>%
  str_split("\\s{2,}", simplify = TRUE)
the_names_1
```

    ##      [,1]           [,2]     [,3]           
    ## [1,] "Applications" "Awards" "Success rates"

Now let’s look at the second line:

``` r
the_names_2
```

    ## [1] "                        Discipline              Total     Men      Women         Total    Men       Women          Total    Men      Women"

Here we want to trim the leading space and then split by space as we did
for the first line:

``` r
the_names_2 <- the_names_2 %>%
  str_trim() %>%
  str_split("\\s+", simplify = TRUE)
the_names_2
```

    ##      [,1]         [,2]    [,3]  [,4]    [,5]    [,6]  [,7]    [,8]    [,9] 
    ## [1,] "Discipline" "Total" "Men" "Women" "Total" "Men" "Women" "Total" "Men"
    ##      [,10]  
    ## [1,] "Women"

Now we can join these to generate one name for each column:

``` r
tmp_names <- str_c(rep(the_names_1, each = 3), the_names_2[-1], sep = "_")
the_names <- c(the_names_2[1], tmp_names) %>%
  str_to_lower() %>%
  str_replace_all("\\s", "_")
the_names
```

    ##  [1] "discipline"          "applications_total"  "applications_men"   
    ##  [4] "applications_women"  "awards_total"        "awards_men"         
    ##  [7] "awards_women"        "success_rates_total" "success_rates_men"  
    ## [10] "success_rates_women"

Now we are ready to get the actual data. By examining the tab object, we
notice that the information is in lines 6 through 14. We can
use str_split() again to achieve our goal:

``` r
new_research_funding_rates <- tab[6:14] %>%
  str_trim %>%
  str_split("\\s{2,}", simplify = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(the_names) %>%
  mutate_at(-1, parse_number)
new_research_funding_rates %>% head()
```

    ##           discipline applications_total applications_men applications_women
    ## 1  Chemical sciences                122               83                 39
    ## 2  Physical sciences                174              135                 39
    ## 3            Physics                 76               67                  9
    ## 4         Humanities                396              230                166
    ## 5 Technical sciences                251              189                 62
    ## 6  Interdisciplinary                183              105                 78
    ##   awards_total awards_men awards_women success_rates_total success_rates_men
    ## 1           32         22           10                26.2              26.5
    ## 2           35         26            9                20.1              19.3
    ## 3           20         18            2                26.3              26.9
    ## 4           65         33           32                16.4              14.3
    ## 5           43         30           13                17.1              15.9
    ## 6           29         12           17                15.8              11.4
    ##   success_rates_women
    ## 1                25.6
    ## 2                23.1
    ## 3                22.2
    ## 4                19.3
    ## 5                21.0
    ## 6                21.8

We can see that the objects are identical:

``` r
identical(research_funding_rates, new_research_funding_rates)
```

    ## [1] TRUE

### Recoder

``` r
# life expectancy time series for Caribbean countries
library(dslabs)
data("gapminder")
gapminder %>% 
  filter(region=="Caribbean") %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()
```

![](practice_part_2_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
# display long country names
gapminder %>% 
  filter(region=="Caribbean") %>%
  filter(str_length(country) >= 12) %>%
  distinct(country) 
```

    ##                          country
    ## 1            Antigua and Barbuda
    ## 2             Dominican Republic
    ## 3 St. Vincent and the Grenadines
    ## 4            Trinidad and Tobago

``` r
# recode long country names and remake plot
gapminder %>% filter(region=="Caribbean") %>%
  mutate(country = recode(country, 
                          'Antigua and Barbuda'="Barbuda",
                          'Dominican Republic' = "DR",
                          'St. Vincent and the Grenadines' = "St. Vincent",
                          'Trinidad and Tobago' = "Trinidad")) %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()
```

![](practice_part_2_files/figure-gfm/unnamed-chunk-19-2.png)<!-- -->
