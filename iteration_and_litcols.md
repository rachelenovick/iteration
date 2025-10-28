iteration_and_listcols
================
2025-10-28

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.2     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

## Make a list

``` r
#this has to be a list, can't be a dataframe, because each vector is of different length
l = 
  list(
    vec_numeric = 1:23,
    char_vec = c("Rachel"),
    mat = matrix(1:8, nrow = 2, ncol = 4),
    summary = summary(rnorm(1000, mean = 4))
  )

l
```

    ## $vec_numeric
    ##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
    ## 
    ## $char_vec
    ## [1] "Rachel"
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8
    ## 
    ## $summary
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.4603  3.2667  3.9897  3.9620  4.6316  7.2069

``` r
l[[1]] #gives just the first vector
```

    ##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23

``` r
l[["vec_numeric"]] #call it by name and get it returned
```

    ##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23

``` r
l$mat #another way of pulling stuff out, not the most preferred way
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8

## Make a different list

``` r
list_normals = 
  list(
    a = rnorm(30, mean = 3, sd = 1),
    b = rnorm(30, mean = 30, sd = 1),
    c = rnorm(30, mean = 3, sd = 10),
    d = rnorm(30, mean = -3, sd = 4)
  )

list_normals
```

    ## $a
    ##  [1] 2.878066 3.120427 2.882880 2.814784 2.704473 4.299191 3.921891 1.030837
    ##  [9] 2.059658 1.881662 3.014322 4.285053 4.058068 2.752923 4.219946 3.442064
    ## [17] 2.835979 2.998298 2.663670 3.143043 1.283424 3.309745 2.753276 3.998338
    ## [25] 3.367342 3.761036 2.854728 4.677508 1.830649 3.140161
    ## 
    ## $b
    ##  [1] 29.33875 29.53726 29.87878 30.38892 30.11169 31.54167 30.61458 30.77295
    ##  [9] 30.03548 29.11446 29.39720 30.45912 29.77804 29.52012 31.54120 30.23323
    ## [17] 30.77813 31.83317 30.39541 29.86358 30.94876 30.44288 30.28300 30.99703
    ## [25] 31.09658 29.57161 28.50294 30.99595 30.99200 29.51071
    ## 
    ## $c
    ##  [1]  -2.4482677   0.7143697  10.2316272   7.0106703  30.5047719   6.3586024
    ##  [7]   8.1916267  12.1739052   1.5516450  -8.8400637   7.8341985  11.9956958
    ## [13]  -1.2964849   2.2962657   2.3465406  19.5868696   0.6276294  -1.7056096
    ## [19]   5.9127571  -8.4869124   1.1109311  14.2992266  14.9600190  -2.1875511
    ## [25]   3.8677907   5.2637439  20.6959790   2.1819125  10.3631255 -12.3927205
    ## 
    ## $d
    ##  [1]   0.7613776  -4.1905495  -2.6716802   0.4659295  -0.4402579  -6.8514092
    ##  [7]  -2.0753906  -3.1189153  -2.0635446  -2.1252214  -3.7457723   2.2931519
    ## [13]  -5.8944442   3.2467639   0.8621376  -4.6811619   2.9037294  -6.1155492
    ## [19]  -2.3984468  -7.3561101   2.4555609  -0.6703851   0.6328634   1.4911528
    ## [25]  -3.5264815  -3.6551135  -2.1159828   1.2703046  -5.2943587 -10.4590520

(copy and paste the function from last time)

``` r
mean_and_sd = function(x) {
  
  if(!is.numeric(x)) {
    error("The input x should be numeric")
  } 
  
  if (length(x) < 5) {
    stop("Only compute mean and sd when the input has 5 or more values")
  }
  
  mean_x = mean(x, na.rm = TRUE)
  sd_x = sd(x, na.rm = TRUE)
  
  tibble(
    mean = mean_x,
    sd = sd_x
  )
  
}
```

``` r
mean_and_sd(list_normals[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.07 0.877

``` r
mean_and_sd(list_normals[[2]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  30.3 0.788

``` r
mean_and_sd(list_normals[[3]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.42  9.15

``` r
mean_and_sd(list_normals[[4]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.10  3.39

But this is inefficient, maybe let’s create a for loop so we don’t have
to separately type the code a bunch of time.

Use a loop to iterate!

``` r
output = vector("list", length = 4)

for (i in 1:4) {
  
  output[[i]] = mean_and_sd(list_normals[[i]])
  
}

output
```

    ## [[1]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.07 0.877
    ## 
    ## [[2]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  30.3 0.788
    ## 
    ## [[3]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.42  9.15
    ## 
    ## [[4]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.10  3.39

``` r
#same effect as code above but way more efficient
```

Use `map` to do the same thing.

``` r
output = map(list_normals, mean_and_sd) 
#syntax: output = map(input, function)

#wow! even MORE efficient!

output = map(list_normals, median) #same thing, different function
```

Check out some `map` variants

``` r
map_dfr(list_normals, mean_and_sd) #creates a dataframe out of the output
```

    ## # A tibble: 4 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.07 0.877
    ## 2 30.3  0.788
    ## 3  5.42 9.15 
    ## 4 -2.10 3.39

``` r
map_dfr(list_normals, mean_and_sd, .id = "sample") #keeps the id variable
```

    ## # A tibble: 4 × 3
    ##   sample  mean    sd
    ##   <chr>  <dbl> <dbl>
    ## 1 a       3.07 0.877
    ## 2 b      30.3  0.788
    ## 3 c       5.42 9.15 
    ## 4 d      -2.10 3.39

``` r
map_dbl(list_normals, median) #if the output will be a single number can use map_dbl
```

    ##         a         b         c         d 
    ##  3.006310 30.335959  4.565767 -2.120602

\##LIST COLUMNS

Try to put my list into a dataframe!!

``` r
#can put these 2 things into a dataframe because they both have length 4
listcol_df =
  tibble(
    name = c("a", "b", "c", "d"),
    sample = list_normals
  )
```

Did this really work?

``` r
pull(listcol_df, sample)
```

    ## $a
    ##  [1] 2.878066 3.120427 2.882880 2.814784 2.704473 4.299191 3.921891 1.030837
    ##  [9] 2.059658 1.881662 3.014322 4.285053 4.058068 2.752923 4.219946 3.442064
    ## [17] 2.835979 2.998298 2.663670 3.143043 1.283424 3.309745 2.753276 3.998338
    ## [25] 3.367342 3.761036 2.854728 4.677508 1.830649 3.140161
    ## 
    ## $b
    ##  [1] 29.33875 29.53726 29.87878 30.38892 30.11169 31.54167 30.61458 30.77295
    ##  [9] 30.03548 29.11446 29.39720 30.45912 29.77804 29.52012 31.54120 30.23323
    ## [17] 30.77813 31.83317 30.39541 29.86358 30.94876 30.44288 30.28300 30.99703
    ## [25] 31.09658 29.57161 28.50294 30.99595 30.99200 29.51071
    ## 
    ## $c
    ##  [1]  -2.4482677   0.7143697  10.2316272   7.0106703  30.5047719   6.3586024
    ##  [7]   8.1916267  12.1739052   1.5516450  -8.8400637   7.8341985  11.9956958
    ## [13]  -1.2964849   2.2962657   2.3465406  19.5868696   0.6276294  -1.7056096
    ## [19]   5.9127571  -8.4869124   1.1109311  14.2992266  14.9600190  -2.1875511
    ## [25]   3.8677907   5.2637439  20.6959790   2.1819125  10.3631255 -12.3927205
    ## 
    ## $d
    ##  [1]   0.7613776  -4.1905495  -2.6716802   0.4659295  -0.4402579  -6.8514092
    ##  [7]  -2.0753906  -3.1189153  -2.0635446  -2.1252214  -3.7457723   2.2931519
    ## [13]  -5.8944442   3.2467639   0.8621376  -4.6811619   2.9037294  -6.1155492
    ## [19]  -2.3984468  -7.3561101   2.4555609  -0.6703851   0.6328634   1.4911528
    ## [25]  -3.5264815  -3.6551135  -2.1159828   1.2703046  -5.2943587 -10.4590520

``` r
pull(listcol_df, name)
```

    ## [1] "a" "b" "c" "d"

Can I apply `mean_and_sd`??

``` r
mean_and_sd(pull(listcol_df, sample)[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.07 0.877

``` r
mean_and_sd(pull(listcol_df, sample)[[2]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  30.3 0.788

``` r
mean_and_sd(pull(listcol_df, sample)[[3]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.42  9.15

``` r
mean_and_sd(pull(listcol_df, sample)[[4]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.10  3.39

Iterate using `map`

``` r
map(pull(listcol_df, sample), mean_and_sd)
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.07 0.877
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  30.3 0.788
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.42  9.15
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.10  3.39

What if i want to add a column to my input dataframe, and that new
column has the output?

Adding a column …

``` r
listcol_df =
  listcol_df |> 
  mutate(
    summary = map(sample, mean_and_sd)
  )

pull(listcol_df, summary)
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.07 0.877
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  30.3 0.788
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.42  9.15
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.10  3.39

USEFUL!! UNNEST

``` r
listcol_df |> 
  select(-sample) |> 
  unnest(summary) #separates into 2 columns the mean and sd
```

    ## # A tibble: 4 × 3
    ##   name   mean    sd
    ##   <chr> <dbl> <dbl>
    ## 1 a      3.07 0.877
    ## 2 b     30.3  0.788
    ## 3 c      5.42 9.15 
    ## 4 d     -2.10 3.39

## Revisit NSDUH

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)

nsduh_import = function(html, table_num) {
  
  data = 
    html |> 
    html_table() |> 
    nth(table_num) |>
    slice(-1) |> 
    select(-contains("P Value")) |>
    pivot_longer(
      -State,
      names_to = "age_year", 
      values_to = "percent") |>
    separate(age_year, into = c("age", "year"), sep = "\\(") |>
    mutate(
      year = str_replace(year, "\\)", ""),
      percent = str_replace(percent, "[a-c]$", ""),
      percent = as.numeric(percent)) |>
    filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
  
  data

}

nsduh_import(nsduh_html, table_num = 1)
```

    ## # A tibble: 510 × 4
    ##    State   age   year      percent
    ##    <chr>   <chr> <chr>       <dbl>
    ##  1 Alabama 12+   2013-2014    9.98
    ##  2 Alabama 12+   2014-2015    9.6 
    ##  3 Alabama 12-17 2013-2014    9.9 
    ##  4 Alabama 12-17 2014-2015    9.71
    ##  5 Alabama 18-25 2013-2014   27.0 
    ##  6 Alabama 18-25 2014-2015   26.1 
    ##  7 Alabama 26+   2013-2014    7.1 
    ##  8 Alabama 26+   2014-2015    6.81
    ##  9 Alabama 18+   2013-2014    9.99
    ## 10 Alabama 18+   2014-2015    9.59
    ## # ℹ 500 more rows

``` r
nsduh_import(nsduh_html, table_num = 2)
```

    ## # A tibble: 510 × 4
    ##    State   age   year      percent
    ##    <chr>   <chr> <chr>       <dbl>
    ##  1 Alabama 12+   2013-2014    5.57
    ##  2 Alabama 12+   2014-2015    5.35
    ##  3 Alabama 12-17 2013-2014    4.98
    ##  4 Alabama 12-17 2014-2015    5.16
    ##  5 Alabama 18-25 2013-2014   15.0 
    ##  6 Alabama 18-25 2014-2015   14.3 
    ##  7 Alabama 26+   2013-2014    4.03
    ##  8 Alabama 26+   2014-2015    3.86
    ##  9 Alabama 18+   2013-2014    5.63
    ## 10 Alabama 18+   2014-2015    5.37
    ## # ℹ 500 more rows

``` r
nsduh_import(nsduh_html, table_num = 3)
```

    ## # A tibble: 510 × 4
    ##    State   age   year      percent
    ##    <chr>   <chr> <chr>       <dbl>
    ##  1 Alabama 12+   2013-2014    1.42
    ##  2 Alabama 12+   2014-2015    1.49
    ##  3 Alabama 12-17 2013-2014    4.46
    ##  4 Alabama 12-17 2014-2015    4.36
    ##  5 Alabama 18-25 2013-2014    6.04
    ##  6 Alabama 18-25 2014-2015    6.39
    ##  7 Alabama 26+   2013-2014    0.15
    ##  8 Alabama 26+   2014-2015    0.2 
    ##  9 Alabama 18+   2013-2014    0.95
    ## 10 Alabama 18+   2014-2015    1.05
    ## # ℹ 500 more rows

Try this with a for loop:

``` r
output = vector("list", length = 3)

for (i in 1:3) {
  
  output[[i]] = nsduh_import(nsduh_html, i)
  
}
```

Do this with `map`

``` r
map(1:3, nsduh_import, html = nsduh_html) #have to add html argument
```

    ## [[1]]
    ## # A tibble: 510 × 4
    ##    State   age   year      percent
    ##    <chr>   <chr> <chr>       <dbl>
    ##  1 Alabama 12+   2013-2014    9.98
    ##  2 Alabama 12+   2014-2015    9.6 
    ##  3 Alabama 12-17 2013-2014    9.9 
    ##  4 Alabama 12-17 2014-2015    9.71
    ##  5 Alabama 18-25 2013-2014   27.0 
    ##  6 Alabama 18-25 2014-2015   26.1 
    ##  7 Alabama 26+   2013-2014    7.1 
    ##  8 Alabama 26+   2014-2015    6.81
    ##  9 Alabama 18+   2013-2014    9.99
    ## 10 Alabama 18+   2014-2015    9.59
    ## # ℹ 500 more rows
    ## 
    ## [[2]]
    ## # A tibble: 510 × 4
    ##    State   age   year      percent
    ##    <chr>   <chr> <chr>       <dbl>
    ##  1 Alabama 12+   2013-2014    5.57
    ##  2 Alabama 12+   2014-2015    5.35
    ##  3 Alabama 12-17 2013-2014    4.98
    ##  4 Alabama 12-17 2014-2015    5.16
    ##  5 Alabama 18-25 2013-2014   15.0 
    ##  6 Alabama 18-25 2014-2015   14.3 
    ##  7 Alabama 26+   2013-2014    4.03
    ##  8 Alabama 26+   2014-2015    3.86
    ##  9 Alabama 18+   2013-2014    5.63
    ## 10 Alabama 18+   2014-2015    5.37
    ## # ℹ 500 more rows
    ## 
    ## [[3]]
    ## # A tibble: 510 × 4
    ##    State   age   year      percent
    ##    <chr>   <chr> <chr>       <dbl>
    ##  1 Alabama 12+   2013-2014    1.42
    ##  2 Alabama 12+   2014-2015    1.49
    ##  3 Alabama 12-17 2013-2014    4.46
    ##  4 Alabama 12-17 2014-2015    4.36
    ##  5 Alabama 18-25 2013-2014    6.04
    ##  6 Alabama 18-25 2014-2015    6.39
    ##  7 Alabama 26+   2013-2014    0.15
    ##  8 Alabama 26+   2014-2015    0.2 
    ##  9 Alabama 18+   2013-2014    0.95
    ## 10 Alabama 18+   2014-2015    1.05
    ## # ℹ 500 more rows

Do this all in a dataframe

``` r
nsduh_df =
  tibble(
    name = c("marj year", "marj month", "marj first"),
    number = 1:3
  ) |> 
  mutate(
    table = map(number, nsduh_import, html = nsduh_html)
  ) |> 
  unnest(table)
```
