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
    ##  0.9854  3.3275  3.9740  3.9995  4.6662  6.8609

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
    ##  [1] 4.224284 3.736453 1.774394 2.397276 4.926281 2.948695 3.678164 3.818325
    ##  [9] 2.035644 2.073745 2.237448 2.477589 2.972608 3.025129 2.037845 2.615543
    ## [17] 2.284954 3.218587 2.602883 3.477217 3.608516 4.056879 3.526184 4.215988
    ## [25] 3.431242 2.985255 2.000382 1.958381 3.661079 2.252947
    ## 
    ## $b
    ##  [1] 27.98045 30.49805 30.96136 28.58245 30.09344 27.56587 31.13468 30.64426
    ##  [9] 30.75027 29.67474 29.69269 31.06038 30.09272 29.86167 30.28563 31.15331
    ## [17] 30.20445 30.37626 29.53710 30.08879 28.36007 30.80570 30.31095 29.57346
    ## [25] 30.02503 30.10500 28.63113 30.20246 29.26113 30.55017
    ## 
    ## $c
    ##  [1]  -3.6981470  -6.6232603  -6.2381413  -5.1245080   3.0412376  14.3638814
    ##  [7] -19.3440049  11.4848376  -3.2802758  -2.0971146  -5.2014306   5.3324034
    ## [13]  16.2096486   5.0267298  -2.0211608  14.3156353  -6.7328984 -14.5435126
    ## [19]   4.0220176   1.1117798   6.8031548  10.2338509  -1.8560211  -0.4309565
    ## [25]  12.4707041   2.6334295  17.0057248  -5.4856339  10.4623991   9.4847550
    ## 
    ## $d
    ##  [1]   0.82351945  -0.44421572  -2.06087850  -3.62027862   1.67812139
    ##  [6]   0.31351154   0.25448438 -11.20407577  -3.85669972  -1.89897289
    ## [11]  -5.71045910  -3.63817426  -2.10585735  -9.24732378  -6.56985715
    ## [16]  -0.06080909  -0.31194217  -3.10845183   0.97962530   2.37719154
    ## [21]   1.24447550  -3.47016413  -4.22230594  -0.97953710  -9.09985553
    ## [26]   2.84558617  -4.79220707  -5.02585492  -8.21442520  -2.94800414

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
    ## 1  3.01 0.829

``` r
mean_and_sd(list_normals[[2]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  29.9 0.927

``` r
mean_and_sd(list_normals[[3]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.04  9.09

``` r
mean_and_sd(list_normals[[4]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.74  3.64

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
    ## 1  3.01 0.829
    ## 
    ## [[2]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  29.9 0.927
    ## 
    ## [[3]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.04  9.09
    ## 
    ## [[4]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.74  3.64

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
    ## 1  3.01 0.829
    ## 2 29.9  0.927
    ## 3  2.04 9.09 
    ## 4 -2.74 3.64

``` r
map_dfr(list_normals, mean_and_sd, .id = "sample") #keeps the id variable
```

    ## # A tibble: 4 × 3
    ##   sample  mean    sd
    ##   <chr>  <dbl> <dbl>
    ## 1 a       3.01 0.829
    ## 2 b      29.9  0.927
    ## 3 c       2.04 9.09 
    ## 4 d      -2.74 3.64

``` r
map_dbl(list_normals, median) #if the output will be a single number can use map_dbl
```

    ##         a         b         c         d 
    ##  2.978931 30.099217  1.872605 -2.526931

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
    ##  [1] 4.224284 3.736453 1.774394 2.397276 4.926281 2.948695 3.678164 3.818325
    ##  [9] 2.035644 2.073745 2.237448 2.477589 2.972608 3.025129 2.037845 2.615543
    ## [17] 2.284954 3.218587 2.602883 3.477217 3.608516 4.056879 3.526184 4.215988
    ## [25] 3.431242 2.985255 2.000382 1.958381 3.661079 2.252947
    ## 
    ## $b
    ##  [1] 27.98045 30.49805 30.96136 28.58245 30.09344 27.56587 31.13468 30.64426
    ##  [9] 30.75027 29.67474 29.69269 31.06038 30.09272 29.86167 30.28563 31.15331
    ## [17] 30.20445 30.37626 29.53710 30.08879 28.36007 30.80570 30.31095 29.57346
    ## [25] 30.02503 30.10500 28.63113 30.20246 29.26113 30.55017
    ## 
    ## $c
    ##  [1]  -3.6981470  -6.6232603  -6.2381413  -5.1245080   3.0412376  14.3638814
    ##  [7] -19.3440049  11.4848376  -3.2802758  -2.0971146  -5.2014306   5.3324034
    ## [13]  16.2096486   5.0267298  -2.0211608  14.3156353  -6.7328984 -14.5435126
    ## [19]   4.0220176   1.1117798   6.8031548  10.2338509  -1.8560211  -0.4309565
    ## [25]  12.4707041   2.6334295  17.0057248  -5.4856339  10.4623991   9.4847550
    ## 
    ## $d
    ##  [1]   0.82351945  -0.44421572  -2.06087850  -3.62027862   1.67812139
    ##  [6]   0.31351154   0.25448438 -11.20407577  -3.85669972  -1.89897289
    ## [11]  -5.71045910  -3.63817426  -2.10585735  -9.24732378  -6.56985715
    ## [16]  -0.06080909  -0.31194217  -3.10845183   0.97962530   2.37719154
    ## [21]   1.24447550  -3.47016413  -4.22230594  -0.97953710  -9.09985553
    ## [26]   2.84558617  -4.79220707  -5.02585492  -8.21442520  -2.94800414

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
    ## 1  3.01 0.829

``` r
mean_and_sd(pull(listcol_df, sample)[[2]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  29.9 0.927

``` r
mean_and_sd(pull(listcol_df, sample)[[3]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.04  9.09

``` r
mean_and_sd(pull(listcol_df, sample)[[4]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.74  3.64

Iterate using `map`

``` r
map(pull(listcol_df, sample), mean_and_sd)
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.01 0.829
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  29.9 0.927
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.04  9.09
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.74  3.64

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
    ## 1  3.01 0.829
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  29.9 0.927
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.04  9.09
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.74  3.64

USEFUL!! UNNEST

``` r
listcol_df |> 
  select(-sample) |> 
  unnest(summary) #separates into 2 columns the mean and sd
```

    ## # A tibble: 4 × 3
    ##   name   mean    sd
    ##   <chr> <dbl> <dbl>
    ## 1 a      3.01 0.829
    ## 2 b     29.9  0.927
    ## 3 c      2.04 9.09 
    ## 4 d     -2.74 3.64
