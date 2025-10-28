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
    ##   0.994   3.327   3.965   3.970   4.579   7.830

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
    ##  [1] 4.244953 3.277846 2.455746 2.270246 2.582913 2.127383 2.767736 2.164117
    ##  [9] 4.724072 4.052759 3.737259 3.579869 3.204402 2.323891 1.746365 1.056241
    ## [17] 1.360279 2.698189 3.878257 3.326839 2.463971 2.010245 3.695785 4.397356
    ## [25] 3.434776 1.361168 3.985187 3.246568 3.136358 2.070013
    ## 
    ## $b
    ##  [1] 29.99047 31.23401 31.50667 29.11832 31.40210 30.85309 31.40435 28.88895
    ##  [9] 30.12674 31.17048 30.47070 30.51035 29.90629 29.48974 29.67628 29.55105
    ## [17] 29.74436 31.07966 31.46719 28.44017 29.05569 29.62940 32.40237 28.95886
    ## [25] 29.29083 29.33439 31.33715 30.75048 30.51235 30.90738
    ## 
    ## $c
    ##  [1]   7.9138605  -3.7302114  -4.1523111  -2.3688590   4.0265608  -6.8391402
    ##  [7]   8.5009936  -7.1371538  -4.4720380   4.8971648  -8.9840575   6.7904130
    ## [13]  10.8321506   0.1725876   7.1355527   3.3899487  -8.6849405   1.5539996
    ## [19]  -7.1497959 -19.6221529 -17.3955965  -0.8989817   2.5604893   7.4248772
    ## [25]   1.2007628  -7.2287648   6.3482529  20.5042926  -2.0590353  14.6300202
    ## 
    ## $d
    ##  [1]  -6.1885383   1.2391705  -2.4040586   3.8436065   4.5387079   2.0185011
    ##  [7]   1.7698523  -2.1882129  -9.0902770  -2.3593786  -1.2781970  -3.2945498
    ## [13]  -4.2207788  -3.7176039   1.5098438  -8.5302637  -1.9559148  -1.5528246
    ## [19]  -0.6683208  -0.9675013  -0.3567797  -5.7961805  -7.1766702  -0.9820243
    ## [25]   0.8760183   3.8275287  -4.4513608  -2.0673070 -14.1755424   3.0548299

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
    ## 1  2.91 0.963

``` r
mean_and_sd(list_normals[[2]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  30.3 0.996

``` r
mean_and_sd(list_normals[[3]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.239  8.80

``` r
mean_and_sd(list_normals[[4]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.02  4.24

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
    ## 1  2.91 0.963
    ## 
    ## [[2]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  30.3 0.996
    ## 
    ## [[3]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.239  8.80
    ## 
    ## [[4]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.02  4.24

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
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1  2.91  0.963
    ## 2 30.3   0.996
    ## 3  0.239 8.80 
    ## 4 -2.02  4.24

``` r
map_dfr(list_normals, mean_and_sd, .id = "sample") #keeps the id variable
```

    ## # A tibble: 4 × 3
    ##   sample   mean    sd
    ##   <chr>   <dbl> <dbl>
    ## 1 a       2.91  0.963
    ## 2 b      30.3   0.996
    ## 3 c       0.239 8.80 
    ## 4 d      -2.02  4.24

``` r
map_dbl(list_normals, median) #if the output will be a single number can use map_dbl
```

    ##          a          b          c          d 
    ##  2.9520466 30.2987180  0.6866752 -1.7543697

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
    ##  [1] 4.244953 3.277846 2.455746 2.270246 2.582913 2.127383 2.767736 2.164117
    ##  [9] 4.724072 4.052759 3.737259 3.579869 3.204402 2.323891 1.746365 1.056241
    ## [17] 1.360279 2.698189 3.878257 3.326839 2.463971 2.010245 3.695785 4.397356
    ## [25] 3.434776 1.361168 3.985187 3.246568 3.136358 2.070013
    ## 
    ## $b
    ##  [1] 29.99047 31.23401 31.50667 29.11832 31.40210 30.85309 31.40435 28.88895
    ##  [9] 30.12674 31.17048 30.47070 30.51035 29.90629 29.48974 29.67628 29.55105
    ## [17] 29.74436 31.07966 31.46719 28.44017 29.05569 29.62940 32.40237 28.95886
    ## [25] 29.29083 29.33439 31.33715 30.75048 30.51235 30.90738
    ## 
    ## $c
    ##  [1]   7.9138605  -3.7302114  -4.1523111  -2.3688590   4.0265608  -6.8391402
    ##  [7]   8.5009936  -7.1371538  -4.4720380   4.8971648  -8.9840575   6.7904130
    ## [13]  10.8321506   0.1725876   7.1355527   3.3899487  -8.6849405   1.5539996
    ## [19]  -7.1497959 -19.6221529 -17.3955965  -0.8989817   2.5604893   7.4248772
    ## [25]   1.2007628  -7.2287648   6.3482529  20.5042926  -2.0590353  14.6300202
    ## 
    ## $d
    ##  [1]  -6.1885383   1.2391705  -2.4040586   3.8436065   4.5387079   2.0185011
    ##  [7]   1.7698523  -2.1882129  -9.0902770  -2.3593786  -1.2781970  -3.2945498
    ## [13]  -4.2207788  -3.7176039   1.5098438  -8.5302637  -1.9559148  -1.5528246
    ## [19]  -0.6683208  -0.9675013  -0.3567797  -5.7961805  -7.1766702  -0.9820243
    ## [25]   0.8760183   3.8275287  -4.4513608  -2.0673070 -14.1755424   3.0548299

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
    ## 1  2.91 0.963

``` r
mean_and_sd(pull(listcol_df, sample)[[2]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  30.3 0.996

``` r
mean_and_sd(pull(listcol_df, sample)[[3]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.239  8.80

``` r
mean_and_sd(pull(listcol_df, sample)[[4]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.02  4.24

Iterate using `map`

``` r
map(pull(listcol_df, sample), mean_and_sd)
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.91 0.963
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  30.3 0.996
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.239  8.80
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.02  4.24
