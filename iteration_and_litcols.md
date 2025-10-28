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
    ##  0.9827  3.2705  3.9615  3.9615  4.6767  7.2954

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
    ##  [1] 3.4307221 3.6923498 3.7756681 3.7528092 1.6415993 3.6232384 3.6494200
    ##  [8] 3.9183974 2.2784622 3.6325469 3.7453789 1.3341924 3.7900674 1.3226736
    ## [15] 2.1785805 2.5706461 4.6741498 0.9387029 4.7513643 3.4690240 3.9334896
    ## [22] 3.6499722 0.7860502 3.8249247 2.6891155 2.1907957 3.3791218 0.1891279
    ## [29] 1.7576130 2.6114045
    ## 
    ## $b
    ##  [1] 29.08109 29.17661 30.57817 30.02284 29.40106 29.90337 30.04810 29.66436
    ##  [9] 30.29849 30.23429 30.14716 29.14969 29.55722 29.76581 29.38388 28.98787
    ## [17] 30.31514 28.00047 29.00495 30.31244 29.43618 30.22306 28.93668 30.84774
    ## [25] 30.01153 31.54278 29.92732 30.04175 30.24149 31.10573
    ## 
    ## $c
    ##  [1]  -4.783992   4.102018  -8.030588 -17.892004 -10.904774   4.094484
    ##  [7]  10.349584 -11.058980  10.854552   1.948362  12.317872   3.893953
    ## [13]  19.146033   0.833442   6.528029  10.116755   2.317453  -9.174028
    ## [19]  13.115898  -7.478252  21.453470  -8.495047  -7.152087   1.619551
    ## [25]  -6.637212  19.907374  21.559928   3.617490   2.047878 -12.023769
    ## 
    ## $d
    ##  [1]  -7.0404187  -5.9746999  -1.8856903  -6.2028397  -2.7860515  -7.1141163
    ##  [7]  -5.5677957  -4.6047202 -10.7368424   1.2620819   0.4560287   5.9190323
    ## [13]   0.5178080  -9.5962482  -1.9884681  -3.8161085   0.2576635   1.0724133
    ## [19]  -0.4710153   2.8997495  -0.6646933  -4.6212122  -5.7744379  -7.0057123
    ## [25]   1.7833478  -3.8915071  -2.0686864  -2.0505952  -0.6773501  -5.4260160

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
    ## 1  2.91  1.19

``` r
mean_and_sd(list_normals[[2]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  29.8 0.725

``` r
mean_and_sd(list_normals[[3]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.21  10.9

``` r
mean_and_sd(list_normals[[4]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.86  3.83

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
    ## 1  2.91  1.19
    ## 
    ## [[2]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  29.8 0.725
    ## 
    ## [[3]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.21  10.9
    ## 
    ## [[4]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.86  3.83

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
