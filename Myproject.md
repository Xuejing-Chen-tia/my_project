My project
================
Tiana
2024-10-03

# load packages and dataset

``` r
library(haven) #to download the package
library(dplyr) #recode variable
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
library(ggplot2)

#hope it's the new one, yes it is 
load ("/Users/chenxuejing/Downloads/ICPSR_36850/DS0005/36850-0005-Data.rda")
dataset <- da36850.0005
rm(da36850.0005)
```
