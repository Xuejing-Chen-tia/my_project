My project
================
2024-10-03

# load packages and dataset

``` r
# Set a CRAN mirror before installing packages
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Install and load the moments package
install.packages("moments")
```

    ## 
    ## The downloaded binary packages are in
    ##  /var/folders/qv/j1c01f7x2l9ctjb7ps7yhmsr0000gn/T//RtmpK2QZ7M/downloaded_packages

``` r
library(moments)
library(haven) #to download the package
library(dplyr) #re-code variable
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
load ("/Users/chenxuejing/Downloads/ICPSR_36850/DS0003/36850-0003-Data.rda")
dataset <- da36850.0003
rm(da36850.0003)
dataset <- dataset %>%
  select(PSYDIST2, SLET2)
```

``` r
#check normality assumption
# Histogram
ggplot(dataset, aes(x = PSYDIST2)) +
  geom_histogram(binwidth = 0.5, fill = 'blue', color = 'black') +
  ggtitle('Histogram of PSYDIST2') +
  xlab('PSYDIST2') +
  ylab('Frequency')
```

    ## Warning: Removed 3 rows containing non-finite outside the scale range
    ## (`stat_bin()`).

![](Myproject_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
#check equal variance assumption
# Install and load car package if not already installed
install.packages("car")
```

    ## 
    ## The downloaded binary packages are in
    ##  /var/folders/qv/j1c01f7x2l9ctjb7ps7yhmsr0000gn/T//RtmpK2QZ7M/downloaded_packages

``` r
library(car)
```

    ## Loading required package: carData

    ## 
    ## Attaching package: 'car'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     recode

``` r
# Levene's Test
leveneTest(PSYDIST2 ~ factor(SLET2), data = dataset)
```

    ## Levene's Test for Homogeneity of Variance (center = median)
    ##        Df F value    Pr(>F)    
    ## group  13  4.0818 1.593e-06 ***
    ##       717                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
ggplot(dataset, aes(x = factor(SLET2), y = PSYDIST2)) +
  geom_boxplot(fill = 'lightblue') +
  ggtitle('Boxplot of PSYDIST2 by SLET2') +
  xlab('SLET2') +
  ylab('PSYDIST2')
```

    ## Warning: Removed 3 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

![](Myproject_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

``` r
dataset$PSYDIST2_log <- log(dataset$PSYDIST2)

#check assumptions for psydist2_log
skewness(dataset$PSYDIST2_log, na.rm = TRUE)
```

    ## [1] 0.08536127

``` r
kurtosis(dataset$PSYDIST2_log, na.rm = TRUE)
```

    ## [1] 2.261098

``` r
ggplot(dataset, aes(x = factor(SLET2), y = PSYDIST2_log)) +
  geom_boxplot(fill = 'lightblue') +
  ggtitle('Boxplot of PSYDIST2_log by SLET2') +
  xlab('SLET2') +
  ylab('PSYDIST2')
```

    ## Warning: Removed 3 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

![](Myproject_files/figure-gfm/unnamed-chunk-2-3.png)<!-- -->
