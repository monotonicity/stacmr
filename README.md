
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stacmr

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/monotonicity/stacmr.svg?branch=master)](https://travis-ci.org/monotonicity/stacmr)
<!-- badges: end -->

The goal of stacmr is to …

## Installation

You can install the released version of stacmr from
[CRAN](https://CRAN.R-project.org) with:

``` r
## NOT YET AVAILABLE FROM CRAN
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("monotonicity/stacmr")
```

## Example

This is a basic example.

``` r
library(stacmr)
#> Loading required package: rJava
## load data from Exp. 1 of Dunn, Newell, & Kalish (2012)
data(delay)
str(delay, width = 78, strict.width = "cut")
#> 'data.frame':    520 obs. of  5 variables:
#>  $ participant: Factor w/ 130 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 1..
#>  $ delay      : Factor w/ 2 levels "no delay","delay": 2 1 1 2 2 1 1 2 2 2 ...
#>  $ structure  : Factor w/ 2 levels "rule-based","information-integration": 1..
#>  $ block      : Factor w/ 4 levels "B1","B2","B3",..: 1 1 1 1 1 1 1 1 1 1 ...
#>  $ pc         : num  0.338 0.287 0.525 0.35 0.237 ...

stats <- sta_stats(data=delay, col_value = "pc", 
          col_participant = "participant",
          col_dv = "structure", 
          col_within = "block", 
          col_between = "delay")
stats
#>   rule.based information.integration within  between N_rule.based
#> 1      0.368                   0.331     B1 no delay           34
#> 2      0.468                   0.455     B2 no delay           34
#> 3      0.576                   0.535     B3 no delay           34
#> 4      0.612                   0.549     B4 no delay           34
#> 5      0.344                   0.284     B1    delay           34
#> 6      0.443                   0.303     B2    delay           34
#> 7      0.508                   0.318     B3    delay           34
#> 8      0.517                   0.310     B4    delay           34
#>   N_information.integration
#> 1                        30
#> 2                        30
#> 3                        30
#> 4                        30
#> 5                        32
#> 6                        32
#> 7                        32
#> 8                        32
str(stats)
#> List of 2
#>  $ rule.based             :List of 10
#>   ..$ means     : Named num [1:8] 0.368 0.468 0.576 0.612 0.344 ...
#>   .. ..- attr(*, "names")= chr [1:8] "B1" "B2" "B3" "B4" ...
#>   ..$ n         : num [1:8, 1:8] 34 34 34 34 0 0 0 0 34 34 ...
#>   ..$ cov       : num [1:8, 1:8] 0.0172 0.0227 0.0174 0.0188 0 ...
#>   ..$ regcov    : num [1:8, 1:8] 0.0172 0.0215 0.0165 0.0179 0 ...
#>   ..$ shrinkage : num [1:2] 0.052 0.0314
#>   ..$ weights   : num [1:8, 1:8] 4909.2 -2498.5 287.2 -74.9 0 ...
#>   ..$ lm        : num [1:8, 1:8] 0.0131 0 0 0 0 ...
#>   ..$ nanflag   : num [1:2] 0 0
#>   ..$ bad       : num [1:2] 0 0
#>   ..$ conditions: Named chr [1:8] "no delay" "no delay" "no delay" "no delay" ...
#>   .. ..- attr(*, "names")= chr [1:8] "B1" "B2" "B3" "B4" ...
#>  $ information.integration:List of 10
#>   ..$ means     : Named num [1:8] 0.331 0.455 0.535 0.549 0.284 ...
#>   .. ..- attr(*, "names")= chr [1:8] "B1" "B2" "B3" "B4" ...
#>   ..$ n         : num [1:8, 1:8] 30 30 30 30 0 0 0 0 30 30 ...
#>   ..$ cov       : num [1:8, 1:8] 0.014 0.0177 0.0166 0.0161 0 ...
#>   ..$ regcov    : num [1:8, 1:8] 0.014 0.0169 0.0158 0.0153 0 ...
#>   ..$ shrinkage : num [1:2] 0.0491 0.265
#>   ..$ weights   : num [1:8, 1:8] 5918 -2644 -308 -204 0 ...
#>   ..$ lm        : num [1:8, 1:8] 0.00921 0 0 0 0 ...
#>   ..$ nanflag   : num [1:2] 0 0
#>   ..$ bad       : num [1:2] 0 0
#>   ..$ conditions: Named chr [1:8] "no delay" "no delay" "no delay" "no delay" ...
#>   .. ..- attr(*, "names")= chr [1:8] "B1" "B2" "B3" "B4" ...
#>  - attr(*, "varnames")= Named chr [1:5] "pc" "participant" "structure" "block" ...
#>   ..- attr(*, "names")= chr [1:5] "value" "participant" "dv" "within" ...
#>  - attr(*, "class")= chr "sta_stats"


### Fit CMR State-Trace Analysis Model
fit1 <- fit_cmr(data=delay, col_value = "pc", 
                col_participant = "participant",
                col_dv = "structure", 
                col_within = "block", 
                col_between = "delay")
cbind(fit1$x[[1]], fit1$x[[2]])
#>           [,1]      [,2]
#> [1,] 0.3751387 0.3148548
#> [2,] 0.4834385 0.4356781
#> [3,] 0.5885461 0.5165494
#> [4,] 0.6251596 0.5316393
#> [5,] 0.3351950 0.2861420
#> [6,] 0.4198553 0.3148548
#> [7,] 0.4834385 0.3256480
#> [8,] 0.4834385 0.3178311
fit1$fval
#> [1] 1.652968
fit1$shrinkage
#>            [,1]       [,2]
#> [1,] 0.05197173 0.04909396
#> [2,] 0.03135966 0.26502326


### Test CMR State-Trace Analysis
test1 <- test_cmr(data=delay, col_value = "pc", 
                  col_participant = "participant",
                  col_dv = "structure", 
                  col_within = "block", 
                  col_between = "delay")
test1
#> $p
#> [1] 0
#> 
#> $fits
#> [1] 1.257526
#> 
#> $datafit
#> [1] 1.652968
```
