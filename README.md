
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dynamicpack

<!-- badges: start -->
<!-- badges: end -->

This package was built to facilitate the analysis of individual
differences in the temporal sequence of psychological states. So far it
consists only of the resample function (more to come!). This function
destroys any signal that may be encoded in the temporal sequence of the
observations and thus creates a reference frame for the empirical data.
Specifically, the function randomly changes the order of the
observations for each participant separately for each variable of
interest. It does so to ensure that in the resampled data, there are no
relationships between these variables anymore due to the order of the
observations (i.e., temporal dependencies).

Although specifically developed for time series data, the function can
be applied to any hierarchical data where the aim is to destroy the
signal that is encoded in the order of the observations.

## Installation

dynamicpack is not yet on CRAN. But you can download it from this
repository using the devtools package as outlined below.

``` r
# install.packages("devtools")
devtools::install_github("anabelbue/dynamicpack")
```

## Example

This basic example demonstrates the resample-function. To use the
function, simply specify your data, a grouping variable (e.g., IDs of
the participants), and the variables for which you want to change the
order.

``` r
library(dynamicpack)

dat <- data.frame(participant=rep(1:10, each=10),              #grouping variable 
                  happiness = rnorm(100, mean = 3.5, sd=1.7),  #order should not be changed 
                  stress = rnorm(100, mean = 2, sd= 1.3),      #order should be changed
                  anxiety = rnorm(100, mean = 1.7, sd= 2))     #order should be changed
resampled_dat <- resample(dat, participant, c("stress", "anxiety"))

head(dat) == head(resampled_dat)
#>   participant happiness stress anxiety
#> 1        TRUE      TRUE   TRUE   FALSE
#> 2        TRUE      TRUE  FALSE   FALSE
#> 3        TRUE      TRUE  FALSE   FALSE
#> 4        TRUE      TRUE  FALSE   FALSE
#> 5        TRUE      TRUE  FALSE   FALSE
#> 6        TRUE      TRUE   TRUE   FALSE
```
