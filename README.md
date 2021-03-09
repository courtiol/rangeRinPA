
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rangeR

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/rangeR)](https://CRAN.R-project.org/package=rangeR)
<!-- badges: end -->

The goal of rangeR is to provide data about the total number of people
working in Protected Areas (PAs) on the planet, as well as identifying
what contributes to variation in the workforce between PAs.

## Installation

You can install the development version of rangeR from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("courtiol/rangeR")
```

## Example

``` r
library(rangeR)

data_rangers <- fetch_data_rangers()

if (require("skimr")) {
  skim(data_rangers)
}

## more to come
```
