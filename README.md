
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
remotes::install_github("courtiol/rangeR", build_vignettes = TRUE, dependencies = TRUE)
```

## How to use our data?

The data are embedded in the package, so you can use them after
installing the package (see above) as any other data frame or tibble in
R.

Here is a simple example:

``` r
library(rangeR)

sum(data_rangers$staff_total, na.rm = TRUE)
#> [1] 369059
```

## How to get the vignettes?

The different analyses we have done are stored in the package as HTML
vignettes.

To view such vignettes, simply install the package (see above) and then
type:

``` r
browseVignettes("rangeR")
```

Note: if the vignettes are not found, it is probably because you did not
successfully install the package. In particular, you must type
`build_vignettes = TRUE` while installing the package as indicated
above.
