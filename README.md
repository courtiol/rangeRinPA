
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rangeRinPA

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/rangeR)](https://CRAN.R-project.org/package=rangeR)
<!-- badges: end -->

The goal of rangeRinPA is to provide data about the total number of
people working in Protected Areas (PAs) on the planet, as well as
identifying what contributes to variation in the workforce between PAs.

Note: this package was previously called rangeR but we changed the name
since rangeRinPA is now using another package called ranger (to run
random forests) with which it conflicted due to the similar name.

## Installation

### Option 1

You can install the development version of rangeRinPA from
[GitHub](https://github.com/) by clicking on the green button above with
the label **“Code”** and then on **Download ZIP** (do not unzip it!).

Then, in R type:

``` r
# install.packages("remotes")
remotes::install_local(path = file.choose(), dependencies = TRUE, build = TRUE, build_vignettes = TRUE)
```

It will open a windows for you to select the file `rangeR-master.zip`
that you just have downloaded.

### Option 2

You can also install the development version of rangeRinPA with:

``` r
# install.packages("remotes")
remotes::install_github("courtiol/rangeR", dependencies = TRUE, build_vignettes = TRUE)
```

One difficulty however is that since this is a private GitHub
repository, you may have to set up your R system so that it “knows” you
GitHub credentials.

If you really want to do that, you first need to create a **Personal
access tokens** in your GitHub settings (see [here for how
to](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token).

Then you need to add your newly created token into your .Renviron file,
which you can access using `usethis::edit_r_environ()` (you need to
install the package `{usethis}`, if you don’t have it).

Your .Renviron file should thus have a line as the following (with a
different set of letters and numbers):

``` txt
GITHUB_PAT = 21b10aXXXXXXXXXXXXXXXXXXXXXdc4c276
```

## How to access the vignettes?

The different analyses we have done are stored in the package as HTML
vignettes.

To view such vignettes, simply install the package (see above) and then
type:

``` r
browseVignettes("rangeRinPA")
```

Note: if the vignettes are not found, it is probably because you did not
successfully install the package. In particular, you must type
`build_vignettes = TRUE` while installing the package as indicated
above.

## How to use our data?

The data are embedded in the package, so you can use them after
installing the package (see above) as any other data frame or tibble in
R.

Here is a simple example:

``` r
library(rangeRinPA)

sum(data_rangers$staff_total, na.rm = TRUE)
#> [1] 369059
```
