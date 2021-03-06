
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rangeRinPA

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/rangeR)](https://CRAN.R-project.org/package=rangeR)
<!-- badges: end -->

The goal of rangeRinPA is to reproduce most results from the paper
“Protected Area Personnel and Ranger Numbers are Insufficient to Deliver
Global Expectations” by Appleton et al. (submitted).

## Installation

### Option 1

You can install rangeRinPA as any other R package stored on GitHub
using:

``` r
# install.packages("remotes")
remotes::install_github("courtiol/rangeRinPA", dependencies = TRUE, build_vignettes = TRUE)
```

### Option 2

You can also install rangeRinPA by clicking on the green button above
with the label **“Code”** and then on **Download ZIP** (do not unzip
it!).

Then, in R type:

``` r
# install.packages("remotes")
remotes::install_local(path = file.choose(), dependencies = TRUE, build = TRUE, build_vignettes = TRUE)
```

It will open a windows for you to select the file `rangeR-master.zip`
that you just have downloaded.

(Note: the same also applies to released zip files stored on Zenodo)

## Usage

Follow the steps documented in the help file which is available via the
following command:

``` r
?rangeRinPA
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
#> [1] 337877
```

## Environment

Here is the information about the R session used to produce the results
presented in the paper:

``` text
R version 4.1.2 (2021-11-01)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Debian GNU/Linux 11 (bullseye)

Matrix products: default
BLAS:   /usr/lib/x86_64-linux-gnu/atlas/libblas.so.3.10.3
LAPACK: /usr/lib/x86_64-linux-gnu/atlas/liblapack.so.3.10.3

locale:
 [1] LC_CTYPE=en_GB.UTF-8       LC_NUMERIC=C               LC_TIME=en_GB.UTF-8        LC_COLLATE=en_GB.UTF-8     LC_MONETARY=en_GB.UTF-8   
 [6] LC_MESSAGES=en_GB.UTF-8    LC_PAPER=en_GB.UTF-8       LC_NAME=C                  LC_ADDRESS=C               LC_TELEPHONE=C            
[11] LC_MEASUREMENT=en_GB.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] rangeRinPA_2021.12.29 testthat_3.1.1        cachem_1.0.6          memoise_2.0.1        

loaded via a namespace (and not attached):
 [1] rnaturalearth_0.1.0 pkgload_1.2.4       tidyr_1.1.4         spelling_2.2        R.utils_2.11.0      assertthat_0.2.1    countrycode_1.3.0  
 [8] sp_1.4-6            googlesheets4_1.0.0 cellranger_1.1.0    yaml_2.2.1          remotes_2.4.2       slam_0.1-49         ggrepel_0.9.1      
[15] sessioninfo_1.2.2   numDeriv_2016.8-1.1 pillar_1.6.4        lattice_0.20-45     glue_1.6.0          digest_0.6.29       minqa_1.2.4        
[22] colorspace_2.0-2    htmltools_0.5.2     Matrix_1.4-0        R.oo_1.24.0         spaMM_3.9.25        pkgconfig_2.0.3     devtools_2.4.3     
[29] purrr_0.3.4         patchwork_1.1.1     scales_1.1.1        ranger_0.13.1       processx_3.5.2      tibble_3.1.6        proxy_0.4-26       
[36] googledrive_2.0.0   generics_0.1.1      ggplot2_3.3.5       usethis_2.1.5       ellipsis_0.3.2      withr_2.4.3         pbapply_1.5-0      
[43] cli_3.1.0           magrittr_2.0.1      crayon_1.4.2        evaluate_0.14       ps_1.6.0            R.methodsS3_1.8.1   fs_1.5.2           
[50] fansi_0.5.0         nlme_3.1-153        MASS_7.3-54         forcats_0.5.1       xml2_1.3.3          class_7.3-19        pkgbuild_1.3.1     
[57] tools_4.1.2         registry_0.5-1      hunspell_3.0.1      prettyunits_1.1.1   gargle_1.2.0        lifecycle_1.0.1     ROI_1.0-0          
[64] munsell_0.5.0       ggsci_2.9           callr_3.7.0         compiler_4.1.2      e1071_1.7-9         rlang_0.4.12        classInt_0.4-3     
[71] units_0.7-2         grid_4.1.2          rstudioapi_0.13     rmarkdown_2.11      boot_1.3-28         gtable_0.3.0        DBI_1.1.2          
[78] R6_2.5.1            knitr_1.37          dplyr_1.0.7         fastmap_1.1.0       utf8_1.2.2          commonmark_1.7      rprojroot_2.0.2    
[85] KernSmooth_2.23-20  desc_1.4.0          ape_5.6             parallel_4.1.2      Rcpp_1.0.7          vctrs_0.3.8         sf_1.0-5           
[92] xfun_0.29           tidyselect_1.1.1 
```
