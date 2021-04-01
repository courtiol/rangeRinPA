
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

if (require("skimr")) {
  skim(data_rangers)
}
#> Loading required package: skimr
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | data\_rangers |
| Number of rows                                   | 254           |
| Number of columns                                | 29            |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |               |
| Column type frequency:                           |               |
| character                                        | 4             |
| numeric                                          | 25            |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |               |
| Group variables                                  | None          |

Data summary

**Variable type: character**

| skim\_variable   | n\_missing | complete\_rate | min | max | empty | n\_unique | whitespace |
|:-----------------|-----------:|---------------:|----:|----:|------:|----------:|-----------:|
| countryname\_iso |          7 |           0.97 |   3 |   3 |     0 |       246 |          0 |
| countryname\_eng |          7 |           0.97 |   3 |  44 |     0 |       247 |          0 |
| data\_year\_info |         94 |           0.63 |   4 |  18 |     0 |        21 |          0 |
| notes            |         42 |           0.83 |  17 | 131 |     0 |        70 |          0 |

**Variable type: numeric**

| skim\_variable                  | n\_missing | complete\_rate |         mean |           sd |          p0 |         p25 |          p50 |          p75 |         p100 | hist  |
|:--------------------------------|-----------:|---------------:|-------------:|-------------:|------------:|------------:|-------------:|-------------:|-------------:|:------|
| staff\_rangers\_others\_known   |        119 |           0.53 | 1.399050e+03 | 4.407870e+03 |        0.00 | 8.10000e+01 | 2.990000e+02 | 8.690000e+02 | 4.236200e+04 | ▇▁▁▁▁ |
| staff\_others\_rangers\_known   |        119 |           0.53 | 1.181600e+03 | 3.942420e+03 |        0.00 | 4.20000e+01 | 1.670000e+02 | 5.835000e+02 | 3.200000e+04 | ▇▁▁▁▁ |
| staff\_rangers\_others\_unknown |        227 |           0.11 | 2.781900e+02 | 3.715300e+02 |        0.00 | 0.00000e+00 | 1.500000e+02 | 3.845000e+02 | 1.408000e+03 | ▇▁▁▁▁ |
| staff\_total\_details\_unknown  |        245 |           0.04 | 1.314220e+03 | 1.829030e+03 |       20.00 | 6.70000e+01 | 2.590000e+02 | 2.325000e+03 | 4.797000e+03 | ▇▁▁▁▁ |
| staff\_total                    |         92 |           0.64 | 2.269920e+03 | 6.755430e+03 |        3.00 | 1.63250e+02 | 4.415000e+02 | 1.310000e+03 | 5.278000e+04 | ▇▁▁▁▁ |
| area\_PA\_surveyed              |         92 |           0.64 | 8.392648e+04 | 2.139454e+05 |       10.00 | 3.96682e+03 | 1.555450e+04 | 5.724825e+04 | 1.461913e+06 | ▇▁▁▁▁ |
| area\_PA\_total                 |         92 |           0.64 | 1.238011e+05 | 3.282236e+05 |       10.00 | 6.46725e+03 | 2.374750e+04 | 9.567400e+04 | 2.582478e+06 | ▇▁▁▁▁ |
| area\_PA\_WDPA                  |          7 |           0.97 | 8.395362e+04 | 2.722548e+05 |        0.00 | 2.34500e+02 | 7.215000e+03 | 4.638550e+04 | 2.582478e+06 | ▇▁▁▁▁ |
| area\_country                   |          7 |           0.97 | 5.440917e+05 | 1.703338e+06 |        0.00 | 1.30050e+03 | 6.450200e+04 | 3.658385e+05 | 1.687484e+07 | ▇▁▁▁▁ |
| reliability                     |         93 |           0.63 | 2.017000e+01 | 3.280000e+00 |       10.00 | 1.80000e+01 | 2.000000e+01 | 2.300000e+01 | 2.500000e+01 | ▁▃▇▇▇ |
| country\_UN\_continent          |          7 |           0.97 | 1.493200e+02 | 1.530100e+02 |        2.00 | 9.00000e+00 | 1.420000e+02 | 1.500000e+02 | 4.190000e+02 | ▇▇▁▁▅ |
| country\_UN\_subcontinent       |          7 |           0.97 | 5.555000e+01 | 5.375000e+01 |        5.00 | 1.40000e+01 | 3.000000e+01 | 6.100000e+01 | 1.550000e+02 | ▇▂▁▁▃ |
| area\_forest\_pct               |         49 |           0.81 | 3.235000e+01 | 2.379000e+01 |        0.00 | 1.11800e+01 | 3.270000e+01 | 4.838000e+01 | 9.826000e+01 | ▇▆▅▂▁ |
| EVI                             |         79 |           0.69 | 4.748000e+01 | 1.198000e+01 |       23.60 | 3.90000e+01 | 4.570000e+01 | 5.455000e+01 | 7.640000e+01 | ▂▇▆▃▂ |
| SPI                             |         99 |           0.61 | 7.324000e+01 | 2.606000e+01 |        0.00 | 5.93500e+01 | 8.100000e+01 | 9.375000e+01 | 1.000000e+02 | ▁▁▂▃▇ |
| EPI\_2020                       |         75 |           0.70 | 4.653000e+01 | 1.545000e+01 |       22.60 | 3.44000e+01 | 4.400000e+01 | 5.500000e+01 | 8.250000e+01 | ▆▇▅▂▃ |
| GDP\_capita                     |         46 |           0.82 | 1.776544e+04 | 2.626768e+04 |      126.92 | 2.27209e+03 | 7.190030e+03 | 2.311128e+04 | 1.858290e+05 | ▇▁▁▁▁ |
| GDP\_2019                       |         54 |           0.79 | 4.284617e+11 | 1.902014e+12 | 47271463.33 | 5.56417e+09 | 2.605902e+10 | 2.076599e+11 | 2.137440e+13 | ▇▁▁▁▁ |
| GPD\_growth                     |         46 |           0.82 | 2.470000e+00 | 3.320000e+00 |      -19.61 | 1.12000e+00 | 2.400000e+00 | 4.430000e+00 | 9.760000e+00 | ▁▁▁▇▅ |
| unemployment                    |         52 |           0.80 | 7.750000e+00 | 5.670000e+00 |        0.11 | 3.69000e+00 | 6.080000e+00 | 1.029000e+01 | 2.867000e+01 | ▇▅▂▁▁ |
| rural\_pct                      |         43 |           0.83 | 3.911000e+01 | 2.373000e+01 |        0.00 | 1.94800e+01 | 3.810000e+01 | 5.763000e+01 | 8.675000e+01 | ▇▇▇▆▅ |
| income                          |         74 |           0.71 | 1.121783e+04 | 1.507081e+04 |      205.60 | 1.46697e+03 | 4.461110e+03 | 1.354048e+04 | 6.601907e+04 | ▇▁▁▁▁ |
| pop\_density                    |         44 |           0.83 | 3.393200e+02 | 1.515500e+03 |        0.14 | 3.73700e+01 | 9.118000e+01 | 2.195700e+02 | 1.919600e+04 | ▇▁▁▁▁ |
| IUCN\_1\_4\_prop                |         39 |           0.85 | 6.800000e-01 | 3.600000e-01 |        0.00 | 4.10000e-01 | 8.500000e-01 | 1.000000e+00 | 1.000000e+00 | ▃▁▂▂▇ |
| IUCN\_1\_2\_prop                |         41 |           0.84 | 4.000000e-01 | 3.700000e-01 |        0.00 | 2.00000e-02 | 3.300000e-01 | 7.400000e-01 | 1.000000e+00 | ▇▃▂▂▅ |

``` r
## more to come
```
