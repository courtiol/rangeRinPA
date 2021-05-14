library(spaMM)
library(rangeRinPA)
library(tidyverse)

Ncpu <- 100
rep_feature_select <- 1000
rep_finetune <- 1000


# Step 1 + 2: General data preparation & preparation of initial training datasets

formula_rangers_full <- staff_rangers_log ~ PA_area_log + lat + long + area_country_log + area_forest_pct + pop_density_log + GDP_2019_log + GDP_capita_log +
  GDP_growth + unemployment_log + EVI + SPI + EPI_2020 + IUCN_1_4_prop + IUCN_1_2_prop + Matern(1|long + lat)

formula_others_full <- update(formula_rangers_full, staff_others_log ~ .)
formula_all_full <- update(formula_rangers_full, staff_total_log ~ .)

data_initial_training_rangers_complete <- build_initial_training_data(data_rangers, formula = formula_rangers_full, survey = "complete_known")
data_initial_training_others_complete  <- build_initial_training_data(data_rangers, formula = formula_others_full, survey = "complete_known")
data_initial_training_all_complete     <- build_initial_training_data(data_rangers, formula = formula_all_full, survey = "complete_known")
data_initial_training_rangers_partial  <- build_initial_training_data(data_rangers, formula = formula_rangers_full, survey = "partial_known")
data_initial_training_others_partial   <- build_initial_training_data(data_rangers, formula = formula_others_full, survey = "partial_known")
data_initial_training_all_partial      <- build_initial_training_data(data_rangers, formula = formula_all_full, survey = "partial_known")

dim(data_initial_training_rangers_complete)
dim(data_initial_training_others_complete)
dim(data_initial_training_all_complete)
dim(data_initial_training_rangers_partial)
dim(data_initial_training_others_partial)
dim(data_initial_training_all_partial)


# Step 3: Selection of predictor variables

fit_data_initial_training_rangers_complete_full <- fitme(formula_rangers_full,
                                                         data = data_initial_training_rangers_complete,
                                                         control.dist = list(dist.method = "Earth"))
fit_data_initial_training_others_complete_full <- fitme(formula_others_full,
                                                        data = data_initial_training_others_complete,
                                                        control.dist = list(dist.method = "Earth"))
fit_data_initial_training_all_complete_full <- fitme(formula_all_full,
                                                     data = data_initial_training_all_complete,
                                                     control.dist = list(dist.method = "Earth"))
fit_data_initial_training_rangers_partial_full <- fitme(formula_rangers_full,
                                                        data = data_initial_training_rangers_partial,
                                                        control.dist = list(dist.method = "Earth"))
fit_data_initial_training_others_partial_full <- fitme(formula_others_full,
                                                       data = data_initial_training_others_partial,
                                                       control.dist = list(dist.method = "Earth"))
fit_data_initial_training_all_partial_full <- fitme(formula_all_full,
                                                    data = data_initial_training_all_partial,
                                                    control.dist = list(dist.method = "Earth"))

selection_training_rangers_complete <- feature_selection_LMM(
  full_fit = fit_data_initial_training_rangers_complete_full,
  rep = rep_feature_select, Ncpu = Ncpu,
  target = "staff_rangers_log",
  seed = 123)

selection_training_others_complete <- feature_selection_LMM(
  full_fit = fit_data_initial_training_others_complete_full,
  rep = rep_feature_select, Ncpu = Ncpu,
  target = "staff_others_log",
  seed = 123)

selection_training_all_complete <- feature_selection_LMM(
  full_fit = fit_data_initial_training_all_complete_full,
  rep = rep_feature_select, Ncpu = Ncpu,
  target = "staff_total_log",
  seed = 123)

selection_training_rangers_partial <- feature_selection_LMM(
  full_fit = fit_data_initial_training_rangers_partial_full,
  rep = rep_feature_select, Ncpu = Ncpu,
  target = "staff_rangers_log",
  seed = 123)

selection_training_others_partial <- feature_selection_LMM(
  full_fit = fit_data_initial_training_others_partial_full,
  rep = rep_feature_select, Ncpu = Ncpu,
  target = "staff_others_log",
  seed = 123)

selection_training_all_partial <- feature_selection_LMM(
  full_fit = fit_data_initial_training_all_partial_full,
  rep = rep_feature_select, Ncpu = Ncpu,
  target = "staff_total_log",
  seed = 123)

selection_training_rangers_complete
# A tibble: 32 x 6
#       k Matern  RMSE RMSE_tol formula                                                                                                                rep
#    <int> <lgl>  <dbl>    <dbl> <chr>                                                                                                                <int>
# 1     4 FALSE   1.73    0     staff_rangers_log ~ area_country_log + pop_density_log + EPI_2020 + long                                              1000
# 2     2 FALSE   1.74    0.582 staff_rangers_log ~ area_country_log + pop_density_log                                                                1000
# 3     5 FALSE   1.74    0.876 staff_rangers_log ~ area_country_log + pop_density_log + EPI_2020 + long + IUCN_1_2_prop                              1000
# 4     2 TRUE    1.75    1.08  staff_rangers_log ~ area_country_log + pop_density_log + Matern(1 | long + lat)                                       1000
# 5     4 TRUE    1.76    1.57  staff_rangers_log ~ area_country_log + pop_density_log + EPI_2020 + long + Matern(1 | long + lat)                     1000
# 6     3 FALSE   1.76    1.64  staff_rangers_log ~ area_country_log + pop_density_log + long                                                         1000
# 7     3 TRUE    1.77    2.14  staff_rangers_log ~ area_country_log + pop_density_log + long + Matern(1 | long + lat)                                1000
# 8     5 TRUE    1.78    2.73  staff_rangers_log ~ area_country_log + pop_density_log + EPI_2020 + long + IUCN_1_2_prop + Matern(1 | long + lat)     1000
# 9     6 FALSE   1.85    6.79  staff_rangers_log ~ pop_density_log + area_country_log + long + EPI_2020 + IUCN_1_2_prop + GDP_growth                 1000
# 10    7 FALSE   1.88    8.88  staff_rangers_log ~ long + EPI_2020 + IUCN_1_2_prop + GDP_growth + area_country_log + pop_density_log + GDP_2019_log  1000
selected_formula_rangers_complete <- staff_rangers_log ~ area_country_log + pop_density_log + EPI_2020 + long # top
#selected_formula_rangers_complete <- staff_rangers_log ~ area_country_log + pop_density_log # 2nd, 0.582% less good

selection_training_others_complete
# A tibble: 32 x 6
#       k Matern  RMSE RMSE_tol formula                                                                                                                                                             rep
#    <int> <lgl>  <dbl>    <dbl> <chr>                                                                                                                                                             <int>
# 1     2 TRUE    2.42    0     staff_others_log ~ area_country_log + pop_density_log + Matern(1 | long + lat)                                                                                     1000
# 2     3 TRUE    2.43    0.525 staff_others_log ~ area_country_log + pop_density_log + EVI + Matern(1 | long + lat)                                                                               1000
# 3     7 TRUE    2.52    4.15  staff_others_log ~ area_country_log + pop_density_log + EPI_2020 + EVI + area_forest_pct + GDP_growth + unemployment_log + Matern(1 | long + lat)                  1000
# 4     1 TRUE    2.54    5.04  staff_others_log ~ area_country_log + Matern(1 | long + lat)                                                                                                       1000
# 5     5 TRUE    2.56    5.86  staff_others_log ~ area_country_log + pop_density_log + EVI + EPI_2020 + unemployment_log + Matern(1 | long + lat)                                                 1000
# 6     4 TRUE    2.59    7.36  staff_others_log ~ area_country_log + pop_density_log + EVI + EPI_2020 + Matern(1 | long + lat)                                                                    1000
# 7     8 TRUE    2.61    8.20  staff_others_log ~ pop_density_log + EPI_2020 + area_forest_pct + area_country_log + GDP_growth + unemployment_log + EVI + IUCN_1_2_prop + Matern(1 | long + lat)  1000
# 8     6 TRUE    2.68   11.1   staff_others_log ~ area_country_log + pop_density_log + EPI_2020 + EVI + unemployment_log + area_forest_pct + Matern(1 | long + lat)                               1000
# 9     8 FALSE   2.70   11.7   staff_others_log ~ pop_density_log + EPI_2020 + area_country_log + area_forest_pct + GDP_growth + IUCN_1_2_prop + unemployment_log + EVI                           1000
# 10    3 FALSE   2.71   12.1   staff_others_log ~ area_country_log + pop_density_log + EPI_2020                                                                                                   1000
selected_formula_others_complete <- staff_others_log ~ area_country_log + pop_density_log + Matern(1 | long + lat) # top

selection_training_all_complete
# A tibble: 32 x 6
#       k Matern  RMSE RMSE_tol formula                                                                                                                                 rep
#     <int> <lgl>  <dbl>    <dbl> <chr>                                                                                                                                 <int>
# 1     5 TRUE    1.75    0     staff_total_log ~ area_country_log + pop_density_log + EPI_2020 + long + EVI + Matern(1 | long + lat)                                  1000
# 2     5 FALSE   1.75    0.338 staff_total_log ~ area_country_log + pop_density_log + EPI_2020 + long + EVI                                                           1000
# 3     6 TRUE    1.78    2.08  staff_total_log ~ area_country_log + pop_density_log + EPI_2020 + EVI + long + IUCN_1_2_prop + Matern(1 | long + lat)                  1000
# 4     6 FALSE   1.82    4.38  staff_total_log ~ pop_density_log + area_country_log + EPI_2020 + long + EVI + IUCN_1_2_prop                                           1000
# 5     4 FALSE   1.84    5.05  staff_total_log ~ area_country_log + pop_density_log + EPI_2020 + long                                                                 1000
# 6     3 FALSE   1.84    5.23  staff_total_log ~ area_country_log + pop_density_log + EPI_2020                                                                        1000
# 7     7 TRUE    1.84    5.34  staff_total_log ~ area_country_log + pop_density_log + EPI_2020 + EVI + long + IUCN_1_2_prop + IUCN_1_4_prop + Matern(1 | long + lat)  1000
# 8     3 TRUE    1.89    7.99  staff_total_log ~ area_country_log + pop_density_log + EPI_2020 + Matern(1 | long + lat)                                               1000
# 9     7 FALSE   1.90    8.76  staff_total_log ~ EPI_2020 + long + EVI + IUCN_1_2_prop + area_country_log + pop_density_log + GDP_2019_log                            1000
# 10    2 FALSE   1.93   10.6   staff_total_log ~ area_country_log + pop_density_log                                                                                   1000
selected_formula_all_complete <- staff_total_log ~ area_country_log + pop_density_log + EPI_2020 + long + EVI + Matern(1 | long + lat) # top

selection_training_rangers_partial
# A tibble: 32 x 6
#       k Matern  RMSE RMSE_tol formula                                                                                                                                 rep
#     <int> <lgl>  <dbl>    <dbl> <chr>                                                                                                                                 <int>
# 1     4 FALSE   3.17    0     staff_rangers_log ~ pop_density_log + PA_area_log + area_country_log + long                                                            1000
# 2     5 FALSE   3.17    0.112 staff_rangers_log ~ pop_density_log + PA_area_log + area_country_log + long + IUCN_1_2_prop                                            1000
# 3     6 FALSE   3.19    0.574 staff_rangers_log ~ pop_density_log + PA_area_log + area_country_log + long + IUCN_1_2_prop + lat                                      1000
# 4     3 FALSE   3.19    0.681 staff_rangers_log ~ pop_density_log + PA_area_log + area_country_log                                                                   1000
# 5     7 FALSE   3.21    1.30  staff_rangers_log ~ pop_density_log + PA_area_log + area_country_log + long + IUCN_1_2_prop + lat + unemployment_log                   1000
# 6     4 TRUE    3.22    1.55  staff_rangers_log ~ pop_density_log + PA_area_log + area_country_log + long + Matern(1 | long + lat)                                   1000
# 7     5 TRUE    3.23    1.87  staff_rangers_log ~ pop_density_log + PA_area_log + area_country_log + long + IUCN_1_2_prop + Matern(1 | long + lat)                   1000
# 8     3 TRUE    3.23    2.00  staff_rangers_log ~ pop_density_log + PA_area_log + area_country_log + Matern(1 | long + lat)                                          1000
# 9     6 TRUE    3.23    2.05  staff_rangers_log ~ pop_density_log + PA_area_log + area_country_log + long + IUCN_1_2_prop + lat + Matern(1 | long + lat)             1000
# 10    8 FALSE   3.24    2.26  staff_rangers_log ~ pop_density_log + PA_area_log + area_country_log + long + lat + IUCN_1_2_prop + GDP_capita_log + unemployment_log  1000
selected_formula_rangers_partial <- staff_rangers_log ~ pop_density_log + PA_area_log + area_country_log + long # top

selection_training_others_partial
# A tibble: 32 x 6
#       k Matern  RMSE RMSE_tol formula                                                                                                                                                                                                                                              rep
#     <int> <lgl>  <dbl>    <dbl> <chr>                                                                                                                                                                                                                                              <int>
# 1     6 TRUE    4.34    0     staff_others_log ~ area_country_log + pop_density_log + GDP_capita_log + IUCN_1_4_prop + GDP_growth + area_forest_pct + Matern(1 | long + lat)                                                                                                      1000
# 2     5 TRUE    4.36    0.341 staff_others_log ~ area_country_log + pop_density_log + GDP_capita_log + IUCN_1_4_prop + GDP_growth + Matern(1 | long + lat)                                                                                                                        1000
# 3     4 TRUE    4.36    0.345 staff_others_log ~ area_country_log + GDP_capita_log + pop_density_log + IUCN_1_4_prop + Matern(1 | long + lat)                                                                                                                                     1000
# 4     7 TRUE    4.39    0.997 staff_others_log ~ area_country_log + pop_density_log + IUCN_1_4_prop + GDP_growth + GDP_capita_log + area_forest_pct + EVI + Matern(1 | long + lat)                                                                                                1000
# 5     3 TRUE    4.43    2.02  staff_others_log ~ area_country_log + pop_density_log + GDP_capita_log + Matern(1 | long + lat)                                                                                                                                                     1000
# 6     8 TRUE    4.46    2.63  staff_others_log ~ area_country_log + pop_density_log + IUCN_1_4_prop + GDP_growth + GDP_capita_log + EVI + area_forest_pct + EPI_2020 + Matern(1 | long + lat)                                                                                     1000
# 7     7 FALSE   4.47    2.85  staff_others_log ~ area_country_log + GDP_capita_log + pop_density_log + area_forest_pct + GDP_growth + long + IUCN_1_4_prop                                                                                                                        1000
# 8     6 FALSE   4.47    2.91  staff_others_log ~ area_country_log + GDP_capita_log + pop_density_log + IUCN_1_4_prop + area_forest_pct + GDP_growth                                                                                                                               1000
# 9     8 FALSE   4.50    3.62  staff_others_log ~ area_country_log + pop_density_log + area_forest_pct + GDP_growth + GDP_capita_log + long + IUCN_1_4_prop + IUCN_1_2_prop                                                                                                        1000
# 10     5 FALSE   4.50    3.64  staff_others_log ~ area_country_log + pop_density_log + GDP_capita_log + IUCN_1_4_prop + GDP_growth                                                                                                                                                 1000
# 11     9 TRUE    4.50    3.71  staff_others_log ~ area_country_log + pop_density_log + IUCN_1_4_prop + GDP_growth + GDP_capita_log + EVI + area_forest_pct + EPI_2020 + SPI + Matern(1 | long + lat)                                                                               1000
# 12    10 TRUE    4.52    4.03  staff_others_log ~ area_country_log + pop_density_log + GDP_growth + GDP_capita_log + IUCN_1_4_prop + EVI + area_forest_pct + EPI_2020 + SPI + long + Matern(1 | long + lat)                                                                        1000
# 13     4 FALSE   4.52    4.20  staff_others_log ~ area_country_log + GDP_capita_log + pop_density_log + IUCN_1_4_prop                                                                                                                                                              1000
# 14     9 FALSE   4.54    4.54  staff_others_log ~ area_country_log + pop_density_log + area_forest_pct + GDP_growth + GDP_capita_log + long + IUCN_1_4_prop + IUCN_1_2_prop + EPI_2020                                                                                             1000
# 15     3 FALSE   4.55    4.88  staff_others_log ~ area_country_log + pop_density_log + GDP_capita_log                                                                                                                                                                              1000
# 16    10 FALSE   4.56    5.09  staff_others_log ~ area_country_log + pop_density_log + area_forest_pct + GDP_growth + long + IUCN_1_4_prop + GDP_capita_log + EPI_2020 + IUCN_1_2_prop + unemployment_log                                                                          1000
# 17    11 TRUE    4.61    6.19  staff_others_log ~ area_country_log + pop_density_log + GDP_growth + GDP_capita_log + IUCN_1_4_prop + EVI + area_forest_pct + EPI_2020 + SPI + long + IUCN_1_2_prop + Matern(1 | long + lat)                                                        1000
# 18    11 FALSE   4.62    6.40  staff_others_log ~ area_country_log + pop_density_log + GDP_growth + area_forest_pct + long + GDP_capita_log + IUCN_1_4_prop + IUCN_1_2_prop + EPI_2020 + unemployment_log + EVI                                                                    1000
# 19    12 TRUE    4.64    6.89  staff_others_log ~ area_country_log + pop_density_log + GDP_growth + GDP_capita_log + IUCN_1_4_prop + EVI + area_forest_pct + EPI_2020 + SPI + long + IUCN_1_2_prop + unemployment_log + Matern(1 | long + lat)                                     1000
# 20    13 TRUE    4.67    7.61  staff_others_log ~ pop_density_log + area_country_log + GDP_growth + GDP_capita_log + IUCN_1_4_prop + EVI + area_forest_pct + EPI_2020 + SPI + long + IUCN_1_2_prop + unemployment_log + lat + Matern(1 | long + lat)                               1000
# 21    12 FALSE   4.68    7.74  staff_others_log ~ area_country_log + pop_density_log + area_forest_pct + GDP_growth + long + GDP_capita_log + IUCN_1_4_prop + IUCN_1_2_prop + EPI_2020 + unemployment_log + EVI + SPI                                                              1000
# 22    13 FALSE   4.72    8.79  staff_others_log ~ area_forest_pct + GDP_growth + long + area_country_log + IUCN_1_4_prop + pop_density_log + IUCN_1_2_prop + EPI_2020 + unemployment_log + GDP_capita_log + EVI + SPI + lat                                                        1000
# 23    14 TRUE    4.73    9.00  staff_others_log ~ GDP_growth + IUCN_1_4_prop + EVI + area_forest_pct + area_country_log + EPI_2020 + SPI + long + pop_density_log + IUCN_1_2_prop + GDP_capita_log + unemployment_log + lat + PA_area_log + Matern(1 | long + lat)                 1000
# 24     2 TRUE    4.78   10.2   staff_others_log ~ area_country_log + pop_density_log + Matern(1 | long + lat)                                                                                                                                                                      1000
# 25     1 TRUE    4.96   14.2   staff_others_log ~ area_country_log + Matern(1 | long + lat)                                                                                                                                                                                        1000
# 26     2 FALSE   5.00   15.1   staff_others_log ~ area_country_log + pop_density_log                                                                                                                                                                                               1000
selected_formula_others_partial <- staff_others_log ~ area_country_log + pop_density_log + GDP_capita_log + IUCN_1_4_prop + GDP_growth + area_forest_pct + Matern(1 | long + lat) # top
#selected_formula_others_partial <- staff_others_log ~ area_country_log + pop_density_log + GDP_capita_log + Matern(1 | long + lat) ## not quite as good (ca. 2% higher RMSE)
#selected_formula_others_partial <- staff_others_log ~ area_country_log + pop_density_log + Matern(1 | long + lat) ## not quite as good (ca. 10% higher RMSE) but complete

selection_training_all_partial
# A tibble: 32 x 6
#       k Matern  RMSE RMSE_tol formula                                                                                                                                                                                                                                             rep
#      <int> <lgl>  <dbl>    <dbl> <chr>                                                                                                                                                                                                                                             <int>
# 1     7 FALSE   3.38   0      staff_total_log ~ pop_density_log + area_country_log + GDP_capita_log + PA_area_log + IUCN_1_2_prop + long + GDP_growth                                                                                                                            1000
# 2     6 FALSE   3.38   0.0324 staff_total_log ~ pop_density_log + area_country_log + GDP_capita_log + PA_area_log + IUCN_1_2_prop + long                                                                                                                                         1000
# 3     6 TRUE    3.39   0.205  staff_total_log ~ pop_density_log + area_country_log + PA_area_log + GDP_capita_log + IUCN_1_2_prop + GDP_growth + Matern(1 | long + lat)                                                                                                          1000
# 4     7 TRUE    3.39   0.282  staff_total_log ~ pop_density_log + area_country_log + PA_area_log + GDP_capita_log + IUCN_1_2_prop + GDP_growth + long + Matern(1 | long + lat)                                                                                                   1000
# 5     8 TRUE    3.39   0.445  staff_total_log ~ pop_density_log + area_country_log + GDP_capita_log + PA_area_log + IUCN_1_2_prop + GDP_growth + EVI + long + Matern(1 | long + lat)                                                                                             1000
# 6     8 FALSE   3.41   0.835  staff_total_log ~ pop_density_log + area_country_log + GDP_capita_log + PA_area_log + IUCN_1_2_prop + long + GDP_growth + SPI                                                                                                                      1000
# 7     9 TRUE    3.41   0.918  staff_total_log ~ pop_density_log + area_country_log + GDP_capita_log + PA_area_log + IUCN_1_2_prop + GDP_growth + EVI + long + SPI + Matern(1 | long + lat)                                                                                       1000
# 8     5 TRUE    3.42   1.21   staff_total_log ~ pop_density_log + area_country_log + PA_area_log + GDP_capita_log + IUCN_1_2_prop + Matern(1 | long + lat)                                                                                                                       1000
# 9     5 FALSE   3.42   1.27   staff_total_log ~ pop_density_log + area_country_log + PA_area_log + GDP_capita_log + long                                                                                                                                                         1000
# 10     4 TRUE    3.43   1.51   staff_total_log ~ pop_density_log + area_country_log + PA_area_log + GDP_capita_log + Matern(1 | long + lat)                                                                                                                                       1000
# 11     9 FALSE   3.43   1.54   staff_total_log ~ pop_density_log + area_country_log + GDP_capita_log + PA_area_log + IUCN_1_2_prop + long + GDP_growth + SPI + EVI                                                                                                                1000
# 12    10 TRUE    3.44   1.96   staff_total_log ~ pop_density_log + area_country_log + GDP_capita_log + PA_area_log + GDP_growth + IUCN_1_2_prop + EVI + long + SPI + area_forest_pct + Matern(1 | long + lat)                                                                     1000
# 13    10 FALSE   3.45   2.20   staff_total_log ~ pop_density_log + area_country_log + GDP_capita_log + PA_area_log + long + IUCN_1_2_prop + GDP_growth + SPI + area_forest_pct + EVI                                                                                              1000
# 14    11 TRUE    3.47   2.59   staff_total_log ~ pop_density_log + area_country_log + PA_area_log + GDP_capita_log + GDP_growth + IUCN_1_2_prop + long + EVI + SPI + area_forest_pct + IUCN_1_4_prop + Matern(1 | long + lat)                                                     1000
# 15     4 FALSE   3.47   2.76   staff_total_log ~ pop_density_log + area_country_log + PA_area_log + GDP_capita_log                                                                                                                                                                1000
# 16    11 FALSE   3.48   2.98   staff_total_log ~ pop_density_log + area_country_log + GDP_capita_log + PA_area_log + long + IUCN_1_2_prop + GDP_growth + SPI + area_forest_pct + EVI + IUCN_1_4_prop                                                                              1000
# 17    12 TRUE    3.50   3.49   staff_total_log ~ pop_density_log + area_country_log + PA_area_log + GDP_capita_log + GDP_growth + IUCN_1_2_prop + long + EVI + SPI + IUCN_1_4_prop + area_forest_pct + EPI_2020 + Matern(1 | long + lat)                                          1000
# 18     3 TRUE    3.50   3.64   staff_total_log ~ pop_density_log + area_country_log + PA_area_log + Matern(1 | long + lat)                                                                                                                                                        1000
# 19    12 FALSE   3.51   3.78   staff_total_log ~ pop_density_log + area_country_log + GDP_capita_log + PA_area_log + long + IUCN_1_2_prop + GDP_growth + SPI + area_forest_pct + EVI + IUCN_1_4_prop + unemployment_log                                                           1000
# 20    13 TRUE    3.52   4.08   staff_total_log ~ pop_density_log + area_country_log + PA_area_log + GDP_capita_log + GDP_growth + IUCN_1_2_prop + long + EVI + SPI + IUCN_1_4_prop + area_forest_pct + EPI_2020 + lat + Matern(1 | long + lat)                                    1000
# 21     3 FALSE   3.53   4.55   staff_total_log ~ pop_density_log + area_country_log + PA_area_log                                                                                                                                                                                 1000
# 22    13 FALSE   3.53   4.63   staff_total_log ~ PA_area_log + long + IUCN_1_2_prop + GDP_growth + SPI + pop_density_log + area_country_log + area_forest_pct + EVI + GDP_capita_log + IUCN_1_4_prop + unemployment_log + lat                                                     1000
# 23    14 TRUE    3.54   4.85   staff_total_log ~ PA_area_log + GDP_growth + IUCN_1_2_prop + long + EVI + SPI + pop_density_log + area_country_log + IUCN_1_4_prop + area_forest_pct + GDP_capita_log + EPI_2020 + lat + unemployment_log + Matern(1 | long + lat)                 1000
# 24     2 TRUE    3.65   7.90   staff_total_log ~ pop_density_log + PA_area_log + Matern(1 | long + lat)                                                                                                                                                                           1000
# 25     2 FALSE   3.65   7.96   staff_total_log ~ pop_density_log + PA_area_log
selected_formula_all_partial <- staff_total_log ~ pop_density_log + area_country_log + GDP_capita_log + PA_area_log + IUCN_1_2_prop + long + GDP_growth # top
#selected_formula_all_partial <- staff_total_log ~ pop_density_log + area_country_log + PA_area_log ## not quite as good (ca. 4% higher RMSE) but complete


# Step 4: Preparation of final training datasets

data_final_training_rangers_complete <- build_final_training_data(
  data = data_rangers,
  formula = selected_formula_rangers_complete,
  survey = "complete_known")

data_final_training_others_complete <- build_final_training_data(
  data = data_rangers,
  formula = selected_formula_others_complete,
  survey = "complete_known")

data_final_training_all_complete <- build_final_training_data(
  data = data_rangers,
  formula = selected_formula_all_complete,
  survey = "complete_known")

data_final_training_rangers_partial <- build_final_training_data(
  data = data_rangers,
  formula = selected_formula_rangers_partial,
  survey = "partial_known")

data_final_training_others_partial <- build_final_training_data(
  data = data_rangers,
  formula = selected_formula_others_partial,
  survey = "partial_known")

data_final_training_all_partial <- build_final_training_data(
  data = data_rangers,
  formula = selected_formula_all_partial,
  survey = "partial_known")

dim(data_final_training_rangers_complete)
dim(data_final_training_others_complete)
dim(data_final_training_all_complete)
dim(data_final_training_rangers_partial)
dim(data_final_training_others_partial)
dim(data_final_training_all_partial)


# Step 5: Selection of function inputs (fine tuning)

finetune_rangers_complete <- finetune_LMM(selected_formula_rangers_complete,
                                          data = data_final_training_rangers_complete,
                                          rep = rep_finetune, Ncpu = Ncpu, seed = 123)

finetune_others_complete <- finetune_LMM(selected_formula_others_complete,
                                         data = data_final_training_others_complete,
                                         rep = rep_finetune, Ncpu = Ncpu, seed = 123)

finetune_all_complete <- finetune_LMM(selected_formula_all_complete,
                                      data = data_final_training_all_complete,
                                      rep = rep_finetune, Ncpu = Ncpu, seed = 123)

finetune_rangers_partial <- finetune_LMM(selected_formula_rangers_partial,
                                         data = data_final_training_rangers_partial,
                                         rep = rep_finetune, Ncpu = Ncpu, seed = 123)

finetune_others_partial <- finetune_LMM(selected_formula_others_partial,
                                        data = data_final_training_others_partial,
                                        rep = rep_finetune, Ncpu = Ncpu, seed = 123)

finetune_all_partial <- finetune_LMM(selected_formula_all_partial,
                                     data = data_final_training_all_partial,
                                     rep = rep_finetune, Ncpu = Ncpu, seed = 123)

finetune_rangers_complete # LM so not different
finetune_others_complete # REML
# $result_mean
# method type  rep     RMSE          ME      MAE         R2      RSE       CCC     MoranI Moran_pv_freq
# 1     ML   CV 1000 2.800581 -0.03583233 1.142045 -0.9042363 1.228519 0.4938368 -0.3185715         0.009
# 2   REML   CV 1000 2.771354 -0.03822241 1.126223 -0.8695190 1.218618 0.5048549 -0.3171278         0.008
#
# $result_sd
# method type  rep     RMSE        ME       MAE       R2       RSE       CCC    MoranI Moran_pv_freq
# 1     ML   CV 1000 1.175591 0.7759994 0.4714104 6.184516 0.9503006 0.3812209 0.1957997    0.09448771
# 2   REML   CV 1000 1.141758 0.7663811 0.4585962 6.092539 0.9320366 0.3807204 0.1958178    0.08912881
#
# $best_method
# [1] "REML"
finetune_all_complete # ML
# $result_mean
# method type  rep     RMSE          ME       MAE        R2      RSE       CCC     MoranI Moran_pv_freq
# 1     ML   CV 1000 2.526831 -0.05979592 0.9033021 0.1718632 1.015819 0.6338095 -0.2361023         0.043
# 2   REML   CV 1000 2.584488 -0.06557363 0.9242719 0.1333124 1.037190 0.6278426 -0.2347172         0.043
#
# $result_sd
# method type  rep     RMSE        ME       MAE       R2       RSE       CCC    MoranI Moran_pv_freq
# 1     ML   CV 1000 1.124349 0.5822261 0.3550570 1.000871 0.8245667 0.3636520 0.1858654     0.2029586
# 2   REML   CV 1000 1.138423 0.5926689 0.3632274 1.043909 0.8372351 0.3669557 0.1861482     0.2029586
#
# $best_method
# [1] "ML"
finetune_rangers_partial # LM so not different
finetune_others_partial # REML
# $result_mean
# method type  rep     RMSE          ME      MAE        R2      RSE       CCC      MoranI Moran_pv_freq
# 1     ML   CV 1000 4.625005 -0.02060995 1.065032 0.3907751 1.180471 0.5106794 -0.07820611         0.070
# 2   REML   CV 1000 4.600350 -0.02284916 1.055494 0.3988577 1.173704 0.5174869 -0.07956496         0.065
#
# $result_sd
# method type  rep      RMSE        ME       MAE        R2       RSE       CCC    MoranI Moran_pv_freq
# 1     ML   CV 1000 0.9895077 0.4214806 0.2348099 0.3438028 0.8613625 0.1884420 0.1151123     0.2552747
# 2   REML   CV 1000 0.9892260 0.4194900 0.2348990 0.3347664 0.8596352 0.1884176 0.1150503     0.2466492
#
# $best_method
# [1] "REML"
finetune_all_partial # LM so not different


# Step 6: Final training

fit_final_rangers_complete <- fitme(selected_formula_rangers_complete,
                                    data = data_final_training_rangers_complete,
                                    control.dist = list(dist.method = "Earth"),
                                    method = finetune_rangers_complete$best_method)

fit_final_others_complete <- fitme(selected_formula_others_complete,
                                   data = data_final_training_others_complete,
                                   control.dist = list(dist.method = "Earth"),
                                   method = finetune_others_complete$best_method)

fit_final_all_complete <- fitme(selected_formula_all_complete,
                                data = data_final_training_all_complete,
                                control.dist = list(dist.method = "Earth"),
                                method = finetune_all_complete$best_method)

fit_final_rangers_partial <- fitme(selected_formula_rangers_partial,
                                   data = data_final_training_rangers_partial,
                                   control.dist = list(dist.method = "Earth"),
                                   method = finetune_rangers_partial$best_method)

fit_final_others_partial <- fitme(selected_formula_others_partial,
                                  data = data_final_training_others_partial,
                                  control.dist = list(dist.method = "Earth"),
                                  method = finetune_others_partial$best_method)

fit_final_all_partial <- fitme(selected_formula_all_partial,
                               data = data_final_training_all_partial,
                               control.dist = list(dist.method = "Earth"),
                               method = finetune_all_partial$best_method)


# Step 7: Preparation of datasets for predictions & simulations

data_final_pred_rangers_complete <- build_final_pred_data(
  data = data_rangers,
  formula = selected_formula_rangers_complete,
  survey = "complete_known")

data_final_pred_others_complete <- build_final_pred_data( # 1 country missing -> Greenland
  data = data_rangers,
  formula = selected_formula_others_complete,
  survey = "complete_known")

data_final_pred_all_complete <- build_final_pred_data(
  data = data_rangers,
  formula = selected_formula_all_complete,
  survey = "complete_known")

data_final_pred_rangers_partial <- build_final_pred_data(
  data = data_rangers,
  formula = selected_formula_rangers_partial,
  survey = "partial_known")

data_final_pred_others_partial <- build_final_pred_data(  # 1 country missing -> Greenland
  data = data_rangers,
  formula = selected_formula_others_partial,
  survey = "partial_known")

data_final_pred_all_partial <- build_final_pred_data(
  data = data_rangers,
  formula = selected_formula_all_partial,
  survey = "partial_known")


# Step 8a: Point predictions

data_final_pred_rangers_complete$data_predictable$staff_rangers_log_predicted <- predict(
  fit_final_rangers_complete, newdata = data_final_pred_rangers_complete$data_predictable)[, 1]
compute_tally(data_final_pred_rangers_complete)
#         sum     value
# 1  observed  50362.00
# 2 predicted  81716.86
# 3     total 132078.86
point_prediction_rangers_complete <- compute_tally(data_final_pred_rangers_complete)[3, "value"]

data_final_pred_others_complete$data_predictable$staff_others_log_predicted <- predict(
  fit_final_others_complete, newdata = data_final_pred_others_complete$data_predictable)[, 1]
compute_tally(data_final_pred_others_complete)
point_prediction_others_complete <- compute_tally(data_final_pred_others_complete)[3, "value"]

data_final_pred_all_complete$data_predictable$staff_total_log_predicted <- predict(
  fit_final_all_complete, newdata = data_final_pred_all_complete$data_predictable)[, 1]
compute_tally(data_final_pred_all_complete)
#         sum    value
# 1  observed 45242.00
# 2 predicted 36349.94
# 3     total 81591.94
point_prediction_all_complete <- compute_tally(data_final_pred_all_complete)[3, "value"]

data_final_pred_rangers_partial$data_predictable$staff_rangers_log_predicted <- predict(
  fit_final_rangers_partial, newdata = data_final_pred_rangers_partial$data_predictable)[, 1]
compute_tally(data_final_pred_rangers_partial)
#        sum     value
#1  observed 196099.00
#2 predicted  62653.94
#3     total 258752.94
point_prediction_rangers_partial <- compute_tally(data_final_pred_rangers_partial)[3, "value"]

data_final_pred_others_partial$data_predictable$staff_others_log_predicted <- predict(
  fit_final_others_partial, newdata = data_final_pred_others_partial$data_predictable)[, 1]
compute_tally(data_final_pred_others_partial)
#         sum     value
# 1  observed 165119.00
# 2 predicted  76365.41
# 3     total 241484.41
point_prediction_others_partial <- compute_tally(data_final_pred_others_partial)[3, "value"]

data_final_pred_all_partial$data_predictable$staff_total_log_predicted <- predict(
  fit_final_all_partial, newdata = data_final_pred_all_partial$data_predictable)[, 1]
compute_tally(data_final_pred_all_partial)
#         sum    value
# 1  observed 369059.0
# 2 predicted 107845.2
# 3     total 476904.2
point_prediction_all_partial <- compute_tally(data_final_pred_all_partial)[3, "value"]


# Step 8b: Simulations

predictions_rangers_complete <- parallel::mclapply(1:1000, function(i) {
  data_final_pred_rangers_complete$data_predictable$staff_rangers_log_predicted <- simulate(
    fit_final_rangers_complete, newdata = data_final_pred_rangers_complete$data_predictable,
    type = "predVar", variances = list(linPred = TRUE, disp = TRUE), verbose = FALSE)
  compute_tally(data_final_pred_rangers_complete)[3, "value"]}, mc.cores = Ncpu, mc.set.seed = 123)
quantile(unlist(predictions_rangers_complete), probs = c(0.025, 0.975))
#     2.5%    97.5%
# 143069.3 213158.2

predictions_others_complete <- parallel::mclapply(1:1000, function(i) {
  data_final_pred_others_complete$data_predictable$staff_others_log_predicted <- simulate(
    fit_final_others_complete, newdata = data_final_pred_others_complete$data_predictable,
    type = "predVar", variances = list(linPred = TRUE, disp = TRUE), verbose = FALSE)
  compute_tally(data_final_pred_others_complete)[3, "value"]}, mc.cores = Ncpu, mc.set.seed = 123)
quantile(unlist(predictions_others_complete), probs = c(0.025, 0.975))
#     2.5%    97.5%
# 109369.9 179821.7

predictions_all_complete <- parallel::mclapply(1:1000, function(i) {
  data_final_pred_all_complete$data_predictable$staff_total_log_predicted <- simulate(
    fit_final_all_complete, newdata = data_final_pred_all_complete$data_predictable,
    type = "predVar", variances = list(linPred = TRUE, disp = TRUE), verbose = FALSE)
  compute_tally(data_final_pred_all_complete)[3, "value"]}, mc.cores = Ncpu, mc.set.seed = 123)
quantile(unlist(predictions_all_complete), probs = c(0.025, 0.975))
#     2.5%    97.5%
# 263774.1 419088.7

predictions_rangers_partial <- parallel::mclapply(1:1000, function(i) {
  data_final_pred_rangers_partial$data_predictable$staff_rangers_log_predicted <- simulate(
    fit_final_rangers_partial, newdata = data_final_pred_rangers_partial$data_predictable,
    type = "predVar", variances = list(linPred = TRUE, disp = TRUE), verbose = FALSE)
  compute_tally(data_final_pred_rangers_partial)[3, "value"]}, mc.cores = Ncpu, mc.set.seed = 123)
quantile(unlist(predictions_rangers_partial), probs = c(0.025, 0.975))
#     2.5%    97.5%
# 267119.4 311374.1

predictions_others_partial <- parallel::mclapply(1:1000, function(i) {
  data_final_pred_others_partial$data_predictable$staff_others_log_predicted <- simulate(
    fit_final_others_partial, newdata = data_final_pred_others_partial$data_predictable,
    type = "predVar", variances = list(linPred = TRUE, disp = TRUE), verbose = FALSE)
  compute_tally(data_final_pred_others_partial)[3, "value"]}, mc.cores = Ncpu, mc.set.seed = 123)
quantile(unlist(predictions_others_partial), probs = c(0.025, 0.975))
#     2.5%    97.5%
# 254060.4 524283.7

predictions_all_partial <- parallel::mclapply(1:1000, function(i) {
  data_final_pred_all_partial$data_predictable$staff_total_log_predicted <- simulate(
    fit_final_all_partial, newdata = data_final_pred_all_partial$data_predictable,
    type = "predVar", variances = list(linPred = TRUE, disp = TRUE), verbose = FALSE)
  compute_tally(data_final_pred_all_partial)[3, "value"]}, mc.cores = Ncpu, mc.set.seed = 123)
quantile(unlist(predictions_all_partial), probs = c(0.025, 0.975))
#     2.5%    97.5%
# 487392.2 634185.0
