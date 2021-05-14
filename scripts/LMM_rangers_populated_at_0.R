library(spaMM)
library(rangeRinPA)
library(tidyverse)

Ncpu <- 100
rep_feature_select <- 1000
rep_finetune <- 1000

data_rangers <- fill_PA_area(data_rangers, coef = 0)

# Step 1 + 2: General data preparation & preparation of initial training datasets

formula_rangers_full <- staff_rangers_log ~ PA_area_log + lat + long + area_country_log + area_forest_pct + pop_density_log + GDP_2019_log + GDP_capita_log +
  GDP_growth + unemployment_log + EVI + SPI + EPI_2020 + IUCN_1_4_prop + IUCN_1_2_prop + Matern(1|long + lat)

formula_others_full <- update(formula_rangers_full, staff_others_log ~ .)
formula_all_full <- update(formula_rangers_full, staff_total_log ~ .)

data_initial_training_rangers <- build_initial_training_data(data_rangers, formula = formula_rangers_full, survey = "complete_known")
data_initial_training_others  <- build_initial_training_data(data_rangers, formula = formula_others_full, survey = "complete_known")
data_initial_training_all     <- build_initial_training_data(data_rangers, formula = formula_all_full, survey = "complete_known")

dim(data_initial_training_rangers)
dim(data_initial_training_others)
dim(data_initial_training_all)
# [1] 120  19
# [1] 106  19
# [1] 127  19

# Step 3: Selection of predictor variables

fit_data_initial_training_rangers_full <- fitme(formula_rangers_full,
                                                data = data_initial_training_rangers,
                                                control.dist = list(dist.method = "Earth"))
fit_data_initial_training_others_full <- fitme(formula_others_full,
                                               data = data_initial_training_others,
                                               control.dist = list(dist.method = "Earth"))
fit_data_initial_training_all_full <- fitme(formula_all_full,
                                            data = data_initial_training_all,
                                            control.dist = list(dist.method = "Earth"))

selection_training_rangers <- feature_selection_LMM(
  full_fit = fit_data_initial_training_rangers_full,
  rep = rep_feature_select, Ncpu = Ncpu,
  target = "staff_rangers_log",
  seed = 123)

selection_training_others <- feature_selection_LMM(
  full_fit = fit_data_initial_training_others_full,
  rep = rep_feature_select, Ncpu = Ncpu,
  target = "staff_others_log",
  seed = 123)

selection_training_all <- feature_selection_LMM(
  full_fit = fit_data_initial_training_all_full,
  rep = rep_feature_select, Ncpu = Ncpu,
  target = "staff_total_log",
  seed = 123)

selection_training_rangers
# A tibble: 32 x 6
#       k Matern  RMSE RMSE_tol formula                                                                                                                                            rep
#       <int> <lgl>  <dbl>    <dbl> <chr>                                                                                                                                            <int>
# 1     5 FALSE   3.35   0      staff_rangers_log ~ pop_density_log + area_country_log + PA_area_log + long + IUCN_1_2_prop                                                       1000
# 2     4 FALSE   3.35   0.0119 staff_rangers_log ~ pop_density_log + area_country_log + PA_area_log + long                                                                       1000
# 3     6 FALSE   3.36   0.317  staff_rangers_log ~ pop_density_log + area_country_log + PA_area_log + IUCN_1_2_prop + long + lat                                                 1000
# 4     3 FALSE   3.36   0.383  staff_rangers_log ~ pop_density_log + area_country_log + PA_area_log                                                                              1000
# 5     7 FALSE   3.38   0.929  staff_rangers_log ~ pop_density_log + area_country_log + PA_area_log + IUCN_1_2_prop + long + lat + unemployment_log                              1000
# 6     8 FALSE   3.40   1.53   staff_rangers_log ~ pop_density_log + area_country_log + PA_area_log + IUCN_1_2_prop + long + lat + GDP_2019_log + unemployment_log               1000
# 7     2 FALSE   3.42   1.95   staff_rangers_log ~ pop_density_log + area_country_log                                                                                            1000
# 8     9 FALSE   3.42   2.07   staff_rangers_log ~ pop_density_log + area_country_log + PA_area_log + long + lat + GDP_2019_log + IUCN_1_2_prop + unemployment_log + GDP_growth  1000
# 9     6 TRUE    3.42   2.20   staff_rangers_log ~ pop_density_log + area_country_log + PA_area_log + IUCN_1_2_prop + long + lat + Matern(1 | long + lat)                        1000
# 10    5 TRUE    3.42   2.22   staff_rangers_log ~ pop_density_log + area_country_log + PA_area_log + long + IUCN_1_2_prop + Matern(1 | long + lat)                              1000

selected_formula_rangers <- staff_rangers_log ~ pop_density_log + area_country_log + PA_area_log + long

selection_training_others
# A tibble: 32 x 6
#       k Matern  RMSE RMSE_tol formula                                                                                                                                                                                rep
#       <int> <lgl>  <dbl>    <dbl> <chr>                                                                                                                                                                                <int>
# 1     6 TRUE    4.34    0     staff_others_log ~ area_country_log + pop_density_log + GDP_capita_log + IUCN_1_4_prop + GDP_growth + area_forest_pct + Matern(1 | long + lat)                                        1000
# 2     5 TRUE    4.36    0.341 staff_others_log ~ area_country_log + pop_density_log + GDP_capita_log + IUCN_1_4_prop + GDP_growth + Matern(1 | long + lat)                                                          1000
# 3     4 TRUE    4.36    0.345 staff_others_log ~ area_country_log + GDP_capita_log + pop_density_log + IUCN_1_4_prop + Matern(1 | long + lat)                                                                       1000
# 4     7 TRUE    4.39    0.997 staff_others_log ~ area_country_log + pop_density_log + IUCN_1_4_prop + GDP_growth + GDP_capita_log + area_forest_pct + EVI + Matern(1 | long + lat)                                  1000
# 5     3 TRUE    4.43    2.02  staff_others_log ~ area_country_log + pop_density_log + GDP_capita_log + Matern(1 | long + lat)                                                                                       1000
# 6     8 TRUE    4.46    2.63  staff_others_log ~ pop_density_log + area_country_log + IUCN_1_4_prop + GDP_growth + GDP_capita_log + EVI + area_forest_pct + EPI_2020 + Matern(1 | long + lat)                       1000
# 7     7 FALSE   4.47    2.85  staff_others_log ~ area_country_log + GDP_capita_log + pop_density_log + area_forest_pct + GDP_growth + long + IUCN_1_4_prop                                                          1000
# 8     6 FALSE   4.47    2.91  staff_others_log ~ area_country_log + GDP_capita_log + pop_density_log + IUCN_1_4_prop + area_forest_pct + GDP_growth                                                                 1000
# 9     9 TRUE    4.47    2.93  staff_others_log ~ pop_density_log + area_country_log + IUCN_1_4_prop + GDP_growth + GDP_capita_log + EVI + area_forest_pct + EPI_2020 + PA_area_log + Matern(1 | long + lat)         1000
# 10   10 TRUE    4.49    3.31  staff_others_log ~ pop_density_log + area_country_log + IUCN_1_4_prop + GDP_growth + GDP_capita_log + EVI + PA_area_log + EPI_2020 + area_forest_pct + long + Matern(1 | long + lat)  1000

selected_formula_others <- staff_others_log ~ area_country_log + pop_density_log + GDP_capita_log + IUCN_1_4_prop + GDP_growth + area_forest_pct + Matern(1 | long + lat)

selection_training_all
# A tibble: 32 x 6
#        k Matern  RMSE RMSE_tol formula                                                                                                                                                        rep
#        <int> <lgl>  <dbl>    <dbl> <chr>                                                                                                                                                        <int>
# 1     7 FALSE   3.43   0      staff_total_log ~ pop_density_log + GDP_capita_log + area_country_log + IUCN_1_2_prop + long + PA_area_log + GDP_growth                                       1000
# 2     7 TRUE    3.43   0.0808 staff_total_log ~ pop_density_log + area_country_log + GDP_capita_log + PA_area_log + IUCN_1_2_prop + GDP_growth + long + Matern(1 | long + lat)              1000
# 3     6 TRUE    3.44   0.181  staff_total_log ~ pop_density_log + area_country_log + GDP_capita_log + PA_area_log + IUCN_1_2_prop + GDP_growth + Matern(1 | long + lat)                     1000
# 4     6 FALSE   3.44   0.225  staff_total_log ~ pop_density_log + GDP_capita_log + area_country_log + PA_area_log + IUCN_1_2_prop + long                                                    1000
# 5     8 TRUE    3.44   0.361  staff_total_log ~ pop_density_log + area_country_log + GDP_capita_log + IUCN_1_2_prop + GDP_growth + PA_area_log + EVI + long + Matern(1 | long + lat)        1000
# 6     8 FALSE   3.47   1.11   staff_total_log ~ pop_density_log + area_country_log + GDP_capita_log + IUCN_1_2_prop + long + PA_area_log + GDP_growth + SPI                                 1000
# 7     9 TRUE    3.47   1.14   staff_total_log ~ pop_density_log + area_country_log + GDP_capita_log + IUCN_1_2_prop + GDP_growth + PA_area_log + EVI + long + SPI + Matern(1 | long + lat)  1000
# 8     5 TRUE    3.48   1.37   staff_total_log ~ pop_density_log + area_country_log + GDP_capita_log + PA_area_log + IUCN_1_2_prop + Matern(1 | long + lat)                                  1000
# 9     4 TRUE    3.48   1.48   staff_total_log ~ pop_density_log + area_country_log + GDP_capita_log + PA_area_log + Matern(1 | long + lat)                                                  1000
# 10    9 FALSE   3.49   1.78   staff_total_log ~ pop_density_log + area_country_log + GDP_capita_log + IUCN_1_2_prop + long + GDP_growth + PA_area_log + SPI + EVI                           1000

selected_formula_all <- staff_total_log ~ pop_density_log + GDP_capita_log + area_country_log + IUCN_1_2_prop + long + PA_area_log + GDP_growth


# Step 4: Preparation of final training datasets

data_final_training_rangers <- build_final_training_data(data = data_rangers,
                                                         formula = selected_formula_rangers,
                                                         survey = "complete_known")

data_final_training_others <- build_final_training_data(data = data_rangers,
                                                        formula = selected_formula_others,
                                                        survey = "complete_known")

data_final_training_all <- build_final_training_data(data = data_rangers,
                                                     formula = selected_formula_all,
                                                     survey = "complete_known")


dim(data_final_training_rangers)
dim(data_final_training_others)
dim(data_final_training_all)
# [1] 143   8
# [1] 119  12
# [1] 142  11


# Step 5: Selection of function inputs (fine tuning)

finetune_rangers <- finetune_LMM(selected_formula_rangers,
                                 data = data_final_training_rangers,
                                 rep = rep_finetune, Ncpu = Ncpu, seed = 123)

finetune_others <- finetune_LMM(selected_formula_others,
                                data = data_final_training_others,
                                rep = rep_finetune, Ncpu = Ncpu, seed = 123)

finetune_all <- finetune_LMM(selected_formula_all,
                             data = data_final_training_all,
                             rep = rep_finetune, Ncpu = Ncpu, seed = 123)


finetune_rangers  # LM so not different
finetune_others
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
finetune_all  # LM so not different


# Step 6: Final training

fit_final_rangers <- fitme(selected_formula_rangers,
                           data = data_final_training_rangers,
                           control.dist = list(dist.method = "Earth"),
                           method = finetune_rangers$best_method)

fit_final_others <- fitme(selected_formula_others,
                          data = data_final_training_others,
                          control.dist = list(dist.method = "Earth"),
                          method = finetune_others$best_method)

fit_final_all <- fitme(selected_formula_all,
                       data = data_final_training_all,
                       control.dist = list(dist.method = "Earth"),
                       method = finetune_all$best_method)


# Step 7: Preparation of datasets for predictions & simulations

data_final_pred_rangers <- build_final_pred_data(
  data = data_rangers,
  formula = selected_formula_rangers,
  survey = "complete_known")

data_final_pred_others <- build_final_pred_data( # 1 country missing -> Greenland
  data = data_rangers,
  formula = selected_formula_others,
  survey = "complete_known")

data_final_pred_all <- build_final_pred_data(
  data = data_rangers,
  formula = selected_formula_all,
  survey = "complete_known")


# Step 8a: Point predictions

data_final_pred_rangers$data_predictable$staff_rangers_log_predicted <- predict(
  fit_final_rangers, newdata = data_final_pred_rangers$data_predictable)[, 1]
compute_tally(data_final_pred_rangers)
# sum      value
# 1  observed 196099.000
# 2 predicted   8985.553
# 3     total 205084.553
point_prediction_rangers <- compute_tally(data_final_pred_rangers)[3, "value"]

data_final_pred_others$data_predictable$staff_others_log_predicted <- predict(
  fit_final_others, newdata = data_final_pred_others$data_predictable)[, 1]
compute_tally(data_final_pred_others)
# sum     value
# 1  observed 165119.00
# 2 predicted  12263.76
# 3     total 177382.76
point_prediction_others <- compute_tally(data_final_pred_others)[3, "value"]

data_final_pred_all$data_predictable$staff_total_log_predicted <- predict(
  fit_final_all, newdata = data_final_pred_all$data_predictable)[, 1]
compute_tally(data_final_pred_all)
# sum      value
# 1  observed 369059.000
# 2 predicted   6278.864
# 3     total 375337.864
point_prediction_all <- compute_tally(data_final_pred_all)[3, "value"]


# Step 8b: Simulations

predictions_rangers <- parallel::mclapply(1:10000, function(i) {
  data_final_pred_rangers$data_predictable$staff_rangers_log_predicted <- simulate(
    fit_final_rangers, newdata = data_final_pred_rangers$data_predictable,
    type = "predVar", variances = list(linPred = TRUE, disp = TRUE), verbose = FALSE)
  compute_tally(data_final_pred_rangers)[3, "value"]}, mc.cores = Ncpu)
quantile(unlist(predictions_rangers), probs = c(0.025, 0.975))
#   2.5%    97.5%
#   203997.4 223540.6

predictions_others <- parallel::mclapply(1:10000, function(i) {
  data_final_pred_others$data_predictable$staff_others_log_predicted <- simulate(
    fit_final_others, newdata = data_final_pred_others$data_predictable,
    type = "predVar", variances = list(linPred = TRUE, disp = TRUE), verbose = FALSE)
  compute_tally(data_final_pred_others)[3, "value"]}, mc.cores = Ncpu)
quantile(unlist(predictions_others), probs = c(0.025, 0.975))
# 2.5%    97.5%
# 177024.7 241496.1

predictions_all <- parallel::mclapply(1:10000, function(i) {
  data_final_pred_all$data_predictable$staff_total_log_predicted <- simulate(
    fit_final_all, newdata = data_final_pred_all$data_predictable,
    type = "predVar", variances = list(linPred = TRUE, disp = TRUE), verbose = FALSE)
  compute_tally(data_final_pred_all)[3, "value"]}, mc.cores = Ncpu)
quantile(unlist(predictions_all), probs = c(0.025, 0.975))
# 2.5%    97.5%
# 374087.2 389113.5
