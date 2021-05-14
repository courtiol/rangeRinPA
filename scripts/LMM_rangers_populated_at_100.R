library(spaMM)
library(rangeRinPA)
library(tidyverse)

Ncpu <- 100
rep_feature_select <- 1000
rep_finetune <- 1000

data_rangers <- fill_PA_area(data_rangers, coef = 1)

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
# k Matern  RMSE RMSE_tol formula                                                                                                                            rep
# <int> <lgl>  <dbl>    <dbl> <chr>                                                                                                                            <int>
#   1     4 FALSE   3.24    0     staff_rangers_log ~ pop_density_log + PA_area_log + area_country_log + long                                                       1000
# 2     3 TRUE    3.26    0.412 staff_rangers_log ~ pop_density_log + PA_area_log + area_country_log + Matern(1 | long + lat)                                     1000
# 3     5 FALSE   3.26    0.450 staff_rangers_log ~ pop_density_log + PA_area_log + area_country_log + long + IUCN_1_2_prop                                       1000
# 4     3 FALSE   3.26    0.612 staff_rangers_log ~ pop_density_log + PA_area_log + area_country_log                                                              1000
# 5     4 TRUE    3.27    0.791 staff_rangers_log ~ pop_density_log + PA_area_log + area_country_log + long + Matern(1 | long + lat)                              1000
# 6     6 FALSE   3.28    1.01  staff_rangers_log ~ pop_density_log + PA_area_log + area_country_log + long + IUCN_1_2_prop + area_forest_pct                     1000
# 7     5 TRUE    3.29    1.40  staff_rangers_log ~ pop_density_log + PA_area_log + area_country_log + long + IUCN_1_2_prop + Matern(1 | long + lat)              1000
# 8     7 FALSE   3.30    1.71  staff_rangers_log ~ pop_density_log + PA_area_log + area_country_log + long + IUCN_1_2_prop + area_forest_pct + lat               1000
# 9     6 TRUE    3.30    1.81  staff_rangers_log ~ pop_density_log + PA_area_log + area_country_log + long + IUCN_1_2_prop + lat + Matern(1 | long + lat)        1000
# 10     7 TRUE    3.32    2.18  staff_rangers_log ~ pop_density_log + PA_area_log + area_country_log + long + IUCN_1_2_prop + lat + SPI + Matern(1 | long + lat)  1000
selected_formula_rangers <- staff_rangers_log ~ pop_density_log + PA_area_log + area_country_log + long

selection_training_others
# A tibble: 32 x 6
#k Matern  RMSE RMSE_tol formula                                                                                                                                                                                  rep
#<int> <lgl>  <dbl>    <dbl> <chr>                                                                                                                                                                                  <int>
#1     5 TRUE    4.85    0     staff_others_log ~ pop_density_log + GDP_capita_log + area_country_log + IUCN_1_4_prop + PA_area_log + Matern(1 | long + lat)                                                           1000
#2     4 TRUE    4.88    0.678 staff_others_log ~ pop_density_log + GDP_capita_log + area_country_log + IUCN_1_4_prop + Matern(1 | long + lat)                                                                         1000
#3     6 TRUE    4.89    0.825 staff_others_log ~ pop_density_log + GDP_capita_log + area_country_log + IUCN_1_4_prop + GDP_growth + PA_area_log + Matern(1 | long + lat)                                              1000
#4     7 TRUE    4.89    1.00  staff_others_log ~ pop_density_log + GDP_capita_log + area_country_log + IUCN_1_4_prop + GDP_growth + PA_area_log + area_forest_pct + Matern(1 | long + lat)                            1000
#5     3 TRUE    4.91    1.42  staff_others_log ~ area_country_log + pop_density_log + GDP_capita_log + Matern(1 | long + lat)                                                                                         1000
#6     8 TRUE    4.92    1.60  staff_others_log ~ pop_density_log + GDP_capita_log + area_country_log + IUCN_1_4_prop + PA_area_log + GDP_growth + area_forest_pct + unemployment_log + Matern(1 | long + lat)         1000
#7     9 TRUE    4.96    2.36  staff_others_log ~ pop_density_log + GDP_capita_log + area_country_log + IUCN_1_4_prop + PA_area_log + area_forest_pct + GDP_growth + long + unemployment_log + Matern(1 | long + lat)  1000
#8     6 FALSE   4.97    2.57  staff_others_log ~ area_country_log + pop_density_log + GDP_capita_log + IUCN_1_4_prop + area_forest_pct + long                                                                         1000
#9     5 FALSE   4.98    2.76  staff_others_log ~ area_country_log + pop_density_log + GDP_capita_log + IUCN_1_4_prop + area_forest_pct                                                                                1000
#10     7 FALSE   4.98    2.84  staff_others_log ~ pop_density_log + GDP_capita_log + area_country_log + IUCN_1_4_prop + area_forest_pct + lat + long                                                                   1000
selected_formula_others <- staff_others_log ~ pop_density_log + GDP_capita_log + area_country_log + IUCN_1_4_prop + PA_area_log + Matern(1 | long + lat)

selection_training_all
# A tibble: 32 x 6
# k Matern  RMSE RMSE_tol formula                                                                                                                                           rep
# <int> <lgl>  <dbl>    <dbl> <chr>                                                                                                                                           <int>
# 1     5 FALSE   3.60    0     staff_total_log ~ pop_density_log + PA_area_log + GDP_2019_log + long + IUCN_1_2_prop                                                            1000
# 2     6 FALSE   3.62    0.559 staff_total_log ~ pop_density_log + GDP_2019_log + long + PA_area_log + IUCN_1_2_prop + area_country_log                                         1000
# 3     4 FALSE   3.63    0.997 staff_total_log ~ PA_area_log + pop_density_log + GDP_2019_log + long                                                                            1000
# 4     4 TRUE    3.64    1.21  staff_total_log ~ pop_density_log + PA_area_log + area_country_log + GDP_capita_log + Matern(1 | long + lat)                                     1000
# 5     7 FALSE   3.65    1.40  staff_total_log ~ pop_density_log + GDP_2019_log + PA_area_log + long + IUCN_1_2_prop + area_country_log + SPI                                   1000
# 6     6 TRUE    3.65    1.47  staff_total_log ~ pop_density_log + PA_area_log + area_country_log + GDP_capita_log + long + IUCN_1_2_prop + Matern(1 | long + lat)              1000
# 7     7 TRUE    3.66    1.82  staff_total_log ~ pop_density_log + area_country_log + GDP_capita_log + PA_area_log + long + IUCN_1_2_prop + EVI + Matern(1 | long + lat)        1000
# 8     8 FALSE   3.67    1.91  staff_total_log ~ pop_density_log + GDP_2019_log + PA_area_log + long + IUCN_1_2_prop + area_country_log + SPI + GDP_growth                      1000
# 9     3 TRUE    3.68    2.13  staff_total_log ~ pop_density_log + PA_area_log + area_country_log + Matern(1 | long + lat)                                                      1000
# 10     8 TRUE    3.68    2.28  staff_total_log ~ pop_density_log + GDP_capita_log + area_country_log + PA_area_log + IUCN_1_2_prop + long + EVI + SPI + Matern(1 | long + lat)  1000
selected_formula_all <- staff_total_log ~ pop_density_log + PA_area_log + GDP_2019_log + long + IUCN_1_2_prop


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
# [1] 119  11
# [1] 136   9


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
# 1     ML   CV 1000 4.950245 -0.03145984 1.164539 0.4023717 1.235889 0.5046178 -0.08443518         0.042
# 2   REML   CV 1000 4.944798 -0.03411558 1.161500 0.4040168 1.232996 0.5097242 -0.08489909         0.039
#
# $result_sd
# method type  rep     RMSE        ME       MAE        R2       RSE       CCC    MoranI Moran_pv_freq
# 1     ML   CV 1000 1.033598 0.4430315 0.2404462 0.3094445 0.9155664 0.1850916 0.1092523     0.2006895
# 2   REML   CV 1000 1.040140 0.4423306 0.2410655 0.3081509 0.9165451 0.1865837 0.1090222     0.1936918
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
# sum    value
# 1  observed 273475.6
# 2 predicted  13590.8
# 3     total 287066.4
point_prediction_rangers <- compute_tally(data_final_pred_rangers)[3, "value"]

data_final_pred_others$data_predictable$staff_others_log_predicted <- predict(
  fit_final_others, newdata = data_final_pred_others$data_predictable)[, 1]
compute_tally(data_final_pred_others)
# sum     value
# 1  observed 263074.66
# 2 predicted  18720.36
# 3     total 281795.02
point_prediction_others <- compute_tally(data_final_pred_others)[3, "value"]

data_final_pred_all$data_predictable$staff_total_log_predicted <- predict(
  fit_final_all, newdata = data_final_pred_all$data_predictable)[, 1]
compute_tally(data_final_pred_all)
# sum      value
# 1  observed 547304.621
# 2 predicted   9746.417
# 3     total 557051.038
point_prediction_all <- compute_tally(data_final_pred_all)[3, "value"]


# Step 8b: Simulations

predictions_rangers <- parallel::mclapply(1:10000, function(i) {
  data_final_pred_rangers$data_predictable$staff_rangers_log_predicted <- simulate(
    fit_final_rangers, newdata = data_final_pred_rangers$data_predictable,
    type = "predVar", variances = list(linPred = TRUE, disp = TRUE), verbose = FALSE)
  compute_tally(data_final_pred_rangers)[3, "value"]}, mc.cores = Ncpu)
quantile(unlist(predictions_rangers), probs = c(0.025, 0.975))
# 2.5%    97.5%
#  284713.5 315156.4

predictions_others <- parallel::mclapply(1:10000, function(i) {
  data_final_pred_others$data_predictable$staff_others_log_predicted <- simulate(
    fit_final_others, newdata = data_final_pred_others$data_predictable,
    type = "predVar", variances = list(linPred = TRUE, disp = TRUE), verbose = FALSE)
  compute_tally(data_final_pred_others)[3, "value"]}, mc.cores = Ncpu)
quantile(unlist(predictions_others), probs = c(0.025, 0.975))
# 2.5%    97.5%
# 282252.8 405265.9

predictions_all <- parallel::mclapply(1:10000, function(i) {
  data_final_pred_all$data_predictable$staff_total_log_predicted <- simulate(
    fit_final_all, newdata = data_final_pred_all$data_predictable,
    type = "predVar", variances = list(linPred = TRUE, disp = TRUE), verbose = FALSE)
  compute_tally(data_final_pred_all)[3, "value"]}, mc.cores = Ncpu)
quantile(unlist(predictions_all), probs = c(0.025, 0.975))
# 2.5%    97.5%
# 554906.1 580311.4
