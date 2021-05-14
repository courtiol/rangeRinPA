library(spaMM)
library(rangeRinPA)
library(tidyverse)

Ncpu <- 1
rep_feature_select <- 1000
rep_finetune <- 10


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
selected_formula_rangers_complete <- staff_rangers_log ~ area_country_log + pop_density_log

selection_training_others_complete
selected_formula_others_complete <- staff_others_log ~ area_country_log + pop_density_log + Matern(1 | long + lat)

selection_training_all_complete
selected_formula_all_complete <- staff_total_log ~ area_country_log + pop_density_log + EPI_2020 + long + EVI + Matern(1 | long + lat)
#selected_formula_all_complete <- staff_total_log ~ area_country_log + pop_density_log ## not quite as good (ca. 10% higher RMSE) but complete

selection_training_rangers_partial
selected_formula_rangers_partial <- staff_rangers_log ~ pop_density_log + PA_area_log + area_country_log + long

selection_training_others_partial
selected_formula_others_partial <- staff_others_log ~ area_country_log + pop_density_log + GDP_capita_log + IUCN_1_4_prop + GDP_growth + area_forest_pct + Matern(1 | long + lat)
#selected_formula_others_partial <- staff_others_log ~ area_country_log + pop_density_log + GDP_capita_log + Matern(1 | long + lat) ## not quite as good (ca. 2% higher RMSE)
#selected_formula_others_partial <- staff_others_log ~ area_country_log + pop_density_log + Matern(1 | long + lat) ## not quite as good (ca. 10% higher RMSE) but complete

selection_training_all_partial
selected_formula_all_partial <- staff_total_log ~ pop_density_log + area_country_log + PA_area_log ## not quite as good (ca. 4% higher RMSE) but complete


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

finetune_rangers_complete
finetune_others_complete
finetune_all_complete
finetune_rangers_partial
finetune_others_partial
finetune_all_partial


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

data_final_pred_others_complete$data_predictable$staff_others_log_predicted <- predict(
  fit_final_others_complete, newdata = data_final_pred_others_complete$data_predictable)[, 1]
compute_tally(data_final_pred_others_complete)

data_final_pred_all_complete$data_predictable$staff_total_log_predicted <- predict(
  fit_final_all_complete, newdata = data_final_pred_all_complete$data_predictable)[, 1]
compute_tally(data_final_pred_all_complete)

data_final_pred_rangers_partial$data_predictable$staff_rangers_log_predicted <- predict(
  fit_final_rangers_partial, newdata = data_final_pred_rangers_partial$data_predictable)[, 1]
compute_tally(data_final_pred_rangers_partial)

data_final_pred_others_partial$data_predictable$staff_others_log_predicted <- predict(
  fit_final_others_partial, newdata = data_final_pred_others_partial$data_predictable)[, 1]
compute_tally(data_final_pred_others_partial)

data_final_pred_all_partial$data_predictable$staff_total_log_predicted <- predict(
  fit_final_all_partial, newdata = data_final_pred_all_partial$data_predictable)[, 1]
compute_tally(data_final_pred_all_partial)


# Step 8b: Simulations
