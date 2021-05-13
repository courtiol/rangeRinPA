library(spaMM)
library(rangeRinPA)
library(tidyverse)
Ncpu <- 100

# Step 1 + 2: General data preparation & preparation of initial training datasets

data_initial_training_rangers_complete <- drop_na(build_initial_training_data(data_rangers, response = staff_rangers, survey = "complete_known"))
data_initial_training_others_complete  <- drop_na(build_initial_training_data(data_rangers, response = staff_others, survey = "complete_known"))
data_initial_training_all_complete     <- drop_na(build_initial_training_data(data_rangers, response = staff_total, survey = "complete_known"))
data_initial_training_rangers_partial  <- drop_na(build_initial_training_data(data_rangers, response = staff_rangers, survey = "partial_known"))
data_initial_training_others_partial   <- drop_na(build_initial_training_data(data_rangers, response = staff_others, survey = "partial_known"))
data_initial_training_all_partial      <- drop_na(build_initial_training_data(data_rangers, response = staff_total, survey = "partial_known"))

dim(data_initial_training_rangers_complete)
dim(data_initial_training_others_complete)
dim(data_initial_training_all_complete)
dim(data_initial_training_rangers_partial)
dim(data_initial_training_others_partial)
dim(data_initial_training_all_partial)


# Step 3: Selection of predictor variables

formula_rangers_full <- staff_rangers_log ~ pop_density_log + PA_area_log + lat + long + area_country_log + area_forest_pct + GDP_2019_log + GDP_capita_log +
  GDP_growth + unemployment_log + EVI + SPI + EPI_2020 + IUCN_1_4_prop + IUCN_1_2_prop + Matern(1|long + lat)

formula_others_full <- update(formula_rangers_full, staff_others_log ~ .)
formula_all_full <- update(formula_rangers_full, staff_total_log ~ .)

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
  rep = 1000, Ncpu = Ncpu,
  target = "staff_rangers_log",
  seed = 123)
selected_formula_rangers_complete <- staff_rangers_log ~ area_country_log + pop_density_log

selection_training_others_complete <- feature_selection_LMM(
  full_fit = fit_data_initial_training_others_complete_full,
  rep = 1000, Ncpu = Ncpu,
  target = "staff_others_log",
  seed = 123)
selected_formula_others_complete <- staff_others_log ~ area_country_log + pop_density_log + Matern(1 | long + lat)

selection_training_all_complete <- feature_selection_LMM(
  full_fit = fit_data_initial_training_all_complete_full,
  rep = 1000, Ncpu = Ncpu,
  target = "staff_total_log",
  seed = 123)
selected_formula_all_complete <- staff_total_log ~ area_country_log + pop_density_log + EPI_2020 + long + EVI + Matern(1 | long + lat)
#selected_formula_all_complete <- staff_total_log ~ area_country_log + pop_density_log ## not quite as good (11% higher RMSE) but complete

selection_training_rangers_partial <- feature_selection_LMM(
  full_fit = fit_data_initial_training_rangers_partial_full,
  rep = 1000, Ncpu = Ncpu,
  target = "staff_rangers_log",
  seed = 123)
selected_formula_rangers_partial <-

selection_training_others_partial <- feature_selection_LMM(
  full_fit = fit_data_initial_training_others_partial_full,
  rep = 1000, Ncpu = Ncpu,
  target = "staff_others_log",
  seed = 123)
selected_formula_others_partial <-

selection_training_all_partial <- feature_selection_LMM(
  full_fit = fit_data_initial_training_all_partial_full,
  rep = 1000, Ncpu = Ncpu,
  target = "staff_total_log",
  seed = 123)
selected_formula_all_partial <-


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

any(is.na(data_final_training_rangers_complete))
any(is.na(data_final_training_others_complete))
any(is.na(data_final_training_all_complete))
any(is.na(data_final_training_rangers_partial))
any(is.na(data_final_training_others_partial))
any(is.na(data_final_training_all_partial))


# Step 5: Selection of function inputs (fine tuning)
CV_rangers_complete_ML <- validate_LMM(selected_formula_rangers_complete,
                                       data = data_final_training_rangers_complete,
                                       rep = 1000, Ncpu = Ncpu,
                                       spatial = any(grepl("Matern", selected_formula_rangers_complete)),
                                       target = "staff_rangers_log", seed = 123,
                                       method = "ML")

CV_rangers_complete_REML <- validate_LMM(selected_formula_rangers_complete,
                                         data = data_final_training_rangers_complete,
                                         rep = 1000, Ncpu = Ncpu,
                                         spatial = any(grepl("Matern", selected_formula_rangers_complete)),
                                         target = "staff_rangers_log", seed = 123,
                                         method = "REML")

CV_others_complete_ML <- validate_LMM(selected_formula_others_complete,
                                      data = data_final_training_others_complete,
                                      rep = 1000, Ncpu = Ncpu,
                                      spatial = any(grepl("Matern", selected_formula_others_complete)),
                                      target = "staff_others_log", seed = 123,
                                      method = "ML")

CV_others_complete_REML <- validate_LMM(selected_formula_others_complete,
                                        data = data_final_training_others_complete,
                                        rep = 1000, Ncpu = Ncpu,
                                        spatial = any(grepl("Matern", selected_formula_others_complete)),
                                        target = "staff_others_log", seed = 123,
                                        method = "REML")

CV_all_complete_ML <- validate_LMM(selected_formula_all_complete,
                                   data = data_final_training_all_complete,
                                   rep = 1000, Ncpu = Ncpu,
                                   spatial = any(grepl("Matern", selected_formula_all_complete)),
                                   target = "staff_all_log", seed = 123,
                                   method = "ML")

CV_all_complete_REML <- validate_LMM(selected_formula_all_complete,
                                     data = data_final_training_all_complete,
                                     rep = 1000, Ncpu = Ncpu,
                                     spatial = any(grepl("Matern", selected_formula_all_complete)),
                                     target = "staff_all_log", seed = 123,
                                     method = "REML")

CV_rangers_partial_ML <- validate_LMM(selected_formula_rangers_partial,
                                      data = data_final_training_rangers_partial,
                                      rep = 1000, Ncpu = Ncpu,
                                      spatial = any(grepl("Matern", selected_formula_rangers_partial)),
                                      target = "staff_rangers_log", seed = 123,
                                      method = "ML")

CV_rangers_partial_REML <- validate_LMM(selected_formula_rangers_partial,
                                        data = data_final_training_rangers_partial,
                                        rep = 1000, Ncpu = Ncpu,
                                        spatial = any(grepl("Matern", selected_formula_rangers_partial)),
                                        target = "staff_rangers_log", seed = 123,
                                        method = "REML")

CV_others_partial_ML <- validate_LMM(selected_formula_others_partial,
                                     data = data_final_training_others_partial,
                                     rep = 1000, Ncpu = Ncpu,
                                     spatial = any(grepl("Matern", selected_formula_others_partial)),
                                     target = "staff_others_log", seed = 123,
                                     method = "ML")

CV_others_partial_REML <- validate_LMM(selected_formula_others_partial,
                                       data = data_final_training_others_partial,
                                       rep = 1000, Ncpu = Ncpu,
                                       spatial = any(grepl("Matern", selected_formula_others_partial)),
                                       target = "staff_others_log", seed = 123,
                                       method = "REML")

CV_all_partial_ML <- validate_LMM(selected_formula_all_partial,
                                  data = data_final_training_all_partial,
                                  rep = 1000, Ncpu = Ncpu,
                                  spatial = any(grepl("Matern", selected_formula_all_partial)),
                                  target = "staff_all_log", seed = 123,
                                  method = "ML")

CV_all_partial_REML <- validate_LMM(selected_formula_all_partial,
                                    data = data_final_training_all_partial,
                                    rep = 1000, Ncpu = Ncpu,
                                    spatial = any(grepl("Matern", selected_formula_all_partial)),
                                    target = "staff_all_log", seed = 123,
                                    method = "REML")

aggregate_metrics(CV_rangers_complete_ML)
aggregate_metrics(CV_rangers_complete_REML)
aggregate_metrics(CV_rangers_others_ML)
aggregate_metrics(CV_rangers_others_REML)
aggregate_metrics(CV_all_complete_ML)
aggregate_metrics(CV_all_complete_REML)
aggregate_metrics(CV_rangers_partial_ML)
aggregate_metrics(CV_rangers_partial_REML)
aggregate_metrics(CV_rangers_others_ML)
aggregate_metrics(CV_rangers_others_REML)
aggregate_metrics(CV_all_partial_ML)
aggregate_metrics(CV_all_partial_REML)


# Step 6: Final training

# Step 7: Preparation of datasets for predictions & simulations

# Step 8: Predictions and simulations

