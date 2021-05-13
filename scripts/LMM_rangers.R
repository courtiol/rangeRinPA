library(spaMM)
library(rangeRinPA)
library(tidyverse)

# Step 1 + 2: General data preparation & preparation of initial training datasets

data_initial_training_rangers_complete <- drop_na(build_initial_trainning_data(data_rangers, response = staff_rangers, survey = "complete_known"))
data_initial_training_others_complete  <- drop_na(build_initial_trainning_data(data_rangers, response = staff_others, survey = "complete_known"))
data_initial_training_all_complete     <- drop_na(build_initial_trainning_data(data_rangers, response = staff_total, survey = "complete_known"))
data_initial_training_rangers_partial  <- drop_na(build_initial_trainning_data(data_rangers, response = staff_rangers, survey = "partial_known"))
data_initial_training_others_partial   <- drop_na(build_initial_trainning_data(data_rangers, response = staff_others, survey = "partial_known"))
data_initial_training_all_partial      <- drop_na(build_initial_trainning_data(data_rangers, response = staff_total, survey = "partial_known"))

dim(data_initial_training_rangers_complete)
dim(data_initial_training_others_complete)
dim(data_initial_training_all_complete)
dim(data_initial_training_rangers_partial)
dim(data_initial_training_others_partial)
dim(data_initial_training_all_partial)

# Step 3: selection of predictor variables

formula_rangers_full <- staff_rangers_log ~ pop_density_log + PA_area_log + lat + long + area_country_log + area_forest_pct + GDP_2019_log + GDP_capita_log +
  GDP_growth + unemployment_log + EVI + SPI + EPI_2020 + IUCN_1_4_prop + IUCN_1_2_prop + Matern(1|long + lat)

fit_data_initial_training_rangers_complete_full <- fitme(formula_rangers_full,
  data = data_initial_training_rangers_complete, control.dist = list(dist.method = "Earth"))

feature_selection_LMM(full_fit = fit_data_initial_training_rangers_complete_full,
                      data = data_initial_training_rangers_complete,
                      metric = "RSE",
                      rep = 5, Ncpu = 4,
                      target = "staff_rangers_log",
                      spatial = "Matern",
                      seed = 123)


