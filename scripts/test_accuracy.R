#library(rangeRinPA)
library(dplyr)
library(tidyr)
library(ggplot2)
library(spaMM)
library(ranger)


## small, not very general function to compute RMSE on test set for LMM
compute_rmse_lmm <- function(rep = 10, formula, data, ...) {
  rmse <- numeric(rep)
  for (rep in seq_len(rep)) {
    data_list <- prepare_data(formula = formula, data = data, test_prop = 0.1)
    newfit <- spaMM::fitme(formula = formula, data = data_list$data_train, ...)
    rmse[rep] <- RMSE(predict(newfit, newdata = data_list$data_test)[, 1], data_list$data_test$staff_rangers_log)
  }
  rmse
}

## small, not very general function to compute RMSE on test set for RF
compute_rmse_rf <- function(rep = 10, formula, data, ...) {
  rmse <- numeric(rep)
  for (rep in seq_len(rep)) {
    data_list <- prepare_data(formula = formula, data = data, test_prop = 0.1)
    newfit <- ranger::ranger(formula = formula, data = data_list$data_train,  ...)
    rmse[rep] <- RMSE(predict(newfit, data = data_list$data_test)$predictions, data_list$data_test$staff_rangers_log)
  }
  rmse
}


## data for test
data_rangers %>%
  filter(countryname_eng != "Greenland") %>%
  drop_na(staff_rangers, PA_area, pop_density, lat, long, area_country, GDP_2019) %>%
  mutate(across(c(staff_rangers, PA_area, pop_density, area_country, GDP_2019),
                ~ log(.x + 1), .names = "{col}_log")) -> data_test ## 135 rows


## let's define the formula
formls_LMM <- list(
  forml_coord                 = staff_rangers_log ~ 1 + Matern(1|long + lat),
  forml_PA                    = staff_rangers_log ~ 1 + PA_area_log,
  forml_pop                   = staff_rangers_log ~ 1 + pop_density_log,
  forml_PA_pop                = staff_rangers_log ~ 1 + PA_area_log + pop_density_log,
  forml_PA_pop_coord          = staff_rangers_log ~ 1 + PA_area_log + pop_density_log + Matern(1|long + lat),
  forml_PA_pop_area_coord     = staff_rangers_log ~ 1 + PA_area_log + pop_density_log + area_country_log +  Matern(1|long + lat),
  forml_PA_pop_GDP_coord      = staff_rangers_log ~ 1 + PA_area_log + pop_density_log + GDP_2019_log +  Matern(1|long + lat),
  forml_area                  = staff_rangers_log ~ 1 + area_country_log,
  forml_GDP                   = staff_rangers_log ~ 1 + GDP_2019_log,
  forml_PA_pop_area_GDP_coord = staff_rangers_log ~ 1 + PA_area_log + pop_density_log + area_country_log + GDP_2019_log + Matern(1|long + lat)
  )

formls_RF <- list(
  forml_coord                 = staff_rangers_log ~ 1 + long + lat,
  forml_PA                    = staff_rangers_log ~ 1 + PA_area_log,
  forml_pop                   = staff_rangers_log ~ 1 + pop_density_log,
  forml_PA_pop                = staff_rangers_log ~ 1 + PA_area_log + pop_density_log,
  forml_PA_pop_coord          = staff_rangers_log ~ 1 + PA_area_log + pop_density_log + long + lat,
  forml_PA_pop_area_coord     = staff_rangers_log ~ 1 + PA_area_log + pop_density_log + area_country_log +  long + lat,
  forml_PA_pop_GDP_coord      = staff_rangers_log ~ 1 + PA_area_log + pop_density_log + GDP_2019_log +  long + lat,
  forml_area                  = staff_rangers_log ~ 1 + area_country_log,
  forml_GDP                   = staff_rangers_log ~ 1 + GDP_2019_log,
  forml_PA_pop_area_GDP_coord = staff_rangers_log ~ 1 + PA_area_log + pop_density_log + area_country_log + GDP_2019_log + long + lat
)


## compute RMSE on test sets
n_tests <- 30
Ncpu <- 2

test_REML <- parallel::mclapply(formls_LMM, function(f) compute_rmse_lmm(rep = n_tests, formula = f, data = data_test,
                                                                         control.dist = list(dist.method = "Earth"), method = "REML"), mc.cores = Ncpu)

test_ML <- parallel::mclapply(formls_LMM, function(f) compute_rmse_lmm(rep = n_tests, formula = f, data = data_test,
                                                       control.dist = list(dist.method = "Earth"), method = "ML"), mc.cores = Ncpu)

test_RF_mtry1 <- parallel::mclapply(formls_RF, function(f) compute_rmse_rf(rep = n_tests, formula = f, data = data_test, mtry = 1), mc.cores = Ncpu)


## let's plot the results
rbind(cbind(do.call("cbind", test_REML), method = "REML" ),
      cbind(do.call("cbind", test_ML), method = "ML" ),
      cbind(do.call("cbind", test_RF_mtry1), method = "RF_mtry1" )) %>%
      as.data.frame() %>%
      pivot_longer(cols = -method, names_to = "model", values_to = "rmse") %>%
      mutate(model = sub(pattern = "forml_", replacement = "", model),
             rmse = as.numeric(rmse),
             model = forcats::fct_inorder(model)) -> test_for_plot

ggplot(test_for_plot) +
  aes(y = rmse, x = model, colour = method, shape = method) +
  geom_boxplot(width = 0.1, position = position_dodge(width = 0.2)) +
  geom_point(position = position_dodge(width = 0.5)) +
  labs(y = "RMSE (on log + 1 response)", x = "Model", title = "Accuracy on test sets (10% of data)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45))
