library(rangeRinPA)
library(dplyr)
library(tidyr)
library(ggplot2)
library(spaMM)
library(ranger)

rm(list = ls())

## function to compute RMSE on test set for LMM
compute_rmse_lmm <- function(formula, data, rep = 10, Ncpu = 1, target = "staff_rangers_log", ...) {
  rmse <- parallel::mclapply(seq_len(rep), function(i) {
    data_list <- prepare_data(formula = formula, data = data, test_prop = 0.1)
    newfit <- spaMM::fitme(formula = formula, data = data_list$data_train, ...)
    RMSE(predict(newfit, newdata = data_list$data_test)[, 1], data_list$data_test[, target, drop = TRUE])
  }, mc.cores = Ncpu)
  unlist(rmse)
}


## function to compute RMSE on test set for RF
compute_rmse_rf <- function(formula, data, rep = 10, Ncpu = 1, target = "staff_rangers_log", ...) {
  rmse <- parallel::mclapply(seq_len(rep), function(i) {
    data_list <- prepare_data(formula = formula, data = data, test_prop = 0.1)
    newfit <- ranger::ranger(formula = formula, data = data_list$data_train,  ...)
    RMSE(predict(newfit, data = data_list$data_test)$predictions, data_list$data_test[, target, drop = TRUE])
  }, mc.cores = Ncpu)
  unlist(rmse)
}


## data for test
data_rangers %>%
  filter(countryname_eng != "Greenland") %>%
  drop_na(staff_rangers,
          pop_density,
          lat, long,
          PA_area, area_country, area_forest_pct,
          GDP_2019, GDP_capita, GDP_growth, unemployment,
          EVI, SPI, EPI_2020,
          IUCN_1_4_prop, IUCN_1_2_prop) %>%
  mutate(across(c(staff_rangers, PA_area, pop_density, area_country, GDP_2019, GDP_capita, unemployment),
                ~ log(.x + 1), .names = "{col}_log")) %>%
  select(staff_rangers_log,
         pop_density_log,
         lat, long,
         PA_area_log, area_country_log, area_forest_pct,
         GDP_2019_log, GDP_capita_log, GDP_growth, unemployment_log,
         EVI, SPI, EPI_2020,
         IUCN_1_4_prop, IUCN_1_2_prop) -> data_test ## 120 rows


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
  forml_PA_pop_area_GDP_coord = staff_rangers_log ~ 1 + PA_area_log + pop_density_log + area_country_log + GDP_2019_log + Matern(1|long + lat),
  forml_all                   = staff_rangers_log ~ pop_density_log +
                                                    lat + long +
                                                    PA_area_log + area_country_log + area_forest_pct +
                                                    GDP_2019_log + GDP_capita_log + GDP_growth + unemployment_log +
                                                    EVI + SPI + EPI_2020 +
                                                    IUCN_1_4_prop + IUCN_1_2_prop +
                                                    Matern(1|long + lat)
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
  forml_PA_pop_area_GDP_coord = staff_rangers_log ~ 1 + PA_area_log + pop_density_log + area_country_log + GDP_2019_log + long + lat,
  forml_all                   = staff_rangers_log ~ .
)


## compute RMSE on test sets
n_tests <- 100
Ncpu <- 100

test_REML       <- lapply(formls_LMM, function(f) compute_rmse_lmm(formula = f, data = data_test, rep = n_tests, Ncpu = Ncpu,
                                                                   control.dist = list(dist.method = "Earth"), method = "REML"))
test_ML         <- lapply(formls_LMM, function(f) compute_rmse_lmm(formula = f, data = data_test, rep = n_tests, Ncpu = Ncpu,
                                                                   control.dist = list(dist.method = "Earth"), method = "ML"))
test_RF_mtry1   <- lapply(formls_RF, function(f) compute_rmse_rf(formula = f, data = data_test, rep = n_tests, Ncpu = Ncpu,
                                                                 mtry = 1))
test_RF_mtry1_20xmoretrees   <- lapply(formls_RF, function(f) compute_rmse_rf(formula = f, data = data_test, rep = n_tests, Ncpu = Ncpu,
                                                                 mtry = 1, num.trees = 10000))
test_RF_mtry3   <- lapply(formls_RF, function(f) compute_rmse_rf(formula = f, data = data_test, rep = n_tests, Ncpu = Ncpu,
                                                                 mtry = function(nv) min(c(nv, 3))))
test_RF_mtryMAX <- lapply(formls_RF, function(f) compute_rmse_rf(formula = f, data = data_test, rep = n_tests, Ncpu = Ncpu,
                                                                 mtry = function(nv) nv))


## let's plot the results
rbind(cbind(do.call("cbind", test_REML), method = "spaMM_REML" ),
      cbind(do.call("cbind", test_ML), method = "spaMM_ML" ),
      cbind(do.call("cbind", test_RF_mtry1), method = "RF_mtry1" ),
      cbind(do.call("cbind", test_RF_mtry1_20xmoretrees), method = "RF_mtry1_20xmoretrees" ),
      cbind(do.call("cbind", test_RF_mtry3), method = "RF_mtry3" ),
      cbind(do.call("cbind", test_RF_mtryMAX), method = "RF_mtryMAX")) %>%
      as.data.frame() %>%
      pivot_longer(cols = -method, names_to = "model", values_to = "rmse") %>%
      mutate(model = sub(pattern = "forml_", replacement = "", model),
             rmse = as.numeric(rmse),
             model = forcats::fct_inorder(model)) -> test_for_plot

pdf("result_accuracy.pdf", width = 15)
ggplot(test_for_plot) +
  aes(y = rmse, x = model, colour = method, shape = method) +
  geom_boxplot(width = 0.3, position = position_dodge(width = 0.5)) +
  labs(y = "RMSE (on log + 1 response)", x = "Model", title = "Accuracy on test sets (10% of data)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45))
dev.off()
