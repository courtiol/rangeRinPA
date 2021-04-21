#library(rangeRinPA)
library(dplyr)
library(tidyr)
library(ggplot2)
library(spaMM)


## small, not very general function to compute RMSE on test set for LMM
compute_rmse_lmm <- function(rep = 10, formula, data, ...) {
  base_fit <- fitme(formula = formula, data = data, ...) ## prefit since using ... within replicate() messes things up
  rmse <- replicate(rep, {
    data_list <- prepare_data(formula = formula, data = data, test_prop = 0.1)
    newfit <- update(base_fit, data = data_list$data_train)
    RMSE(predict(newfit, newdata = data_list$data_test)[, 1], log(data_list$data_test$staff_rangers + 1))
  })

  rmse
}


## data for test
data_rangers %>%
  filter(countryname_eng != "Greenland") %>%
  drop_na(staff_rangers, PA_area, pop_density, lat, long, area_country, GDP_2019) -> data_test ## 135 rows


## let's define the formula
formls <- list(
  forml_NULL                = log(staff_rangers + 1) ~ 1,
  forml_Mat                 = log(staff_rangers + 1) ~ 1 + Matern(1|long + lat),
  forml_PA                  = log(staff_rangers + 1) ~ 1 + log(PA_area),
  forml_pop                 = log(staff_rangers + 1) ~ 1 + log(pop_density),
  forml_PA_pop              = log(staff_rangers + 1) ~ 1 + log(PA_area) + log(pop_density),
  forml_PA_pop_Mat          = log(staff_rangers + 1) ~ 1 + log(PA_area) + log(pop_density) + Matern(1|long + lat),
  forml_PA_pop_area_Mat     = log(staff_rangers + 1) ~ 1 + log(PA_area) + log(pop_density) + log(area_country) +  Matern(1|long + lat),
  forml_PA_pop_GDP_Mat      = log(staff_rangers + 1) ~ 1 + log(PA_area) + log(pop_density) + log(GDP_2019) +  Matern(1|long + lat),
  forml_area                = log(staff_rangers + 1) ~ 1 + log(area_country),
  forml_GDP                 = log(staff_rangers + 1) ~ 1 + log(GDP_2019),
  forml_PA_pop_area_GDP_Mat = log(staff_rangers + 1) ~ 1 + log(PA_area) + log(pop_density) + log(area_country) + log(GDP_2019) + Matern(1|long + lat)
  )


## compute RMSE on test sets
n_tests <- 2

test_REML <- lapply(formls, function(f) compute_rmse_lmm(rep = n_tests, formula = f, data = data_test,
                                                         control.dist = list(dist.method = "Earth"), method = "REML"))

test_ML <- lapply(formls, function(f) compute_rmse_lmm(rep = n_tests, formula = f, data = data_test,
                                                       control.dist = list(dist.method = "Earth"), method = "ML"))


## let's plot the results
rbind(cbind(do.call("cbind", test_REML), method = "REML" ),
      cbind(do.call("cbind", test_ML), method = "ML" )) %>%
      as.data.frame() %>%
      pivot_longer(cols = -method, names_to = "model", values_to = "rmse") %>%
      mutate(model = sub(pattern = "forml_", replacement = "", model),
             rmse = as.numeric(rmse),
             model = forcats::fct_reorder(model, rmse, .fun = mean, .desc = TRUE)) -> test_for_plot

ggplot(test_for_plot) +
  aes(y = rmse, x = model, colour = method, shape = method) +
  geom_boxplot(width = 0.1, position = position_dodge(width = 0.2)) +
  geom_point(position = position_dodge(width = 0.5)) +
  labs(y = "RMSE (on log + 1 response)", x = "Model (sorted by mean overall accuracy)", title = "Accuracy on test sets (10% of data)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45))
