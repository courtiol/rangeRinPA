library(rangeRinPA)
library(dplyr)
library(tidyr)
library(ggplot2)
library(spaMM)
library(ranger)

rm(list = ls())

## function to compute RMSE on test set for LMM
compute_rmse_lmm <- function(formula, data, rep = 10, Ncpu = 1, target = "staff_rangers_log", ...) {
  seed <- 123 # we make sure all dataset are the same across comparisons
  rmse <- parallel::mclapply(seq_len(rep), function(i) {
    data_list <- prepare_data(formula = formula, data = data, test_prop = 0.1, seed = seed + i)
    newfit <- spaMM::fitme(formula = formula, data = data_list$data_train, ...)
    RMSE(predict(newfit, newdata = data_list$data_test)[, 1], data_list$data_test[, target, drop = TRUE])
  }, mc.cores = Ncpu)
  unlist(rmse)
}


## function to compute RMSE on test set for RF
compute_rmse_rf <- function(formula, data, rep = 10, Ncpu = 1, target = "staff_rangers_log", add_noise = FALSE, ...) {
  seed <- 123 # we make sure all dataset are the same across comparisons
  rmse <- parallel::mclapply(seq_len(rep), function(i) {
    data_list <- prepare_data(formula = formula, data = data, test_prop = 0.1, seed = seed + i)
    if (add_noise) {
      data_list$data_train[, target] <- data_list$data_train[, target, drop = TRUE] +
        rnorm(length(data_list$data_train[, target, drop = TRUE]), mean = 0, sd = sd(data_list$data_train[, target, , drop = TRUE]))
    }
    newfit <- ranger::ranger(formula = formula, data = data_list$data_train, num.threads = 1, ...)
    RMSE(predict(newfit, data = data_list$data_test, num.threads = 1)$predictions, data_list$data_test[, target, drop = TRUE])
    #NOTE: num.threads = 1 is an attempt not to use multiple threads since we do parallelisation at a higher level, but does not seem to work :-(
  }, mc.cores = Ncpu)
  unlist(rmse)
}


## data for test
data_rangers %>%
  filter(countryname_eng != "Greenland") %>% # Greenland is a clear outlier, so we drop this country
  drop_na(staff_rangers,
          pop_density,
          lat, long, country_UN_subcontinent,
          PA_area, area_country, area_forest_pct,
          GDP_2019, GDP_capita, GDP_growth, unemployment,
          EVI, SPI, EPI_2020, IUCN_1_4_prop, IUCN_1_2_prop) %>%
  mutate(across(c(staff_rangers, PA_area, pop_density, area_country, GDP_2019, GDP_capita, unemployment), # we log transform to get hump-shaped distributions
                ~ log(.x + 1), .names = "{col}_log")) %>%
  select(staff_rangers_log, # the number of rangers (log)
         pop_density_log,   # the density of the population
         lat, long, country_UN_subcontinent, # the coordinate of the centroid of the largest polygon associated with a country/territory + subcontinent
         PA_area_log, area_country_log, area_forest_pct, # areas of Protected area, country and pct of forest
         GDP_2019_log, GDP_capita_log, GDP_growth, unemployment_log, # economic indices
         EVI, SPI, EPI_2020, IUCN_1_4_prop, IUCN_1_2_prop # ecological indices
         ) -> data_test ## 120 rows


## attempt to turn all numeric predictor into uncorrelated PCA axes
data_test %>%
  select(-country_UN_subcontinent, -staff_rangers_log) -> data_for_PCA

pca <- prcomp(data_for_PCA, center = TRUE, scale. = TRUE)
#biplot(pca)
#plot(pca)
data_test_pca <- as.data.frame(cbind(staff_rangers_log = data_test$staff_rangers_log, pca$x))


## interlude to check the importance of variables
# same on PCA
forest <- ranger(staff_rangers_log ~ . , data = data_test_pca, importance = "permutation")
tibble::as_tibble_row(importance(forest)) %>%
  pivot_longer(everything(), names_to = "Predictor", values_to = "Importance") %>%
  arrange(desc(Importance)) %>%
  mutate(Predictor = forcats::fct_inorder(Predictor)) %>%
  ggplot() +
  aes(y = Importance, x = Predictor) +
  geom_col(width = 0.2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45))
# NOTE: since only PC2 seems really important, let's add it to data_test

data_test$PC2 <- data_test_pca$PC2

forest2 <- ranger(staff_rangers_log ~ . , data = data_test, importance = "permutation")
tibble::as_tibble_row(importance(forest2)) %>%
  pivot_longer(everything(), names_to = "Predictor", values_to = "Importance") %>%
  arrange(desc(Importance)) %>%
  mutate(Predictor = forcats::fct_inorder(Predictor)) %>%
  ggplot() +
    aes(y = Importance, x = Predictor) +
    geom_col(width = 0.2) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45))

# NOTE: this reveals that the top predictors are PA_area_log, PC2, GDP_2019_log, area_country_log and then pop_density_log
# but since not all tree contains all predictors, let's see what happens if all tree contain PA_area_log

forest3 <- ranger(staff_rangers_log ~ . , data = data_test, importance = "permutation", always.split.variables = "PA_area_log")
tibble::as_tibble_row(importance(forest3)) %>%
  pivot_longer(everything(), names_to = "Predictor", values_to = "Importance") %>%
  arrange(desc(Importance)) %>%
  mutate(Predictor = forcats::fct_inorder(Predictor)) %>%
  ggplot() +
  aes(y = Importance, x = Predictor) +
  geom_col(width = 0.2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45))
# NOTE: now the top predictors become PA_area_log & to a lesser degree pop_density_log, PC2, area_country_log and GDP_2019_log
# the reason why PC2, GDP_2019_log & area_country_log vanish is that they are quite correlated to PA_area_log


# strong correlations between predictors
cor_obj <- cor(data_test %>% select(-country_UN_subcontinent, -staff_rangers_log), method = "spearman")
as.data.frame(round(cor_obj, 2), row.names = rownames(cor_obj)) %>%
  as_tibble(rownames = "var") %>%
  pivot_longer(-var) %>%
  filter((value > 0.4 | value < -0.4) & value != 1) %>%
  rowwise() %>%
  mutate(vars = paste0(sort(c(var, name)), collapse = "<-->"), .before = 1) %>%
  select(-var, -name) %>%
  distinct() %>%
  arrange(desc(abs(value)))


## let's define the formula
formls_LMM <- list(
  forml_coord                 = staff_rangers_log ~ 1 + Matern(1|long + lat),
  forml_PA                    = staff_rangers_log ~ 1 + PA_area_log,
  forml_pop                   = staff_rangers_log ~ 1 + pop_density_log,
  forml_PC2                   = staff_rangers_log ~ 1 + PC2,
  forml_area                  = staff_rangers_log ~ 1 + area_country_log,
  forml_GDP                   = staff_rangers_log ~ 1 + GDP_2019_log,
  forml_PA_coord              = staff_rangers_log ~ 1 + PA_area_log + Matern(1|long + lat),
  forml_pop_coord             = staff_rangers_log ~ 1 + pop_density_log + Matern(1|long + lat),
  forml_PC2_coord             = staff_rangers_log ~ 1 + PC2 + Matern(1|long + lat),
  forml_area_coord            = staff_rangers_log ~ 1 + area_country_log + Matern(1|long + lat),
  forml_GDP_coord             = staff_rangers_log ~ 1 + GDP_2019_log + Matern(1|long + lat),
  forml_PA_pop                = staff_rangers_log ~ 1 + PA_area_log + pop_density_log,
  forml_PA_pop_coord          = staff_rangers_log ~ 1 + PA_area_log + pop_density_log + Matern(1|long + lat),
  forml_PA_pop_PC2            = staff_rangers_log ~ 1 + PA_area_log + pop_density_log + PC2,
  forml_PA_pop_PC2_coord      = staff_rangers_log ~ 1 + PA_area_log + pop_density_log + PC2 + Matern(1|long + lat),
  forml_PA_pop_area           = staff_rangers_log ~ 1 + PA_area_log + pop_density_log + area_country_log,
  forml_PA_pop_area_coord     = staff_rangers_log ~ 1 + PA_area_log + pop_density_log + area_country_log +  Matern(1|long + lat),
  forml_PA_pop_GDP            = staff_rangers_log ~ 1 + PA_area_log + pop_density_log + GDP_2019_log,
  forml_PA_pop_GDP_coord      = staff_rangers_log ~ 1 + PA_area_log + pop_density_log + GDP_2019_log +  Matern(1|long + lat),
  forml_PA_pop_area_GDP       = staff_rangers_log ~ 1 + PA_area_log + pop_density_log + area_country_log + GDP_2019_log,
  forml_PA_pop_area_GDP_coord = staff_rangers_log ~ 1 + PA_area_log + pop_density_log + area_country_log + GDP_2019_log + Matern(1|long + lat),
  forml_PA_pop_PC2_area_GDP   = staff_rangers_log ~ 1 + PA_area_log + pop_density_log + PC2 + area_country_log + GDP_2019_log,
  forml_PA_pop_PC2_area_GDP_coord = staff_rangers_log ~ 1 + PA_area_log + pop_density_log + PC2 + area_country_log + GDP_2019_log + Matern(1|long + lat),
  forml_all                   = staff_rangers_log ~ pop_density_log +
                                                    lat + long +
                                                    PA_area_log + area_country_log + area_forest_pct +
                                                    GDP_2019_log + GDP_capita_log + GDP_growth + unemployment_log +
                                                    EVI + SPI + EPI_2020 +
                                                    IUCN_1_4_prop + IUCN_1_2_prop +
                                                    Matern(1|long + lat)
  )

formls_RF <- list(
  forml_coord                 = staff_rangers_log ~ 1 + lat + long,
  forml_PA                    = staff_rangers_log ~ 1 + PA_area_log,
  forml_pop                   = staff_rangers_log ~ 1 + pop_density_log,
  forml_PC2                   = staff_rangers_log ~ 1 + PC2,
  forml_area                  = staff_rangers_log ~ 1 + area_country_log,
  forml_GDP                   = staff_rangers_log ~ 1 + GDP_2019_log,
  forml_PA_coord              = staff_rangers_log ~ 1 + PA_area_log + lat + long,
  forml_pop_coord             = staff_rangers_log ~ 1 + pop_density_log + lat + long,
  forml_PC2_coord             = staff_rangers_log ~ 1 + PC2 + lat + long,
  forml_area_coord            = staff_rangers_log ~ 1 + area_country_log + lat + long,
  forml_GDP_coord             = staff_rangers_log ~ 1 + GDP_2019_log + lat + long,
  forml_PA_pop                = staff_rangers_log ~ 1 + PA_area_log + pop_density_log,
  forml_PA_pop_coord          = staff_rangers_log ~ 1 + PA_area_log + pop_density_log + lat + long,
  forml_PA_pop_PC2            = staff_rangers_log ~ 1 + PA_area_log + pop_density_log + PC2,
  forml_PA_pop_PC2_coord      = staff_rangers_log ~ 1 + PA_area_log + pop_density_log + PC2 + lat + long,
  forml_PA_pop_area           = staff_rangers_log ~ 1 + PA_area_log + pop_density_log + area_country_log,
  forml_PA_pop_area_coord     = staff_rangers_log ~ 1 + PA_area_log + pop_density_log + area_country_log +  lat + long,
  forml_PA_pop_GDP            = staff_rangers_log ~ 1 + PA_area_log + pop_density_log + GDP_2019_log,
  forml_PA_pop_GDP_coord      = staff_rangers_log ~ 1 + PA_area_log + pop_density_log + GDP_2019_log +  lat + long,
  forml_PA_pop_area_GDP       = staff_rangers_log ~ 1 + PA_area_log + pop_density_log + area_country_log + GDP_2019_log,
  forml_PA_pop_area_GDP_coord = staff_rangers_log ~ 1 + PA_area_log + pop_density_log + area_country_log + GDP_2019_log + lat + long,
  forml_PA_pop_PC2_area_GDP   = staff_rangers_log ~ 1 + PA_area_log + pop_density_log + PC2 + area_country_log + GDP_2019_log,
  forml_PA_pop_PC2_area_GDP_coord = staff_rangers_log ~ 1 + PA_area_log + pop_density_log + PC2 + area_country_log + GDP_2019_log + lat + long,
  forml_all                   = staff_rangers_log ~ . # same as fixed effects in formls_LMM$forml_all
)


## compute RMSE on test sets
n_tests <- 1000
Ncpu <- 100

# NOTES:
# We compare ML & REML predictions
#
test_REML                  <- lapply(formls_LMM, function(f) compute_rmse_lmm(formula = f, data = data_test, rep = n_tests, Ncpu = Ncpu,
                                                                              control.dist = list(dist.method = "Earth"), method = "REML"))
test_ML                    <- lapply(formls_LMM, function(f) compute_rmse_lmm(formula = f, data = data_test, rep = n_tests, Ncpu = Ncpu,
                                                                              control.dist = list(dist.method = "Earth"), method = "ML"))

# NOTES:
# 1. we try different values for mtry
# mtry defines the number of predictors to be used in each tree,
# in theory increasing mtry increases the correlation between trees which decreases the accuracy,
# but at the same time increasing mtry increases the so-called strength of each tree.
# So, there may be a best mtry. although it does not always have a large effect
#
# 2. we try increasing the number of trees,
# more trees imply to reduce the MCMC variance, but
# after reaching some number, increasing the number of trees has no effect but consuming CPU and memory.
# The default (500) may be enough.
#
# 3. we try adding noise to the response.
# Breiman showed that this can help in some case, he recommended a gaussian noise of 1 SD
#
# Many more things could be changed... (how to sample, how to split, how to aggregate...)
#
test_RF_mtry1              <- lapply(formls_RF, function(f) compute_rmse_rf(formula = f, data = data_test, rep = n_tests, Ncpu = Ncpu,
                                                                            mtry = 1))
test_RF_mtry1_noised       <- lapply(formls_RF, function(f) compute_rmse_rf(formula = f, data = data_test, rep = n_tests, Ncpu = Ncpu,
                                                                            add_noise = TRUE,
                                                                            mtry = 1))
test_RF_mtry1_20xmoretrees <- lapply(formls_RF, function(f) compute_rmse_rf(formula = f, data = data_test, rep = n_tests, Ncpu = Ncpu,
                                                                            mtry = 1, num.trees = 10000))
test_RF_mtry3              <- lapply(formls_RF, function(f) compute_rmse_rf(formula = f, data = data_test, rep = n_tests, Ncpu = Ncpu,
                                                                            mtry = function(nv) min(c(nv, 3))))
test_RF_mtry3_alwaysPA     <- lapply(formls_RF, function(f) {
                                      mtry_fn <- function(nv) { # mtry argument can take a function but the value needs to "work" (i.e. never be too large or too small)
                                        cannot_do <- ifelse(grepl("PA_area_log", as.character(f)[3]), 1, 0)
                                        max(c(1, min(c(nv - cannot_do, 3 - cannot_do))))
                                      }
                                      split <- ifelse(grepl("PA_area_log", as.character(f)[3]) & length(all.vars(f)) > 2, "PA_area_log", NA) # same here: cannot impose split if not in formula
                                      if (is.na(split)) split <- NULL
                                      compute_rmse_rf(formula = f, data = data_test, rep = n_tests, Ncpu = Ncpu,
                                                                            mtry = mtry_fn,
                                                                            always.split.variables = split)
                                      })
test_RF_mtryMAX            <- lapply(formls_RF, function(f) compute_rmse_rf(formula = f, data = data_test, rep = n_tests, Ncpu = Ncpu,
                                                                            mtry = function(nv) nv))


## let's reformat the results into a long data frame
rbind(cbind(do.call("cbind", test_REML), method = "spaMM_REML" ),
      cbind(do.call("cbind", test_ML), method = "spaMM_ML" ),
      cbind(do.call("cbind", test_RF_mtry1), method = "RF_mtry1" ),
      cbind(do.call("cbind", test_RF_mtry1_noised), method = "RF_mtry1_noised" ),
      cbind(do.call("cbind", test_RF_mtry1_20xmoretrees), method = "RF_mtry1_20xmoretrees" ),
      cbind(do.call("cbind", test_RF_mtry3), method = "RF_mtry3" ),
      cbind(do.call("cbind", test_RF_mtry3_alwaysPA), method = "RF_mtry3_alwaysPA" ),
      cbind(do.call("cbind", test_RF_mtryMAX), method = "RF_mtryMAX")) %>%
      as.data.frame() %>%
      pivot_longer(cols = -method, names_to = "model", values_to = "rmse") %>%
      mutate(model = sub(pattern = "forml_", replacement = "", model),
             rmse = as.numeric(rmse),
             model = forcats::fct_inorder(model)) -> test_results


## let's identify the best methods/models
test_results %>%
  group_by(method, model) %>%
  summarise(median = median(rmse),
            mean = mean(rmse),
            var = var(rmse)) %>%
  ungroup() -> test_results_summary

bind_rows(test_results_summary %>% arrange(mean) %>% slice(1:5),
          test_results_summary %>% arrange(median) %>% slice(1:5),
          test_results_summary %>% arrange(var) %>% slice(1:5)) %>%
  distinct()


# let's plot the results
pdf("result_accuracy.pdf", width = 20, height = 10)
ggplot(test_results) +
  aes(y = rmse, x = model, colour = method) +
  geom_boxplot(width = 0.3, position = position_dodge(width = 0.5)) +
  labs(y = "RMSE (on log + 1 response)", x = "Model", title = "Accuracy on test sets (10% of data)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45))
dev.off()

pdf("result_accuracy2.pdf", width = 20, height = 10)
ggplot(test_results) +
  aes(y = rmse, x = method, colour = model) +
  geom_boxplot(width = 0.3, position = position_dodge(width = 0.5)) +
  labs(y = "RMSE (on log + 1 response)", x = "Model", title = "Accuracy on test sets (10% of data)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45))
dev.off()

pdf("result_accuracy_spaMM_ML.pdf", width = 20, height = 10)
ggplot(test_results %>% filter(method == "spaMM_ML")) +
  aes(y = rmse, x = forcats::fct_reorder(model, rmse)) +
  geom_boxplot(width = 0.3, position = position_dodge(width = 0.5)) +
  stat_summary(fun = ~ mean(.x),
               fun.max = ~ mean(.x) + 2*sd(.x)/sqrt(length(.x)),
               fun.min = ~ mean(.x) - 2*sd(.x)/sqrt(length(.x)),
               colour = "red", shape = 1) +
  scale_y_continuous(minor_breaks = seq(0, 10, 0.1), breaks = 0:10) +
  labs(y = "RMSE (on log + 1 response)", x = "Model", title = "Accuracy on test sets (10% of data)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45))
dev.off()

pdf("result_accuracy_RF_mtry1.pdf", width = 20, height = 10)
ggplot(test_results %>% filter(method == "RF_mtry1")) +
  aes(y = rmse, x = forcats::fct_reorder(model, rmse)) +
  geom_boxplot(width = 0.3, position = position_dodge(width = 0.5)) +
  stat_summary(fun = ~ mean(.x),
               fun.max = ~ mean(.x) + 2*sd(.x)/sqrt(length(.x)),
               fun.min = ~ mean(.x) - 2*sd(.x)/sqrt(length(.x)),
               colour = "red", shape = 1) +
  scale_y_continuous(minor_breaks = seq(0, 10, 0.1), breaks = 0:10) +
  labs(y = "RMSE (on log + 1 response)", x = "Model", title = "Accuracy on test sets (10% of data)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45))
dev.off()
