library(rangeRinPA)
library(dplyr)
library(tidyr)
library(ggplot2)
library(spaMM)
library(ranger)

rm(list = ls())

## function to compute RMSE on test set for LMM
compute_rmse_lmm <- function(formula, data, rep = 10, Ncpu = 1, target = "staff_rangers_log", spatial, ...) {
  if (spatial == "Matern") {
    formula <- update.formula(formula, . ~ . + Matern(1 |lat + long))
    } else stopifnot(!spatial)
  seed <- 123 # we make sure all dataset are the same across comparisons
  rmse <- parallel::mclapply(seq_len(rep), function(i) {
    data_list <- prepare_data(formula = formula, data = data, test_prop = 0.1, seed = seed + i)
    newfit <- spaMM::fitme(formula = formula, data = data_list$data_train, ...)
    RMSE(predict(newfit, newdata = data_list$data_test)[, 1], data_list$data_test[, target, drop = TRUE])
  }, mc.cores = Ncpu)
  unlist(rmse)
}

add_dist_predictors_to_data <- function(data) {
  df <- as.data.frame(spaMM::make_scaled_dist(data[, c("lat", "long")], rho = 1, dist.method = "Earth", return_matrix = TRUE))
  colnames(df) <- paste0("dist_", 1:ncol(df))
  dplyr::bind_cols(data, df)
}
#add_dist_predictors_to_data(data_test)

add_dist_predictors_to_formula <- function(formula, data) {
  dist_vars <- colnames(data)[grep("dist_", colnames(data))]
  as.formula(paste(formula[[2]], paste(c(formula[[3]], dist_vars), collapse = "+"), sep = " ~ "))
}
#add_dist_predictors_to_formula(test ~ bla, add_dist_predictors_to_data(data_test))

## function to compute RMSE on test set for RF (using Cross Validation or Out Of Bag obs)
compute_rmse_rf <- function(formula, data, rep = 10, Ncpu = 1, target = "staff_rangers_log", spatial, method = "CV", ...) {
  if (spatial == "latlong") {
      formula <- update.formula(formula, . ~ . + lat + long)
    } else if (spatial == "dist") {
      data <- add_dist_predictors_to_data(data)
      formula <- add_dist_predictors_to_formula(formula = formula, data = data)
    } else if (spatial != FALSE) {
      stop("Spatial method not found")
    }
  seed <- 123 # we make sure all dataset are the same across comparisons
  rmse <- parallel::mclapply(seq_len(rep), function(i) {
    if (method == "CV") {
      data_list <- prepare_data(formula = formula, data = data, test_prop = 0.1, seed = seed + i)
      newfit <- ranger::ranger(formula = formula, data = data_list$data_train, num.threads = 1, ...)
      RMSE(predict(newfit, data = data_list$data_test, num.threads = 1)$predictions, data_list$data_test[, target, drop = TRUE])
      #NOTE: num.threads = 1 is an attempt not to use multiple threads since we do parallelisation at a higher level, but does not seem to work :-(
    } else if (method == "OOB") {
      newfit <- ranger::ranger(formula = formula, data = data, num.threads = 1, ...)
      sqrt(newfit$prediction.error)
    } else stop("method unknown")
  }, mc.cores = Ncpu)
  unlist(rmse)
}


## function to compute RMSE on test set for LMM_RF
compute_rmse_lmm_rf <- function(formula_lmm, formula_RF, data, rep = 10, Ncpu = 1, target = "staff_rangers_log", spatial, args_spaMM = list(), args_RF = list()) {
  if (spatial == "Matern") {
    formula_lmm <- update.formula(formula_lmm, . ~ . + Matern(1 |lat + long))
  } else stopifnot(!spatial)
  seed <- 123 # we make sure all dataset are the same across comparisons
  rmse <- parallel::mclapply(seq_len(rep), function(i) {
    data_list <- prepare_data(formula = formula_lmm, data = data, test_prop = 0.1, seed = seed + i)
    args_spaMM$formula <- formula_lmm
    args_spaMM$data <- data_list$data_train
    newfit_LMM <- do.call(spaMM::fitme, args_spaMM)
    data_list$data_train$pred_spaMM <- predict(newfit_LMM, newdata = data_list$data_train)[, 1]
    formula_RF <- update.formula(formula_RF, . ~ . + pred_spaMM)
    args_RF$formula <- formula_RF
    args_RF$data <- data_list$data_train
    args_RF$num.threads <- 1
    newfit_RF <- do.call(ranger::ranger, args_RF)
    data_list$data_test$pred_spaMM <- predict(newfit_LMM, newdata = data_list$data_test)[, 1]
    RMSE(predict(newfit_RF, data = data_list$data_test, num.threads = 1)$predictions, data_list$data_test[, target, drop = TRUE])
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
# on PCA
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


## let's define the formulas
formls <- list(
  # forml_PA                    = staff_rangers_log ~ PA_area_log,
  # forml_pop                   = staff_rangers_log ~ pop_density_log,
  # forml_PC2                   = staff_rangers_log ~ PC2,
  # forml_area                  = staff_rangers_log ~ area_country_log,
  # forml_GDP                   = staff_rangers_log ~ GDP_2019_log,
  forml_PA_pop                = staff_rangers_log ~ PA_area_log + pop_density_log,
  forml_PA_pop_PC2            = staff_rangers_log ~ PA_area_log + pop_density_log + PC2,
  forml_PA_pop_area           = staff_rangers_log ~ PA_area_log + pop_density_log + area_country_log,
  forml_PA_pop_GDP            = staff_rangers_log ~ PA_area_log + pop_density_log + GDP_2019_log,
  forml_PA_pop_area_GDP       = staff_rangers_log ~ PA_area_log + pop_density_log + area_country_log + GDP_2019_log,
  forml_PA_pop_PC2_area_GDP   = staff_rangers_log ~ PA_area_log + pop_density_log + PC2 + area_country_log + GDP_2019_log,
  forml_all                   = staff_rangers_log ~ pop_density_log +
                                                    PA_area_log + area_country_log + area_forest_pct +
                                                    GDP_2019_log + GDP_capita_log + GDP_growth + unemployment_log +
                                                    EVI + SPI + EPI_2020 +
                                                    IUCN_1_4_prop + IUCN_1_2_prop)



## compute RMSE on test sets
n_tests <- 1000
Ncpu <- 100

test_LM  <- lapply(formls, function(f) compute_rmse_lmm(formula = f, data = data_test, rep = n_tests, Ncpu = Ncpu, spatial = FALSE, method = "ML"))

test_LMM <- lapply(formls, function(f) compute_rmse_lmm(formula = f, data = data_test, rep = n_tests, Ncpu = Ncpu, spatial = "Matern",
                                                        control.dist = list(dist.method = "Earth"), method = "REML"))

test_RF  <- lapply(formls, function(f) compute_rmse_rf(formula = f, data = data_test, rep = n_tests, Ncpu = Ncpu, spatial = FALSE))
test_RF_latlong  <- lapply(formls, function(f) compute_rmse_rf(formula = f, data = data_test, rep = n_tests, Ncpu = Ncpu, spatial = "latlong"))
test_RF_dist  <- lapply(formls, function(f) compute_rmse_rf(formula = f, data = data_test, rep = n_tests, Ncpu = Ncpu, spatial = "dist"))

test_LMM_RF <- lapply(formls, function(f) compute_rmse_lmm_rf(formula_lmm = f, formula_RF = f, data = data_test, rep = n_tests, Ncpu = Ncpu, spatial = "Matern",
                                                              args_spaMM = list(control.dist = list(dist.method = "Earth"), method = "REML")))

# test_RF_mtry3_alwaysPA     <- lapply(formls, function(f) {
#                                       mtry_fn <- function(nv) { # mtry argument can take a function but the value needs to "work" (i.e. never be too large or too small)
#                                         cannot_do <- ifelse(grepl("PA_area_log", as.character(f)[3]), 1, 0)
#                                         max(c(1, min(c(nv - cannot_do, 3 - cannot_do))))
#                                       }
#                                       split <- ifelse(grepl("PA_area_log", as.character(f)[3]) & length(all.vars(f)) > 2, "PA_area_log", NA) # same here: cannot impose split if not in formula
#                                       if (is.na(split)) split <- NULL
#                                       compute_rmse_rf(formula = f, data = data_test, rep = n_tests, Ncpu = Ncpu,
#                                                                             mtry = mtry_fn,
#                                                                             always.split.variables = split)
#                                       })
# test_RF_mtryMAX            <- lapply(formls, function(f) compute_rmse_rf(formula = f, data = data_test, rep = n_tests, Ncpu = Ncpu,
#                                                                             mtry = function(nv) nv))


## let's reformat the results into a long data frame
rbind(cbind(do.call("cbind", test_LM), method = "LM" ),
      cbind(do.call("cbind", test_LMM), method = "LMM" ),
      cbind(do.call("cbind", test_RF), method = "RF" ),
      cbind(do.call("cbind", test_RF_latlong), method = "RF_latlong"),
      cbind(do.call("cbind", test_RF_dist), method = "RF_dist"),
      cbind(do.call("cbind", test_LMM_RF), method = "LMM_RF")
      ) %>%
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

bind_rows(test_results_summary %>% arrange(mean) %>% slice(1:10),
          test_results_summary %>% arrange(median) %>% slice(1:10),
          test_results_summary %>% arrange(var) %>% slice(1:10)) %>%
  distinct()


## let's plot the results
pdf("result_accuracy.pdf", width = 20, height = 10)
ggplot(test_results) +
  aes(y = rmse, x = forcats::fct_reorder(model, rmse, .fun = mean), colour = method) +
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


## Fine tuning the parameters for the RF

### let's define a custom function allowing for fine tuning any parameter (one at a time):
fine_tune_param <- function(values_to_try, param_to_tune, formula, data, rep = 1000, Ncpu = 1, target = "staff_rangers_log", fn = mean, ...) {

  call_OOB  <- paste("sapply(values_to_try, function(i) fn(compute_rmse_rf(formula = formula, data = data,
                      rep = rep, Ncpu = Ncpu, target = target, method = 'OOB',", deparse1(substitute(param_to_tune)), " = i, ...)))")

  call_CV  <- paste("sapply(values_to_try, function(i) fn(compute_rmse_rf(formula = formula, data = data,
                      rep = rep, Ncpu = Ncpu, target = target, method = 'CV',", deparse1(substitute(param_to_tune)), " = i, ...)))")

  test_OOB <- eval(parse(text = call_OOB))
  test_CV <- eval(parse(text = call_CV))

  data.frame(param = c(values_to_try, values_to_try),
             RMSE = c(test_OOB, test_CV),
             test = c(rep("OOB", length(values_to_try)),
                      rep("CV", length(values_to_try)))) %>%
    group_by(test) %>%
    mutate(RMSE_rel = RMSE - min(RMSE)) %>%
    ungroup() -> output

  ggplot(output) +
    aes(y = RMSE_rel, x = param, colour = test) +
    geom_line() +
    scale_x_continuous(breaks = values_to_try) +
    labs(y = "RMSE - min(RMSE)",
         x = "Parameter values",
         title = paste0("Effect of ", deparse1(substitute(param_to_tune)))) +
    theme_minimal() -> plot

  list(output = output, plot = plot)
}

### Testing effect of min.node.size in RF
test_nodesize <- fine_tune_param(values_to_try = 1:20, param_to_tune = min.node.size,
                                 formula = formls_RF$forml_PA_pop_area, data = data_test, rep = 1000, Ncpu = 100, mtry = 1)
test_nodesize

### Testing effect of num.trees in RF
test_numtree <- fine_tune_param(values_to_try = 2^(7:15), param_to_tune = num.trees,
                                formula = formls_RF$forml_PA_pop_area, data = data_test, rep = 1000, Ncpu = 100, mtry = 1)
test_numtree$plot + coord_trans(x = "sqrt")

### Testing effect of sample.fraction in RF
test_samplefraction <- fine_tune_param(values_to_try = seq(0.5, 1, by = 0.05), param_to_tune = sample.fraction,
                                       formula = formls_RF$forml_PA_pop_area, data = data_test, rep = 1000, Ncpu = 100, mtry = 1)
test_samplefraction

# NOTES:
# mtry defines the number of predictors to be used in each tree,
# in theory increasing mtry increases the correlation between trees which decreases the accuracy,
# but at the same time increasing mtry increases the so-called strength of each tree.
# So, there may be a best mtry. although it does not always have a large effect
#
# more trees imply to reduce the MCMC variance, but
# after reaching some number, increasing the number of trees has little effect but consuming CPU and memory.
#
