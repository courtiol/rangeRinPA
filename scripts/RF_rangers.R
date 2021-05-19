library(rangeRinPA)
data <- data_rangers
Ncpu = 2
coef = 0
rep_feature_select = 10
rep_finetune = 100
rep_simu = 100
n_trees = 1000

data <- fill_PA_area(data, coef = coef)

record <- list(meta = tibble::tibble(start = Sys.time(),
                                     Ncpu = Ncpu,
                                     rep_feature_select = rep_feature_select,
                                     rep_finetune = rep_finetune,
                                     rep_simu = rep_simu,
                                     coef_population = coef))


cat("Step 1 + 2: General data preparation & preparation of initial training datasets\n")

formula_rangers_full <- staff_rangers_log ~ PA_area_log + lat + long + area_country_log + area_forest_pct + pop_density_log + GDP_2019_log + GDP_capita_log +
  GDP_growth + unemployment_log + EVI + SPI + EPI_2020 + IUCN_1_4_prop + IUCN_1_2_prop
formula_others_full <- stats::update(formula_rangers_full, staff_others_log ~ .)
formula_all_full <- stats::update(formula_rangers_full, staff_total_log ~ .)

data_initial_training_rangers <- build_initial_training_data(data,
                                                             formula = formula_rangers_full,
                                                             survey = "complete_known")
data_initial_training_others  <- build_initial_training_data(data,
                                                             formula = formula_others_full,
                                                             survey = "complete_known")
data_initial_training_all     <- build_initial_training_data(data,
                                                             formula = formula_all_full,
                                                             survey = "complete_known")

record <- c(record,
            list(rangers = tibble::tibble(initial_training_nrow = nrow(data_initial_training_rangers),
                                          initial_training_ncol = ncol(data_initial_training_rangers)),
                 others = tibble::tibble(initial_training_nrow = nrow(data_initial_training_others),
                                         initial_training_ncol = ncol(data_initial_training_others)),
                 all = tibble::tibble(initial_training_nrow = nrow(data_initial_training_all),
                                      initial_training_ncol = ncol(data_initial_training_all))))


cat("Step 3: Selection of predictor variables\n")

fit_data_initial_training_rangers_full <- ranger::ranger(formula_rangers_full,
                                                         data = data_initial_training_rangers,
                                                         splitrule = "extratrees", replace = FALSE,
                                                         mtry = function(n) n, min.node.size = 1, sample.fraction = 1,
                                                         num.trees = n_trees,
                                                         importance = "impurity")
fit_data_initial_training_others_full  <- ranger::ranger(formula_others_full,
                                                         data = data_initial_training_others,
                                                         splitrule = "extratrees", replace = FALSE,
                                                         mtry = function(n) n, min.node.size = 1, sample.fraction = 1,
                                                         num.trees = n_trees,
                                                         importance = "impurity")
fit_data_initial_training_all_full     <- ranger::ranger(formula_all_full,
                                                         data = data_initial_training_all,
                                                         splitrule = "extratrees", replace = FALSE,
                                                         mtry = function(n) n, min.node.size = 1, sample.fraction = 1,
                                                         num.trees = n_trees,
                                                         importance = "impurity")

cat("Step 3a: selection for rangers\n")
selection_training_rangers <- feature_selection_RF(
  full_fit = fit_data_initial_training_rangers_full,
  data = data_initial_training_rangers,
  rep = rep_feature_select, Ncpu = Ncpu,
  target = "staff_rangers_log",
  splitrule = "extratrees", replace = FALSE,
  mtry = function(n) n, min.node.size = 1, sample.fraction = 1,
  num.trees = n_trees,
  importance = "impurity")
