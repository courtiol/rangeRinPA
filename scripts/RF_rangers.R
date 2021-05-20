library(rangeRinPA)
options(width = 350)
data <- data_rangers
Ncpu = 100
coef = 0
rep_feature_select = 100
rep_finetune = 100
rep_simu = 100
n_trees = 100
rerank = TRUE

data <- fill_PA_area(data, coef = coef)

record <- list(meta = tibble::tibble(start = Sys.time(),
                                     Ncpu = Ncpu,
                                     rerank = rerank,
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
  rerank = rerank,
  rep = rep_feature_select, Ncpu = Ncpu,
  target = "staff_rangers_log",
  splitrule = "extratrees", replace = FALSE,
  mtry = function(n) n, min.node.size = 1, sample.fraction = 1,
  num.trees = n_trees,
  importance = "impurity")

cat("Step 3b: selection for others\n")
selection_training_others <- feature_selection_RF(
  full_fit = fit_data_initial_training_others_full,
  data = data_initial_training_others,
  rerank = rerank,
  rep = rep_feature_select, Ncpu = Ncpu,
  target = "staff_others_log",
  splitrule = "extratrees", replace = FALSE,
  mtry = function(n) n, min.node.size = 1, sample.fraction = 1,
  num.trees = n_trees,
  importance = "impurity")

cat("Step 3c: selection for all\n")
selection_training_all <- feature_selection_RF(
  full_fit = fit_data_initial_training_all_full,
  data = data_initial_training_all,
  rerank = rerank,
  rep = rep_feature_select, Ncpu = Ncpu,
  target = "staff_total_log",
  splitrule = "extratrees", replace = FALSE,
  mtry = function(n) n, min.node.size = 1, sample.fraction = 1,
  num.trees = n_trees,
  importance = "impurity")

selected_formula_rangers <- stats::as.formula(selection_training_rangers$formula[1])
selected_formula_others <- stats::as.formula(selection_training_others$formula[1])
selected_formula_all <- stats::as.formula(selection_training_all$formula[1])

record$rangers$selected_features <- list(selection_training_rangers)
record$rangers$selected_formula <- deparse(selected_formula_rangers, width.cutoff = 500)
record$rangers$selected_spatial <- selection_training_rangers$spatial[1]

record$others$selected_features <- list(selection_training_others)
record$others$selected_formula <- deparse(selected_formula_others, width.cutoff = 500)
record$others$selected_spatial <- selection_training_others$spatial[1]

record$all$selected_features <- list(selection_training_all)
record$all$selected_formula <- deparse(selected_formula_all, width.cutoff = 500)
record$all$selected_spatial <- selection_training_all$spatial[1]


cat("Step 4: Preparation of final training datasets\n")

data_final_training_rangers <- build_final_training_data(data = data,
                                                         formula = selected_formula_rangers,
                                                         survey = "complete_known",
                                                         spatial = record$rangers$selected_spatial)

data_final_training_others <- build_final_training_data(data = data,
                                                        formula = selected_formula_others,
                                                        survey = "complete_known",
                                                        spatial = record$others$selected_spatial)

data_final_training_all <- build_final_training_data(data = data,
                                                     formula = selected_formula_all,
                                                     survey = "complete_known",
                                                     spatial = record$all$selected_spatial)

record$rangers$final_training_nrow <- nrow(data_final_training_rangers)
record$others$final_training_nrow <- nrow(data_final_training_others)
record$all$final_training_nrow <- nrow(data_final_training_all)
record$rangers$final_training_ncol <- ncol(data_final_training_rangers)
record$others$final_training_ncol <- ncol(data_final_training_others)
record$all$final_training_ncol <- ncol(data_final_training_all)


cat("Step 5: Selection of function inputs (fine tuning)\n")

param_grid_for_finetuning <- expand.grid(replace = c(TRUE, FALSE),
                                         splitrule = c("'variance'", "'extratrees'"),
                                         min.node.size = 1:10,
                                         sample.fraction = c(0.632, 1),
                                         mtry = c(function(n) 1, function(n) floor(n/3), function(n) n),
                                         stringsAsFactors = FALSE) # important!

finetuning_rangers <- finetune_RF_grid(grid = param_grid_for_finetuning,
                                       formula = selected_formula_rangers,
                                       data = data_final_training_rangers,
                                       spatial = record$rangers$selected_spatial,
                                       rep = rep_finetune, Ncpu = Ncpu, num.trees = n_trees)

finetuning_others <- finetune_RF_grid(grid = param_grid_for_finetuning,
                                      formula = selected_formula_others,
                                      data = data_final_training_others,
                                      spatial = record$others$selected_spatial,
                                      rep = rep_finetune, Ncpu = Ncpu, num.trees = n_trees)

finetuning_all <- finetune_RF_grid(grid = param_grid_for_finetuning,
                                   formula = selected_formula_all,
                                   data = data_final_training_all,
                                   spatial = record$all$selected_spatial,
                                   rep = rep_finetune, Ncpu = Ncpu, num.trees = n_trees)

record$rangers$fine_tuning <- list(finetuning_rangers)
record$others$fine_tuning <- list(finetuning_others)
record$all$fine_tuning <- list(finetuning_all)
