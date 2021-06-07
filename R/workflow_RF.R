
#' Run the full workflow for RF
#'
#' @inheritParams validate_RF
#' @inheritParams feature_selection_RF
#' @param coef the coefficient used to population rangers in unsurveyed part of surveyed countries (see [`fill_PA_area()`])
#' @param rep_feature_select the number of replicates for the feature selection (default = 1000)
#' @param rep_finetune the number of replicates for fine tuning (default = 1000)
#' @param rep_simu the number of simulation replicates (default = 10000)
#' @param n_trees the number of trees in the random forest
#'
#' @return a list with all the output information
#' @export
#'
#' @examples
#' \dontrun{
#'   RF_small_test <- run_RF_workflow(data = data_rangers, Ncpu = 2, coef = 0,
#'                                    rep_feature_select = 2, rep_finetune = 2, rep_simu = 2)
#' }
#'
run_RF_workflow <- function(data, rerank = TRUE, Ncpu = 2,  coef = 0, rep_feature_select = 1000, rep_finetune = 1000, rep_simu = 10000, n_trees = 1000) {
  set.seed(123)
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
                                           splitrule = c("variance", "extratrees"),
                                           min.node.size = 1:10,
                                           sample.fraction = c(0.632, 1),
                                           mtry = c(function(n) 1, function(n) floor(n/3), function(n) n),
                                           stringsAsFactors = FALSE) # important!

  cat("Step 5a: Fine tuning for rangers\n")
  finetuning_rangers <- finetune_RF_grid(grid = param_grid_for_finetuning,
                                         formula = selected_formula_rangers,
                                         data = data_final_training_rangers,
                                         spatial = record$rangers$selected_spatial,
                                         rep = rep_finetune, Ncpu = Ncpu, num.trees = n_trees)

  cat("Step 5b: Fine tuning for others\n")
  finetuning_others <- finetune_RF_grid(grid = param_grid_for_finetuning,
                                        formula = selected_formula_others,
                                        data = data_final_training_others,
                                        spatial = record$others$selected_spatial,
                                        rep = rep_finetune, Ncpu = Ncpu, num.trees = n_trees)

  cat("Step 5c: Fine tuning for all\n")
  finetuning_all <- finetune_RF_grid(grid = param_grid_for_finetuning,
                                     formula = selected_formula_all,
                                     data = data_final_training_all,
                                     spatial = record$all$selected_spatial,
                                     rep = rep_finetune, Ncpu = Ncpu, num.trees = n_trees)

  record$rangers$fine_tuning <- list(finetuning_rangers)
  record$rangers$best_tuning <- list(as.list(finetuning_rangers$mean[which.min(finetuning_rangers$mean$RMSE),
                                                                     c("replace", "splitrule", "min.node.size", "sample.fraction", "mtry")]))

  record$others$fine_tuning <- list(finetuning_others)
  record$others$best_tuning <- list(as.list(finetuning_others$mean[which.min(finetuning_others$mean$RMSE),
                                                                   c("replace", "splitrule", "min.node.size", "sample.fraction", "mtry")]))

  record$all$fine_tuning <- list(finetuning_all)
  record$all$best_tuning <- list(as.list(finetuning_all$mean[which.min(finetuning_all$mean$RMSE),
                                                             c("replace", "splitrule", "min.node.size", "sample.fraction", "mtry")]))

  cat("Step 6: Final training\n")

  fit_final_rangers <- ranger::ranger(selected_formula_rangers,
                                      data = data_final_training_rangers,
                                      splitrule = record$rangers$best_tuning[[1]]$splitrule,
                                      replace = record$rangers$best_tuning[[1]]$replace,
                                      mtry = record$rangers$best_tuning[[1]]$mtry[[1]],
                                      min.node.size = record$rangers$best_tuning[[1]]$min.node.size,
                                      sample.fraction = record$rangers$best_tuning[[1]]$sample.fraction,
                                      num.trees = n_trees,
                                      importance = "impurity", quantreg = TRUE)

  fit_final_others <- ranger::ranger(selected_formula_others,
                                     data = data_final_training_others,
                                     splitrule = record$others$best_tuning[[1]]$splitrule,
                                     replace = record$others$best_tuning[[1]]$replace,
                                     mtry = record$others$best_tuning[[1]]$mtry[[1]],
                                     min.node.size = record$others$best_tuning[[1]]$min.node.size,
                                     sample.fraction = record$others$best_tuning[[1]]$sample.fraction,
                                     num.trees = n_trees,
                                     importance = "impurity", quantreg = TRUE)

  fit_final_all <- ranger::ranger(selected_formula_all,
                                  data = data_final_training_all,
                                  splitrule = record$all$best_tuning[[1]]$splitrule,
                                  replace = record$all$best_tuning[[1]]$replace,
                                  mtry = record$all$best_tuning[[1]]$mtry[[1]],
                                  min.node.size = record$all$best_tuning[[1]]$min.node.size,
                                  sample.fraction = record$all$best_tuning[[1]]$sample.fraction,
                                  num.trees = n_trees,
                                  importance = "impurity", quantreg = TRUE)


  cat("Step 7: Preparation of datasets for predictions & simulations\n")

  data_final_pred_rangers <- build_final_pred_data(
    data = data,
    formula = selected_formula_rangers,
    survey = "complete_known",
    spatial = record$rangers$selected_spatial)

  data_final_pred_others <- build_final_pred_data( # 1 country missing -> Greenland
    data = data,
    formula = selected_formula_others,
    survey = "complete_known",
    spatial = record$others$selected_spatial)

  data_final_pred_all <- build_final_pred_data(
    data = data,
    formula = selected_formula_all,
    survey = "complete_known",
    spatial = record$all$selected_spatial)


  record$rangers$nrow_obs_or_imputed <- length(data_final_pred_rangers$data_known$PA_area_surveyed)
  record$others$nrow_obs_or_imputed <- length(data_final_pred_others$data_known$PA_area_surveyed)
  record$all$nrow_obs_or_imputed <- length(data_final_pred_all$data_known$PA_area_surveyed)

  record$rangers$nrow_no_predict <- length(data_final_pred_rangers$data_not_predictable$PA_area_surveyed)
  record$others$nrow_no_predict <- length(data_final_pred_others$data_not_predictable$PA_area_surveyed)
  record$all$nrow_no_predict <- length(data_final_pred_all$data_not_predictable$PA_area_surveyed)

  record$rangers$nrow_predict <- length(data_final_pred_rangers$data_predictable$PA_area_surveyed)
  record$others$nrow_predict <- length(data_final_pred_others$data_predictable$PA_area_surveyed)
  record$all$nrow_predict <- length(data_final_pred_all$data_predictable$PA_area_surveyed)

  record$rangers$PA_area_obs_or_imputed <- sum(data_final_pred_rangers$data_known$PA_area_surveyed)
  record$others$PA_area_obs_or_imputed <- sum(data_final_pred_others$data_known$PA_area_surveyed)
  record$all$PA_area_obs_or_imputed <- sum(data_final_pred_all$data_known$PA_area_surveyed)

  record$rangers$PA_area_no_predict <- sum(data_final_pred_rangers$data_not_predictable$PA_area_surveyed)
  record$others$PA_area_no_predict <- sum(data_final_pred_others$data_not_predictable$PA_area_surveyed)
  record$all$PA_area_no_predict <- sum(data_final_pred_all$data_not_predictable$PA_area_surveyed)

  record$rangers$PA_area_predict <- sum(data_final_pred_rangers$data_predictable$PA_area_surveyed)
  record$others$PA_area_predict <- sum(data_final_pred_others$data_predictable$PA_area_surveyed)
  record$all$PA_area_predict <- sum(data_final_pred_all$data_predictable$PA_area_surveyed)


  cat("Step 8a: Point predictions\n")

  data_final_pred_rangers$data_predictable$staff_rangers_log_predicted <- stats::predict(
    fit_final_rangers, data = data_final_pred_rangers$data_predictable)$predictions
  tallies_rangers <- compute_tally(data_final_pred_rangers)

  data_final_pred_others$data_predictable$staff_others_log_predicted <- stats::predict(
    fit_final_others, data = data_final_pred_others$data_predictable)$predictions
  tallies_others <- compute_tally(data_final_pred_others)

  data_final_pred_all$data_predictable$staff_total_log_predicted <- stats::predict(
    fit_final_all, data = data_final_pred_all$data_predictable)$predictions
  tallies_all <- compute_tally(data_final_pred_all)

  record$rangers$tally_obs_or_imputed <- tallies_rangers[1, "value"]
  record$others$tally_obs_or_imputed <- tallies_others[1, "value"]
  record$all$tally_obs_or_imputed <- tallies_all[1, "value"]

  record$rangers$tally_predicted <- tallies_rangers[2, "value"]
  record$others$tally_predicted <- tallies_others[2, "value"]
  record$all$tally_predicted <- tallies_all[2, "value"]

  record$rangers$tally_total <- tallies_rangers[3, "value"]
  record$others$tally_total <- tallies_others[3, "value"]
  record$all$tally_total <- tallies_all[3, "value"]


  cat("Step 8b: Simulations\n")

  predictions_rangers <- parallel::mclapply(seq_len(rep_simu), function(i) {
    sim_rangers <- stats::predict(fit_final_rangers,
                                  data = data_final_pred_rangers$data_predictable,
                                  type = "quantiles",
                                  quantiles = stats::runif(n = nrow(data_final_pred_rangers$data_predictable)))
    data_final_pred_rangers$data_predictable$staff_rangers_log_predicted <- diag(sim_rangers$predictions)
    compute_tally(data_final_pred_rangers)[3, "value"]}, mc.cores = Ncpu)

  predictions_others <- parallel::mclapply(seq_len(rep_simu), function(i) {
    sim_others <- stats::predict(fit_final_others,
                                 data = data_final_pred_others$data_predictable,
                                 type = "quantiles",
                                 quantiles = stats::runif(n = nrow(data_final_pred_others$data_predictable)))
    data_final_pred_others$data_predictable$staff_others_log_predicted <- diag(sim_others$predictions)
    compute_tally(data_final_pred_others)[3, "value"]}, mc.cores = Ncpu)

  predictions_all <- parallel::mclapply(seq_len(rep_simu), function(i) {
    sim_all <- stats::predict(fit_final_all,
                              data = data_final_pred_all$data_predictable,
                              type = "quantiles",
                              quantiles = stats::runif(n = nrow(data_final_pred_all$data_predictable)))
    data_final_pred_all$data_predictable$staff_total_log_predicted <- diag(sim_all$predictions)
    compute_tally(data_final_pred_all)[3, "value"]}, mc.cores = Ncpu)

  record$rangers$lwr <- stats::quantile(unlist(predictions_rangers), probs = 0.025)
  record$others$lwr <- stats::quantile(unlist(predictions_others), probs = 0.025)
  record$all$lwr <- stats::quantile(unlist(predictions_all), probs = 0.025)

  record$rangers$upr <- stats::quantile(unlist(predictions_rangers), probs = 0.975)
  record$others$upr <- stats::quantile(unlist(predictions_others), probs = 0.975)
  record$all$upr <- stats::quantile(unlist(predictions_all), probs = 0.975)

  cat("DONE!\n")
  record$meta$duration_h <- as.double(difftime(Sys.time(), record$meta$start, units = "hours"))
  record
}
