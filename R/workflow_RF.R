
#' Run the full workflow for RF
#'
#' @inheritParams validate_RF
#' @inheritParams feature_selection_RF
#' @inheritParams prepare_grid_finetuning
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
#'   RF_small_test <- run_RF_workflow(data = data_rangers, Ncpu = 2, coef = 1,
#'                                    rep_feature_select = 2, rep_finetune = 2, rep_simu = 2,
#'                                    grid_type = "coarse", n_trees = 100)
#' }
#'
run_RF_workflow <- function(data, rerank = TRUE, Ncpu = 2,  coef = 1, rep_feature_select = 1000, rep_finetune = 1000, rep_simu = 10000, n_trees = 10000, grid_type = "fine") {

  set.seed(123)

  record <- list(meta = tibble::tibble(start = Sys.time(),
                                       Ncpu = Ncpu,
                                       rerank = rerank,
                                       rep_feature_select = rep_feature_select,
                                       rep_finetune = rep_finetune,
                                       rep_simu = rep_simu,
                                       coef_population = coef))


  cat("Step 1 + 2: General data preparation & preparation of initial training datasets\n")

  data_not_imputed <- data
  data <- fill_PA_area(data, coef = coef) ## Imputation step

  formula_rangers_full <- staff_rangers_log ~ PA_area_log + lat + long + area_country_log + area_forest_pct + pop_density_log + GDP_2019_log + GDP_capita_log +
    GDP_growth + unemployment_log + EVI + SPI + EPI_2020 + IUCN_1_4_prop + IUCN_1_2_prop
  formula_others_full <- stats::update(formula_rangers_full, staff_others_log ~ .)
  formula_all_full <- stats::update(formula_rangers_full, staff_total_log ~ .)

  data_initial_training_rangers <- build_initial_training_data(data,
                                                               formula = formula_rangers_full,
                                                               survey = "complete_known") # note: complete_known includes imputed PAs!
  data_initial_training_others  <- build_initial_training_data(data,
                                                               formula = formula_others_full,
                                                               survey = "complete_known")
  data_initial_training_all     <- build_initial_training_data(data,
                                                               formula = formula_all_full,
                                                               survey = "complete_known")

  record <- c(record,
              list(rangers = tibble::tibble(initial_training_nrow = nrow(data_initial_training_rangers),
                                            initial_training_ncol = ncol(data_initial_training_rangers),
                                            initial_PA_included =  sum(data_initial_training_rangers$PA_area_surveyed)),
                   others = tibble::tibble(initial_training_nrow = nrow(data_initial_training_others),
                                           initial_training_ncol = ncol(data_initial_training_others),
                                           initial_PA_included =  sum(data_initial_training_others$PA_area_surveyed)),
                   all = tibble::tibble(initial_training_nrow = nrow(data_initial_training_all),
                                        initial_training_ncol = ncol(data_initial_training_all),
                                        initial_PA_included =  sum(data_initial_training_all$PA_area_surveyed))))


  cat("Step 3: Selection of predictor variables\n")

  fit_data_initial_training_rangers_full <- ranger::ranger(formula_rangers_full,
                                                           data = data_initial_training_rangers,
                                                           splitrule = "extratrees", replace = FALSE,
                                                           mtry = function(p) p, min.node.size = 1, sample.fraction = 1,
                                                           num.trees = n_trees,
                                                           importance = "impurity",
                                                           verbose = FALSE)
  fit_data_initial_training_others_full  <- ranger::ranger(formula_others_full,
                                                           data = data_initial_training_others,
                                                           splitrule = "extratrees", replace = FALSE,
                                                           mtry = function(p) p, min.node.size = 1, sample.fraction = 1,
                                                           num.trees = n_trees,
                                                           importance = "impurity",
                                                           verbose = FALSE)
  fit_data_initial_training_all_full     <- ranger::ranger(formula_all_full,
                                                           data = data_initial_training_all,
                                                           splitrule = "extratrees", replace = FALSE,
                                                           mtry = function(p) p, min.node.size = 1, sample.fraction = 1,
                                                           num.trees = n_trees,
                                                           importance = "impurity",
                                                           verbose = FALSE)

  cat("Step 3a: selection for rangers\n")
  selection_training_rangers <- feature_selection_RF(
    full_fit = fit_data_initial_training_rangers_full,
    data = data_initial_training_rangers,
    rerank = rerank,
    rep = rep_feature_select, Ncpu = Ncpu,
    target = "staff_rangers_log",
    splitrule = "extratrees", replace = FALSE,
    mtry = function(p) p, min.node.size = 1, sample.fraction = 1,
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
    mtry = function(p) p, min.node.size = 1, sample.fraction = 1,
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
    mtry = function(p) p, min.node.size = 1, sample.fraction = 1,
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

  record$rangers$final_PA_included <- sum(data_final_training_rangers$PA_area_surveyed)
  record$others$final_PA_included <- sum(data_final_training_others$PA_area_surveyed)
  record$all$final_PA_included <- sum(data_final_training_all$PA_area_surveyed)


  cat("Step 5: Selection of function inputs (fine tuning)\n")

  param_grid_for_finetuning <- prepare_grid_finetuning(grid_type = grid_type)

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
                                      importance = "impurity", quantreg = TRUE,
                                      verbose = FALSE)

  fit_final_others <- ranger::ranger(selected_formula_others,
                                     data = data_final_training_others,
                                     splitrule = record$others$best_tuning[[1]]$splitrule,
                                     replace = record$others$best_tuning[[1]]$replace,
                                     mtry = record$others$best_tuning[[1]]$mtry[[1]],
                                     min.node.size = record$others$best_tuning[[1]]$min.node.size,
                                     sample.fraction = record$others$best_tuning[[1]]$sample.fraction,
                                     num.trees = n_trees,
                                     importance = "impurity", quantreg = TRUE,
                                     verbose = FALSE)

  fit_final_all <- ranger::ranger(selected_formula_all,
                                  data = data_final_training_all,
                                  splitrule = record$all$best_tuning[[1]]$splitrule,
                                  replace = record$all$best_tuning[[1]]$replace,
                                  mtry = record$all$best_tuning[[1]]$mtry[[1]],
                                  min.node.size = record$all$best_tuning[[1]]$min.node.size,
                                  sample.fraction = record$all$best_tuning[[1]]$sample.fraction,
                                  num.trees = n_trees,
                                  importance = "impurity", quantreg = TRUE,
                                  verbose = FALSE)


  cat("Step 7: Preparation of datasets for predictions\n")

  data %>%
    dplyr::filter(.data$area_PA_total == 0) %>%
    dplyr::pull(.data$countryname_eng) -> dont_predict

  message(paste(c("The number of staff for the following countries/territories are not predicted since no PA exist in these locations according to WDPA:", dont_predict), collapse = "\n"))

  data_final_pred_rangers <- build_final_pred_data(
    data = data,
    formula = selected_formula_rangers,
    survey = "complete_known",
    spatial = record$rangers$selected_spatial,
    outliers = dont_predict)

  data_final_pred_others <- build_final_pred_data(
    data = data,
    formula = selected_formula_others,
    survey = "complete_known",
    spatial = record$others$selected_spatial,
    outliers = dont_predict)

  data_final_pred_all <- build_final_pred_data(
    data = data,
    formula = selected_formula_all,
    survey = "complete_known",
    spatial = record$all$selected_spatial,
    outliers = dont_predict)


  record$rangers$nrow_obs_or_imputed <- length(data_final_pred_rangers$data_known$PA_area_surveyed)
  record$others$nrow_obs_or_imputed <- length(data_final_pred_others$data_known$PA_area_surveyed)
  record$all$nrow_obs_or_imputed <- length(data_final_pred_all$data_known$PA_area_surveyed)

  record$rangers$nrow_predict <- length(data_final_pred_rangers$data_predictable$PA_area_surveyed)
  record$others$nrow_predict <- length(data_final_pred_others$data_predictable$PA_area_surveyed)
  record$all$nrow_predict <- length(data_final_pred_all$data_predictable$PA_area_surveyed)

  record$rangers$nrow_no_predict <- length(data_final_pred_rangers$data_not_predictable$PA_area_surveyed)
  record$others$nrow_no_predict <- length(data_final_pred_others$data_not_predictable$PA_area_surveyed)
  record$all$nrow_no_predict <- length(data_final_pred_all$data_not_predictable$PA_area_surveyed)


  cat("Step 8a: Point predictions\n")

  ## We compute the tallies:
  data_final_pred_rangers$data_predictable$staff_rangers_log_predicted <- stats::predict(
    fit_final_rangers, data = data_final_pred_rangers$data_predictable)$predictions
  record$rangers$tallies_details <- list(compute_tally(data_final_pred_rangers, data_all = data_not_imputed,
                                                       who = "rangers", coef_population = coef))

  data_final_pred_others$data_predictable$staff_others_log_predicted <- stats::predict(
    fit_final_others, data = data_final_pred_others$data_predictable)$predictions
  record$others$tallies_details <- list(compute_tally(data_final_pred_others, data_all = data_not_imputed,
                                                      who = "others", coef_population = coef))

  data_final_pred_all$data_predictable$staff_total_log_predicted <- stats::predict(
    fit_final_all, data = data_final_pred_all$data_predictable)$predictions
  record$all$tallies_details <- list(compute_tally(data_final_pred_all, data_all = data_not_imputed,
                                                   who = "all", coef_population = coef))

  record$rangers$tallies_details[[1]] %>%
    dplyr::summarise(dplyr::across(-.data$continent, sum)) -> rangers_tally_world

  record$others$tallies_details[[1]] %>%
    dplyr::summarise(dplyr::across(-.data$continent, sum)) -> others_tally_world

  record$all$tallies_details[[1]] %>%
    dplyr::summarise(dplyr::across(-.data$continent, sum)) -> all_tally_world

  record$rangers$tally_obs <- rangers_tally_world[1, "sum_known", drop = TRUE]
  record$others$tally_obs <- others_tally_world[1, "sum_known", drop = TRUE]
  record$all$tally_obs <- all_tally_world[1, "sum_known", drop = TRUE]

  record$rangers$tally_imputed <- rangers_tally_world[1, "sum_imputed", drop = TRUE]
  record$others$tally_imputed <- others_tally_world[1, "sum_imputed", drop = TRUE]
  record$all$tally_imputed <- all_tally_world[1, "sum_imputed", drop = TRUE]

  record$rangers$tally_obs_or_imputed <- rangers_tally_world[1, "sum_known_imputed", drop = TRUE]
  record$others$tally_obs_or_imputed <- others_tally_world[1, "sum_known_imputed", drop = TRUE]
  record$all$tally_obs_or_imputed <- all_tally_world[1, "sum_known_imputed", drop = TRUE]

  record$rangers$tally_predicted <- rangers_tally_world[1, "sum_predicted", drop = TRUE]
  record$others$tally_predicted <- others_tally_world[1, "sum_predicted", drop = TRUE]
  record$all$tally_predicted <- all_tally_world[1, "sum_predicted", drop = TRUE]

  record$rangers$tally_total <- rangers_tally_world[1, "sum_total", drop = TRUE]
  record$others$tally_total <- others_tally_world[1, "sum_total", drop = TRUE]
  record$all$tally_total <- all_tally_world[1, "sum_total", drop = TRUE]


  cat("Step 8b: prediction intervals for the sums\n")

  run_sim <- function(fit, data_pred, var_name, who) {
    do.call("cbind", parallel::mclapply(seq_len(rep_simu), function(i) {
      sim <- stats::predict(fit,
                            data = data_pred$data_predictable,
                            type = "quantiles",
                            quantiles = stats::runif(n = nrow(data_pred$data_predictable)),
                            num.threads = 1L)
      data_pred$data_predictable[[var_name]] <- diag(sim$predictions)
      tallies <- compute_tally(data_pred, data_all = data_not_imputed, who = who, coef_population = coef)
      stats::setNames(tallies$sum_total, nm = tallies$continent)
      }, mc.cores = Ncpu)) %>%
      as.data.frame() %>%
      tibble::as_tibble(rownames = "continent") %>%
      tidyr::pivot_longer(cols = -.data$continent, names_to = "simu")
  }

  sim_rangers <- run_sim(fit = fit_final_rangers,
                         data_pred = data_final_pred_rangers,
                         var_name = "staff_rangers_log_predicted",
                         who = "rangers")

  sim_others <- run_sim(fit = fit_final_others,
                        data_pred = data_final_pred_others,
                        var_name = "staff_others_log_predicted",
                        who = "others")

  sim_all <- run_sim(fit = fit_final_all,
                     data_pred = data_final_pred_all,
                     var_name = "staff_total_log_predicted",
                     who = "all")


  extract_quant <- function(x, probs) {
    x %>%
      dplyr::group_by(.data$simu) %>%
      dplyr::summarise(tally = sum(.data$value)) %>%
      dplyr::pull(.data$tally) %>%
      stats::quantile(probs = probs)
  }

  record$rangers$lwr <- extract_quant(sim_rangers, probs = 0.025)
  record$others$lwr  <- extract_quant(sim_others, probs = 0.025)
  record$all$lwr     <- extract_quant(sim_all, probs = 0.025)

  record$rangers$upr <- extract_quant(sim_rangers, probs = 0.975)
  record$others$upr  <- extract_quant(sim_others, probs = 0.975)
  record$all$upr     <- extract_quant(sim_all, probs = 0.975)

  extract_quant_cont <- function(x) {
    x %>%
      dplyr::group_by(.data$simu, .data$continent) %>%
      dplyr::summarise(tally = sum(.data$value)) %>%
      dplyr::group_by(.data$continent) %>%
      dplyr::summarise(lwr = stats::quantile(.data$tally, probs = 0.025),
                       upr = stats::quantile(.data$tally, probs = 0.975))
  }

  record$rangers$tallies_details[[1]] %>%
    dplyr::left_join(extract_quant_cont(sim_rangers), by = "continent") -> record$rangers$tallies_details[[1]]

  record$others$tallies_details[[1]] %>%
    dplyr::left_join(extract_quant_cont(sim_others), by = "continent") -> record$others$tallies_details[[1]]

  record$all$tallies_details[[1]] %>%
    dplyr::left_join(extract_quant_cont(sim_all), by = "continent") -> record$all$tallies_details[[1]]


  cat("DONE!\n")
  record$meta$duration_h <- as.double(difftime(Sys.time(), record$meta$start, units = "hours"))

  ## We add country level data:
  record$rangers$country_info <- list(extract_PA_areas(data_final_pred = data_final_pred_rangers, resp = "staff_rangers_log", data = data))
  record$others$country_info <- list(extract_PA_areas(data_final_pred = data_final_pred_others, resp = "staff_others_log", data = data))
  record$all$country_info <- list(extract_PA_areas(data_final_pred = data_final_pred_all, resp = "staff_total_log", data = data))

  record
}
