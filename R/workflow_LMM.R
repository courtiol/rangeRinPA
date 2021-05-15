
#' Run the full workflow for LMM
#'
#' @inheritParams validate_LMM
#' @param coef the coefficient used to population rangers in unsurveyed part of surveyed countries (see [`fill_PA_area()`])
#' @param rep_feature_select the number of replicates for the feature selection (default = 1000)
#' @param rep_finetune the number of replicates for fine tuning (default = 1000)
#' @param rep_simu the number of simulation replicates (default = 10000)
#'
#' @return a list with all the output information
#' @export
#'
#' @examples
#' \dontrun{
#'   LMM_000 <- run_LMM_workflow(data = data_rangers, Ncpu = 100, coef = 0)
#'   LMM_025 <- run_LMM_workflow(data = data_rangers, Ncpu = 100, coef = 0.25)
#'   LMM_050 <- run_LMM_workflow(data = data_rangers, Ncpu = 100, coef = 0.50)
#'   LMM_075 <- run_LMM_workflow(data = data_rangers, Ncpu = 100, coef = 0.75)
#'   LMM_100 <- run_LMM_workflow(data = data_rangers, Ncpu = 100, coef = 1)
#' }
#'
run_LMM_workflow <- function(data, Ncpu = 2,  coef = 0, rep_feature_select = 1000, rep_finetune = 1000, rep_simu = 10000) {

  data <- fill_PA_area(data, coef = coef)

  record <- list(meta = tibble::tibble(start = Sys.time(),
                                       Ncpu = Ncpu,
                                       rep_feature_select = rep_feature_select,
                                       rep_finetune = rep_finetune,
                                       rep_simu = rep_simu,
                                       coef_population = coef))

  cat("Step 1 + 2: General data preparation & preparation of initial training datasets\n")

  formula_rangers_full <- staff_rangers_log ~ PA_area_log + lat + long + area_country_log + area_forest_pct + pop_density_log + GDP_2019_log + GDP_capita_log +
    GDP_growth + unemployment_log + EVI + SPI + EPI_2020 + IUCN_1_4_prop + IUCN_1_2_prop + Matern(1|long + lat)
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

  fit_data_initial_training_rangers_full <- spaMM::fitme(formula_rangers_full,
                                                         data = data_initial_training_rangers,
                                                         control.dist = list(dist.method = "Earth"))
  fit_data_initial_training_others_full <- spaMM::fitme(formula_others_full,
                                                        data = data_initial_training_others,
                                                        control.dist = list(dist.method = "Earth"))
  fit_data_initial_training_all_full <- spaMM::fitme(formula_all_full,
                                                     data = data_initial_training_all,
                                                     control.dist = list(dist.method = "Earth"))

  cat("Step 3a: selection for rangers\n")
  selection_training_rangers <- feature_selection_LMM(
    full_fit = fit_data_initial_training_rangers_full,
    rep = rep_feature_select, Ncpu = Ncpu,
    target = "staff_rangers_log")

  cat("Step 3b: selection for others\n")
  selection_training_others <- feature_selection_LMM(
    full_fit = fit_data_initial_training_others_full,
    rep = rep_feature_select, Ncpu = Ncpu,
    target = "staff_others_log")

  cat("Step 3c: selection for all\n")
  selection_training_all <- feature_selection_LMM(
    full_fit = fit_data_initial_training_all_full,
    rep = rep_feature_select, Ncpu = Ncpu,
    target = "staff_total_log")


  selected_formula_rangers <- stats::as.formula(selection_training_rangers$formula[1])
  selected_formula_others <- stats::as.formula(selection_training_others$formula[1])
  selected_formula_all <- stats::as.formula(selection_training_all$formula[1])

  record$rangers$selected_features <- list(selection_training_rangers)
  record$rangers$selected_formula <- deparse(selected_formula_rangers, width.cutoff = 500)
  record$others$selected_features <- list(selection_training_others)
  record$others$selected_formula <- deparse(selected_formula_others, width.cutoff = 500)
  record$all$selected_features <- list(selection_training_all)
  record$all$selected_formula <- deparse(selected_formula_all, width.cutoff = 500)


  cat("Step 4: Preparation of final training datasets\n")

  data_final_training_rangers <- build_final_training_data(data = data,
                                                           formula = selected_formula_rangers,
                                                           survey = "complete_known")

  data_final_training_others <- build_final_training_data(data = data,
                                                          formula = selected_formula_others,
                                                          survey = "complete_known")

  data_final_training_all <- build_final_training_data(data = data,
                                                       formula = selected_formula_all,
                                                       survey = "complete_known")

  record$rangers$final_training_nrow <- nrow(data_final_training_rangers)
  record$others$final_training_nrow <- nrow(data_final_training_others)
  record$all$final_training_nrow <- nrow(data_final_training_all)
  record$rangers$final_training_ncol <- ncol(data_final_training_rangers)
  record$others$final_training_ncol <- ncol(data_final_training_others)
  record$all$final_training_ncol <- ncol(data_final_training_all)

  cat("Step 5: Selection of function inputs (fine tuning)\n")

  finetune_rangers <- finetune_LMM(selected_formula_rangers,
                                   data = data_final_training_rangers,
                                   rep = rep_finetune, Ncpu = Ncpu)

  finetune_others <- finetune_LMM(selected_formula_others,
                                  data = data_final_training_others,
                                  rep = rep_finetune, Ncpu = Ncpu)

  finetune_all <- finetune_LMM(selected_formula_all,
                               data = data_final_training_all,
                               rep = rep_finetune, Ncpu = Ncpu)

  record$rangers$fine_tuning <- list(finetune_rangers)
  record$rangers$fitting_method <- finetune_rangers$best_method
  record$others$fine_tuning <- list(finetune_others)
  record$others$fitting_method <- finetune_others$best_method
  record$all$fine_tuning <- list(finetune_all)
  record$all$fitting_method <- finetune_all$best_method


  cat("Step 6: Final training\n")

  fit_final_rangers <- spaMM::fitme(selected_formula_rangers,
                                    data = data_final_training_rangers,
                                    control.dist = list(dist.method = "Earth"),
                                    method = finetune_rangers$best_method)

  fit_final_others <- spaMM::fitme(selected_formula_others,
                                   data = data_final_training_others,
                                   control.dist = list(dist.method = "Earth"),
                                   method = finetune_others$best_method)

  fit_final_all <- spaMM::fitme(selected_formula_all,
                                data = data_final_training_all,
                                control.dist = list(dist.method = "Earth"),
                                method = finetune_all$best_method)


  cat("Step 7: Preparation of datasets for predictions & simulations\n")

  data_final_pred_rangers <- build_final_pred_data(
    data = data,
    formula = selected_formula_rangers,
    survey = "complete_known")

  data_final_pred_others <- build_final_pred_data( # 1 country missing -> Greenland
    data = data,
    formula = selected_formula_others,
    survey = "complete_known")

  data_final_pred_all <- build_final_pred_data(
    data = data,
    formula = selected_formula_all,
    survey = "complete_known")


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

  data_final_pred_rangers$data_predictable$staff_rangers_log_predicted <- spaMM::predict.HLfit(
    fit_final_rangers, newdata = data_final_pred_rangers$data_predictable)[, 1]
  tallies_rangers <- compute_tally(data_final_pred_rangers)

  data_final_pred_others$data_predictable$staff_others_log_predicted <- spaMM::predict.HLfit(
    fit_final_others, newdata = data_final_pred_others$data_predictable)[, 1]
  tallies_others <- compute_tally(data_final_pred_others)

  data_final_pred_all$data_predictable$staff_total_log_predicted <- spaMM::predict.HLfit(
    fit_final_all, newdata = data_final_pred_all$data_predictable)[, 1]
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
    data_final_pred_rangers$data_predictable$staff_rangers_log_predicted <- spaMM::simulate.HLfit(
      fit_final_rangers, newdata = data_final_pred_rangers$data_predictable,
      type = "predVar", variances = list(linPred = TRUE, disp = TRUE), verbose = FALSE)
    compute_tally(data_final_pred_rangers)[3, "value"]}, mc.cores = Ncpu)

  predictions_others <- parallel::mclapply(seq_len(rep_simu), function(i) {
    data_final_pred_others$data_predictable$staff_others_log_predicted <- spaMM::simulate.HLfit(
      fit_final_others, newdata = data_final_pred_others$data_predictable,
      type = "predVar", variances = list(linPred = TRUE, disp = TRUE), verbose = FALSE)
    compute_tally(data_final_pred_others)[3, "value"]}, mc.cores = Ncpu)

  predictions_all <- parallel::mclapply(seq_len(rep_simu), function(i) {
    data_final_pred_all$data_predictable$staff_total_log_predicted <- spaMM::simulate.HLfit(
      fit_final_all, newdata = data_final_pred_all$data_predictable,
      type = "predVar", variances = list(linPred = TRUE, disp = TRUE), verbose = FALSE)
    compute_tally(data_final_pred_all)[3, "value"]}, mc.cores = Ncpu)

  record$rangers$lwr <- stats::quantile(unlist(predictions_rangers), probs = 0.025)
  record$others$lwr <- stats::quantile(unlist(predictions_others), probs = 0.025)
  record$all$lwr <- stats::quantile(unlist(predictions_all), probs = 0.025)

  record$rangers$upr <- stats::quantile(unlist(predictions_rangers), probs = 0.975)
  record$others$upr <- stats::quantile(unlist(predictions_others), probs = 0.975)
  record$all$upr <- stats::quantile(unlist(predictions_all), probs = 0.975)

  cat("DONE!")
  record$meta$duration_h <- as.double(difftime(Sys.time(), record$meta$start, units = "hours"))
  record
}

