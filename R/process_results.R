#' Extract and format the results produced by the worflow
#'
#' @param what an object produced by [`run_LMM_workflow()`] or [`run_RF_workflow()`]
#' @param who the type of staff ("rangers", "others" or "all")
#' @param type the type of workflow ("LMM" or "RF")
#' @param list_results_LMM a list of objects produced by [`run_LMM_workflow()`]
#' @param list_results_RF a list of objects produced by [`run_RF_workflow()`]
#' @param data a dataset with info on continents if breakdown by continent is required (optional)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' LMM_small_test <- run_LMM_workflow(data = data_rangers, Ncpu = 2, coef = 1,
#'                                    rep_feature_select = 2, rep_finetune = 2, rep_simu = 2)
#'
#' RF_small_test <- run_RF_workflow(data = data_rangers, Ncpu = 2, coef = 1,
#'                                  rep_feature_select = 2, rep_finetune = 2, rep_simu = 2,
#'                                  grid_type = "coarse",
#'                                  n_trees = 100)
#'
#' extract_results(list_results_LMM = list(LMM_small_test),
#'                 list_results_RF  = list(RF_small_test)) %>%
#'   tidyr::unnest_wider(PA_areas)
#'
#' plot_features_selected(list_results_LMM = list(LMM_small_test),
#'                        list_results_RF  = list(RF_small_test), data = data_rangers)
#'
#' }
#'
extract_results <- function(list_results_LMM = list(), list_results_RF = list(), data = NULL) {
  d_LMM <- d_RF <- data.frame()

  if (length(list_results_LMM) > 0) {
    rangers_list_LMM <- lapply(list_results_LMM, function(x) extract_results_internal(what = x, who = "rangers", type = "LMM", data = data))
    others_list_LMM  <- lapply(list_results_LMM, function(x) extract_results_internal(what = x, who = "others", type = "LMM", data = data))
    all_list_LMM     <- lapply(list_results_LMM, function(x) extract_results_internal(what = x, who = "all", type = "LMM", data = data))
    rbind(cbind(who = "All",     as.data.frame(do.call("rbind", all_list_LMM))),
          cbind(who = "Rangers", as.data.frame(do.call("rbind", rangers_list_LMM))),
          cbind(who = "Others",  as.data.frame(do.call("rbind", others_list_LMM)))) -> d_LMM
  }
  if (length(list_results_RF) > 0) {
    rangers_list_RF <- lapply(list_results_RF, function(x) extract_results_internal(what = x, who = "rangers", type = "RF", data = data))
    others_list_RF  <- lapply(list_results_RF, function(x) extract_results_internal(what = x, who = "others", type = "RF", data = data))
    all_list_RF     <- lapply(list_results_RF, function(x) extract_results_internal(what = x, who = "all", type = "RF", data = data))
    rbind(cbind(who = "All",     as.data.frame(do.call("rbind", all_list_RF))),
          cbind(who = "Rangers", as.data.frame(do.call("rbind", rangers_list_RF))),
          cbind(who = "Others",  as.data.frame(do.call("rbind", others_list_RF)))) -> d_RF
  }
  if (ncol(d_LMM) > 0 && ncol(d_RF) > 0) {
    d <- rbind(d_LMM, d_RF)
  } else if (ncol(d_LMM) > 0) {
    d <- d_LMM
  } else if (ncol(d_RF) > 0) {
    d <- d_RF
  }

  d$who <- factor(d$who, levels = c("All", "Rangers", "Others"))
  tibble::as_tibble(d)
}

#' @describeIn extract_results an internal function fetching the results
#' @export
#'
extract_results_internal <- function(what, who, type, data) {

  if (!is.null(data)) {
    what[[who]]$country_info[[1]] %>%
      add_continents(data = data) %>%
      dplyr::group_by(.data$continent) -> country_info
  } else {
     country_info <- what[[who]]$country_info[[1]]
  }

  ### extract info about PA:

  ## Add levels to prevent auto-drop when a type is missing:
  country_info %>%
    dplyr::mutate(type = factor(.data$type, levels = c("known", "predicted", "unknown"))) -> country_info


  if (!"PA_area_known" %in% colnames(country_info)) {
    country_info$PA_area_known <- 0
  }

  if (!"PA_area_imputed" %in% colnames(country_info)) {
    country_info$PA_area_imputed <- 0
  }

  if (!"PA_area_predicted" %in% colnames(country_info)) {
    country_info$PA_area_predicted <- 0
    }

  if (!"PA_area_unknown" %in% colnames(country_info)) {
    country_info$PA_area_unknown <- 0
  }

  country_info %>%
    dplyr::summarise(PA_area_known     = sum(.data$PA_area_known),
                     PA_area_imputed   = sum(.data$PA_area_imputed),
                     PA_area_predicted = sum(.data$PA_area_predicted),
                     PA_area_unknown   = sum(.data$PA_area_unknown)
                     ) %>%
    dplyr::mutate(PA_area_total = .data$PA_area_known + .data$PA_area_imputed + .data$PA_area_predicted + .data$PA_area_unknown,
                  PA_area_total_without_unknown = .data$PA_area_known + .data$PA_area_imputed + .data$PA_area_predicted) -> .PA_areas

  tibble::tibble(type = type,
                 coef = what$meta$coef_population,
                 rerank = what$meta$rerank,
                 Ncpu = what$meta$Ncpu,
                 run_time = what$meta$duration_h,
                 point_pred = what[[who]]$tally_total,
                 lwr = what[[who]]$lwr[[1]],
                 upr = what[[who]]$upr[[1]],
                 PA_areas = list(.PA_areas),
                 PA_total = sum(.PA_areas$PA_area_total),
                 PA_total_without_unknown = sum(.PA_areas$PA_area_total_without_unknown),
                 pred_details = list(what[[who]]$tallies_details[[1]]),
                 formula = what[[who]]$selected_formula,
                 spatial = what[[who]]$selected_spatial
  )
}



#' An internal function fetching the results at the country level
#'
#' @inheritParams extract_results
#' @inheritParams validate_LMM
#' @param result a result table produced by [`run_LMM_workflow()`] or [`run_RF_workflow()`]
#' @param resp the quoted name of the response variable
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   LMM_small_test <- run_LMM_workflow(data = data_rangers, Ncpu = 2, coef = 0,
#'                                      rep_feature_select = 2, rep_finetune = 2, rep_simu = 2)
#'   single_summary_internal(result = LMM_small_test,
#'                           who = "rangers", resp = "staff_rangers_log", data = data_rangers)
#' }
#'
single_summary_internal <- function(result, who, resp, data) {
  res_in_context <- dplyr::left_join(data, result[[who]]$country_info[[1]], by = "countryname_eng")
  res_in_context %>%
    dplyr::mutate(staff = delog1p(res_in_context[[resp]])) -> res_in_context

  res_in_context %>%
    dplyr::filter(.data$type != "unknown") %>%
    dplyr::group_by(.data$country_UN_continent, .data$type) %>%
    dplyr::summarise(total = sum(.data$staff, na.rm = TRUE),
              Ncountry = dplyr::n(),
              PA_area_surveyed = sum(.data$PA_area_surveyed),
              PA_area_unsurveyed = sum(.data$PA_area_unsurveyed)) %>%
    dplyr::ungroup() -> totals

  totals %>%
    dplyr::group_by(.data$country_UN_continent) %>%
    dplyr::summarise(type = "all", total = sum(.data$total),
                     Ncountry = sum(.data$Ncountry),
                     PA_area_surveyed = sum(.data$PA_area_surveyed),
                     PA_area_unsurveyed = sum(.data$PA_area_unsurveyed)) -> gd_totals

  dplyr::full_join(totals, gd_totals,
          by = c("country_UN_continent", "type", "total", "Ncountry", "PA_area_surveyed", "PA_area_unsurveyed")) %>%
    dplyr::arrange(.data$country_UN_continent) -> all_small_totals

  all_small_totals %>%
    dplyr::group_by(.data$type) %>%
    dplyr::summarise(country_UN_continent = "EARTH", total = sum(.data$total), Ncountry = sum(.data$Ncountry),
                     PA_area_surveyed = sum(.data$PA_area_surveyed),
                     PA_area_unsurveyed = sum(.data$PA_area_unsurveyed)) -> marginal_totals

  dplyr::full_join(all_small_totals, marginal_totals,
                   by = c("country_UN_continent", "type", "total", "Ncountry", "PA_area_surveyed", "PA_area_unsurveyed"))

}


#' Extract training data info
#'
#' @inheritParams extract_results
#' @param which either `"initial"` or `"final"` depending on which training dataset to summarise
#' @inheritParams run_LMM_workflow
#'
#' @export
#'
#' @examples
#' \dontrun{
#' LMM_small_test <- run_LMM_workflow(data = data_rangers, Ncpu = 2, coef = 0,
#'                                      rep_feature_select = 2, rep_finetune = 2, rep_simu = 2)
#'
#' RF_small_test <- run_RF_workflow(data = data_rangers, Ncpu = 2, coef = 0,
#'                                  rep_feature_select = 2, rep_finetune = 2, rep_simu = 2,
#'                                  grid_type = "coarse",
#'                                  n_trees = 100)
#'
#' extract_training_info(list_results_LMM = list(LMM_small_test),
#'                 list_results_RF  = list(RF_small_test))
#' }
#'
extract_training_info <- function(which, list_results_LMM = list(), list_results_RF = list(), data = NULL) {
  d_LMM <- d_RF <- data.frame()

  if (length(list_results_LMM) > 0) {
    rangers_list_LMM <- lapply(list_results_LMM, function(x) extract_training_info_internal(which = which, what = x, who = "rangers", type = "LMM", data = data))
    others_list_LMM  <- lapply(list_results_LMM, function(x) extract_training_info_internal(which = which, what = x, who = "others", type = "LMM", data = data))
    all_list_LMM     <- lapply(list_results_LMM, function(x) extract_training_info_internal(which = which, what = x, who = "all", type = "LMM", data = data))
    rbind(cbind(who = "All",     type = "LMM", as.data.frame(do.call("rbind", all_list_LMM))),
          cbind(who = "Rangers", type = "LMM", as.data.frame(do.call("rbind", rangers_list_LMM))),
          cbind(who = "Others",  type = "LMM", as.data.frame(do.call("rbind", others_list_LMM)))) -> d_LMM
  }
  if (length(list_results_RF) > 0) {
    rangers_list_RF <- lapply(list_results_RF, function(x) extract_training_info_internal(which = which, what = x, who = "rangers", type = "RF", data = data))
    others_list_RF  <- lapply(list_results_RF, function(x) extract_training_info_internal(which = which, what = x, who = "others", type = "RF", data = data))
    all_list_RF     <- lapply(list_results_RF, function(x) extract_training_info_internal(which = which, what = x, who = "all", type = "RF", data = data))
    rbind(cbind(who = "All",     type = "RF/ETs", as.data.frame(do.call("rbind", all_list_RF))),
          cbind(who = "Rangers", type = "RF/ETs", as.data.frame(do.call("rbind", rangers_list_RF))),
          cbind(who = "Others",  type = "RF/ETs", as.data.frame(do.call("rbind", others_list_RF)))) -> d_RF
  }
  if (ncol(d_LMM) > 0 && ncol(d_RF) > 0) {
    d <- rbind(d_LMM, d_RF)
  } else if (ncol(d_LMM) > 0) {
    d <- d_LMM
  } else if (ncol(d_RF) > 0) {
    d <- d_RF
  }

  d$who <- factor(d$who, levels = c("All", "Rangers", "Others"))
  d <- tibble::as_tibble(d)

  if (which == "initial") {
    d %>%
      dplyr::rename(PA = .data$initial_PA_included,
                    obs = .data$initial_training_nrow,
                    ncol = .data$initial_training_ncol) -> d
  } else if (which == "final") {
    d %>%
      dplyr::rename(PA = .data$final_PA_included,
                    obs = .data$final_training_nrow,
                    ncol = .data$final_training_ncol) -> d
  } else {
    stop("Argument `which =` should be either 'initial' or 'final'!")
  }

  if (!is.null(data)) {
    d$total_obs <- nrow(data)
    d$total_PA <- sum(data$area_PA_total)
    d$obs_coverage <- d$obs / d$total_obs
    d$PA_coverage <- d$PA / d$total_PA
  }

  d
}


#' @describeIn extract_training_info an internal function fetching the info on training datasets
#' @export
#'
extract_training_info_internal <- function(which, what, who, type, data) {

  if (!is.null(data)) {
    what[[who]] -> data_info
  } else {
     data_info <- what[[who]]
  }

  coef <- what[["meta"]]$coef_population

  if (which == "initial") {
  data_info %>%
    dplyr::select(.data$initial_training_nrow, .data$initial_training_ncol, .data$initial_PA_included) %>%
    dplyr::mutate(coef = coef, .before = 1L)
  } else if (which == "final") {
  data_info %>%
    dplyr::select(.data$final_training_nrow, .data$final_training_ncol, .data$final_PA_included) %>%
    dplyr::mutate(coef = coef, .before = 1L)
  } else {
    stop("Argument `which =` should be either 'initial' or 'final'!")
  }
}


#' Extract and format the finetuning results produced by the worflow
#'
#' @inheritParams extract_results
#'
#' @export
#'
#' @examples
#' \dontrun{
#' LMM_small_test <- run_LMM_workflow(data = data_rangers, Ncpu = 2, coef = 0,
#'                                    rep_feature_select = 2, rep_finetune = 2, rep_simu = 2)
#'
#' RF_small_test <- run_RF_workflow(data = data_rangers, Ncpu = 2, coef = 0,
#'                                  rep_feature_select = 2, rep_finetune = 2, rep_simu = 2,
#'                                  grid_type = "coarse", n_trees = 100)
#'
#' extract_finetuning(list_results_LMM = list(LMM_small_test),
#'                    list_results_RF  = list(RF_small_test))
#' }
#'
extract_finetuning <- function(list_results_LMM = list(), list_results_RF = list(), data = NULL) {
  d_LMM <- d_RF <- data.frame()

  if (length(list_results_LMM) > 0) {
    rangers_list_LMM <- lapply(list_results_LMM, function(x) extract_finetuning_internal(what = x, who = "rangers", type = "LMM", data = data))
    others_list_LMM  <- lapply(list_results_LMM, function(x) extract_finetuning_internal(what = x, who = "others", type = "LMM", data = data))
    all_list_LMM     <- lapply(list_results_LMM, function(x) extract_finetuning_internal(what = x, who = "all", type = "LMM", data = data))
    rbind(cbind(who = "All",     as.data.frame(do.call("rbind", all_list_LMM))),
          cbind(who = "Rangers", as.data.frame(do.call("rbind", rangers_list_LMM))),
          cbind(who = "Others",  as.data.frame(do.call("rbind", others_list_LMM)))) -> d_LMM
  }
  if (length(list_results_RF) > 0) {
    rangers_list_RF <- lapply(list_results_RF, function(x) extract_finetuning_internal(what = x, who = "rangers", type = "RF/ETs", data = data))
    others_list_RF  <- lapply(list_results_RF, function(x) extract_finetuning_internal(what = x, who = "others", type = "RF/ETs", data = data))
    all_list_RF     <- lapply(list_results_RF, function(x) extract_finetuning_internal(what = x, who = "all", type = "RF/ETs", data = data))
    rbind(cbind(who = "All",     as.data.frame(do.call("rbind", all_list_RF))),
          cbind(who = "Rangers", as.data.frame(do.call("rbind", rangers_list_RF))),
          cbind(who = "Others",  as.data.frame(do.call("rbind", others_list_RF)))) -> d_RF
  }
  if (ncol(d_LMM) > 0 && ncol(d_RF) > 0) {
    d <- rbind(d_LMM, d_RF)
  } else if (ncol(d_LMM) > 0) {
    d <- d_LMM
  } else if (ncol(d_RF) > 0) {
    d <- d_RF
  }

  d$who <- factor(d$who, levels = c( "All", "Rangers", "Others"))
  tibble::as_tibble(d)
}


#' @describeIn extract_finetuning an internal function fetching the info on finetuning
#' @export
#'
extract_finetuning_internal <- function(what, who, type, data) {

  if (!is.null(data)) {
    what[[who]] -> data_info
  } else {
     data_info <- what[[who]]
  }

  if (type == "LMM") {
    fine_tunning_res <- tibble::tibble(method =  data_info$fine_tuning[[1]][["result_mean"]] %>% dplyr::slice_min(.data$RMSE) %>% dplyr::pull(.data$method),
                                       mtry = NA, splitrule = NA, min.node.size = NA, replace = NA, sample.fraction = NA)
  } else if (type == "RF/ETs") {
    fine_tunning_res <- tibble::tibble(method = NA,
                                       mtry = sub(pattern = "function (p) \n", replacement = "",
                                                  x = as.character(data_info$best_tuning[[1]]$mtry), fixed = TRUE),
                                       splitrule = data_info$best_tuning[[1]]$splitrule,
                                       min.node.size = data_info$best_tuning[[1]]$min.node.size,
                                       replace = data_info$best_tuning[[1]]$replace,
                                       sample.fraction = data_info$best_tuning[[1]]$sample.fraction)
  } else {
    stop("argument type must be 'LMM' or 'RF/ETs'>!")
  }

   tibble::tibble(type = type,
                  coef = what$meta$coef_population) %>%
     dplyr::bind_cols(fine_tunning_res)
}

