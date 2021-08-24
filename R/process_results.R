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
#' LMM_small_test <- run_LMM_workflow(data = data_rangers, Ncpu = 2, coef = 0,
#'                                      rep_feature_select = 2, rep_finetune = 2, rep_simu = 2)
#'
#' RF_small_test <- run_RF_workflow(data = data_rangers, Ncpu = 2, coef = 0,
#'                                    rep_feature_select = 2, rep_finetune = 2, rep_simu = 2,
#'                                    grid_type = "coarse")
#'
#' extract_results(list_results_LMM = list(LMM_small_test),
#'                 list_results_RF  = list(RF_small_test)) %>%
#'   tidyr::unnest_wider(PA_areas)
#' }
#'
extract_results <- function(list_results_LMM = list(), list_results_RF = list(), data = NULL) {
  d_LMM <- d_RF <- data.frame()

  if (length(list_results_LMM) > 0) {
    rangers_list_LMM <- lapply(list_results_LMM, function(x) extract_results_internal(what = x, who = "rangers", type = "LMM", data = data))
    others_list_LMM  <- lapply(list_results_LMM, function(x) extract_results_internal(what = x, who = "others", type = "LMM", data = data))
    all_list_LMM     <- lapply(list_results_LMM, function(x) extract_results_internal(what = x, who = "all", type = "LMM", data = data))
    rbind(cbind(who = "rangers", as.data.frame(do.call("rbind", rangers_list_LMM))),
          cbind(who = "others",  as.data.frame(do.call("rbind", others_list_LMM))),
          cbind(who = "all",     as.data.frame(do.call("rbind", all_list_LMM)))) -> d_LMM
  }
  if (length(list_results_RF) > 0) {
    rangers_list_RF <- lapply(list_results_RF, function(x) extract_results_internal(what = x, who = "rangers", type = "RF", data = data))
    others_list_RF  <- lapply(list_results_RF, function(x) extract_results_internal(what = x, who = "others", type = "RF", data = data))
    all_list_RF     <- lapply(list_results_RF, function(x) extract_results_internal(what = x, who = "all", type = "RF", data = data))
    rbind(cbind(who = "rangers", as.data.frame(do.call("rbind", rangers_list_RF))),
          cbind(who = "others",  as.data.frame(do.call("rbind", others_list_RF))),
          cbind(who = "all",     as.data.frame(do.call("rbind", all_list_RF)))) -> d_RF
  }
  if (ncol(d_LMM) > 0 && ncol(d_RF) > 0) {
    d <- rbind(d_LMM, d_RF)
  } else if (ncol(d_LMM) > 0) {
    d <- d_LMM
  } else if (ncol(d_RF) > 0) {
    d <- d_RF
  }

  d$who <- as.factor(d$who)
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

  country_info %>%
    dplyr::summarise(PA_area_known     = ifelse("PA_area_known" %in% colnames(country_info), sum(country_info$PA_area_known), 0), #using .data$ crashes... why?? dplyr bug?
                     PA_area_imputed   = ifelse("PA_area_imputed" %in% colnames(country_info), sum(country_info$PA_area_imputed), 0),
                     PA_area_predicted = ifelse("PA_area_predicted" %in% colnames(country_info), sum(country_info$PA_area_predicted), 0),
                     PA_area_unknown   = ifelse("PA_area_unknown" %in% colnames(country_info), sum(country_info$PA_area_unknown), 0)
                     ) -> .PA_areas

  #.PA_areas %>%
  #  dplyr::summarise(PA_area_total = sum(dplyr::c_across())) -> .PA_area_total

  country_info %>%
    dplyr::group_by(.data$type, .add = TRUE) %>%
    dplyr::summarise(dplyr::across(tidyselect::starts_with("staff"), \(x) sum(delog1p(x), na.rm = TRUE), .names = "staff")) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = .data$type, values_from = .data$staff) -> .predictions

  tibble::tibble(type = type,
                 coef = what$meta$coef_population,
                 rerank = what$meta$rerank,
                 Ncpu = what$meta$Ncpu,
                 run_time = what$meta$duration_h,
                 point_pred = what[[who]]$tally_total,
                 lwr = what[[who]]$lwr[[1]],
                 upr = what[[who]]$upr[[1]],
                 PA_areas = list(.PA_areas),
                 pred_details = list(.predictions),
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
  res_in_context <- dplyr::left_join(data, result[[who]]$country_preds[[1]], by = "countryname_eng")
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
