#' Create completeness table (observations)
#'
#' This function creates the table that indicate how many countries and territories have ranger staff and other staff documented in our data.
#'
#' @param data the complete dataset
#'
#' @return a tibble
#' @export
#'
#' @examples
#' table_completeness_obs(data_rangers)
#'
table_completeness_obs <- function(data) {

  tibble::tibble(
    category = c("rangers and non-rangers both separately known",
                 "rangers and non-rangers both separately known",
                 "rangers and non-rangers both known but lumped together",
                 "rangers and non-rangers both known but lumped together",
                 "only rangers known",
                 "only rangers known",
                 "only non-rangers known",
                 "only non-rangers known",
                 "no data",
                 "no data"),
    category_PA = rep(c("full_survey", "partial_survey"), 5),
    N = c(sum(!is.na(data$staff_rangers_others_known) & data$PA_area_unsurveyed == 0),
          sum(!is.na(data$staff_rangers_others_known) & data$PA_area_unsurveyed > 0),
          sum(!is.na(data$staff_total_details_unknown) & data$PA_area_unsurveyed == 0),
          sum(!is.na(data$staff_total_details_unknown) & data$PA_area_unsurveyed > 0),
          sum(!is.na(data$staff_rangers_others_unknown) & data$PA_area_unsurveyed == 0),
          sum(!is.na(data$staff_rangers_others_unknown) & data$PA_area_unsurveyed > 0),
          sum(!is.na(data$staff_others) & is.na(data$staff_rangers) & data$PA_area_unsurveyed == 0),
          sum(!is.na(data$staff_others) & is.na(data$staff_rangers) & data$PA_area_unsurveyed > 0),
          sum((is.na(data$staff_rangers_others_known) &
               is.na(data$staff_rangers_others_unknown) &
               is.na(data$staff_total_details_unknown) & data$PA_area_unsurveyed == 0)),
          sum((is.na(data$staff_rangers_others_known) &
               is.na(data$staff_rangers_others_unknown) &
               is.na(data$staff_total_details_unknown) & data$PA_area_unsurveyed > 0))
          )) -> tab

  tab <- tidyr::pivot_wider(tab, names_from = "category_PA", values_from = "N")

  tab %>%
    dplyr::bind_rows(tibble::tibble(category = "total",
                                    full_survey = sum(tab$full_survey),
                                    partial_survey = sum(tab$partial_survey))) %>%
    dplyr::mutate(all_survey = .data$full_survey + .data$partial_survey)
}


#' Create completeness table (km2)
#'
#' This function creates the table that indicate the surface of protected areas with ranger staff and other staff documented in our data.
#'
#' @inheritParams table_completeness_obs
#'
#' @return a tibble
#' @export
#'
#' @examples
#' table_completeness_km2(data_rangers)
#'
table_completeness_km2 <- function(data) {

  tibble::tibble(
    category = c("rangers and non-rangers both separately known",
                     "rangers and non-rangers both separately known",
                     "rangers and non-rangers both known but lumped together",
                     "rangers and non-rangers both known but lumped together",
                     "only rangers known",
                     "only rangers known",
                     "only non-rangers known",
                     "only non-rangers known",
                     "no data",
                     "no data"),
    category_PA = rep(c("km2_surveyed", "km2_unsurveyed"), 5),
    N = c(sum(data$PA_area_surveyed[!is.na(data$staff_rangers_others_known)]),
          sum(data$PA_area_unsurveyed[!is.na(data$staff_rangers_others_known)]),
          sum(data$PA_area_surveyed[!is.na(data$staff_total_details_unknown)]),
          sum(data$PA_area_unsurveyed[!is.na(data$staff_total_details_unknown)]),
          sum(data$PA_area_surveyed[!is.na(data$staff_rangers_others_unknown)]),
          sum(data$PA_area_unsurveyed[!is.na(data$staff_rangers_others_unknown)]),
          sum(data$PA_area_surveyed[!is.na(data$staff_others) & is.na(data$staff_rangers)]),
          sum(data$PA_area_unsurveyed[!is.na(data$staff_others) & is.na(data$staff_rangers)]),
          sum(data$PA_area_surveyed[(is.na(data$staff_rangers_others_known) &
               is.na(data$staff_rangers_others_unknown) &
               is.na(data$staff_total_details_unknown))]),
          sum(data$PA_area_unsurveyed[(is.na(data$staff_rangers_others_known) &
               is.na(data$staff_rangers_others_unknown) &
               is.na(data$staff_total_details_unknown))])
          )) -> tab

  tab <- tidyr::pivot_wider(tab, names_from = "category_PA", values_from = "N")

  tab %>%
    dplyr::bind_rows(tibble::tibble(category = "total",
                                    km2_surveyed = sum(tab$km2_surveyed),
                                    km2_unsurveyed = sum(tab$km2_unsurveyed))) %>%
    dplyr::mutate(both = .data$km2_surveyed + .data$km2_unsurveyed)
}


#' Create completeness table (variables)
#'
#' This function creates the table that indicate for each variable, how much information is available.
#'
#' @inheritParams table_completeness_obs
#'
#' @return a tibble
#' @export
#'
#' @examples
#' table_completeness_vars(data_rangers)
#'
table_completeness_vars <- function(data) {

  var_table <- function(name, var) {
   tibble::tibble(variable = c(name),
                  nb_known = c(sum(!is.na(data[, var]))),
                  nb_unknown = c(sum(is.na(data[, var]))),
                  proportion_nb_known = .data$nb_known / (.data$nb_known + .data$nb_unknown),
                  total_PA_known = c(sum(data$PA_area_surveyed[!is.na(data[, var])])),
                  total_PA_unknown = c(sum(data$PA_area_surveyed[is.na(data[, var])])),
                  proportion_total_PA_known = .data$total_PA_known / (.data$total_PA_known + .data$total_PA_unknown))
  }

  dplyr::bind_rows(
    var_table(name = "Number of rangers", var = "staff_rangers"),
    var_table(name = "Number of non-rangers", var = "staff_others"),
    var_table(name = "Number of both rangers and non-rangers", var = "staff_total"),
    var_table(name = "Latitude", var = "lat"),
    var_table(name = "Longitude", var = "long"),
    var_table(name = "Country/Territory area", var = "area_country"),
    var_table(name = "Population density", var = "pop_density"),
    var_table(name = "GDP", var = "GDP_2019"),
    var_table(name = "GDP per capita", var = "GDP_capita"),
    var_table(name = "GDP growth rate", var = "GDP_growth"),
    var_table(name = "Percentage of rural inhabitants", var = "rural_pct"),
    var_table(name = "Percentage of unemployed inhabitants", var = "unemployment"),
    var_table(name = "Percentage of area covered with forests", var = "area_forest_pct"),
    var_table(name = "Surface of protected area", var = "area_PA_total"),
    var_table(name = "Proportion of protected areas in IUCN categories I & II", var = "IUCN_1_2_prop"),
    var_table(name = "Proportion of protected areas in IUCN categories I-IV", var = "IUCN_1_4_prop"),
    var_table(name = "Ecosystem Viability Index", var = "EVI"),
    var_table(name = "Environmental Performance Index", var = "EPI_2020"),
    var_table(name = "Species Protection Index", var = "SPI")
  )

}


#' Create table with predictions (per method)
#'
#' This function creates the table that shows the point predictions and their intervals, for the
#' different methods and coefficient values used for the imputation.
#'
#' @inheritParams extract_results
#' @param density a logical indicating whether to return densities instead of raw numbers (default =
#'   `FALSE`)
#'
#' @return a tibble
#' @export
#'
#' @examples
#' # see see ?rangeRinPA
#'
table_predictions_per_method <- function(list_results_LMM, list_results_RF, data, density = FALSE) {

  extract_results(list_results_LMM = list_results_LMM,
                  list_results_RF = list_results_RF,
                  data = data) -> results_predictions

  results_predictions %>%
   dplyr::select(1:3, "point_pred", "lwr", "upr", "PA_total_without_unknown") %>%
   tidyr::pivot_wider(values_from = c("point_pred", "lwr", "upr", "PA_total_without_unknown"), names_from = "type") %>%
   dplyr::select(.data$who, coef_imputation = .data$coef, tidyselect::contains("PA_total_without"),
                 tidyselect::contains("LMM"),
                 tidyselect::contains("RF")) -> res

  if (density) {
    if (any(grepl(pattern = "LMM", colnames(res)))) {
      res %>%
        dplyr::mutate(dplyr::across(.cols =  c(.data$point_pred_LMM, .data$lwr_LMM, .data$upr_LMM),
                                    ~ .data$PA_total_without_unknown_LMM / .x)) %>%
        dplyr::rename(lwr_LMM = .data$upr_LMM, upr_LMM = .data$lwr_LMM) %>%
        dplyr::relocate(.data$upr_LMM, .after = .data$lwr_LMM) -> res
      }

    if (any(grepl(pattern = "RF", colnames(res)))) {
      res %>%
        dplyr::mutate(dplyr::across(.cols =  c(.data$point_pred_RF, .data$lwr_RF, .data$upr_RF),
                                    ~ .data$PA_total_without_unknown_RF / .x)) %>%
        dplyr::rename(lwr_RF = .data$upr_RF, upr_RF = .data$lwr_RF) %>%
        dplyr::relocate(.data$upr_RF, .after = .data$lwr_RF) -> res
    }
  }

  res %>%
    dplyr::select(-tidyselect::contains("PA_total_without")) %>%
    dplyr::rename_with(\(n) sub("RF", "RF/ETs", n), tidyselect::contains("RF")) %>%
    dplyr::arrange(.data$who, dplyr::desc(.data$coef_imputation))
}


#' Create table with predictions (per continent)
#'
#' This function creates the table that shows the point predictions and their intervals, for the
#' different continents.
#'
#' @inheritParams table_predictions_per_method
#' @inheritParams plot_PA_by_data_type
#'
#' @return a tibble
#' @export
#'
#' @examples
#' # see see ?rangeRinPA
#'
table_predictions_per_continent <- function(what, data) {

  extract_results(list_results_LMM = list(what),
                  data = data) -> results_predictions

  results_predictions  %>%
      dplyr::rowwise() %>%
      dplyr::mutate(pred_details = .data$pred_details %>%
                    dplyr::filter(.data$continent != "Antarctica") %>%
                    dplyr::select(-.data$continent) %>% list()) %>%
      dplyr::select(.data$who, .data$pred_details, .data$PA_areas) %>%
      tidyr::unnest(cols = c(.data$PA_areas, .data$pred_details)) %>%
      dplyr::select(.data$who, .data$continent, .data$sum_total, .data$lwr, .data$upr, .data$PA_area_total_without_unknown) %>%
      dplyr::mutate(km2_per_staff_point_pred = .data$PA_area_total_without_unknown / .data$sum_total,
                    km2_per_staff_lwr = .data$PA_area_total_without_unknown / .data$upr,
                    km2_per_staff_upr = .data$PA_area_total_without_unknown / .data$lwr) %>%
      dplyr::select(-.data$PA_area_total_without_unknown) %>%
      dplyr::rename(point_pred = .data$sum_total)
}


#' This function creates the table that shows the point predictions, for the
#' different continents and for the world. It creates of the main text tables.
#'
#' @inheritParams table_predictions_per_method
#' @inheritParams plot_PA_by_data_type
#' @param with_PI whether or not to add prediction intervals (default = `FALSE`)
#'
#' @return a tibble
#' @export
#'
#' @examples
#' # see see ?rangeRinPA
#'
table_predictions_summary <- function(what, data, with_PI = FALSE) {

  lapply(c("all", "rangers"), \(who) {
    var_staff <- colnames(what[[who]]$country_info[[1]])[grepl("staff", colnames(what[[who]]$country_info[[1]]))]

    what[[who]]$country_info[[1]] %>%
      add_continents(data = data) %>%
      dplyr::mutate(staff = delog1p(!!rlang::sym(var_staff))) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(PA_total = sum(dplyr::c_across(tidyselect::starts_with("PA")))) %>%
      dplyr::ungroup() %>%
      dplyr::select(.data$countryname_eng, .data$continent, .data$staff, .data$PA_total) -> country_info

    country_info %>%
      dplyr::summarise(who = !!who,
                       PA = sum(.data$PA_total),
                       number = sum(.data$staff, na.rm = TRUE),
                       density = .data$PA / .data$number) %>%
      dplyr::mutate(continent = "Global", .before = 1L) -> world_summary_estimates_all

    world_summary_estimates_all %>%
      dplyr::mutate(number_lwr = what[[who]]$lwr[[1]],
                    number_upr = what[[who]]$upr[[1]],
                    density_lwr = .data$PA / .data$number_upr,
                    density_upr = .data$PA / .data$number_lwr) -> world_summary

    country_info %>%
      dplyr::group_by(.data$continent) %>%
      dplyr::summarise(who = !!who,
                       PA = sum(.data$PA_total),
                       number = sum(.data$staff, na.rm = TRUE),
                       density = .data$PA / .data$number) -> continents_summary_estimates

    continents_summary_estimates %>%
      dplyr::full_join(what$all$tallies_details[[1]] %>% dplyr::select(.data$continent, .data$lwr, .data$upr),
                       by = "continent") %>%
      dplyr::rename(number_lwr = .data$lwr,
                    number_upr = .data$upr) %>%
      dplyr::filter(.data$continent != "Antarctica") %>%
      dplyr::mutate(density_lwr = .data$PA / .data$number_upr,
                    density_upr = .data$PA / .data$number_lwr) -> continents_summary

    dplyr::bind_rows(world_summary, continents_summary) %>%
      dplyr::select(.data$who, .data$continent, .data$PA, tidyselect::starts_with("number"), tidyselect::starts_with("density")) %>%
      dplyr::rename_with(.fn = ~ paste0(who, "_", .))

  }) -> tables

  do.call("cbind", tables) %>%
    tibble::as_tibble() -> table

  if (!with_PI) {
    table %>%
      dplyr::select(-tidyselect::contains("upr"), -tidyselect::contains("lwr")) -> table
  }

  table %>%
    dplyr::select(-.data$all_who, -.data$rangers_who, -.data$rangers_continent, -.data$rangers_PA) %>%
    dplyr::rename(continent = .data$all_continent,
                  PA = .data$all_PA)

}


#' This function creates the table that shows the requirements in rangers and all personal.
#' IIt creates of the main text tables.
#'
#' @inheritParams table_predictions_summary
#'
#' @return a tibble
#' @export
#'
#' @examples
#' # see see ?rangeRinPA
#'
table_projections <- function(what, data) {

  table_pred <- table_predictions_summary(what = what, data = data, with_PI = FALSE)

  table_pred %>%
    dplyr::filter(.data$continent == "Global") %>%
    dplyr::select(-.data$continent) %>%
    dplyr::rename_with(.cols = tidyselect::matches("number|density"), .fn = ~ paste0(., "_pred")) -> table_pred

  current_pct <- sum(data$area_PA_total, na.rm = TRUE) / sum(data$area_country, na.rm = TRUE) # does not contain Greenland

  table_pred %>%
    dplyr::select(-tidyselect::contains("rangers")) %>%
    dplyr::mutate(who = "all",
                  number = .data$all_number_pred,
                  density = .data$all_density_pred,
                  number_required = .data$number * 1/0.36,
                  increase_required = .data$number_required - .data$number,
                  density_required = .data$PA/.data$number_required,
                  number_required_2030 = .data$number * 1/0.36 * 0.3/current_pct,
                  increase_required_2030 = .data$number_required_2030 - .data$number,
                  density_required_2030 = (.data$PA  * 0.3/current_pct)/.data$number_required_2030,
                  .keep = "none") -> table_all

  table_pred %>%
    dplyr::select(-tidyselect::contains("all")) %>%
    dplyr::mutate(who = "rangers",
                  number = .data$rangers_number_pred,
                  density = .data$rangers_density_pred,
                  number_required = .data$number * 1/0.36,
                  increase_required = .data$number_required - .data$number,
                  density_required = .data$PA/.data$number_required,
                  number_required_2030 = .data$number * 1/0.36 * 0.3/current_pct,
                  increase_required_2030 = .data$number_required_2030 - .data$number,
                  density_required_2030 = (.data$PA  * 0.3/current_pct)/.data$number_required_2030,
                  .keep = "none") -> table_rangers

  dplyr::bind_rows(table_all, table_rangers) -> table_full

  table_full

  }
