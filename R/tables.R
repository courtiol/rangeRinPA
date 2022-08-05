#' Create core data table
#'
#' This function creates the table with the core information about surveyed countries/territories.
#' If the argument `what` is not null, it will also include estimates and densities for all PAs.
#'
#' @inheritParams plot_PA_by_data_type
#' @param data the complete dataset created with [`fetch_data`()](fetch_data).
#'
#' @return a tibble
#' @export
#'
#' @examples
#' table_core_data(data_rangers)
#'
table_core_data <- function(data, what = NULL) {

  data %>%
    dplyr::select(.data$countryname_iso,
                  .data$countryname_eng,
                  .data$data_year_info,
                  .data$staff_others, .data$staff_rangers, .data$staff_total,
                  .data$PA_area_surveyed, .data$area_PA_total) %>%
    dplyr::filter(!is.na(.data$staff_others) | !is.na(.data$staff_rangers) | !is.na(.data$staff_total)) %>%
    dplyr::mutate(pct_covered = round(100 * .data$PA_area_surveyed / .data$area_PA_total, 1),
                  density_staff_others = round(.data$PA_area_surveyed / .data$staff_others, 1),
                  density_staff_rangers = round(.data$PA_area_surveyed / .data$staff_rangers, 1),
                  density_staff_total = round(.data$PA_area_surveyed / .data$staff_total, 1)) %>%
    dplyr::mutate(dplyr::across(.cols = tidyselect::starts_with("density"), ~ dplyr::na_if(.x, y = Inf))) %>%
    dplyr::mutate(dplyr::across(.cols = tidyselect::starts_with("density"), ~ dplyr::if_else(is.nan(.x), NA_real_, .x))) %>%
    dplyr::mutate(pct_covered = dplyr::if_else(is.nan(.data$pct_covered), 100, .data$pct_covered)) %>%
    dplyr::mutate(dplyr::across(.cols = c("PA_area_surveyed", "area_PA_total"), round)) %>%
    dplyr::select("ISO Code" = .data$countryname_iso,
                  "Country/territory" = .data$countryname_eng,
                  "Total area of terrestrial PAs in country/territory (km^2) World Database on Protected Areas 2020" = .data$area_PA_total,
                  "Year(s) of the data" = .data$data_year_info,
                  "Area of terrestrial PA surveyed (km^2)" = .data$PA_area_surveyed,
                  "Percentage of the total terrestrial PA area of the country/territory surveyed" = .data$pct_covered,
                  "All personnel in surveyed area" = .data$staff_total,
                  "Rangers in surveyed area" = .data$staff_rangers,
                  "Non-rangers in surveyed area" = .data$staff_others,
                  "Observed densities for all personnel in surveyed area( km^2 of PA per person)" = .data$density_staff_total,
                  "Observed densities for rangers in surveyed area (km^2 of PA per person)" = .data$density_staff_rangers
                  ) -> table_raw

  if (is.null(what)) return(table_raw)

  table_predictions_per_country(what = what, data = data) %>%
    dplyr::mutate(dplyr::across(.cols = tidyselect::ends_with("density"), ~ dplyr::na_if(.x, y = Inf))) %>%
    dplyr::mutate(countryname_eng = dplyr::if_else(.data$countryname_eng == "W African Country", "Other country", .data$countryname_eng)) %>%
    dplyr::left_join(data %>% dplyr::select(.data$countryname_iso, .data$countryname_eng), by = "countryname_eng") %>%
    dplyr::mutate(PA_total = round(.data$PA_total)) %>%
    dplyr::rename("ISO Code" = .data$countryname_iso,
                  "Country/territory" = .data$countryname_eng,
                  "Total area of terrestrial PAs in country/territory (km^2) World Database on Protected Areas 2020" = .data$PA_total) %>%
    dplyr::mutate(dplyr::across(.cols = tidyselect::ends_with("density"),
                                .fns = round, digits = 1)) %>%
    dplyr::mutate(dplyr::across(.cols = tidyselect::ends_with("estimate"),
                                .fns = round, digits = 0)) -> table_estimates

  dplyr::full_join(table_raw, table_estimates,
                   by = c("ISO Code", "Country/territory", "Total area of terrestrial PAs in country/territory (km^2) World Database on Protected Areas 2020")) -> table_all

  table_all %>%
    dplyr::mutate(all_estimate = dplyr::if_else(.data$"Non-rangers in surveyed area" == 0 & .data$"Percentage of the total terrestrial PA area of the country/territory surveyed" == 100 & is.na(.data$all_estimate), 0, .data$all_estimate)) %>%
    dplyr::mutate(rangers_estimate = dplyr::if_else(.data$"Rangers in surveyed area" == 0 & .data$"Percentage of the total terrestrial PA area of the country/territory surveyed" == 100 & is.na(.data$rangers_estimate), 0, .data$rangers_estimate)) %>%
    dplyr::rename("Total personnel" = .data$all_estimate,
                  "Total rangers" = .data$rangers_estimate,
                  "Density of all personnel (km^2 of PA per person))" = .data$all_density,
                  "Density of rangers (km^2 of PA per person)" = .data$rangers_density) %>%
    dplyr::select("ISO Code",
                  "Country/territory",
                  "Total area of terrestrial PAs in country/territory (km^2) World Database on Protected Areas 2020",
                  "Year(s) of the data",
                  "Area of terrestrial PA surveyed (km^2)",
                  "Percentage of the total terrestrial PA area of the country/territory surveyed",
                  "All personnel in surveyed area",
                  "Rangers in surveyed area",
                  "Non-rangers in surveyed area",
                  "Observed densities for all personnel in surveyed area( km^2 of PA per person)",
                  "Observed densities for rangers in surveyed area (km^2 of PA per person)",
                  "Total personnel",
                  "Total rangers",
                  "Density of all personnel (km^2 of PA per person))",
                  "Density of rangers (km^2 of PA per person)")

}


#' Create completeness table (observations)
#'
#' This function creates the table that indicate how many countries and territories have ranger staff and other staff documented in our data.
#'
#' @inheritParams table_core_data
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

#' Create table with info on initial training datasets
#'
#' This function creates the table that shows the information about the initial training datasets.
#'
#' @inheritParams extract_results
#'
#' @return a tibble
#' @export
#'
#' @examples
#' # see see ?rangeRinPA
#'
table_training_initial <- function(list_results_LMM, list_results_RF, data) {
  extract_training_info(which = "initial",
                        list_results_LMM = list_results_LMM,
                        list_results_RF = list_results_RF,
                        data = data) -> training_info_initial

  training_info_initial %>% # remove duplicates
    dplyr::mutate(who = dplyr::case_when(.data$who == "All" ~ "All personnel",
                                         .data$who == "Rangers" ~ "Rangers",
                                         .data$who == "Others" ~ "Non-rangers"),
                  who = forcats::fct_inorder(.data$who)) %>%
    dplyr::group_by(.data$who) %>%
    dplyr::slice(1) %>%
    dplyr::select(-.data$type, -.data$coef, -.data$ncol) %>%
    dplyr::ungroup()
  }


#' Create table with info on final training datasets
#'
#' This function creates the table that shows the information about the final training datasets.
#'
#' @inheritParams extract_results
#'
#' @return a tibble
#' @export
#'
#' @examples
#' # see see ?rangeRinPA
#'
table_training_final <- function(list_results_LMM, list_results_RF, data) {

  extract_training_info(which = "final",
                        list_results_LMM = list_results_LMM,
                        list_results_RF = list_results_RF,
                        data = data) -> training_info_final

  training_info_final %>%
    dplyr::mutate(who = dplyr::case_when(.data$who == "All" ~ "All personnel",
                                         .data$who == "Rangers" ~ "Rangers",
                                         .data$who == "Others" ~ "Non-rangers"),
                  who = forcats::fct_inorder(.data$who)) %>%
    dplyr::mutate(n_pred = .data$ncol - 4L, .after = .data$obs) %>%
    dplyr::select(-.data$ncol)
  }



#' Create table with info on predictions datasets
#'
#' This function creates the table that shows the information about the datasets used for predictions.
#'
#' @inheritParams extract_results
#'
#' @return a tibble
#' @export
#'
#' @examples
#' # see see ?rangeRinPA
#'
table_predictions_data <- function(list_results_LMM, list_results_RF, data) {

  extract_predictions_info(list_results_LMM = list_results_LMM,
                           list_results_RF = list_results_RF,
                           data = data) -> predictions_info

  predictions_info %>%
    dplyr::mutate(who = dplyr::case_when(.data$who == "All" ~ "All personnel",
                                         .data$who == "Rangers" ~ "Rangers",
                                         .data$who == "Others" ~ "Non-rangers"),
                  who = forcats::fct_inorder(.data$who))
  }


#' Create table with info on fine tuning
#'
#' This function creates the table that shows the information about the fine tuning results.
#'
#' @inheritParams extract_results
#'
#' @return a tibble
#' @export
#'
#' @examples
#' # see see ?rangeRinPA
#'
table_tuning <- function(list_results_LMM, list_results_RF, data) {

  extract_finetuning(list_results_LMM = list_results_LMM,
                     list_results_RF = list_results_RF,
                     data = data) -> fine_tunning_selected

  fine_tunning_selected %>%
    dplyr::mutate(who = dplyr::case_when(.data$who == "All" ~ "All personnel",
                                         .data$who == "Rangers" ~ "Rangers",
                                         .data$who == "Others" ~ "Non-rangers"),
                  who = forcats::fct_inorder(.data$who))
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
    dplyr::arrange(.data$who, dplyr::desc(.data$coef_imputation)) %>%
    dplyr::mutate(who = dplyr::case_when(.data$who == "All" ~ "All personnel",
                                         .data$who == "Rangers" ~ "Rangers",
                                         .data$who == "Others" ~ "Non-rangers"),
                  who = forcats::fct_inorder(.data$who))
}


#' Create table with predictions (per continent)
#'
#' This function creates the table that shows the point predictions and their intervals, for the
#' different continents.
#' NOTE: superseeded by [`table_predictions_summary`()](table_predictions_summary).
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
      dplyr::rename(point_pred = .data$sum_total) %>%
      dplyr::mutate(who = dplyr::case_when(.data$who == "All" ~ "All personnel",
                                           .data$who == "Rangers" ~ "Rangers",
                                           .data$who == "Others" ~ "Non-rangers"),
                    who = forcats::fct_inorder(.data$who))
}


#' Create table with predictions (per country)
#'
#' This function creates the table that shows the point predictions and their intervals, for the
#' different countries.
#'
#' @inheritParams table_predictions_per_method
#' @inheritParams plot_PA_by_data_type
#' @importFrom rlang :=
#'
#' @return a tibble
#' @export
#'
#' @examples
#' # see see ?table_core_data
#'
table_predictions_per_country <- function(what, data) {

  who_to_do <- c("all", "rangers", "others")

    lapply(who_to_do, \(who) {
    var_staff <- colnames(what[[who]]$country_info[[1]])[grepl("staff", colnames(what[[who]]$country_info[[1]]))]

    what[[who]]$country_info[[1]] %>%
      dplyr::mutate(staff = pmax(delog1p(!!rlang::sym(var_staff)), 0)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(PA_total = sum(dplyr::c_across(tidyselect::starts_with("PA")))) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(density = .data$PA_total / .data$staff) %>%
      dplyr::mutate(density = dplyr::if_else(.data$staff < 1, NA_real_, .data$density)) %>%
      dplyr::arrange(.data$countryname_eng) %>%
      dplyr::select(.data$countryname_eng, .data$staff, .data$PA_total, .data$density) %>%
      dplyr::rename("{who}_estimate" := .data$staff,
                    "{who}_density" := .data$density)
    }) -> tables

  names(tables) <- who_to_do

  tables[["all"]] %>%
    dplyr::full_join(tables[["rangers"]], by = c("countryname_eng", "PA_total")) %>%
    dplyr::full_join(tables[["others"]], by = c("countryname_eng", "PA_total")) %>%
    dplyr::select(.data$countryname_eng, .data$PA_total,
                  .data$others_estimate, .data$rangers_estimate, .data$all_estimate,
                  .data$others_density, .data$rangers_density, .data$all_density)

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

  if (with_PI) {
      who_to_do <- c("all", "rangers", "others")
    } else {
      who_to_do <-  c("all", "rangers")
    }

  lapply(who_to_do, \(who) {
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
      dplyr::mutate(continent = "World", .before = 1L) -> world_summary_estimates_all

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
      dplyr::full_join(what[[who]]$tallies_details[[1]] %>% dplyr::select(.data$continent, .data$lwr, .data$upr),
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
    dplyr::rename(continent = .data$all_continent, PA = .data$all_PA) -> table_out

  if (with_PI) {
    table_out %>%
      dplyr::select( -.data$others_who, -.data$others_continent, -.data$others_PA) -> table_out
  }


  ## pivot table as too wide if it contains PIs:
  if (with_PI) {
    table_out %>%
      tidyr::pivot_longer(-c(.data$PA, .data$continent)) %>%
      tidyr::separate(.data$name, into = c("who", "what"), sep = "\\_", extra = "merge") %>%
      tidyr::pivot_wider(names_from = .data$what, values_from = .data$value) %>%
      dplyr::relocate(.data$who, .before = 1) %>%
      dplyr::select(-.data$PA)  %>%
      dplyr::mutate(who = dplyr::case_when(.data$who == "all" ~ "All personnel",
                                           .data$who == "rangers" ~ "Rangers",
                                           .data$who == "others" ~ "Non-rangers"),
                    who = forcats::fct_inorder(.data$who)) %>%
      dplyr::arrange(.data$who) -> table_out
  }

  table_out

}


#' This function creates the table that shows the requirements in rangers and all personal.
#' It creates of the main text tables.
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
    dplyr::filter(.data$continent == "World") %>%
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


#' Create table with correlations between variables
#'
#' @inheritParams table_predictions_summary
#' @param method a character string indicating which correlation coefficient (or covariance) is to
#' be computed. One of "pearson" (default), "kendall", or "spearman": can be abbreviated.
#'
#' @return a correlation matrix
#' @export
#'
#' @examples
#' # see see ?rangeRinPA
#'
table_correlates <- function(data, method = "pearson") {

  all_responses <- c("staff_rangers_log", "staff_others_log", "staff_total_log")
  all_predictors <- c("PA_area_log", "pop_density_log", "area_country_log", "long", "lat", "area_forest_pct","GDP_2019_log", "GDP_capita_log",
                      "GDP_growth", "unemployment_log", "EVI", "EPI_2020", "SPI", "IUCN_1_4_prop", "IUCN_1_2_prop")
  all_vars <- c(all_responses, all_predictors)
  data <- handle_transform(data)
  data$PA_area_log <- log(data$PA_area_surveyed + data$PA_area_unsurveyed + 1)
  m <- matrix(NA, nrow = length(all_vars), ncol = length(all_vars))
  for (col in seq_along(all_vars)) {
    for (row in seq_along(all_vars)) {
      m[row, col] <- stats::cor(data[, colnames(data) == all_vars[col], drop = TRUE],
                                data[, colnames(data) == all_vars[row], drop = TRUE],
                                use = "pairwise.complete.obs")

    }
  }
  all_vars[all_vars == "staff_rangers_log"] <- "rangers_log"
  all_vars[all_vars == "staff_others_log"] <- "non-rangers_log"
  all_vars[all_vars == "staff_total_log"] <- "all_PA_personnel_log"

  all_vars <- totitle(gsub(pattern = "_", replacement = " ", x = all_vars, fixed = TRUE))

  colnames(m) <- rownames(m) <- all_vars

  d <- as.data.frame(round(m, digits = 2L))
  tibble::rownames_to_column(d, var = "Variable")
}

