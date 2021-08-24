#' Create completeness table (observations)
#'
#' This function creates the table that indicate how many countries and territories have ranger staff and other staff documented in our data.
#'
#' @param data the complete dataset
#' @param outliers a vector with the iso code for the countries/territories to discard (default = `NULL`)
#'
#' @return a tibble
#' @export
#'
#' @examples
#' table_completeness_obs(data_rangers)
#'
table_completeness_obs <- function(data, outliers = NULL) {

  data %>%
    dplyr::filter(!.data$countryname_iso %in% !!outliers) -> data

  tibble::tibble(
    category = c("rangers and other staff both separately known",
                 "rangers and other staff both separately known",
                 "rangers and other staff both known but lumped together",
                 "rangers and other staff both known but lumped together",
                 "only ranger staff known",
                 "only ranger staff known",
                 "only other staff known",
                 "only other staff known",
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
    dplyr::mutate(both = .data$full_survey + .data$partial_survey)
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
table_completeness_km2 <- function(data, outliers = NULL) {

  data %>%
    dplyr::filter(!.data$countryname_iso %in% !!outliers) -> data

  tibble::tibble(
    category = c("rangers and other staff both separately known",
                 "rangers and other staff both separately known",
                 "rangers and other staff both known but lumped together",
                 "rangers and other staff both known but lumped together",
                 "only ranger staff known",
                 "only ranger staff known",
                 "only other staff known",
                 "only other staff known",
                 "no data",
                 "no data"),
    category_PA = rep(c("km2_surveyed", "km2_unsurveyed"), 5),
    N = c(sum(data$PA_area_surveyed[!is.na(data$staff_rangers_others_known) & data$PA_area_unsurveyed == 0]),
          sum(data$PA_area_unsurveyed[!is.na(data$staff_rangers_others_known) & data$PA_area_unsurveyed > 0]),
          sum(data$PA_area_surveyed[!is.na(data$staff_total_details_unknown) & data$PA_area_unsurveyed == 0]),
          sum(data$PA_area_unsurveyed[!is.na(data$staff_total_details_unknown) & data$PA_area_unsurveyed > 0]),
          sum(data$PA_area_surveyed[!is.na(data$staff_rangers_others_unknown) & data$PA_area_unsurveyed == 0]),
          sum(data$PA_area_unsurveyed[!is.na(data$staff_rangers_others_unknown) & data$PA_area_unsurveyed > 0]),
          sum(data$PA_area_surveyed[!is.na(data$staff_others) & is.na(data$staff_rangers) & data$PA_area_unsurveyed == 0]),
          sum(data$PA_area_unsurveyed[!is.na(data$staff_others) & is.na(data$staff_rangers) & data$PA_area_unsurveyed > 0]),
          sum(data$PA_area_surveyed[(is.na(data$staff_rangers_others_known) &
               is.na(data$staff_rangers_others_unknown) &
               is.na(data$staff_total_details_unknown) & data$PA_area_unsurveyed == 0)]),
          sum(data$PA_area_unsurveyed[(is.na(data$staff_rangers_others_known) &
               is.na(data$staff_rangers_others_unknown) &
               is.na(data$staff_total_details_unknown) & data$PA_area_unsurveyed > 0)])
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
table_completeness_vars <- function(data, outliers = NULL) {

  data %>%
    dplyr::filter(!.data$countryname_iso %in% !!outliers) -> data

  var_table <- function(name, var) {
   tibble::tibble(variable = c(name),
                  nb_known = c(sum(!is.na(data[, var]))),
                  nb_unknown = c(sum(is.na(data[, var]))),
                  proportion_nb_known = .data$nb_known / (.data$nb_known + .data$nb_unknown),
                  total_PA_known = c(sum(data$PA_area_surveyed[!is.na(data[, var])])),
                  total_PA_unknown = c(sum(data$PA_area_unsurveyed[is.na(data[, var])])),
                  proportion_total_PA_known = .data$total_PA_known / (.data$total_PA_known + .data$total_PA_unknown))
  }

  dplyr::bind_rows(
    var_table(name = "Number of ranger staff", var = "staff_rangers"),
    var_table(name = "Number of other staff", var = "staff_others"),
    var_table(name = "Number of total staff", var = "staff_total"),
    var_table(name = "Latitude", var = "lat"),
    var_table(name = "Longitude", var = "long"),
    var_table(name = "Area", var = "area_country"),
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
    var_table(name = "Surface of protected area", var = "EVI"),
    var_table(name = "Surface of protected area", var = "EPI_2020"),
    var_table(name = "Surface of protected area", var = "SPI")
  )

}


