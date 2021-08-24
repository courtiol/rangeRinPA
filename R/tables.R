#' Create completeness table (numbers)
#'
#' This function creates the table that indicate how many countries and territories have ranger staff and other staff documented in our data.
#'
#' @param data the complete dataset
#' @param outliers a vector with the iso code for the countries/territories to discard (default = `"GRL"`)
#'
#' @return a tibble
#' @export
#'
#' @examples
#' table_completeness(data_rangers)
#'
table_completeness <- function(data, outliers = "GRL") {

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
#' @inheritParams table_completeness
#'
#' @return a tibble
#' @export
#'
#' @examples
#' table_completeness_km2(data_rangers)
#'
table_completeness_km2 <- function(data, outliers = "GRL") {

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
