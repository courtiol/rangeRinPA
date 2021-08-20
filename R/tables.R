#' Create completeness table
#'
#' This function creates the table that indicate how many countries and territories have ranger staff and other staff documented in our data.
#'
#' @param data the complete dataset
#'
#' @return a tibble
#' @export
#'
#' @examples
#' table_completeness(data_rangers)
#'
table_completeness <- function(data) {
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
