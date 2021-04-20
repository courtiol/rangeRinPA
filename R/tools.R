
#' Tool function to compute missing values and missing PA
#'
#' @param vars a vector of quoted variable names
#' @param data a data.frame or tibble
#' @param resp_var the name of the response variable (to exclude it from computation)
#' @param PA_var the name of the variable storing the PA area information
#' @param PA_invlink the function to turn PA_var into km2 (default = exp)
#'
#' @return a tibble
#' @export
#'
check_losses <- function(vars, data, resp_var = "staff_rangers_log", PA_var = "PA_area_log", PA_invlink = exp) {

  data %>%
    dplyr::select(-resp_var) -> data

  nrow_ini <- nrow(data)
  PA_area_ini <- sum(PA_invlink(data[, PA_var, drop = TRUE]), na.rm = TRUE)

  output <- tibble::tibble(predictor = c("none", vars),
                           n_missing  = NA_real_,
                           PA_area_missing = NA_real_,
                           n_missing_cumul = NA_real_,
                           PA_area_missing_cumul = NA_real_)

  output[1, "n_missing"] <- 0
  output[1, "PA_area_missing"] <- 0
  output[1, "n_missing_cumul"] <- 0
  output[1, "PA_area_missing_cumul"] <- 0

  for (var in vars) {
    output[output$predictor == var, "n_missing"] <- sum(is.na(data[, var, drop = TRUE]))
    output[output$predictor == var, "PA_area_missing"] <- sum(PA_invlink(data[is.na(data[, var, drop = TRUE]), PA_var, drop = TRUE]))
  }

  # cumulative computation
  for (i in seq_len(length(vars))) {
    data %>%
      tidyr::drop_na(vars[seq_len(i)]) -> data

    output[output$predictor == vars[i], "n_missing_cumul"] <- nrow_ini - nrow(data)
    output[output$predictor == vars[i], "PA_area_missing_cumul"] <- PA_area_ini - sum(PA_invlink(data[, PA_var, drop = TRUE]), na.rm = TRUE)
  }

  output[-1, ]
}
