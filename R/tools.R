
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



#' Tool function to compute train and test datasets given a formula
#'
#' @param formula the formula for the LMM or RF
#' @param data the complete dataset
#' @param test_prop the amount of rows to keep in the test dataset (default = 0)
#' @param seed an optional seed for the RNG
#' @export
#'
#' @examples
#' prepare_data(log(staff_rangers + 1) ~ log(GDP_2019) + Matern(1|lat + long),
#'              data = data_rangers, test_prop = 0.1)
#'
prepare_data <- function(formula, data, test_prop = 0, seed = NULL) {
  if (test_prop < 0 | test_prop > 1) stop("test_prop must be between 0 and 1")
  vars <- all.vars(formula)
  if (any(vars == ".")) {
    vars <- colnames(data)
  }
  #mf <- stats::model.frame(formula = formula, data = data) ## cannot handle random effects so we do differently
  data <- data[, vars]
  omit_obj <- stats::na.omit(data)
  row_to_drop <- as.numeric(attr(omit_obj, "na.action"))
  if (length(row_to_drop) > 0) {
    data_train <- data[-row_to_drop, ]
  } else {
    data_train <- data
  }
  data_test <- tibble::tibble()
  if (test_prop > 0) {
    nrow_test <- ceiling(nrow(data_train)*test_prop)
    if (!is.null(seed)) {
      set.seed(seed)
    }
    row_test <- sample(seq_len(nrow(data_train)), size = nrow_test, replace = FALSE)
    data_test <- data_train[row_test, ]
    data_train <- data_train[-row_test, ]
  }
  list(data_train = data_train, data_test = data_test, missing = length(row_to_drop))
}


#' Tool function to identify variables available for focal countries
#'
#' @param country a vector of ISO code(s) of the focal country or countries
#' @param data the complete dataset
#' @export
#'
#' @examples
#' prepare_vars(country = "VUT", data = data_rangers)
#'
prepare_vars <- function(country, data) {
  data %>%
    dplyr::filter(.data$countryname_iso %in% country) %>%
    dplyr::select(where(~ !any(is.na(.x)))) %>%
    colnames()
}

globalVariables("where") ## since not properly exported from tidyselect...


#' Compute root mean square error
#'
#' @param pred a vector of predictions
#' @param obs a vector of observations
#'
#' @export
#'
RMSE <- function(pred, obs) {
  sqrt(sum((pred - obs)^2))
  }
