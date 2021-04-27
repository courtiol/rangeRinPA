
#' Count missing values and missing PA
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


#' Create train and test datasets given a formula and data
#'
#' @param formula the formula for the LMM or RF
#' @param data the complete dataset
#' @param test_prop the amount of rows to keep in the test dataset (default = 0)
#' @param keep.var a vector of character strings indicating which variables to keep in the dataset on top of those defined by the formula
#' @param seed an optional seed for the RNG
#' @export
#'
#' @examples
#' prepare_data(log(staff_rangers + 1) ~ log(GDP_2019) + Matern(1|lat + long),
#'              data = data_rangers, test_prop = 0.1)
#'
prepare_data <- function(formula, data, test_prop = 0, keep.var = NULL, seed = NULL) {
  if (test_prop < 0 | test_prop > 1) stop("test_prop must be between 0 and 1")
  vars <- all.vars(formula)
  if (any(vars == ".")) {
    vars <- colnames(data)
  }
  if (!is.null(keep.var)) {
    vars <- unique(c(vars, keep.var))
    if (any(!keep.var %in% colnames(data))) {
      warning("Some variables in keep.var are not found in data")
      vars <- vars[vars %in% colnames(data)]
    }
  }
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


#' Identify variables available for focal countries
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


#' Compare predictions and observations using various metrics
#'
#' @param pred a vector of predicted values
#' @param obs a vector of observed values
#' @param inv.dist a matrix of inverse distance between points (optional but needed to compute Moran's I)
#'
#' @return a data frame with all the metrics
#' @export
#'
#' @examples
#' compute_metrics(pred = c(1.1, 1.8, 2.5, 4.5),
#'                 obs = c(1, 2, 3, 4))
#'
#' proxy <- compute_distance(long = c(13.41, -0.13, -77.04, 116.24),
#'                           lat = c(52.52, 51.51, 38.89, 39.90),
#'                           inv = TRUE)
#'
#' compute_metrics(pred = c(1.1, 1.8, 2.5, 4.5),
#'                 obs = c(1, 2, 3, 4),
#'                 inv.dist = proxy)
#'
compute_metrics <- function(pred, obs, inv.dist = NULL) {

  resid <- obs - pred

  RMSE <- sqrt(sum((pred - obs)^2)) # root mean squared error

  ME   <- mean(resid) # average bias in predictions

  MAE <- mean(abs(resid)) # mean absolute error

  R2 <- 1 - (sum((resid)^2) / sum((obs - mean(obs))^2)) # R2

  RRSE <- sqrt(((sum(pred) - sum(obs))^2) / sum(obs)) # root relative sum error (homemade)

  CCC <- (2*stats::cor(pred, obs)*stats::sd(pred)*stats::sd(pred))/(stats::var(pred) + stats::var(obs) + (mean(pred) - mean(obs))^2)
  ## Note: CCC = Lin's concordance correlation coef (Steichen & Cox, 2002)

  MoranI <- c(obs = NA, expected = NA, sd = NA, p.value = NA) # Moran's I

  if (!is.null(inv.dist)) {
    MoranI <- unlist(ape::Moran.I(resid, inv.dist))
    names(MoranI) <- paste0("MoranI_", names(MoranI))
  }
  c(c(RMSE = RMSE, ME = ME, MAE = MAE, R2 = R2, RRSE = RRSE, CCC = CCC), MoranI)
}


#' Compute the distance (or inverse distance) between locations
#'
#' @param long a vector of longitudes
#' @param lat a vector of latitudes
#' @param inv whether to return the inverse of the distance (default = FALSE)
#'
#' @return a square data frame
#' @export
#'
#' @examples
#' # distance Berlin/London:
#' compute_distance(long = c(13.41, -0.13), lat = c(52.52, 51.51))
#'
compute_distance <- function(long, lat, inv = FALSE) {

  dist <- spaMM::make_scaled_dist(data.frame(long = long, lat = lat),
                                  rho = 1, dist.method = "Earth",
                                  return_matrix = TRUE)
  if (inv) {
    dist <- 1/dist
    diag(dist) <- 0
  }

  dist <- as.data.frame(dist)
  colnames(dist) <- paste0("loc_", seq_len(ncol(dist)))
  rownames(dist) <- paste0("loc_", seq_len(ncol(dist)))

  dist
}
