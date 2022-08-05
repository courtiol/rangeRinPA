
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
#' @param drop_na whether to drop NAs or not (default = TRUE)
#' @param spatial whether or not keeping predictor for fitting spatial effects (default = FALSE)
#' @param seed an optional seed for the RNG
#' @export
#'
#' @examples
#' prepare_data(log(staff_rangers + 1) ~ log(GDP_2019) + Matern(1|long + lat),
#'              data = data_rangers, test_prop = 0.1)
#'
prepare_data <- function(formula, data, test_prop = 0, keep.var = NULL, drop_na = TRUE, spatial = FALSE, seed = NULL) {
  if (test_prop < 0 | test_prop > 1) stop("test_prop must be between 0 and 1")
  vars <- all.vars(formula)
  if (any(vars == ".")) {
    vars <- colnames(data)
  }
  if (is.null(keep.var)) {
    if (spatial) {
      keep.var <- c("long", "lat")
    }
  } else {
    if (spatial) {
      keep.var <- c(keep.var, "long", "lat")
    }
    vars <- unique(c(vars, keep.var))
    if (any(!keep.var %in% colnames(data))) {
      warning("Some variables in keep.var are not found in data")
      vars <- vars[vars %in% colnames(data)]
    }
  }

  data <- data[, vars]
  omit_obj <- stats::na.omit(data)
  row_to_drop <- as.numeric(attr(omit_obj, "na.action"))
  if (length(row_to_drop) > 0 && drop_na) {
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
#' We use the the root mean square error (RMSE) as default in this package, but we were also
#' interested in exploring alternative metrics to quantify various aspects of the prediction
#' accuracy by cross validation: 1) the mean error (ME), 2) the mean absolute error (MAE), 3) the
#' R-squared, 4) the concordance correlation coefficient (CCC; Lin 1989; Steichen & Cox, 2002). We
#' also defined a metric which we call the root sum error (RSE) aiming at quantifying the prediction
#' uncertainty directly on the total numbers of staff instead of on the prediction for individual
#' country/territory. The rationale for considering this additional metric is to measure the
#' prediction uncertainty at the scale we are actually interested in (a tally across
#' countries/territories and not prediction for a particular country/territory). Indeed, our
#' predictions are a priori not independent and thus the (expected) squared prediction error of the
#' sum is not the sum of (expected) squared errors for each response. If the package ape is
#' installed, we finally computed Moran's I statistics (Moran 1950) via its implementation in the R
#' package ape (Paradis & Schliep 2019) to quantify the amount of spatial autocorrelation in cross
#' validation residuals.
#'
#' @references Lin, L. I. 1989. A concordance correlation coefficient to evaluate reproducibility.
#' Biometrics 45: 255–268.
#'
#' Steichen, T. J., & Cox, N. J. (2002). A note on the concordance correlation coefficient. The
#' Stata Journal, 2(2), 183-189.
#'
#' Moran, P. A. (1950). Notes on continuous stochastic phenomena. Biometrika, 37(1/2), 17-23.
#'
#' Paradis E. & Schliep K. 2019. ape 5.0: an environment for modern phylogenetics and evolutionary
#' analyses in R. Bioinformatics 35: 526-528.
#'
#'
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

  RSE <- sqrt(((sum(pred) - sum(obs))^2) / length(obs)) # root sum error (homemade)

  CCC <- (2*stats::cor(pred, obs)*stats::sd(pred)*stats::sd(pred))/(stats::var(pred) + stats::var(obs) + (mean(pred) - mean(obs))^2)
  ## Note: CCC = Lin's concordance correlation coef (Steichen & Cox, 2002)

  MoranI <-  c("MoranI" = NA, "MoranI_pv" = NA) # Moran's I

  if (!is.null(inv.dist) && requireNamespace("ape", quietly = TRUE)) {
    MoranI <- unlist(ape::Moran.I(resid, inv.dist))
    MoranI <- MoranI[c("observed", "p.value")]
    names(MoranI) <- c("MoranI", "MoranI_pv")
  }
  c(c(RMSE = RMSE, ME = ME, MAE = MAE, R2 = R2, RSE = RSE, CCC = CCC), MoranI)
}


#' Aggregate the accuracy metrics
#'
#' @param metrics_df a data frame with metrics produced by [`compute_metrics()`]
#' @param fn the function for aggregating all metrics but p-values
#'
#' @return a data frame
#' @export
#'
#'
aggregate_metrics <- function(metrics_df, fn = mean) {
  type <- ifelse(any(colnames(metrics_df) == "OOB_test"), "OOB", "CV")
  n <- nrow(metrics_df)
  metrics_df <- metrics_df[, !grepl("_test", colnames(metrics_df))]
  Moran_pv <- metrics_df[, "MoranI_pv", drop = TRUE]
  metrics_df <- metrics_df[, !grepl("MoranI_pv", colnames(metrics_df))]
  metrics_aggregated <- as.data.frame(t(apply(metrics_df, 2, fn)))
  cbind(type = type, rep = n, metrics_aggregated, Moran_pv_freq = fn(Moran_pv < 0.05))
}


#' Compute the distance (or inverse distance) between locations
#'
#' @param long a vector of longitudes
#' @param lat a vector of latitudes
#' @param inv whether to return the inverse of the distance (default = FALSE)
#'
#' @return a square data frame (if inv = FALSE) or matrix (if inv = TRUE)
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

  colnames(dist) <- paste0("loc_", seq_len(ncol(dist)))
  rownames(dist) <- paste0("loc_", seq_len(ncol(dist)))

  if (inv) {
    dist <- 1/dist
    diag(dist) <- 0
  } else {
    ## note: inv distance only used in ape::Moran.I call which is very slow if fed with a df instead of a matrix
    dist <- as.data.frame(dist)
  }

  dist
}


#' Drop logs from formula
#'
#' @param formula a formula
#'
#' @return a formula
#' @export
#'
#' @examples
#' f <- staff_rangers_log ~ pop_density_log + PA_area_log + long + Matern(1|long + lat)
#' drop_logs(f)
#'
drop_logs <- function(formula) {
  resp <- as.character(formula)[2]
  if (grepl(pattern = "_log", resp)) {
    resp <- sub(pattern = "_log", replacement = "", x = resp)
  }
  preds <- as.character(formula)[3]
  if (grepl(pattern = "_log", preds)) {
    preds <- gsub(pattern = "_log", replacement = "", x = preds)
  }
  if (length(preds) == 1 && preds == "") {
    preds <- "1"
  }
  if (grepl(pattern = "PA_area", preds)) {
    preds <- gsub(pattern = "PA_area", replacement = "PA_area_surveyed + PA_area_unsurveyed", x = preds)
  }
  stats::as.formula(paste(resp, "~", preds))
}


#' Add Matern to formula
#'
#' @param formula a formula
#'
#' @return a formula
#' @export
#'
#' @examples
#' f <- staff_rangers_log ~ pop_density_log + PA_area_log + long
#' add_Matern(f)
#'
add_Matern <- function(formula) {
  f <- stats::as.formula(formula)
  stats::update.formula(f, . ~ . + Matern(1|long + lat))
}


#' Compute tally
#'
#' @param data the prediction data
#' @param data_all the dataset of observations
#' @param who `"rangers"`, `"others"`, or `"all"
#' @param coef_population  the coefficient used to population rangers in unsurveyed part of surveyed countries (see [`fill_PA_area()`])
#'
#' @export
#'
compute_tally <- function(data, data_all, who, coef_population) {

  vars <-  colnames(data$data_predictable)
  resp_predicted <- vars[grep(pattern = "predicted", vars)]

  col_staff_in_data <- paste0("staff_", who)
  if (col_staff_in_data == "staff_all") {
    col_staff_in_data <- "staff_total"
  }

  ## add original PAs
  data_all %>%
    dplyr::select(.data$countryname_eng,
                  original = !!rlang::sym(col_staff_in_data),
                  .data$PA_area_surveyed, .data$PA_area_unsurveyed) %>%
    dplyr::right_join(data$data_known %>%
                        dplyr::select(-.data$PA_area_surveyed, -.data$PA_area_unsurveyed),
                      by = "countryname_eng") -> data$data_known

  data$data_predictable %>%
    add_continents(data = data_all, levels = c("Africa", "Antarctica", "Asia", "Europe",
                                               "Latin America & Caribbean", "Northern America", "Oceania")) %>%
    dplyr::group_by(.data$continent, .drop = FALSE) %>%
    dplyr::summarise(sum_predicted = sum(exp(!!rlang::sym(resp_predicted)) - 1)) -> .pred

  data$data_known %>%
    add_continents(data = data_all, levels = c("Africa", "Antarctica", "Asia", "Europe",
                                               "Latin America & Caribbean", "Northern America", "Oceania")) %>%
    dplyr::mutate(known = .data$original,
                  imputed = coef_population * .data$original/.data$PA_area_surveyed * .data$PA_area_unsurveyed,
                  known_imputed = .data$known + .data$imputed) %>%
    dplyr::group_by(.data$continent, .drop = FALSE) %>%
    dplyr::summarise(sum_known =  sum(.data$known),
                     sum_imputed = sum(.data$imputed),
                     sum_known_imputed = sum(.data$known_imputed)) -> .obs

  dplyr::full_join(.pred, .obs, by = "continent") %>%
    dplyr::mutate(sum_total = .data$sum_predicted + .data$sum_known_imputed)
}


#' Unstranform log1p variable
#'
#' @param var a vector
#'
#' @export
#'
delog1p <- function(var) {
  exp(var) - 1
}


#' Internal function used to extract PA_areas at the level of countries/territories
#'
#' This function is called by [`run_LMM_workflow()`] or [`run_RF_workflow()`].
#'
#' @inheritParams run_RF_workflow
#' @param data_final_pred A final prediction dataset
#' @param resp The quoted name of the response variable
#'
#' @export
#'
#' @examples
#' dat <- fill_PA_area(data_rangers, coef = 0.5)
#' d <- build_final_pred_data(data = dat,
#'                            formula = staff_rangers_log ~ GDP_2019,
#'                            survey = "complete_known",
#'                            spatial = TRUE,
#'                            outliers = "ATA")
#' d$data_predictable$staff_rangers_log_predicted <- 0
#' extract_PA_areas(d, "staff_rangers_log", dat)
#'
extract_PA_areas <- function(data_final_pred, resp, data) {

  ## we add back the original info about PA_area from before imputation:
  data %>%
    dplyr::select(.data$countryname_eng, .data$PA_area_surveyed_notfilled, .data$PA_area_unsurveyed_notfilled) -> original_PA_info

  data_final_pred$data_known %>%
    dplyr::left_join(original_PA_info, by = "countryname_eng") -> data_final_pred$data_known

  res_a <- dplyr::bind_cols(data_final_pred$data_predictable[, c("countryname_eng", paste0(resp, "_predicted"))], type = "predicted")
  colnames(res_a)[2] <- resp
  res_b <- dplyr::bind_cols(data_final_pred$data_not_predictable[, "countryname_eng"], staff_log = NA, type = "unknown")
  colnames(res_b)[colnames(res_b) == "staff_log"] <- resp
  res_c <- dplyr::bind_cols(data_final_pred$data_known[, c("countryname_eng", resp)], type = "known")
  res <- dplyr::bind_rows(res_a, res_b, res_c)

  data_final_pred$data_known %>%
    dplyr::select(.data$countryname_eng, PA_area_known = .data$PA_area_surveyed_notfilled) %>%
    dplyr::right_join(res, by = "countryname_eng") -> res

  data_final_pred$data_known %>%
    dplyr::select(.data$countryname_eng, PA_area_imputed = .data$PA_area_unsurveyed_notfilled) %>%
    dplyr::right_join(res, by = "countryname_eng") -> res

  data_final_pred$data_predictable %>%
    dplyr::select(.data$countryname_eng, PA_area_predicted = .data$PA_area_surveyed) %>%
    dplyr::right_join(res, by = "countryname_eng") -> res

  data_final_pred$data_not_predictable %>%
    dplyr::select(.data$countryname_eng, PA_area_unknown = .data$PA_area_surveyed) %>%
    dplyr::right_join(res, by = "countryname_eng") -> res


  res %>%
    dplyr::mutate(dplyr::across(tidyselect::starts_with("PA_area"), tidyr::replace_na, replace = 0))
}


#' Internal function used to prepare the grid of the parameters to try when finetuning the RF
#'
#' This function is called by [`run_RF_workflow()`].
#'
#' @param grid_type The type of grid to use for finetuning the RF ("fine", the default, or "coarse")
#'
#' @export
#'
#' @examples
#' nrow(prepare_grid_finetuning())
#' nrow(prepare_grid_finetuning(grid_type = "coarse"))
#'
prepare_grid_finetuning <- function(grid_type = "fine") {
  if (grid_type == "fine") {
    param_grid_for_finetuning <- expand.grid(replace = c(TRUE, FALSE),
                                             splitrule = c("variance", "extratrees"),
                                             min.node.size = 1:10,
                                             sample.fraction = c(0.632, 1),
                                             mtry = c(function(p) 1, function(p) floor(p/3), function(p) p),
                                             stringsAsFactors = FALSE) # important!
  } else if (grid_type == "coarse") {
    param_grid_for_finetuning <- expand.grid(replace = c(TRUE, FALSE),
                                         splitrule = c("variance", "extratrees"),
                                         min.node.size = c(1, 10),
                                         sample.fraction = c(0.632, 1),
                                         mtry = c(function(p) 1, function(p) p),
                                         stringsAsFactors = FALSE) # important!
  } else {
    stop("grid_type unknown!")
  }
  param_grid_for_finetuning
}


#' Add continent to a table
#'
#' @param tbl The table for which continents must be added
#' @param data The dataset containing the info on continents
#' @param levels a list of levels if the variable continent must be turned into a factor
#'
#' @export
#'
#' @examples
#' add_continents(data.frame(countryname_eng = c("Turkey", "France")), data_rangers)
#'
add_continents <- function(tbl, data, levels = NULL) {
  data[, c("countryname_eng", "country_UN_continent")] %>%
    dplyr::right_join(tbl, by = "countryname_eng") %>%
    dplyr::rename(continent = .data$country_UN_continent) -> tbl

  if (!is.null(levels)) {
    tbl %>%
      dplyr::mutate(continent = factor(.data$continent, levels = levels)) -> tbl
  }

  tbl
}


#' Extract polygon at a given location from a set of polygons
#'
#' This functions is a helper function used during the data preparation to extract polygons nested within a
#' multipolygon sf geometry. We use this because our data set makes distinction between territories that
#' are not down in the dataset we use to retrieve the polygons.
#'
#' @param data A dataset containing at least the columns `geometry` and `name` or `rne_name`
#' @param countryname_eng The name of a country in English
#' @param lon A vector of 2 longitude coordinates defining a box containing the polygons to extract
#' @param lat A vector of 2 latitude coordinates defining a box containing the polygons to extract
#'
#' @export
#'
#' @examples
#' ## Extracting the polygon for French Guiana
#' if (requireNamespace("rnaturalearth")) {
#'   world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
#'   FrenchGuiana <- extract_polygon(data = world,
#'                                   countryname_eng = "France",
#'                                   lon = c(-55, -50), lat = c(6, 2))
#'   plot(FrenchGuiana)
#' }
#'
extract_polygon <- function(data, countryname_eng, lon, lat) {
  if (!requireNamespace("sf", quietly = TRUE)) stop("You need to install the package sf for this function to run")

  if (!any(colnames(data) %in% "rne_name")) {
    data$rne_name <- data$name
  }
  data$geometry[data$rne_name == countryname_eng] |>
    sf::st_cast("POLYGON") -> polys
  selection_box <-  sf::st_polygon(list(matrix(c(lon[1], lat[1], lon[2], lat[1], lon[2], lat[2], lon[1], lat[2], lon[1], lat[1]), ncol = 2L, byrow = TRUE)))
  selection <- sapply(polys, \(x) as.numeric(sf::st_intersects(x, selection_box)) == 1)
  selection <- which(!is.na(selection))
  sf::st_combine(polys[selection])
}

#' Remove polygon at a given location from a set of polygons
#'
#' This functions is a helper function to be used in combination to [`extract_polygon()`].
#' It removes a set of polygons nested within a multipolygon sf geometry.
#'
#' @inheritParams extract_polygon
#'
#' @export
#'
#' @examples
#' ## Clipping out the polygon for French Guiana
#' if (requireNamespace("rnaturalearth")) {
#'   world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
#'   France <- world$geometry[world$name == "France"]
#'   plot(France)
#'   France2 <- clipout_polygon(data = world,
#'                              countryname_eng = "France",
#'                              lon = c(-55, -50), lat = c(6, 2))
#'   plot(France2)
#' }
#'
clipout_polygon <- function(data, countryname_eng, lon, lat) {

  if (!requireNamespace("sf", quietly = TRUE)) stop("You need to install the package sf for this function to run")

  data2 <- data
  if (!any(colnames(data2) %in% "rne_name")) {
    data2$rne_name <- data2$name
  }
  data2$geometry[data2$rne_name == countryname_eng] |>
    sf::st_cast("POLYGON") -> raw_polygons
  polygon_to_remove <- extract_polygon(data = data2, countryname_eng = countryname_eng, lon = lon, lat = lat)
  sf::st_combine(sf::st_difference(raw_polygons, polygon_to_remove))
}


#' Mixed case capitalizing
#'
#' This function turn the first letter of each word into a capital letter.
#'
#' @param x a string of characters
#'
#' @return a string of characters
#' @export
#'
#' @examples
#' totitle("once upon a time")
#'
totitle <- function(x) {

  simpleCap <- function(x) { ## from ?toupper
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
  }

  sapply(x, simpleCap)
}
