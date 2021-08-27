
#' Compute accuracy metrics on test set for LMM
#'
#' @param formula a model formula used to fit the data
#' @param data the full dataset
#' @param rep the number of cross validation replicates (default = 10)
#' @param Ncpu the number of CPU cores to be used (default = 1)
#' @param spatial either FALSE (default) or TRUE
#' @param seed the seed used to control the reproducibility of the cross validation
#' @param return_fit whether to return the fit on all the data as attribute (default = FALSE)
#' @param ... additional parameters to be passed to [`spaMM::fitme()`]
#'
#' @return a tibble with the CV replicates in row and accuracy metrics in columns
#' @export
#'
#' @seealso [`compute_metrics()`] for details on the function computing the values outputted here
#'
#' @examples
#' validate_LMM(staff_rangers_log ~ PA_area_log, data = data_test)
#'
validate_LMM <- function(formula, data, rep = 10, Ncpu = 1, spatial = FALSE, seed = 123, return_fit = TRUE, ...) {

  target <- as.character(formula[[2]])

  if (spatial) {
    formula <- stats::update.formula(formula, . ~ . + Matern(1|long + lat))
  } else stopifnot(!spatial)

  metrics <- parallel::mclapply(seq_len(rep), function(i) {
    data_list <- prepare_data(formula = formula, data = data, test_prop = 0.2, # 20% out 80% in
                              keep.var =  c("long", "lat"), seed = seed + i)
    newfit <- spaMM::fitme(formula = formula, data = data_list$data_train, ...)
    predicted <- spaMM::predict.HLfit(newfit, newdata = data_list$data_test,
                                      control = list(fix_predVar = FALSE))[, 1]
    observed <- data_list$data_test[, target, drop = TRUE]
    inv.dist <- NULL
    if (all(c("long", "lat") %in% colnames(data_list$data_test))) {
      inv.dist <- compute_distance(long = data_list$data_test$long,
                                   lat = data_list$data_test$lat,
                                   inv = TRUE)
    }
    compute_metrics(predicted, observed, inv.dist = inv.dist)
  }, mc.cores = Ncpu)
  out <- cbind(CV_test = seq_len(rep), as.data.frame(do.call("rbind", metrics)))
  if (return_fit) {
    fit_fulldata <- spaMM::fitme(formula = formula, data = data, ...)
    attr(out, "fit_fulldata") <- fit_fulldata
  }
  out
}


#' Compute accuracy metrics on test set for RF
#'
#' @inheritParams validate_LMM
#' @param spatial either FALSE (default), or TRUE (to use matrix of distances as predictor)
#' @param method either "CV" for cross-validation or "OOB" for directly using the out-of-bag observations generated when growing the forest
#' @param ... additional parameters to be passed to [`ranger::ranger()`]
#'
#' @return a tibble with the CV/OOB replicates in row and accuracy metrics in columns
#' @export
#'
#' @seealso [`compute_metrics()`] for details on the function computing the values outputted here
#'
#' @examples
#' validate_RF(staff_rangers_log ~ PA_area_log, data = data_test, method = "CV")
#' validate_RF(staff_rangers_log ~ PA_area_log, data = data_test,
#'                  rep = 1, num.trees = 10, method = "OOB")
#'
#'
validate_RF <- function(formula, data, rep = 10, Ncpu = 1, spatial = FALSE, seed = 123, method = "CV", return_fit = TRUE, ...) {

  target <- as.character(formula[[2]])

  if (spatial) { ## add distance to each location as predictors
      data <- cbind(data, compute_distance(long = data$long, lat = data$lat))
      dist_vars <- colnames(data)[grep("loc_", colnames(data))]
      formula <- stats::as.formula(paste(formula[[2]], paste(c(formula[[3]], dist_vars), collapse = "+"), sep = " ~ "))
  } else if (spatial != FALSE) {
    stop("Spatial method not found")
  }
  if (method == "CV") {
    metrics <- parallel::mclapply(seq_len(rep), function(i) {
      data_list <- prepare_data(formula = formula, data = data, test_prop = 0.2, # 20% out 80% in
                                keep.var =  c("long", "lat"), seed = seed + i)
      newfit <- ranger::ranger(formula = formula, data = data_list$data_train, num.threads = 1, verbose = FALSE, ...)
      predicted <- stats::predict(newfit, data = data_list$data_test, num.threads = 1)$predictions
      observed <- data_list$data_test[, target, drop = TRUE]
      inv.dist <- NULL
      if (all(c("long", "lat") %in% colnames(data_list$data_test))) {
        inv.dist <- compute_distance(long = data_list$data_test$long,
                                     lat = data_list$data_test$lat,
                                     inv = TRUE)
      }
      compute_metrics(predicted, observed, inv.dist = inv.dist)
    }, mc.cores = Ncpu)
    out <- cbind(CV_test = seq_len(rep), as.data.frame(do.call("rbind", metrics)))
    } else if (method == "OOB") {
      if (rep > 1) {
        warning("Argument rep ignore when method = 'OOB', you can influence repetitions using the argument num.trees passed to ranger() instead")
      }
      newfit <- ranger::ranger(formula = formula, data = data, num.threads = Ncpu, keep.inbag = TRUE, seed = seed, verbose = FALSE, ...)
      inbag <- do.call(cbind, newfit$inbag.counts)
      preds <- stats::predict(newfit, data = data, predict.all = TRUE, num.threads = Ncpu)$predictions
      preds <- preds * ifelse(inbag == 0, NA, 1)
      inv.dist_full <- NULL
      if (all(c("long", "lat") %in% colnames(data))) {
        inv.dist_full <- compute_distance(long = data$long,
                                          lat = data$lat,
                                          inv = TRUE)
      }
      metrics <- lapply(seq_len(ncol(preds)), function(i) {
        predicted <- preds[, i, drop = TRUE][!is.na(preds[, i])]
        observed <- data[!is.na(preds[, i]), target, drop = TRUE]
        inv.dist <- NULL
        if (all(c("long", "lat") %in% colnames(data))) {
          inv.dist <- inv.dist_full[!is.na(preds[, i]), !is.na(preds[, i])]
        }
        compute_metrics(pred = predicted, obs = observed, inv.dist = inv.dist)
      })
      out <- cbind(OOB_test = seq_len(ncol(preds)), as.data.frame(do.call("rbind", metrics)))
    } else stop("method unknown")

  if (return_fit) {
    if (method == "OOB") {
      fit_fulldata <- newfit
    } else if (method == "CV") {
      fit_fulldata <- ranger::ranger(formula = formula, data = data, num.threads = Ncpu, keep.inbag = TRUE, seed = seed, verbose = FALSE, ...)
    }
    attr(out, "fit_fulldata") <- fit_fulldata
  }
  out
}
