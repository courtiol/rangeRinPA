#' Extract top predictors
#'
#' This function extract the top (fixed-effects) predictors from a RF based on their importance.
#'
#' @param fit a model fitted with [`ranger()`](ranger::ranger)
#' @param k the maximum number of predictors to return (default = NULL for no sub-selection)
#'
#' @return predictors with their importance
#' @export
#'
rank_predictors_RF <- function(fit, k = NULL) {
  preds <- sort(ranger::importance(fit), decreasing = TRUE)
  preds <- preds[!grepl(pattern = "loc_", names(preds))]
  if (is.null(k)) {
    k <- length(preds)
  }
  preds <- preds[seq_len(k)]
  d <- data.frame(predictor = names(preds), importance = preds)
  rownames(d) <- NULL
  d
}

#' Build formula from top predictors
#'
#' @inheritParams rank_predictors_RF
#' @param resp the name of the response variable
#'
#' @return a formula
#' @export
#'
formula_top_pred_RF <- function(fit, resp, k = NULL) {
  ranks <- rank_predictors_RF(fit = fit, k = k)
  preds <- paste(ranks$predictor, collapse = "+")
  if (preds == "") {
    preds <- "1"
  }
  stats::as.formula(paste(resp, "~ ", preds))
}

#' Perform feature selection on RFs
#'
#' @param full_fit a full fitted model
#' @param metric the metric used for computing prediction accuracy (see [`compute_metrics()`])
#' @param minimise whether the metric should be minimise (TRUE, default) or maximise (FALSE)
#' @inheritParams validate_RF
#' @name feature_selection_RF
#' @aliases feature_selection_RF, feature_selection_RF_internal
#'
NULL

#' @describeIn feature_selection_RF wrapper function performing the feature selection on RFs with and without the spatial terms
#' @export
#'
feature_selection_RF <- function(full_fit, data, metric = "RMSE", minimise = TRUE, rep = 10, Ncpu = 1, target = "staff_rangers_log", seed = 123, ...) {

  all_res_spatial <- feature_selection_RF_internal(full_fit = full_fit, data = data, rep = rep, Ncpu = Ncpu, target = target, spatial = TRUE, seed = seed, ...)
  all_res_spatial$spatial <- TRUE

  all_res_no_spatial <- feature_selection_RF_internal(full_fit = full_fit, data = data, rep = rep, Ncpu = Ncpu, target = target, spatial = FALSE, seed = seed, ...)
  all_res_no_spatial$spatial <- FALSE

  all_res <- rbind(all_res_spatial, all_res_no_spatial)
  if (minimise) {
    best_k <- all_res$k[which.min(all_res[, metric])]
    best_metric <- min(all_res[, metric])
    decreasing <- FALSE
    extreme <- min
  } else {
    best_k <- all_res$k[which.max(all_res[, metric])]
    best_metric <- max(all_res[, metric])
    decreasing <- TRUE
    extreme <- max
  }
  all_res <- all_res[order(all_res[, metric], decreasing = decreasing), ]
  rownames(all_res) <- NULL
  all_res[[paste0(metric, "_tol")]] <- 100*(abs(all_res[, metric] - extreme(all_res[, metric])))/extreme(all_res[, metric]) #as in caret::pickSizeTolerance
  tibble::as_tibble(all_res[, c("k", "spatial", metric, paste0(metric, "_tol"), "formula", "rep")])
}


#' @describeIn feature_selection_RF internal function performing the feature selection on RFs
#' @export
#'
feature_selection_RF_internal <- function(full_fit, data, rep = 10, Ncpu = 1, target = "staff_rangers_log", spatial = TRUE, seed = 123, ...) {
  k_to_do <- nrow(rank_predictors_RF(full_fit)):1L ## must stop at 1 not 0 since intercept only model not possible in ranger
  fit <- full_fit
  res <- list()
  for (i in seq_along(k_to_do)) {
    k <- k_to_do[i]
    new_formula <- formula_top_pred_RF(fit, resp = target, k = k)
    v <- validate_RF(new_formula, data = data, rep = rep, Ncpu = Ncpu, target = target, spatial = spatial, seed = seed, ...)
    res[[i]] <- aggregate_metrics(v)
    res[[i]]$formula <- deparse(new_formula, width.cutoff = 500)
    fit <- attr(v, "fit_fulldata")
  }
  cbind(k = k_to_do, as.data.frame(do.call("rbind", res)))
}


