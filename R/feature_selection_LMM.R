#' Extract top predictors
#'
#' This function extract the top (fixed-effects) predictors from a LMM based on their absolute t-value.
#'
#' @param fit a model fitted with [`fitme()`](spaMM::fitme)
#' @param k the maximum number of predictors to return (default = NULL for no sub-selection)
#'
#' @return predictors with their absolute t-values
#' @export
#'
rank_predictors_LMM <- function(fit, k = NULL) {
  preds <- sort(abs(spaMM::summary.HLfit(fit, verbose = FALSE)$beta_table[, 3]), decreasing = TRUE)
  preds <- preds[names(preds) != "(Intercept)"]
  if (is.null(k)) {
    k <- length(preds)
  }
  preds <- preds[seq_len(k)]
  d <- data.frame(predictor = names(preds), abs_t = preds)
  rownames(d) <- NULL
  d
}

#' Build formula from top predictors
#'
#' @inheritParams rank_predictors_LMM
#'
#' @return a formula
#' @export
#'
formula_top_pred_LMM <- function(fit, k = NULL) {
  ranks <- rank_predictors_LMM(fit = fit, k = k)
  resp <- stats::formula(fit)[[2]]
  preds <- paste(ranks$predictor, collapse = "+")
  if (preds == "") {
    preds <- "1"
  }
  stats::as.formula(paste(resp, "~ ", preds))
}

#' Perform feature selection on LMMs
#'
#' @param full_fit a full fitted model
#' @param rerank whether or not to recompute variable importance recursively during selection (default = TRUE)
#' @param metric the metric used for computing prediction accuracy (see [`compute_metrics()`])
#' @param minimise whether the metric should be minimise (TRUE, default) or maximise (FALSE)
#' @inheritParams validate_LMM
#' @name feature_selection_LMM
#' @aliases feature_selection_LMM, feature_selection_LMM_internal
#'
NULL

#' @describeIn feature_selection_LMM wrapper function performing the feature selection on LMMs with and without the Matern term
#' @export
#'
feature_selection_LMM <- function(full_fit, rerank = TRUE, metric = "RMSE", minimise = TRUE, rep = 10, Ncpu = 1, target = "staff_rangers_log", seed = 123, ...) {
  all_res_spatial <- feature_selection_LMM_internal(full_fit = full_fit, rerank = rerank, rep = rep, Ncpu = Ncpu, target = target, spatial = TRUE, seed = seed, ...)
  all_res_spatial$spatial <- TRUE
  data <- full_fit$data
  full_fit_no_spatial <- spaMM::update.HLfit(full_fit, . ~ . - Matern(1|long + lat), data = data)
  all_res_no_spatial <- feature_selection_LMM_internal(full_fit = full_fit_no_spatial, rerank = rerank, rep = rep, Ncpu = Ncpu, target = target, spatial = FALSE, seed = seed, ...)
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

#' @describeIn feature_selection_LMM internal function performing the feature selection on LMMs
#' @export
#'
feature_selection_LMM_internal <- function(full_fit, rerank = TRUE, rep = 10, Ncpu = 1, target = "staff_rangers_log", spatial = TRUE, seed = 123, ...) {
  data <- full_fit$data
  k_to_do <- nrow(rank_predictors_LMM(full_fit)):0L
  fit <- full_fit
  res <- list()
  for (i in seq_along(k_to_do)) {
    k <- k_to_do[i]
    if (rerank) {
      fit_for_selection <- fit
    } else {
      fit_for_selection <- full_fit
    }
    new_formula <- formula_top_pred_LMM(fit_for_selection, k = k)
    v <- validate_LMM(new_formula, data = data, rep = rep, Ncpu = Ncpu, target = target, spatial = spatial, seed = seed, ...)
    res[[i]] <- aggregate_metrics(v)
    res[[i]]$formula <- deparse(new_formula, width.cutoff = 500)
    fit <- attr(v, "fit_fulldata")
    ##TODO: extract and store AIC (marginal for LM, conditional for LMM)
  }
  cbind(k = k_to_do, as.data.frame(do.call("rbind", res)))
}
