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

#' Perform feature selection on LMM
#'
#' @param full_fit a full fitted model
#' @param metric the metric used for computing prediction accuracy (see [`compute_metrics()`])
#' @param minimise whether the metric should be minimise (TRUE, default) or maximise (FALSE)
#' @inheritParams validate_LMM
#'
#' @return a list with metrics and best formula
#' @export
#'
feature_selection_LMM <- function(full_fit, data, metric = "RMSE", minimise = TRUE, rep = 10, Ncpu = 1, target = "staff_rangers_log", spatial = "Matern", seed = 123, ...) {
  test_k <- function(fit, k) {
    f <- formula_top_pred_LMM(fit, k = k)
    v <- validate_LMM(f, data = data, rep = rep, Ncpu = Ncpu, target = target, spatial = spatial, seed = seed, ...)
    aggregate_metrics(v)
  }
  k_to_do <- nrow(rank_predictors_LMM(full_fit)):0
  fit <- full_fit
  res <- list()
  for (i in seq_along(k_to_do)) {
    k <- k_to_do[i]
    res[[i]] <- test_k(fit, k = k)
    new_formula <- formula_top_pred_LMM(fit, k = k)
    if (spatial == "Matern") {
      new_formula <- stats::as.formula(paste(as.character(new_formula)[2], "~", as.character(new_formula)[3], "+ Matern(1|long + lat)"))
    }
    res[[i]]$formula <- deparse(new_formula, width.cutoff = 500)
    fit <- stats::update(fit, new_formula)
  }
  all_res <- cbind(k = k_to_do, as.data.frame(do.call("rbind", res)))
  if (minimise) {
    best_k <- all_res$k[which.min(all_res[, metric])]
    best_metric <- min(all_res[, metric])
    decreasing <- FALSE
  } else {
    best_k <- all_res$k[which.max(all_res[, metric])]
    best_metric <- max(all_res[, metric])
    decreasing <- TRUE
  }
  best_form <- formula_top_pred_LMM(full_fit, k = best_k)
  all_res <- all_res[order(all_res[, metric], decreasing = decreasing), ]
  rownames(all_res) <- NULL
  list(results = all_res, best_formula = best_form, best_metric = best_metric)
}
