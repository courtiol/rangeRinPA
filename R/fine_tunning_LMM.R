#' Fine tune LMM
#'
#' @inheritParams validate_LMM
#'
#' @export
#'
finetune_LMM <- function(formula, data, spatial, rep = 10, Ncpu = 1, seed = 123) {
  ML_res <- validate_LMM(formula,
                         data = data,
                         rep = rep, Ncpu = Ncpu,
                         spatial = spatial,
                         seed = seed,
                         method = "ML")
  REML_res <- validate_LMM(formula,
                           data = data,
                           rep = rep, Ncpu = Ncpu,
                           spatial = spatial,
                           seed = seed,
                           method = "REML")

  res_mean <- rbind(cbind(method = "ML", aggregate_metrics(ML_res)),
               cbind(method = "REML", aggregate_metrics(REML_res)))

  res_sd <- rbind(cbind(method = "ML", aggregate_metrics(ML_res, fn = stats::sd)),
               cbind(method = "REML", aggregate_metrics(REML_res, fn = stats::sd)))

  list(result_mean = res_mean, result_sd = res_sd, best_method = ifelse(which.min(res_mean$RMSE) == 1, "ML", "REML"))
}
