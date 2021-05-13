#' Fine tune LMM
#'
#' @inheritParams validate_LMM
#'
#' @export
#'
finetune_LMM <- function(formula, data, rep = 10, Ncpu = 1, seed = 123) {
  ML_res <- validate_LMM(formula,
                         data = data,
                         rep = rep, Ncpu = Ncpu,
                         spatial = any(grepl("Matern", formula)),
                         target = as.character(formula)[2],
                         seed = seed,
                         method = "ML")
  REML_res <- validate_LMM(formula,
                           data = data,
                           rep = rep, Ncpu = Ncpu,
                           spatial = any(grepl("Matern", formula)),
                           target = as.character(formula)[2],
                           seed = seed,
                           method = "REML")
  res <- rbind(cbind(method = "ML", aggregate_metrics(ML_res)),
               cbind(method = "REML", aggregate_metrics(REML_res)))

  list(result = res, best_method = ifelse(which.min(res$RMSE) == 1, "ML", "REML"))
}
