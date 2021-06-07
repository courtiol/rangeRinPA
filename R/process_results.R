#' Extract and format the results produced by the worflow
#'
#' @param what an object produced by [`run_LMM_workflow()`] or [`run_RF_workflow()`]
#' @param who the type of staff ("rangers", "others" or "all")
#' @param type the type of workflow ("LMM" or "RF")
#' @param list_results_LMM a list of objects produced by [`run_LMM_workflow()`]
#' @param list_results_RF a list of objects produced by [`run_RF_workflow()`]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' res <- extract_results(
#'        list_results_LMM = list(LMM_000, LMM_025, LMM_050, LMM_075, LMM_100),
#'        list_results_RF  = list(RF_000, RF_025, RF_050, RF_075, RF_100))
#' }
#'
extract_results <- function(list_results_LMM = list(), list_results_RF = list()) {
  d_LMM <- d_RF <- data.frame()

  if (length(list_results_LMM) > 0) {
    rangers_list_LMM <- lapply(list_results_LMM, function(x) extract_results_internal(what = x, who = "rangers", type = "LMM"))
    others_list_LMM  <- lapply(list_results_LMM, function(x) extract_results_internal(what = x, who = "others", type = "LMM"))
    all_list_LMM     <- lapply(list_results_LMM, function(x) extract_results_internal(what = x, who = "all", type = "LMM"))
    rbind(cbind(who = "rangers", as.data.frame(do.call("rbind", rangers_list_LMM))),
          cbind(who = "others",  as.data.frame(do.call("rbind", others_list_LMM))),
          cbind(who = "all",     as.data.frame(do.call("rbind", all_list_LMM)))) -> d_LMM
  }
  if (length(list_results_RF) > 0) {
    rangers_list_RF <- lapply(list_results_RF, function(x) extract_results_internal(what = x, who = "rangers", type = "RF"))
    others_list_RF  <- lapply(list_results_RF, function(x) extract_results_internal(what = x, who = "others", type = "RF"))
    all_list_RF     <- lapply(list_results_RF, function(x) extract_results_internal(what = x, who = "all", type = "RF"))
    rbind(cbind(who = "rangers", as.data.frame(do.call("rbind", rangers_list_RF))),
          cbind(who = "others",  as.data.frame(do.call("rbind", others_list_RF))),
          cbind(who = "all",     as.data.frame(do.call("rbind", all_list_RF)))) -> d_RF
  }
  if (ncol(d_LMM) > 0 && ncol(d_RF) > 0) {
    d <- rbind(d_LMM, d_RF)
  } else if (ncol(d_LMM) > 0) {
    d <- d_LMM
  } else if (ncol(d_RF) > 0) {
    d <- d_RF
  }

  d$who <- as.factor(d$who)
  tibble::as_tibble(d)
}

#' @describeIn extract_results an internal funciton fetching the results
#' @export
#'
extract_results_internal <- function(what, who, type) {
  data.frame(type = type,
             coef = what$meta$coef_population,
             rerank = what$meta$rerank,
             Ncpu = what$meta$Ncpu,
             run_time = what$meta$duration_h,
             point_pred = what[[who]]$tally_total,
             lwr = what[[who]]$lwr[[1]],
             upr = what[[who]]$upr[[1]],
             coverage = with(what[[who]], (PA_area_obs_or_imputed + PA_area_predict) / (PA_area_obs_or_imputed + PA_area_predict + PA_area_no_predict)),
             formula = what[[who]]$selected_formula,
             spatial = what[[who]]$selected_spatial
  )
}


