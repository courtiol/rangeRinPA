
#' Compute accuracy metrics on test set for LMM
#'
#' @param formula a model formula used to fit the data
#' @param data the full dataset
#' @param rep the number of cross validation replicates (default = 10)
#' @param Ncpu the number of CPU cores to be used (default = 1)
#' @param target the quoted name of the response variable (default = "staff_rangers_log")
#' @param spatial either FALSE (default) or "Matern"
#' @param seed the seed used to control the reproducibility of the cross validation
#' @param ... additional parameters to be passed to [`spaMM::fitme()`]
#'
#' @return a tibble with the CV replicates in row and accuracy metrics in columns
#' @export
#'
#' @seealso [`compute_metrics()`] for details on the function computing the values outputted here
#'
#' @examples
#' crossvalidate_LMM(staff_rangers_log ~ PA_area_log, data = data_test)
#'
crossvalidate_LMM <- function(formula, data, rep = 10, Ncpu = 1, target = "staff_rangers_log", spatial = FALSE, seed = 123, ...) {
  if (spatial == "Matern") {
    formula <- stats::update.formula(formula, . ~ . + Matern(1 |long + lat))
  } else stopifnot(!spatial)
  metrics <- parallel::mclapply(seq_len(rep), function(i) {
    data_list <- prepare_data(formula = formula, data = data, test_prop = 0.1,
                              keep.var =  c("long", "lat"), seed = seed + i)
    newfit <- spaMM::fitme(formula = formula, data = data_list$data_train, ...)
    predicted <- spaMM::predict.HLfit(newfit, newdata = data_list$data_test)[, 1]
    observed <- data_list$data_test[, target, drop = TRUE]
    inv.dist <- NULL
    if (all(c("long", "lat") %in% colnames(data_list$data_test))) {
      inv.dist <- compute_distance(long = data_list$data_test$long,
                                   lat = data_list$data_test$lat,
                                   inv = TRUE)
    }
    compute_metrics(predicted, observed, inv.dist = inv.dist)
  }, mc.cores = Ncpu)
  cbind(CV_test = seq_len(rep), as.data.frame(do.call("rbind", metrics)))
}

