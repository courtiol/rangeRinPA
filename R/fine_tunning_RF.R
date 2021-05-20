#' Fine tune the parameters of a random forest
#'
#' @inheritParams validate_RF
#' @inheritParams aggregate_metrics
#' @inheritParams feature_selection_RF
#' @param values_to_try the parameter value to try
#' @param param_to_tune the quoted name of the parameter to study
#' @param formula the formula for the forest
#'
#' @return a list with a data frame and a plot
#' @export
#'
#'
finetune_RF <- function(values_to_try, param_to_tune, formula, data, rep = 10, Ncpu = 1, target = "staff_rangers_log", fn = mean, ...) {

  call_CV  <- paste("lapply(values_to_try, function(i) {
                    validate_RF(formula = formula, data = data, rep = rep, Ncpu = Ncpu, method = 'CV'," , deparse1(substitute(param_to_tune)), " = i, ...)
                    })")

  test_CV <- eval(parse(text = call_CV))

  output_point <- cbind(param_to_tune = param_to_tune, value = values_to_try, do.call("rbind", lapply(test_CV, aggregate_metrics, fn = fn)))
  se <- function(x) stats::sd(x)/sqrt(length(x))
  output_SE <- cbind(param_to_tune = param_to_tune, value = values_to_try, do.call("rbind", lapply(test_CV, aggregate_metrics, fn = se)))

  output_point %>%
    tidyr::pivot_longer(cols = c(-.data$param_to_tune, -.data$value, -.data$type, -.data$rep), values_to = "metric_value", names_to = "metric") %>%
    dplyr::mutate(metric = forcats::fct_inorder(.data$metric)) -> output_point_long

  output_SE %>%
    tidyr::pivot_longer(cols = c(-.data$param_to_tune, -.data$value, -.data$type, -.data$rep), values_to = "metric_value_se", names_to = "metric") %>%
    dplyr::mutate(metric = forcats::fct_inorder(.data$metric)) -> output_SE_long

  output_long <- dplyr::full_join(output_point_long, output_SE_long, by = c("param_to_tune", "value", "type", "rep", "metric"))

  ggplot2::ggplot(output_long) +
    ggplot2::aes(y = .data$metric_value, x = .data$value, ymin = .data$metric_value - .data$metric_value_se, ymax = .data$metric_value + .data$metric_value_se) +
    ggplot2::geom_errorbar(width = 0.25*((max(values_to_try) - min(values_to_try)) / length(values_to_try)), colour = "lightgrey") +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous(breaks = values_to_try) +
    ggplot2::labs(y = "Metric value (+/- SE)",
                  x = "Parameter values",
                  title = paste0("Effect of ", deparse1(substitute(param_to_tune)))) +
    ggplot2::facet_wrap(~ .data$metric, scales = "free") +
    ggplot2::theme_minimal() -> plot

  list(output = output_long, plot = plot)
}


#' @describeIn finetune_RF Fine tune the parameters of a random forest all at once
#' @export
#' @param grid a data frame defining the combination of parameters to try
#'
finetune_RF_grid <- function(grid, formula, data, rep = 10, Ncpu = 1, fn = mean, ...) {

  list_test <- list()
  for (i in seq_len(nrow(grid))) {
    cat("Running combination of parameters ", i, "/", nrow(grid), "\n")
    grid_line <- grid[i, ]
    if (any("mtry" %in% names(grid_line))) {
      mtry <- grid_line$mtry[[1]]
      settings <- paste(paste(names(grid_line[names(grid_line) != "mtry"]),
                              grid_line[names(grid_line) != "mtry"],
                              sep = " = "), collapse = ", ")
      settings <- paste(settings, ", mtry =", paste(deparse(mtry), collapse = ""))
    } else {
      settings <- paste(paste(names(grid_line), grid_line, sep = " = "), collapse = ", ")
    }
    if (any("splitrule" %in% names(grid_line))) {
      settings <- sub(pattern = "= variance", replacement = "= 'variance'", x = settings)
      settings <- sub(pattern = "= extratrees", replacement = "= 'extratrees'", x = settings)
    }
    call_CV  <- paste("validate_RF(formula = formula, data = data, rep = rep, Ncpu = Ncpu, method = 'CV',", settings, ", ...)")
    list_test[[i]] <- eval(parse(text = call_CV))
  }
  output_point <- cbind(grid, do.call("rbind", lapply(list_test, aggregate_metrics, fn = fn)))
  se <- function(x) stats::sd(x)/sqrt(length(x))
  output_SE <- cbind(grid, do.call("rbind", lapply(list_test, aggregate_metrics, fn = se)))
  output <- list(output_point, SE = output_SE)
  names(output)[[1]] <- deparse1(substitute(fn))
  output
}
