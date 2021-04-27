#' Fine tune the parameters of a random forest
#'
#' @inheritParams validate_RF
#' @inheritParams aggregate_metrics
#' @param values_to_try the parameter value to try
#' @param param_to_tune the quoted name of the parameter to study
#' @param formula the formula for the forest
#'
#' @return a list with a data frame and a plot
#' @export
#'
#'
fine_tune_RF <- function(values_to_try, param_to_tune, formula, data, rep = 10, Ncpu = 1, target = "staff_rangers_log", fn = mean, ...) {

  call_CV  <- paste("lapply(values_to_try, function(i) {
                    validate_RF(formula = formula, data = data, rep = rep, Ncpu = Ncpu, target = target, method = 'CV'," , deparse1(substitute(param_to_tune)), " = i, ...)
                    })")

  test_CV <- eval(parse(text = call_CV))

  output_point <- cbind(param_to_tune = param_to_tune, value = values_to_try, do.call("rbind", lapply(test_CV, aggregate_metrics, fn = fn)))
  se <- function(x) sd(x)/sqrt(length(x))
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
    ggplot2::geom_line() +
    ggplot2::geom_errorbar(width = 0.1) +
    ggplot2::scale_x_continuous(breaks = values_to_try) +
    ggplot2::labs(y = "Metric value (+/- SE)",
                  x = "Parameter values",
                  title = paste0("Effect of ", deparse1(substitute(param_to_tune)))) +
    ggplot2::facet_wrap(~ .data$metric, scales = "free") +
    ggplot2::theme_minimal() -> plot

  list(output = output_long, plot = plot)
}
