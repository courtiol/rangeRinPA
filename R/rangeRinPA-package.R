#' Estimate and Study the Total Number of People Working in Protected Areas
#'
#' The goal of rangeRinPA is to provide data about the total number of people working in Protected Areas (PAs) on the planet, as well as identifying what contributes to variation in the workforce between PAs.
#'
#' In the examples below, we provide a simple example using the data contained in this package.
#'
#' The results of the paper are provided in R scripts using the functions from this package.
#'
#' @name rangeRinPA-package
#' @aliases rangeRinPA-package rangeR
#' @docType package
#'
#' @references
#' TODO
#'
#' @keywords package
#' @examples
#' \dontrun{
#'
#' if (require("skimr")) {
#'   skim(data_rangers)
#' }
#'
#'
#' readr::write_excel_csv(table_completeness(data_rangers),
#'                        file = "inst/extdata/tables/table_completeness.csv")
#'
#' plot_map_sampling(data_rangers)
#' ggplot2::ggsave(filename = "inst/extdata/figures/figure_sampling.pdf",
#'                 width = ggplot2::unit(15, "cm"))
#' ggplot2::ggsave(filename = "inst/extdata/figures/figure_sampling.png",
#'                 width = ggplot2::unit(15, "cm"))
#'
#' plot_map_reliability(data_rangers)
#' ggplot2::ggsave(filename = "inst/extdata/figures/figure_reliability.pdf",
#'                 width = ggplot2::unit(15, "cm"))
#' ggplot2::ggsave(filename = "inst/extdata/figures/figure_reliability.png",
#'                 width = ggplot2::unit(15, "cm"))
#'
#'
#'
#' }
