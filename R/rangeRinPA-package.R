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
#' library(rnaturalearth)
#' library(dplyr)
#' library(ggplot2)
#' library(sf)
#'
#' ggplot(data_rangers) +
#'    geom_sf(mapping = aes(fill = area_PA_total / staff_total, geometry = geometry),
#'           colour = "white", size = 0.1) +
#'    scale_fill_fermenter(palette = 2,
#'                         breaks = c(0, 10, 100, 1000, 10000), trans = "log10",
#'                         guide = guide_colorsteps(title = expression(paste(km^{2}, "/staff")),
#'                                                  title.vjust = 1, barwidth = 20,
#'                                                  label.theme = element_text(angle = 0),
#'                                                  label.hjust = 0.5, label.vjust = 1)) +
#'    theme_minimal() +
#'    theme(legend.position = "bottom", panel.grid = element_line(colour = "black", size = 0.3),
#'          plot.title = element_text(size = 20, hjust = 0.5)) +
#'    labs(title = "Protected Area per Staff")
#' }
