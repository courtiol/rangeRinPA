#' Estimate and Study the Total Number of People Working in Protected Areas
#'
#' The goal of rangeR is to provide data about the total number of people working in Protected Areas (PAs) on the planet, as well as identifying what contributes to variation in the workforce between PAs.
#'
#' In the examples below, we provide the workflow leading the results presented
#' in the paper.
#'
#' @name rangeR-package
#' @aliases rangeR-package rangeR
#' @docType package
#'
#' @references
#' TODO
#'
#' @keywords package
#' @examples
#' \dontrun{
#'
#' ######################
#' ## Loading the data ##
#' ######################
#'
#' data_rangers <- fetch_data_rangers()
#'
#' if (require("skimr")) {
#'   skim(data_rangers)
#' }
#'
#' }
