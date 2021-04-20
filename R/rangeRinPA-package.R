#' Estimate and Study the Total Number of People Working in Protected Areas
#'
#' The goal of rangeR is to provide data about the total number of people working in Protected Areas (PAs) on the planet, as well as identifying what contributes to variation in the workforce between PAs.
#'
#' In the examples below, we provide the workflow leading the results presented
#' in the paper.
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
#' ######################
#' ## Loading the data ##
#' ######################
#'
#' # data_rangers <- fetch_data_rangers() # to download data instead of loading them
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
#' world_sf <- ne_countries(scale = "medium", returnclass = "sf")
#'
#' i <- which(names(world_sf) != "geometry")
#' names(world_sf)[i] <- paste0("rne_", names(world_sf)[i])
#'
#' ## check locations not found in map (depend on scale defined above):
#' data_rangers %>%
#'   anti_join(world_sf, by = c(countryname_iso = "rne_iso_a3")) %>%
#'   pull(countryname_eng)
#'
#' ## only keep locations found in map:
#' data_rangers %>%
#'   right_join(world_sf, by = c(countryname_iso = "rne_iso_a3")) %>%
#'   st_as_sf() -> world_rangers
#'
#' ## applying projection:
#' world_rangers %>%
#'   st_transform(crs = "+proj=moll") -> world_rangers
#'
#' ggplot() +
#'    geom_sf(mapping = aes(fill = area_PA_total / staff_total),
#'            data = world_rangers, colour = "white", size = 0.1) +
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
