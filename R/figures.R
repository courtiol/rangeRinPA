
#' Plot the map of the sampling design
#'
#' @param data the complete dataset
#' @param proj the projection to be used (e.g. "+proj=moll" - the default)
#'
#' @export
#'
#' @examples
#' plot_map_sampling(data_rangers)
#'
#' if(require(patchwork)) {
#'   plot_map_sampling(data_rangers, proj = "+proj=moll") +
#'   plot_map_sampling(data_rangers, proj = "+proj=robin" ) +
#'   plot_map_sampling(data_rangers, proj = "+proj=wintri") +
#'   plot_map_sampling(data_rangers, proj = "+proj=natearth2") +
#'   plot_map_sampling(data_rangers, proj = "+proj=mbt_fps") +
#'   plot_map_sampling(data_rangers, proj = "+proj=hammer") +
#'   plot_map_sampling(data_rangers, proj = "+proj=wag1") +
#'   plot_map_sampling(data_rangers, proj = "+proj=goode") +
#'   plot_map_sampling(data_rangers, proj = "+proj=eqearth") +
#'   plot_map_sampling(data_rangers, proj = "+proj=eck4") +
#'   plot_map_sampling(data_rangers, proj = "+proj=boggs") +
#'   plot_map_sampling(data_rangers, proj = "+proj=aitoff")
#' }
#'
plot_map_sampling <- function(data, proj = "+proj=moll") {

  ## obtain sf all all countries:
  world_sf <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

  ## add prefix to variable others than geometry:
  i <- which(names(world_sf) != "geometry")
  names(world_sf)[i] <- paste0("rne_", names(world_sf)[i])

  ## manual fix of some iso codes discrepancies:
  world_sf$rne_iso_a3[world_sf$rne_name == "Kosovo"] <- "KOS"

  ## check locations not found in map (depend on scale defined above):
  data |>
    dplyr::anti_join(world_sf, by = c(countryname_iso = "rne_iso_a3")) |>
    dplyr::pull(.data$countryname_eng) -> missing1

  world_sf |>
    dplyr::anti_join(data_rangers, by = c(rne_iso_a3 = "countryname_iso")) |>
    dplyr::pull(.data$rne_name) -> missing2

  missing <- c(missing1, missing2)

  if (length(missing1) > 0) {
    cat("Here are the countries/territories for which polygons are not found (some or all are merged with their country of belonging):\n")
    print(missing1)
  }

  if (length(missing1) > 0) {
    cat("\n Here are the countries/territories for which data are not found:\n")
    print(missing2)
  }

  ## only keep locations found in map:
  data |>
    dplyr::right_join(world_sf, by = c(countryname_iso = "rne_iso_a3")) |>
    sf::st_as_sf() -> world_rangers

  world_rangers |>
    dplyr::mutate(PA_area_surveyed = ifelse(.data$PA_area_surveyed == 0, NA, .data$PA_area_surveyed)) -> world_rangers

  ## applying projection:
  world_rangers |>
    sf::st_transform(crs = proj) -> world_rangers

  ## computing variable of interest:
  world_rangers |>
    dplyr::mutate(sampled_coverage = 100*.data$PA_area_surveyed / (.data$PA_area_surveyed + .data$PA_area_unsurveyed),
                  sampled_coverage2 = cut(.data$sampled_coverage, breaks = seq(0, 100, 20),
                                          labels = c(paste0(floor(min(.data$sampled_coverage, na.rm = TRUE)), "-20"), "20-40", "40-60", "60-80", "80-100")),
                  sampled_coverage2 = forcats::fct_rev(.data$sampled_coverage2)) -> world_rangers

  ## plotting:
  ggplot2::ggplot() +
    ggplot2::geom_sf(mapping = ggplot2::aes(fill = .data$sampled_coverage2),
                     data = world_rangers, colour = "black", size = 0.05) +
    #ggplot2::scale_fill_fermenter(palette = 2, ## for use with sampled_coverage not sampled_coverage2
    #                              direction = 1,
    #                              breaks = seq(0, 80, 20),
    #                              na.value = "orange",
    #                              guide = ggplot2::guide_colorsteps(title = "Protected areas\n sampled (%)",
    #                                                                title.vjust = 1, barwidth = 1.5,
    #                                                                label.theme = ggplot2::element_text(angle = 0),
    #                                                                label.hjust = 0, label.vjust = 0.5)) +
    ggplot2::scale_fill_manual(values = scales::brewer_pal(type = "seq", palette = 2, direction = -1)(length(unique(world_rangers$sampled_coverage2))),
                               labels = c(levels(world_rangers$sampled_coverage2), "0 (not sampled)"),
                               na.value = "orange",
                               guide = ggplot2::guide_legend(title = "Protected areas\n sampled (%)")) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "left",
                   #panel.grid = ggplot2::element_line(colour = "GREY", size = 0.3),
                   panel.grid = ggplot2::element_blank()) +
    ggplot2::coord_sf(expand = FALSE, crs = proj)

  }
