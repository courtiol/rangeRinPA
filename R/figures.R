
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
#'   plot_map_sampling(data_rangers, proj = "+proj=natearth2") +
#'   plot_map_sampling(data_rangers, proj = "+proj=mbt_fps") +
#'   plot_map_sampling(data_rangers, proj = "+proj=hammer") +
#'   plot_map_sampling(data_rangers, proj = "+proj=wag1") +
#'   plot_map_sampling(data_rangers, proj = "+proj=eqearth") +
#'   plot_map_sampling(data_rangers, proj = "+proj=eck4") +
#'   plot_map_sampling(data_rangers, proj = "+proj=boggs")
#' }
#'
plot_map_sampling <- function(data, proj = "+proj=moll") {

  ## applying projection:
  data$geometry <- sf::st_transform(data$geometry, crs = proj)

  ## binning variable of interest for plotting:
  data |>
    dplyr::mutate(sampled_coverage2 = cut(.data$sampled_coverage, breaks = c(0.1, seq(0, 100, 20)),
                                          labels = c("0", paste0(floor(min(data$sampled_coverage[data$sampled_coverage > 0 & !is.na(data$sampled_coverage)])), "-20"), "20-40", "40-60", "60-80", "80-100")),
                  sampled_coverage2 = forcats::fct_rev(.data$sampled_coverage2)) -> data

  data$sampled_coverage2[data$PA_area_surveyed == 0 & data$PA_area_unsurveyed > 0] <- "0"

  #browser()
  #table(data$sampled_coverage2)

  ## creating world border:
  sf::st_graticule(ndiscr = 10000, margin = 10e-6) |>
    dplyr::filter(.data$degree %in% c(-180, 180)) |>
    sf::st_transform(crs = proj) |>
    #sf::st_convex_hull() %>% # if need to use fill to color oceans
    dplyr::summarise(geometry = sf::st_union(.data$geometry)) -> border

  ## plotting:
  ggplot2::ggplot() +
    ggplot2::geom_sf(data = border, fill = NA, size = 0.1, colour = "black") +
    ggplot2::geom_sf(mapping = ggplot2::aes(fill = .data$sampled_coverage2, geometry = .data$geometry),
                     data = data, colour = "black", size = 0.05) +
    ggplot2::scale_fill_manual(values = c(scales::brewer_pal(type = "seq", palette = 2, direction = -1)(length(unique(data$sampled_coverage2)) - 2), "#FFA500"),
                               labels = c(levels(data$sampled_coverage2), "no terrestrial PAs listed in the WDPA"),
                               na.value = "grey50",
                               guide = ggplot2::guide_legend(title = "Protected Areas\n sampled (%)")) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "left",
                   #panel.grid = ggplot2::element_line(colour = "GREY", size = 0.3),
                   panel.grid = ggplot2::element_blank()) +
    ggplot2::coord_sf(expand = FALSE, crs = proj)

}

#' Plot the map for the reliability score
#'
#' @inheritParams plot_map_sampling
#' @export
#'
#' @examples
#' plot_map_reliability(data_rangers)
#'
plot_map_reliability <- function(data, proj = "+proj=moll") {

  ## applying projection:
  data$geometry <- sf::st_transform(data$geometry, crs = proj)

  ## binning variable of interest for plotting:
  data |>
    dplyr::mutate(reliability2 = cut(.data$reliability, breaks = c(0, 9, 12, 14, 16, 18, 20),
                                     labels = c("0-9", "10-12", "13-14", "15-16", "17-18", "19-20")),
                  reliability2 = forcats::fct_rev(droplevels(.data$reliability2))) -> data

  #browser()
  #table(data$reliability2)

  ## creating world border:
  sf::st_graticule(ndiscr = 10000, margin = 10e-6) |>
    dplyr::filter(.data$degree %in% c(-180, 180)) |>
    sf::st_transform(crs = proj) |>
    #sf::st_convex_hull() %>% # if need to use fill to color oceans
    dplyr::summarise(geometry = sf::st_union(.data$geometry)) -> border

  ## plotting:
  ggplot2::ggplot() +
    ggplot2::geom_sf(data = border, fill = NA, size = 0.1, colour = "black") +
    ggplot2::geom_sf(mapping = ggplot2::aes(fill = .data$reliability2, geometry = .data$geometry),
                     data = data, colour = "black", size = 0.05) +
    ggplot2::scale_fill_manual(values = c(scales::brewer_pal(type = "seq", palette = 2, direction = -1)(length(unique(data$reliability2)) - 1)),
                               labels = c(levels(data$reliability2), "no data"),
                               na.value = "grey50",
                               guide = ggplot2::guide_legend(title = "Reliability score (/20)")) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "left",
                   legend.box.spacing = ggplot2::unit(2.5, "cm"),
                   #panel.grid = ggplot2::element_line(colour = "GREY", size = 0.3),
                   panel.grid = ggplot2::element_blank()) +
    ggplot2::coord_sf(expand = FALSE, crs = proj)

}
