
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

  ## obtain sf all all countries:
  world_sf <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

  ## add prefix to variable others than geometry:
  i <- which(names(world_sf) != "geometry")
  names(world_sf)[i] <- paste0("rne_", names(world_sf)[i])

  ## manual fix for some iso codes discrepancies:
  world_sf$rne_iso_a3[world_sf$rne_name == "Kosovo"] <- "KOS"
  world_sf$rne_iso_a3[world_sf$rne_name == "Indian Ocean Ter."] <- "IOT"
  world_sf$rne_iso_a3[world_sf$rne_name == "Siachen Glacier"] <- "KAS"

  ## manual fix for countries/territories referred to differently in the data:
  world_sf$rne_iso_a3[world_sf$rne_name == "Ashmore and Cartier Is."] <- world_sf$rne_iso_a3[world_sf$rne_name == "Australia"] ## Ashmore is part of Australia in our data
  world_sf$rne_iso_a3[world_sf$rne_name == "N. Cyprus"] <- world_sf$rne_iso_a3[world_sf$rne_name == "Cyprus"] ## we don't make the difference between N and S Cyprus in our data
  world_sf$rne_iso_a3[world_sf$rne_name == "Somaliland"] <- world_sf$rne_iso_a3[world_sf$rne_name == "Somalia"] ## we don't make the difference between Somaliland and Somalia in our data

  ## manual fix for polygons combined with others which we need to appear as distinct rows:
  extract_poly <- function(country, lon, lat) {

    world_sf$geometry[world_sf$rne_name == country] |>
      sf::st_cast("POLYGON") -> polys

    selection_box <-  sf::st_polygon(list(matrix(c(lon[1], lat[1], lon[2], lat[1], lon[2], lat[2], lon[1], lat[2], lon[1], lat[1]), ncol = 2L, byrow = TRUE)))

    selection <- sapply(polys, \(x) as.numeric(sf::st_intersects(x, selection_box)) == 1)
    selection <- which(!is.na(selection))

    sf::st_combine(polys[selection])
  }

  GLP_geom <- extract_poly("France", lon = c(-62, -61), lat = c(16.5, 15.7)) ## Guadeloupe
  GUF_geom <- extract_poly("France", lon = c(-55, -50), lat = c(6, 2)) ## French Guiana
  REU_geom <- extract_poly("France", lon = c(55, 56), lat = c(-20.7, -21.5)) ## Reunion
  BES_geom <- extract_poly("Netherlands", lon = c(-69, -58), lat = c(19, 10)) ## Bonaire, Sint Eustatius and Saba
  MTQ_geom <- extract_poly("France", lon = c(-62, -60), lat = c(15, 14)) ## Martinique
  MYT_geom <- extract_poly("France", lon = c(44, 46), lat = c(-12, -13)) ## Mayotte
  SJM1_geom <- extract_poly("Norway", lon = c(5, 40), lat = c(75.7, 82)) ## Svalbard...
  SJM2_geom <- extract_poly("Norway", lon = c(-10, -7.2), lat = c(70.7, 71.4)) ## and Jan Mayen
  SJM_geom <- sf::st_combine(c(SJM1_geom, SJM2_geom))
  TKL_geom <- extract_poly("New Zealand", lon = c(-174, -169),lat =  c(-6, -11)) ## Tuvalu
  ## Bouvet Island -> no polygon but tiny
  ## Cocos (Keeling) Islands -> no polygon but tiny
  ## Christmas Island -> no polygon but tiny
  ## Gibraltar -> no polygon but tiny
  ## United States Minor Outlying Islands -> no polygon but tiny

  polygons_data_to_add <- tibble::tibble(rne_iso_a3 = c("GLP", "GUF", "REU", "BES", "MTQ", "MYT", "SJM", "TKL"),
                                         rne_name = c("Guadeloupe", "French Guiana", "Reunion", "Bonaire, Sint Eustatius and Saba", "Martinique", "Mayotte", "Svalbard and Jan Mayen", "Tuvalu"),
                                         geometry = c(GLP_geom, GUF_geom, REU_geom, BES_geom, MTQ_geom, MYT_geom, SJM_geom, TKL_geom))

  world_sf |>
    dplyr::bind_rows(polygons_data_to_add) -> world_sf

  ## check locations not found in map (depend on scale defined above) or not found in data:
  data |>
    dplyr::anti_join(world_sf, by = c(countryname_iso = "rne_iso_a3")) |>
    dplyr::pull(.data$countryname_eng) -> missing1

  world_sf |>
    dplyr::anti_join(data, by = c(rne_iso_a3 = "countryname_iso")) |>
    dplyr::pull(.data$rne_name) -> missing2

  missing <- c(missing1, missing2)

  if (length(missing1) > 0) {
    cat("Here are the countries/territories for which polygons are not found (not plotted):\n")
    print(missing1)
  }

  if (length(missing1) > 0) {
    cat("\n Here are the countries/territories for which data are not found in our dataset:\n")
    print(missing2)
    #world_sf |>
    #  dplyr::filter(!.data$rne_name %in% missing2) -> world_sf ## to remove but then no plotted
  }

  ## only keep locations found in map:
  data |>
    dplyr::right_join(world_sf, by = c(countryname_iso = "rne_iso_a3")) |>
    sf::st_as_sf() -> world_rangers

  ## applying projection:
  world_rangers |>
    sf::st_transform(crs = proj) -> world_rangers

  ## computing variable of interest:
  world_rangers |>
    dplyr::mutate(sampled_coverage = 100*.data$PA_area_surveyed / (.data$PA_area_surveyed + .data$PA_area_unsurveyed)) -> world_rangers

  ## checking for potential issues:
  if (any(world_rangers$sampled_coverage[!is.na(world_rangers$sampled_coverage)] > 100)) {
    cat("\n Some countries have sampled coverage > 100%, there must be an issue. The problematic countries are:\n")
    print(world_rangers$countryname_eng[world_rangers$sampled_coverage > 100 & !is.na(world_rangers$sampled_coverage)])
    world_rangers$sampled_coverage[world_rangers$sampled_coverage > 100 & !is.na(world_rangers$sampled_coverage)] <- 100
  }

  if (any(is.na(world_rangers$sampled_coverage))) {
    cat("\n Some countries have no PAs in our dataset. These countries are:\n")
    print(world_rangers$countryname_eng[is.na(world_rangers$sampled_coverage)])
  }

  ## binning variable of interest for plotting:
  world_rangers |>
    dplyr::mutate(sampled_coverage2 = cut(.data$sampled_coverage, breaks = c(0.1, seq(0, 100, 20)),
                                          labels = c("0", paste0(floor(min(world_rangers$sampled_coverage[world_rangers$sampled_coverage > 0 & !is.na(world_rangers$sampled_coverage)])), "-20"), "20-40", "40-60", "60-80", "80-100")),
                  sampled_coverage2 = forcats::fct_rev(.data$sampled_coverage2)) -> world_rangers

  world_rangers$sampled_coverage2[world_rangers$PA_area_surveyed == 0 & world_rangers$PA_area_unsurveyed > 0] <- "0"

  #browser()
  #table(world_rangers$sampled_coverage2)

  ## creating world border:
  sf::st_graticule(ndiscr = 10000, margin = 10e-6) |>
    dplyr::filter(.data$degree %in% c(-180, 180)) |>
    sf::st_transform(crs = proj) |>
    #sf::st_convex_hull() %>% # if need to use fill to color oceans
    dplyr::summarise(geometry = sf::st_union(.data$geometry)) -> border

  ## plotting:
  ggplot2::ggplot() +
    ggplot2::geom_sf(data = border, fill = NA, size = 0.1, colour = "black") +
    ggplot2::geom_sf(mapping = ggplot2::aes(fill = .data$sampled_coverage2),
                     data = world_rangers, colour = "black", size = 0.05) +
    ggplot2::scale_fill_manual(values = c(scales::brewer_pal(type = "seq", palette = 2, direction = -1)(length(unique(world_rangers$sampled_coverage2)) - 2), "#FFA500"),
                               labels = c(levels(world_rangers$sampled_coverage2), "no terrestrial PAs listed in the WDPA"),
                               na.value = "grey50",
                               guide = ggplot2::guide_legend(title = "Protected Areas\n sampled (%)")) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "left",
                   #panel.grid = ggplot2::element_line(colour = "GREY", size = 0.3),
                   panel.grid = ggplot2::element_blank()) +
    ggplot2::coord_sf(expand = FALSE, crs = proj)

  }
