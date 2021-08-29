
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


#' Plot the reliability score against the sampling intensity
#'
#' @inheritParams plot_map_sampling
#' @export
#'
#' @examples
#' plot_reliability_vs_sampling(data_rangers)
#'
plot_reliability_vs_sampling <- function(data){
  ggplot2::ggplot(data) +
    ggplot2::aes(y = .data$reliability, x = .data$sampled_coverage) +
    ggplot2::geom_jitter(width = 0.75, height = 0.1, shape = 1) +
    ggplot2::scale_x_continuous(breaks = seq(0, 100, 5), minor_breaks = 0:100) +
    ggplot2::scale_y_continuous(breaks = 10:20, minor_breaks = NULL) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Protected Areas sampled (%)", y = "Reliability score (/20)")
}


#' Plot the influence of the fine tuning parameters for RF/ETs fits on the RMSE
#'
#' @param result the output of a call to [`run_RF_workflow()`]
#' @param who `"rangers"` (default), `"others"` or `"all"`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ## Note: this example takes some time to run!
#' RF_test <- run_RF_workflow(data = data_rangers, Ncpu = 2, coef = 0,
#'                            rep_feature_select = 2, rep_finetune = 10, rep_simu = 2,
#'                            grid_type = "fine", n_trees = 100)
#'
#' plot_finetuning(result = RF_test, who = "rangers")
#' }
#'
plot_finetuning <- function(result, who = "rangers") {

  finetune_res_mean <- result[[who]]$fine_tuning[[1]][["mean"]]
  finetune_res_SE <- result[[who]]$fine_tuning[[1]][["SE"]]

  finetune_res_mean %>%
    dplyr::mutate(splitrule = dplyr::if_else(.data$splitrule == "extratrees", "splitrule = 'extratrees'", "splitrule = 'variance'"),
                  replace = dplyr::if_else(.data$replace, "replace = TRUE", "replace = FALSE")) %>%
    dplyr::bind_cols(SE = finetune_res_SE$RMSE) %>%
    dplyr::mutate(RMSE_upr = .data$RMSE + .data$SE,
                  RMSE_lwr = .data$RMSE - .data$SE) -> finetune_res

  finetune_res %>%
    dplyr::distinct(dplyr::across(!.data$mtry), .keep_all = TRUE) %>% ## remove duplicate caused by similar effective value for mtry
    ggplot2::ggplot() +
      ggplot2::aes(y = .data$RMSE, x = .data$min.node.size,
                   shape = sub("function (n) \n", "", paste(.data$mtry), fixed = TRUE),
                   colour = paste(.data$sample.fraction),
                   ymin = .data$RMSE_upr, ymax = .data$RMSE_lwr) +
      ggplot2::geom_point(size = 2) +
      ggplot2::geom_line(alpha = 0.7) +
      #ggplot2::geom_errorbar(width = 0) +
      ggplot2::geom_hline(yintercept = min(finetune_res$RMSE), linetype = "dashed") +
      ggplot2::scale_x_continuous(breaks = 1:10, minor_breaks = NULL) +
      ggplot2::theme_bw() +
      ggplot2::facet_grid(.data$splitrule ~ .data$replace) +
      ggplot2::labs(colour = "sample.fraction", shape = "mtry") +
      ggplot2::theme()
}


#' Plot the influence of the selection of predictors on the RMSE
#'
#' @inheritParams plot_finetuning
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ## Note: this example takes some time to run!
#' RF_test <- run_RF_workflow(data = data_rangers, Ncpu = 2, coef = 0,
#'                            rep_feature_select = 2, rep_finetune = 10, rep_simu = 2,
#'                            grid_type = "fine", n_trees = 100)
#'
#' plot_features_selection(result = RF_test, who = "rangers")
#' }
#'
plot_features_selection <- function(result, who = "rangers") {

  selected_features_res <- result[[who]][["selected_features"]][[1]]

  selected_features_res %>%
    ggplot2::ggplot() +
      ggplot2::aes(y = .data$RMSE, x = .data$k,
                   shape = factor(.data$spatial, levels = c("TRUE", "FALSE")),
                   colour = factor(.data$spatial, levels = c("TRUE", "FALSE"))
                   ) +
      ggplot2::geom_point(size = 2) +
      ggplot2::geom_line(alpha = 0.7) +
      ggplot2::geom_hline(yintercept = min(selected_features_res$RMSE), linetype = "dashed") +
      ggplot2::scale_x_continuous(breaks = 1:19, minor_breaks = NULL) +
      ggplot2::theme_bw() +
      ggplot2::labs(shape = "spatial autocorrelation", colour = "spatial autocorrelation") +
      ggplot2::theme()
}


#' Plot the output of the selection of predictors fits
#'
#' @inheritParams extract_results
#' @param size the size of the points on the plot
#'
#' @export
#'
#' @examples
#' # see ?extract_results for example
#'
plot_features_selected <- function(list_results_LMM, list_results_RF, data, size = 4) {

  res <- extract_results(list_results_LMM = list_results_LMM, list_results_RF = list_results_RF, data = data)

  all_predictors <- c("PA_area_log", "pop_density_log", "area_country_log", "long", "lat", "area_forest_pct","GDP_2019_log", "GDP_capita_log",
                      "GDP_growth", "unemployment_log", "EVI", "SPI", "EPI_2020", "IUCN_1_4_prop", "IUCN_1_2_prop", "spatial_autocorr.")

  res$predictor <- list(all_predictors)
  res$predictor_included <- lapply(res$formula, \(x) all_predictors %in% all.vars(stats::as.formula(x)[-2]))

  res %>%
    tidyr::unnest(c(.data$predictor, .data$predictor_included)) -> res_long

  res_long$predictor_included[res_long$spatial & res_long$predictor == "spatial_autocorr."] <- TRUE

  res_long$predictor <- gsub(pattern = "_", replacement = " ", x = res_long$predictor, fixed = TRUE)

  res_long$predictor <- factor(res_long$predictor, levels = rev(gsub(pattern = "_", replacement = " ", x = all_predictors, fixed = TRUE)))

  res_long %>%
    dplyr::mutate(type = dplyr::if_else(.data$type == "LMM", "LMM", "RF/ETs")) -> res_long

  ggplot2::ggplot(res_long) +
    ggplot2::aes(y = .data$predictor, x = .data$coef, shape = factor(.data$predictor_included, levels = c("TRUE", "FALSE"))) +
    ggplot2::geom_point(size = size) +
    ggplot2::scale_shape_manual(values = c("circle", "circle open")) +
    ggplot2::labs(x = "Relative density of staff in unsurveyed area", y = "Candidate predictor", shape = "Predictor selected") +
    ggplot2::facet_grid(.data$who ~ .data$type) +
    ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.25), minor_breaks = NULL) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")

}


#' Plot tallies across methods
#'
#' @inheritParams extract_results
#' @export
#'
#' @examples
#' # see ?rangeRinPA
#'
plot_tallies_across_methods <- function(list_results_LMM, list_results_RF, data) {

  res <- extract_results(list_results_LMM = list_results_LMM, list_results_RF = list_results_RF, data = data)

  ggplot2::ggplot(res) +
    ggplot2::aes(y = .data$point_pred, x = as.factor(.data$coef), fill = .data$type,
                 ymin = pmin(.data$lwr, .data$point_pred), ymax = .data$upr) +
    ggplot2::geom_col(position = "dodge", colour = "black", size = 0.2) +
    ggplot2::geom_linerange(position = ggplot2::position_dodge(width = 0.9), size = 0.5) +
    ggplot2::scale_y_continuous(breaks = (0:10) * 1e5, minor_breaks = (0:200) * 1e4, labels = scales::comma) +
    ggsci::scale_fill_npg(guide = ggplot2::guide_legend(reverse = TRUE), alpha = 0.8) + # values = c("#52734D", "#FEFFDE", "#91C788")
    ggplot2::theme_minimal() +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "Coefficient used for imputation",
                  y = "Estimated worldwide total number of staff",
                  fill = "Statistical framework used for predictions:") +
    ggplot2::facet_wrap(~ .data$who, scales = "free") +
    ggplot2::theme(legend.position = "bottom")
}


