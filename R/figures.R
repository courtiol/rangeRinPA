
#' Plot the map of the sampling design
#'
#' @param data the complete dataset
#' @param proj the projection to be used (e.g. "+proj=moll" - the default)
#'
#' @export
#'
#' @examples
#' plot_map_sampling(data_rangers_with_geo)
#'
#' if(require(patchwork)) {
#'   plot_map_sampling(data_rangers_with_geo, proj = "+proj=moll") +
#'   plot_map_sampling(data_rangers_with_geo, proj = "+proj=robin" ) +
#'   plot_map_sampling(data_rangers_with_geo, proj = "+proj=natearth2") +
#'   plot_map_sampling(data_rangers_with_geo, proj = "+proj=mbt_fps") +
#'   plot_map_sampling(data_rangers_with_geo, proj = "+proj=hammer") +
#'   plot_map_sampling(data_rangers_with_geo, proj = "+proj=wag1") +
#'   plot_map_sampling(data_rangers_with_geo, proj = "+proj=eqearth") +
#'   plot_map_sampling(data_rangers_with_geo, proj = "+proj=eck4") +
#'   plot_map_sampling(data_rangers_with_geo, proj = "+proj=boggs")
#' }
#'
plot_map_sampling <- function(data, proj = "+proj=moll") {

  if (!requireNamespace("sf", quietly = TRUE)) stop("You need to install the package sf for this function to run")

  ## applying projection:
  data$geometry <- sf::st_transform(data$geometry, crs = proj)

  ## binning variable of interest for plotting:
  data %>%
    dplyr::mutate(sampled_coverage2 = cut(.data$sampled_coverage, breaks = c(0.1, seq(0, 100, 20)),
                                          labels = c("0", paste0(floor(min(data$sampled_coverage[data$sampled_coverage > 0 & !is.na(data$sampled_coverage)])), "-20"), "20-40", "40-60", "60-80", "80-100")),
                  sampled_coverage2 = forcats::fct_rev(.data$sampled_coverage2)) -> data

  data$sampled_coverage2[data$PA_area_surveyed == 0 & data$PA_area_unsurveyed > 0] <- "0"

  #browser()
  #table(data$sampled_coverage2)

  ## creating world border:
  sf::st_graticule(ndiscr = 10000, margin = 10e-6) %>%
    dplyr::filter(.data$degree %in% c(-180, 180)) %>%
    sf::st_transform(crs = proj) %>%
    #sf::st_convex_hull() %>% # if need to use fill to color oceans
    dplyr::summarise(geometry = sf::st_union(.data$geometry)) -> border

  ## plotting:
  ggplot2::ggplot() +
    ggplot2::geom_sf(data = border, fill = NA, size = 0.1, colour = "black") +
    ggplot2::geom_sf(mapping = ggplot2::aes(fill = .data$sampled_coverage2, geometry = .data$geometry),
                     data = data, colour = "black", size = 0.05) +
    ggplot2::scale_fill_manual(values = c(scales::brewer_pal(type = "seq", palette = 2, direction = -1)(length(unique(data$sampled_coverage2)) - 2), "white"),
                               labels = c(levels(data$sampled_coverage2), "excluded (see legend)"),
                               na.value = "grey50",
                               guide = ggplot2::guide_legend(title = "Area surveyed (%)")) +
    ggplot2::theme_void(base_size = 24) +
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
#' plot_map_reliability(data_rangers_with_geo)
#'
plot_map_reliability <- function(data, proj = "+proj=moll") {

  if (!requireNamespace("sf", quietly = TRUE)) stop("You need to install the package sf for this function to run")

  ## applying projection:
  data$geometry <- sf::st_transform(data$geometry, crs = proj)

  ## binning variable of interest for plotting:
  data %>%
    dplyr::mutate(reliability = dplyr::if_else(is.na(.data$reliability) & .data$sampled_coverage == 0, 0, .data$reliability),
                  reliability2 = cut(.data$reliability, breaks = c(-1, 9, 12, 14, 16, 18, 20),
                                     labels = c("not surveyed", "10-12", "13-14", "15-16", "17-18", "19-20")),
                  reliability2 = forcats::fct_rev(droplevels(.data$reliability2))) %>%
    dplyr::select(.data$reliability, .data$reliability2, .data$geometry) -> data

  #browser()
  #table(data$reliability2)

  ## creating world border:
  sf::st_graticule(ndiscr = 10000, margin = 10e-6) %>%
    dplyr::filter(.data$degree %in% c(-180, 180)) %>%
    sf::st_transform(crs = proj) %>%
    #sf::st_convex_hull() %>% # if need to use fill to color oceans
    dplyr::summarise(geometry = sf::st_union(.data$geometry)) -> border

  ## plotting:
  ggplot2::ggplot() +
    ggplot2::geom_sf(data = border, fill = NA, size = 0.1, colour = "black") +
    ggplot2::geom_sf(mapping = ggplot2::aes(fill = .data$reliability2, geometry = .data$geometry),
                     data = data, colour = "black", size = 0.05) +
    ggplot2::scale_fill_manual(values = c(scales::brewer_pal(type = "seq", palette = 2, direction = -1)(length(unique(data$reliability2)) - 2), "white"),
                               labels = c(levels(data$reliability2), "excluded (see legend)"),
                               na.value = "grey50",
                               guide = ggplot2::guide_legend(title = "Reliability score (/20)")) +
    ggplot2::theme_void(base_size = 24) +
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
    ggplot2::theme_bw(base_size = 18) +
    ggplot2::labs(x = "Surface of protected areas surveyed in country/territory (%)", y = "Reliability score (/20)")
}


#' Plot the density of staff against the sampling intensity
#'
#' @inheritParams plot_map_sampling
#' @param who `"rangers"` (default), `"others"` or `"all"`
#' @export
#'
#' @examples
#' plot_density_vs_sampling(data_rangers)
#'
plot_density_vs_sampling <- function(data, who = "rangers") {

  if (who == "all") who <- "total"

  data %>%
    dplyr::select(.data$countryname_eng, .data$PA_area_surveyed, .data$sampled_coverage,
                  staff = tidyselect::matches(paste0("staff_", who, "$"))) %>%
    dplyr::mutate(coverage_staff = .data$PA_area_surveyed/.data$staff,
                  coverage_sampling = .data$sampled_coverage) %>%
    dplyr::filter(.data$PA_area_surveyed > 100) %>%
    dplyr::arrange(dplyr::desc(.data$coverage_staff)) -> d

  d %>%
    ggplot2::ggplot() +
      ggplot2::aes(y = .data$coverage_staff, x = .data$coverage_sampling) +
      ggplot2::geom_point(pch = 1) +
      ggplot2::scale_y_continuous(breaks = c(10^(0:3), 2*10^(0:3), 5*10^(0:3)), minor_breaks = NULL,
                                  labels = scales::label_number(accuracy = 1)) +
      ggplot2::scale_x_continuous(breaks = seq(0, 100, 5), minor_breaks = 0:100) +
      ggplot2::coord_trans(y = "log") +
      ggplot2::labs(x = "Protected areas surveyed (%)",
                    y = expression(paste("Protected areas per individual staff (km"^"2", ")"))) +
      ggplot2::theme_bw()
}


#' Plot the influence of the fine tuning parameters for RF/ETs fits on the RMSE
#'
#' @param result the output of a call to [`run_RF_workflow()`] or [`run_LMM_workflow()`]
#' @inheritParams plot_density_vs_sampling
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
                  replace = dplyr::if_else(.data$replace, "replace = TRUE", "replace = FALSE"),
                  replace = factor(.data$replace, levels = c("replace = TRUE", "replace = FALSE"))) %>%
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
      ggplot2::scale_x_continuous(breaks = 0:19, minor_breaks = NULL) +
      ggplot2::scale_y_continuous(breaks = seq(1, 20, 0.5), minor_breaks = NULL) +
      ggplot2::coord_cartesian(ylim = c(floor(min(selected_features_res$RMSE) * 2)/2,
                                        ceiling(max(selected_features_res$RMSE)))) +
      ggplot2::theme_bw() +
      ggplot2::labs(shape = "spatial autocorrelation", colour = "spatial autocorrelation")
}


#' Plot the influence of the selection of predictors on the RMSE
#'
#' @inheritParams plot_finetuning
#' @param result1 the first output of a call to [`run_RF_workflow()`] or [`run_LMM_workflow()`]
#' @param result2 the second output of a call to [`run_RF_workflow()`] or [`run_LMM_workflow()`]
#'
#' @export
#'
#' @seealso [plot_features_selection]
#' @examples
#' # see see ?rangeRinPA
#'
plot_features_selection_panel <- function(result1, result2, who = "rangers") {

  if (!requireNamespace("patchwork", quietly = TRUE)) stop("You need to install the package patchwork for this function to run")

  p1 <- plot_features_selection(result = result1, who = "rangers") + ggplot2::labs(tag = "A.")
  p2 <- plot_features_selection(result = result2, who = "rangers") + ggplot2::labs(tag = "B.") + ggplot2::theme(legend.position = "none")

  p1 / p2
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
                      "GDP_growth", "unemployment_log", "EVI", "EPI_2020", "SPI", "IUCN_1_4_prop", "IUCN_1_2_prop", "spatial_autocorr.")

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
    ggplot2::labs(x = "Coeffficient used for the imputation", y = "Candidate predictor", shape = "Predictor selected") +
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

  extract_results(list_results_LMM = list_results_LMM,
                  list_results_RF = list_results_RF, data = data) %>%
    dplyr::mutate(type = dplyr::if_else(.data$type == "LMM", "LMM", "RF/ETs")) -> res

  ggplot2::ggplot(res) +
    ggplot2::aes(y = .data$point_pred, x = as.factor(.data$coef), fill = .data$type,
                 ymin = pmin(.data$lwr, .data$point_pred), ymax = .data$upr) +
    ggplot2::geom_col(position = "dodge", colour = "black", size = 0.2) +
    ggplot2::geom_linerange(position = ggplot2::position_dodge(width = 0.9), size = 0.5) +
    ggplot2::scale_y_continuous(breaks = (0:10) * 1e5, minor_breaks = (0:200) * 1e4,
                                labels = scales::label_number(accuracy = 1)) +
    ggsci::scale_fill_npg(guide = ggplot2::guide_legend(reverse = TRUE), alpha = 0.8) + # values = c("#52734D", "#FEFFDE", "#91C788")
    ggplot2::theme_minimal() +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "Coefficient used for imputation",
                  y = "Estimated total number of staff",
                  fill = "Statistical framework used for predictions:") +
    ggplot2::facet_wrap(~ .data$who, scales = "free") +
    ggplot2::theme(legend.position = "bottom")
}


#' Plot details about estimations
#'
#' @inheritParams extract_results
#' @export
#'
#' @examples
#' # see ?rangeRinPA
#'
plot_tallies_across_continents <- function(what, data) {

  if (length(what) != 4) stop("Wrong input: 'what' should be a list produced by a workflow function")

  res <- extract_results(list_results_LMM = list(what)) ## note: works also when input is from RF, makes no difference

  res %>%
    dplyr::filter(.data$who != "Others") -> res_focus

  res_focus %>%
    dplyr::select(.data$who, .data$pred_details) %>%
    tidyr::unnest_wider(col = .data$pred_details) %>%
    tidyr::unnest(-.data$who) %>%
    tidyr::pivot_longer(cols = c("sum_known", "sum_imputed", "sum_predicted")) %>%
    dplyr::select(-.data$sum_known_imputed, -.data$sum_total) %>%
    dplyr::mutate(name = sapply(.data$name, \(x) sub(pattern = "sum_", replacement = "", x, fixed = TRUE)),
                  name = factor(.data$name, levels = c("predicted", "imputed", "known"))) %>%
    dplyr::ungroup() -> breakdown

  breakdown %>%
    dplyr::group_by(.data$who, .data$name) %>%
    dplyr::summarise(value = sum(.data$value)) %>%
    dplyr::mutate(continent = "World") %>%
    dplyr::bind_rows(breakdown) -> breakdown

  breakdown$lwr[breakdown$continent == "World"] <- rep(res_focus$lwr, each = 3)
  breakdown$upr[breakdown$continent == "World"] <- rep(res_focus$upr, each = 3)

  breakdown %>%
    dplyr::filter(.data$who != "Others", .data$continent != "Antarctica") %>%
    dplyr::mutate(continent = dplyr::case_when(.data$continent == "Latin America & Caribbean" ~ "Latin America &\nCarribbean",
                                               .data$continent == "Northern America" ~ "Northern\nAmerica",
                                               TRUE ~ .data$continent)) %>%
    ggplot2::ggplot() +
      ggplot2::aes(y = .data$value, x = .data$who, fill = .data$name, ymin = .data$lwr, ymax = .data$upr) +
      ggplot2::geom_col() +
      ggplot2::geom_errorbar(width = 0, colour = "#E64B35FF") +
      ggplot2::facet_wrap(~ .data$continent, nrow = 1) +
      ggplot2::scale_y_continuous(breaks = (0:10) * 1e5, minor_breaks = (0:100) * 1e4,
                                  labels = scales::label_number(accuracy = 1)) +
      ggsci::scale_fill_npg() +
      ggplot2::theme_bw() +
      ggplot2::labs(fill = "Type of data:", y = "Number of staff", x = NULL) +
      ggplot2::theme(plot.title.position = "plot",
                     legend.position = "bottom") +
      ggplot2::coord_cartesian(ylim = c(0, ceiling(max(breakdown$upr) / 10e4)*10e4))

}


#' Plot details about estimations in terms of PA
#'
#' @inheritParams extract_results
#' @export
#'
#' @examples
#' # see ?rangeRinPA
#'
plot_PA_by_data_type <- function(what, data) {

  if (length(what) != 4) stop("Wrong input: 'what' should be a list produced by a workflow function")

  res <- extract_results(list_results_LMM = list(what), data = data) ## note: works also when input is from RF, makes no difference

  res %>%
    dplyr::mutate(who = dplyr::case_when(.data$who == "All" ~ "All personnel",
                                         .data$who == "Rangers" ~ "Rangers",
                                         .data$who == "Others" ~ "Non-rangers"),
                  who = forcats::fct_inorder(.data$who)) %>%
    dplyr::select(.data$who, .data$PA_areas) %>%
    tidyr::unnest_wider(.data$PA_areas) %>%
    tidyr::unnest(-.data$who) %>%
    tidyr::pivot_longer(cols = tidyselect::starts_with("PA")) %>%
    dplyr::mutate(name = sapply(.data$name, \(x) sub(pattern = "PA_area_", replacement = "", x))) %>%
    dplyr::filter(.data$name %in% c("unknown", "predicted", "imputed", "known")) %>%
    dplyr::mutate(name = dplyr::case_when(.data$name == "unknown" ~ "unpredictable",
                                          TRUE ~ .data$name)) %>%
    dplyr::mutate(name = factor(.data$name, levels = c("unpredictable", "predicted", "imputed", "known"))) %>%
    dplyr::group_by(.data$who, .data$continent) %>%
    dplyr::mutate(total = sum(.data$value),
                  value = .data$value / .data$total) %>%
    dplyr::ungroup() -> PA_areas_breakdown

  PA_areas_breakdown %>%
    ggplot2::ggplot() +
      ggplot2::aes(y = .data$value, x = .data$total/2, fill = .data$name, width = .data$total) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::facet_grid(.data$who ~ .data$continent, switch = "y") +
      ggplot2::coord_polar(theta = "y", start = 0, direction = 1) +
      ggplot2::theme_void() +
      ggsci::scale_fill_npg() +
      ggplot2::labs(fill = "Source of data on personnel:") +
      ggplot2::theme(plot.title.position = "plot",
                     legend.position = "bottom",
                     plot.margin = ggplot2::margin(t = 1, l = 0.5, unit = "cm"))
}



#' Plot densities of rangers or all personnel
#'
#' @inheritParams extract_results
#' @inheritParams run_RF_workflow
#' @param ymax the maximal value for the y-axis
#' @param breaks a vector of number defining the horizontal lines in the plot (i.e. breaks in ggplot jargon)
#' @param tag a tag to be used for building a panel (e.g "A.")
#' @export
#' @examples
#' # see ?rangeRinPA
#'
plot_density_staff <- function(what, who, data, ymax = 6000, breaks = c(10^(0:3), 2*10^(0:3), 5*10^(0:3)), tag = "") {

    if (!requireNamespace("patchwork", quietly = TRUE)) stop("You need to install the package patchwork for this function to run")

  what[[who]]$country_info[[1]] %>%
    dplyr::rowwise() %>%
    dplyr::mutate(PA = sum(dplyr::c_across(tidyselect::starts_with("PA"))),
                  exp(dplyr::across(tidyselect::ends_with("log"), .names = "value")) - 1) %>%
    dplyr::select(-tidyselect::starts_with("PA_"), -tidyselect::ends_with("log")) %>%
    dplyr::mutate(km2_per_staff = .data$PA / .data$value) -> d ## TODO: check who is missing

  order_continents <- c("World", "Latin America \n& Caribbean", "Africa", "Oceania", "Asia", "Europe", "Northern\n America")

  d %>%
    dplyr::filter(!is.na(.data$km2_per_staff)) %>%
    add_continents(data = data) %>%
    dplyr::mutate(continent = dplyr::case_when(.data$continent == "Latin America & Caribbean" ~ "Latin America \n& Caribbean",
                                               .data$continent == "Northern America" ~ "Northern\n America",
                                               TRUE ~ .data$continent),
                  continent = factor(.data$continent, levels = order_continents)) -> dd

  ## fix small negative values caused by fitting data on log(x + 1) to near 0 that can be plotted:
  dd$km2_per_staff[dd$km2_per_staff < 0] <- 0.01
  dd$km2_per_staff[dd$value <= 0] <- dd$PA[dd$value <= 0]
  dd$value[dd$value < 0] <- 0

  threshold <- sum(dd$PA)/(sum(dd$value)*1/0.36) # recommendation is that current force is 36% of what is needed

  dd %>%
    dplyr::mutate(continent = "World") %>%
    dplyr::mutate(continent = factor(.data$continent, levels =  order_continents)) -> dd_world

  dd %>%
    dplyr::bind_rows(dd_world) -> dd_all

  dd %>%
    dplyr::group_by(.data$continent) %>%
    dplyr::summarise(mean = sum(.data$PA) / sum(.data$value), #stats::weighted.mean(.data$km2_per_staff, .data$PA),
                     good = sum(.data$PA[.data$km2_per_staff <= threshold]),
                     bad = sum(.data$PA[.data$km2_per_staff > threshold])) -> dd_mean_continents

  dd %>%
    dplyr::summarise(mean = sum(.data$PA) / sum(.data$value), #stats::weighted.mean(.data$km2_per_staff, .data$PA),
                     good = sum(.data$PA[.data$km2_per_staff <= threshold]),
                     bad = sum(.data$PA[.data$km2_per_staff > threshold])) %>%
    dplyr::mutate(continent = "World") -> dd_mean_world

  dd_mean_continents %>%
    dplyr::bind_rows(dd_mean_world) %>%
    dplyr::mutate(continent = factor(.data$continent, levels =  order_continents)) -> dd_mean

  dd_mean %>%
    tidyr::pivot_longer(cols = c("good", "bad")) %>%
    dplyr::group_nest(.data$continent) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(gg = list(ggplot2::ggplot(data) +
                              ggplot2::aes(y = .data$value, x = "", fill = .data$name) +
                              ggplot2::geom_bar(stat = "identity", position = "stack", show.legend = FALSE) +
                              ggplot2::coord_polar(theta = "y", start = 0, direction = 1) +
                              ggsci::scale_fill_npg() +
                              ggplot2::labs(title = paste0(round(100*data$value[1] / sum(data$value), 2), "%")) +
                              ggplot2::theme_void() +
                              ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)))) -> data_pies

  dd %>%
    ggplot2::ggplot() +
    ggplot2::geom_polygon(data = data.frame(x = c(0, 0, 8, 8, 0), y = c(6000, 0.01, 0.01, 6000, 6000)),
                          mapping = ggplot2::aes(x = .data$x, y = .data$y), fill = NA, colour = NA, alpha = 0) +
    ggplot2::geom_segment(data = data.frame(x = 7.8, xend = 7.8, y = 0.01, yend = 6000),
                          mapping = ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
                          arrow = ggplot2::arrow(type = "closed", length = ggplot2::unit(0.3, units = "cm"), ends = "both")) +
    ggplot2::geom_text(data.frame(x = 7.9, y = 2), mapping = ggplot2::aes(x = .data$x, y = .data$y), label = "better", angle = 270) +
    ggplot2::geom_text(data.frame(x = 7.9, y = 3000), mapping = ggplot2::aes(x = .data$x, y = .data$y), label = "worse", angle = 270) +
    ggplot2::geom_polygon(data = data.frame(x = c(0.5, 0.5, 1.5, 1.5, 0.5), y = c(6000, 0.1, 0.1, 6000, 6000)),
                          mapping = ggplot2::aes(x = .data$x, y = .data$y), fill = "grey", colour = NA, alpha = 0.5) +
    ggplot2::geom_jitter(ggplot2::aes(y = .data$km2_per_staff, x = .data$continent, size = .data$PA,
                                  alpha = .data$km2_per_staff < threshold,
                                  colour = .data$continent, fill = .data$continent,
                                  shape = .data$type),
                     position = ggplot2::position_jitter(seed = 1L, width = 0.15, height = 0), data = dd_all) +
    ggplot2::geom_point(ggplot2::aes(y = .data$mean, x = .data$continent, fill = .data$continent),
                        shape = 23, colour = "black", size = 3, data = dd_mean) +
    ggplot2::geom_polygon(data = data.frame(x = c(0.2, 0.2, 7.5, 7.5, 0.2), y = c(threshold, 0.01, 0.01, threshold, threshold)),
                          mapping = ggplot2::aes(x = .data$x, y = .data$y), fill = "darkgreen", colour = NA, alpha = 0.1) +
    { if (who == "rangers") ggplot2::geom_hline(yintercept = 5, colour = "darkgreen", linetype = "dashed") } +
    { if (who == "rangers") ggplot2::geom_text(ggplot2::aes(y = .data$y, x = .data$x), colour = "darkgreen",
                                               size = 3, label = "Recommended by IUCN",
         alpha = 0.95,
         hjust = 0.5, vjust = -0.2,
         data = data.frame(x = 7, y = 5)) } +
    ggplot2::geom_text(ggplot2::aes(y = .data$mean, x = .data$continent, label = round(.data$mean)),
                       nudge_x = 0.3, size = 6, data = dd_mean) +
    ggplot2::geom_hline(yintercept = threshold, colour = "darkgreen", linetype = "dashed") +
    ggplot2::geom_text(ggplot2::aes(y = .data$y, x = .data$x), colour = "darkgreen",
                       size = 3, label = "Average requirement",
                       hjust = 0.5, vjust = -0.2,
                       data = data.frame(x = 7, y = threshold)) + # if not in data, coord_trans does not pick it up...
    ggplot2::scale_y_continuous(limits = c(ymax, 0.01), breaks = breaks, minor_breaks = NULL,
                                labels = scales::label_number(accuracy = 1), trans = "reverse") +
    ggplot2::scale_x_discrete(position = "bottom") +
    ggplot2::scale_shape_manual(values = c(21, 1)) +
    ggsci::scale_colour_npg() +
    ggsci::scale_fill_npg() +
    ggplot2::scale_alpha_discrete(range = c(0.3, 0.95)) +
    ggplot2::scale_size_continuous(range = c(1, 10)) +
    ggplot2::coord_trans(y = "pseudo_log") +
    { if (who == "rangers") ggplot2::labs(x = "", y = expression(paste("Area per ranger (km"^"2", ")")), tag = tag) } +
    { if (who == "all") ggplot2::labs(x = "", y = expression(paste("Area per person (km"^"2", ")")), tag = tag) } +
    ggplot2::theme_minimal(base_size = 18) +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                   plot.caption = ggplot2::element_text(hjust = 0, vjust = 45), axis.text.x = ggplot2::element_text(size = 12),
                   plot.margin = ggplot2::margin(t = 0, b = 2, r = 0.5, unit = "in")) +
    ggplot2::guides(colour = "none", size = "none", fill = "none", alpha = "none", shape = "none") -> main_plot

  main_plot +
    patchwork::inset_element(data_pies$gg[[1]], 0.15, 0,    0.05 + 1/4.5, 0.2, align_to = "full") +
    patchwork::inset_element(data_pies$gg[[2]], 0.15, 0.03, 0.05 + 2/4.5, 0.17, align_to = "full") +
    patchwork::inset_element(data_pies$gg[[3]], 0.15, 0.03, 0.05 + 3/4.5, 0.17, align_to = "full") +
    patchwork::inset_element(data_pies$gg[[4]], 0.15, 0.03, 0.05 + 4/4.5, 0.17, align_to = "full") +
    patchwork::inset_element(data_pies$gg[[5]], 0.15, 0.03, 0.05 + 5/4.5, 0.17, align_to = "full") +
    patchwork::inset_element(data_pies$gg[[6]], 0.15, 0.03, 0.05 + 6/4.5, 0.17, align_to = "full") +
    patchwork::inset_element(data_pies$gg[[7]], 0.15, 0.03, 0.05 + 7/4.5, 0.17, align_to = "full") +
    patchwork::plot_annotation(caption = ifelse(who == "rangers",
                                                "Proportion of area where each ranger manages less than the average requirement:",
                                                "Proportion of area where each person manages less than the average requirement:"),
                               theme = ggplot2::theme(plot.caption = ggplot2::element_text(face = "italic", size = 14, hjust = 0.5, vjust = 45))) #-> plot_final

  #print(plot_final)

  #invisible(dd_all %>% dplyr::arrange(dplyr::desc(.data$km2_per_staff)))
}


#' Plot densities of rangers and all personnel
#'
#' @inheritParams plot_density_staff
#' @export
#' @examples
#' # see ?rangeRinPA
#'
plot_density_panel <- function(what, data, ymax = 6000, breaks = c(10^(0:3), 2*10^(0:3), 5*10^(0:3))) {

  if (!requireNamespace("patchwork", quietly = TRUE)) stop("You need to install the package patchwork for this function to run")

  plot_density_staff(what = what, who = "all", ymax = ymax, breaks = breaks, data = data, tag = "A.") -> plot_all

  plot_density_staff(what = what, who = "rangers", ymax = ymax, breaks = breaks, data = data, tag = "B.") -> plot_rangers

  plot_all + plot_rangers
}


#' Plot densities of staff vs area PA
#'
#' @inheritParams plot_density_vs_sampling
#' @param coef the coefficient for the imputation
#' @export
#' @examples
#' plot_density_vs_PA(data = data_rangers, who = "rangers", coef = 1)
#'
plot_density_vs_PA <- function(data, who, coef = 1) {

  if (!requireNamespace("ggrepel", quietly = TRUE)) stop("You need to install the package ggrepel for this function to run")

  if (who == "all") who <- "total"

  old_opt <- options("ggrepel.max.overlaps" = Inf)

  ybreaks <- 10^(0:5)
  xbreaks <- 10^(1:7)

  data %>%
    fill_PA_area(coef = coef) %>%
    dplyr::rename(staff = tidyselect::matches(paste0("staff_", who, "$"))) %>%
    dplyr::mutate(staff = dplyr::if_else(.data$area_PA_total > 0 & .data$staff == 0, 1, .data$staff),
                  coverage_staff = .data$area_PA_total/.data$staff) %>%
    tidyr::drop_na(.data$staff) -> d

  fit <- spaMM::fitme(log(coverage_staff + 1) ~ log(area_PA_total + 1), data = d)
  tibble::tibble(y = exp(spaMM::predict.HLfit(fit)[, 1]) - 1,
                 x = fit$data$area_PA_total) -> preds

  d %>%
    ggplot2::ggplot() +
    ggplot2::aes(y = .data$coverage_staff, x = .data$area_PA_total,
                 label = .data$countryname_iso, colour = .data$country_UN_continent) +
    ggplot2::geom_line(ggplot2::aes(y = .data$y, x = .data$x), data = preds, colour = "blue",
                       size = 2, linetype = "dashed", alpha = 0.5,
                       inherit.aes = FALSE) +
    ggrepel::geom_text_repel(key_glyph = "point", alpha = 0.4, size = 3) +
    ggplot2::geom_point() +
    ggplot2::coord_trans(y = "log1p", x = "log1p") +
    ggsci::scale_colour_npg() +
    ggplot2::scale_x_continuous(breaks = xbreaks, minor_breaks = NULL,
                                limits = c(5, min(xbreaks[xbreaks > max(d$area_PA_total)])),
                                labels = scales::label_number(accuracy = 1)) +
    ggplot2::scale_y_continuous(breaks = ybreaks, minor_breaks = NULL,
                                limits = c(min(d$coverage_staff, na.rm = TRUE), min(ybreaks[ybreaks > max(d$coverage_staff, na.rm = TRUE)])),
                                labels = scales::label_number(accuracy = 1)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(y = expression(paste("Protected areas per individual staff (km"^"2", ")")),
         x = expression(paste("Area of protected areas (km"^"2", ")")),
         colour = "Continent:") -> plot

   options(old_opt) ## restore original options

   plot
}


#' Plot projections for required numbers
#'
#' @inheritParams plot_map_sampling
#' @inheritParams extract_results
#' @export
#' @examples
#' # see ?rangeRinPA
#'
plot_projections <- function(what, data) {

  if (!requireNamespace("patchwork", quietly = TRUE)) stop("You need to install the package patchwork for this function to run")

  d <- table_projections(what = what, data = data)

  d %>%
    tidyr::pivot_longer(cols = tidyselect::contains("number")) %>%
    dplyr::mutate(name = dplyr::case_when(.data$name == "number" ~ "current",
                                          .data$name == "number_required" ~ "required now",
                                          .data$name == "number_required_2030" ~ "required 2030"),
                  name = forcats::fct_inorder(.data$name),
                  who = dplyr::case_when(.data$who == "all" ~ "All personnel",
                                         .data$who == "rangers" ~ "Rangers")) -> d_long_number

  d %>%
    tidyr::pivot_longer(cols = tidyselect::contains("density")) %>%
    dplyr::filter(.data$name != "density_required_2030") %>% ## density required is the same now and in 2030!
    dplyr::mutate(name = dplyr::case_when(.data$name == "density" ~ "current",
                                          .data$name == "density_required" ~ "required (now & 2030)"),
                  name = forcats::fct_inorder(.data$name),
                  who = dplyr::case_when(.data$who == "all" ~ "All personnel",
                                         .data$who == "rangers" ~ "Rangers")) -> d_long_density


  ggplot2::ggplot(d_long_number) +
    ggplot2::aes(y = .data$value, x = .data$who, fill = .data$name) +
    ggplot2::geom_col(width = 0.4, position = "dodge2") +
    ggplot2::labs(y = "Number of persons", x = "", fill = "", tag = "A.") +
    ggplot2::scale_y_continuous(breaks = seq(0, 10e6, by = 0.5e6),
                                limits = c(0, 3.5e6),
                                labels = scales::label_number(accuracy = 1)) +
    ggplot2::scale_fill_manual(values = ggsci::pal_npg()(3)) +
    ggplot2::theme_bw(base_size = 18) +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(), legend.position = "top", plot.margin = ggplot2::margin(r = 1, unit = "in")) -> plot_numbers

  d_long_density %>%
    dplyr::mutate(who = forcats::fct_rev(.data$who),
                  name = forcats::fct_rev(.data$name)) -> d_long_density2

  ggplot2::ggplot(d_long_density2) +
    ggplot2::aes(y = .data$value, x = .data$who, fill = .data$name) +
    ggplot2::geom_col(width = 0.4, position = "dodge2") +
    ggplot2::labs(y = expression(paste("Area per person (km"^"2", ")")), x = "", fill = "", tag = "B.") +
    ggplot2::scale_y_continuous(breaks = seq(0, 100, by = 10),
                                labels = scales::label_number(accuracy = 1),
                                limits = c(0, 80)) +
    ggplot2::scale_fill_manual(values = c(grDevices::colorRampPalette(c(ggsci::pal_npg()(3)[2], ## compute mid colour
                                                                        ggsci::pal_npg()(3)[3]),
                                                                      space = "Lab")(3)[2],
                                          ggsci::pal_npg()(3)[1])) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE)) +
    ggplot2::coord_flip() +
    ggplot2::theme_bw(base_size = 18) +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(), legend.position = "top") -> plot_density

  plot_numbers / plot_density

}

