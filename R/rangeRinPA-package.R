#' Protected Area Personnel and Ranger Numbers are Insufficient to Deliver Global Expectations
#'
#' The goal of rangeRinPA is to reproduce most results from the paper Protected Area Personnel and
#' Ranger Numbers are Insufficient to Deliver Global Expectations by Appleton et al. (submitted)
#'
#' @name rangeRinPA-package
#' @aliases rangeRinPA-package rangeR
#' @docType package
#'
#' @references
#' TODO (now under submission)
#'
#' @keywords package
#' @examples
#' \dontrun{
#'
#' ## Set and create all directories to store files
#'
#' path <- "inst/extdata/" ## set the path where you want to store all created files
#' if (!dir.exists(path)) stop("You must use an existing path")
#'
#' path_tables <- paste0(path, "tables/")
#' if (!dir.exists(path_tables)) dir.create(path_tables)
#'
#' path_figures <- paste0(path, "figures/")
#' if (!dir.exists(path_figures)) dir.create(path_figures)
#'
#' path_predictions <- paste0(path, "predictions/")
#' if (!dir.exists(path_predictions)) dir.create(path_predictions)
#'
#'
#' ### Predictions
#'
#' #Ncpu <- 2L
#' Ncpu <- 100 ## define the number of CPUs to use
#' n_trees <- 2000
#'
#'
#' ## Run all LMM workflows and save results
#'
#' LMM_100 <- run_LMM_workflow(data = data_rangers, Ncpu = Ncpu, coef = 1)
#' save(LMM_100, file = paste0(path_predictions, "LMM_100.Rdata"))
#' rm(LMM_100)
#' gc()
#'
#' LMM_075 <- run_LMM_workflow(data = data_rangers, Ncpu = Ncpu, coef = 0.75)
#' save(LMM_075, file = paste0(path_predictions, "LMM_075.Rdata"))
#' rm(LMM_075)
#' gc()
#'
#' LMM_050 <- run_LMM_workflow(data = data_rangers, Ncpu = Ncpu, coef = 0.50)
#' save(LMM_050, file = paste0(path_predictions, "LMM_050.Rdata"))
#' rm(LMM_050)
#' gc()
#'
#' LMM_025 <- run_LMM_workflow(data = data_rangers, Ncpu = Ncpu, coef = 0.25)
#' save(LMM_025, file = paste0(path_predictions, "LMM_025.Rdata"))
#' rm(LMM_025)
#' gc()
#'
#' LMM_000 <- run_LMM_workflow(data = data_rangers, Ncpu = Ncpu, coef = 0)
#' save(LMM_000, file = paste0(path_predictions, "LMM_000.Rdata"))
#' rm(LMM_000)
#' gc()
#'
#'
#' ## Run all RF workflows and save results
#'
#' RF_100 <- run_RF_workflow(data = data_rangers, Ncpu = Ncpu, coef = 1, n_trees = n_trees)
#' save(RF_100, file = paste0(path_predictions, paste0("RF_100_", n_trees, ".Rdata")))
#' rm(RF_100)
#' gc()
#'
#' RF_075 <- run_RF_workflow(data = data_rangers, Ncpu = Ncpu, coef = 0.75, n_trees = n_trees)
#' save(RF_075,file = paste0(path_predictions, paste0("RF_075_", n_trees, ".Rdata")))
#' rm(RF_075)
#' gc()
#'
#' RF_050 <- run_RF_workflow(data = data_rangers, Ncpu = Ncpu, coef = 0.50, n_trees = n_trees)
#' save(RF_050, file = paste0(path_predictions, paste0("RF_050_", n_trees, ".Rdata")))
#' rm(RF_050)
#' gc()
#'
#' RF_025 <- run_RF_workflow(data = data_rangers, Ncpu = Ncpu, coef = 0.25, n_trees = n_trees)
#' save(RF_025, file = paste0(path_predictions, paste0("RF_025_", n_trees, ".Rdata")))
#' rm(RF_025)
#' gc()
#'
#' RF_000 <- run_RF_workflow(data = data_rangers, Ncpu = Ncpu, coef = 0, n_trees = n_trees)
#' save(RF_000, file = paste0(path_predictions, paste0("RF_000_", n_trees, ".Rdata")))
#' rm(RF_000)
#' gc()
#'
#'
#' ## Load all results saved on disk (useful only if all objects created above are not already in memory)
#'
#' files_to_load <- c("LMM_000.Rdata", "LMM_025.Rdata", "LMM_050.Rdata", "LMM_075.Rdata",
#'                    "LMM_100.Rdata", paste0(c("RF_000_", "RF_025_", "RF_050_", "RF_075_", "RF_100_"),
#'                    n_trees, ".Rdata"))
#'
#' sapply(paste0(path_predictions, files_to_load), function(file) load(file, envir = .GlobalEnv))
#'
#'
#'
#' ###################################################### MAIN FIGURES
#'
#' ## Figure 1
#'
#' plot_map_sampling(data_rangers_with_geo)
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_main_1.pdf"),
#'                 width = ggplot2::unit(15, "cm"))
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_main_1.png"),
#'                 width = ggplot2::unit(15, "cm"))
#'
#'
#' ## Figure 2
#'
#' plot_density_panel(what = LMM_100, data = data_rangers)
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_main_2.pdf"),
#'                  width = 26, height = 12, scale = 0.7)
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_main_2.png"),
#'                  width = 26, height = 12, scale = 0.7)
#'
#'
#' ## Figure 3
#'
#' plot_density_vs_PA_panel(data = data_rangers, coef = 1)
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_main_3.pdf"),
#'                 width = ggplot2::unit(10, "cm"))
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_main_3.png"),
#'                 width = ggplot2::unit(10, "cm"))
#'
#'
#' ## Figure 4 & 5 not made with R!
#'
#'
#'
#' ###################################################### MAIN TABLES
#'
#' ## Table 1
#'
#' table_predictions_main <- table_predictions_summary(what = LMM_100, data = data_rangers)
#' readr::write_excel_csv(table_predictions_main,
#'                        file = paste0(path_tables, "table_main_1.csv"))
#'
#'
#'
#' ###################################################### EXTENDED DATA FIGURES
#'
#'
#' ## Extended Data Figure 1
#'
#' plot_tallies_across_continents(what = LMM_100, data = data_rangers)
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_extended_1.pdf"),
#'                 width = ggplot2::unit(14, "cm"))
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_extended_1.png"),
#'                 width = ggplot2::unit(14, "cm"))
#'
#'
#' ## Extended Data Figure 2
#'
#' plot_tallies_across_methods(list_results_LMM = list(LMM_000, LMM_025, LMM_050, LMM_075, LMM_100),
#'                             list_results_RF = list(RF_000, RF_025, RF_050, RF_075, RF_100),
#'                             data = data_rangers)
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_extended_2.pdf"),
#'                 width = ggplot2::unit(14, "cm"))
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_extended_2.png"),
#'                 width = ggplot2::unit(14, "cm"))
#'
#'
#'
#' ###################################################### EXTENDED DATA TABLES
#'
#' ## Extended Data Table 1
#'
#' table_raw_data_formatted <- table_raw_data(data_rangers)
#'
#' readr::write_excel_csv(table_raw_data_formatted,
#'                        file = paste0(path_tables, "table_extended_1.csv"))
#'
#'
#' ## Extended Data Table 2
#'
#' table_predictions_main_with_PI <- table_predictions_summary(what = LMM_100, data = data_rangers,
#'                                                             with_PI = TRUE)
#' readr::write_excel_csv(table_predictions_main_with_PI,
#'                        file = paste0(path_tables, "table_extended_2.csv"))
#'
#'
#' ## Extended Data Table 3
#'
#' table_predictions_methods <- table_predictions_per_method(
#'                            list_results_LMM = list(LMM_000, LMM_025, LMM_050, LMM_075, LMM_100),
#'                            list_results_RF = list(RF_000, RF_025, RF_050, RF_075, RF_100),
#'                            data = data_rangers)
#'
#' readr::write_excel_csv(table_predictions_methods,
#'                        file = paste0(path_tables, "table_extended_3.csv"))
#'
#'
#' ## Extended Data Table 4
#'
#' table_predictions_methods_densities <- table_predictions_per_method(
#'                             list_results_LMM = list(LMM_000, LMM_025, LMM_050, LMM_075, LMM_100),
#'                             list_results_RF = list(RF_000, RF_025, RF_050, RF_075, RF_100),
#'                             data = data_rangers, density = TRUE)
#'
#' readr::write_excel_csv(table_predictions_methods_densities,
#'                        file = paste0(path_tables, "table_extended_4.csv"))
#'
#'
#' ## Extended Data Table 5, 6, 7 & 8 not done with R!
#'
#'
#'
#' ###################################################### SUPPLEMENTARY DATA FIGURES
#'
#' ## Supplementary Data Figure 1
#'
#' set.seed(123)
#' plot_reliability_vs_sampling(data_rangers)
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_SI_1.pdf"),
#'                 width = ggplot2::unit(8, "cm"))
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_SI_1.png"),
#'                 width = ggplot2::unit(8, "cm"))
#'
#'
#' ## Supplementary Data Figure 2
#'
#' plot_density_vs_sampling(data_rangers)
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_SI_2.pdf"),
#'                 width = ggplot2::unit(8, "cm"))
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_SI_2.png"),
#'                 width = ggplot2::unit(8, "cm"))
#'
#'
#' ## Supplementary Data Figure 3
#'
#' plot_features_selected(list_results_LMM = list(LMM_000, LMM_025, LMM_050, LMM_075, LMM_100),
#'                        list_results_RF  = list(RF_000, RF_025, RF_050, RF_075, RF_100),
#'                        data = data_rangers)
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_SI_3.pdf"),
#'                 width = ggplot2::unit(8, "cm"), height = ggplot2::unit(10, "cm"))
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_SI_3.png"),
#'                 width = ggplot2::unit(8, "cm"), height = ggplot2::unit(10, "cm"))
#'
#'
#' ## Supplementary Data Figure 4
#'
#' plot_features_selection_panel(result1 = LMM_100, result2 = RF_100, who = "rangers")
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_SI_4.pdf"),
#'                 width = ggplot2::unit(8, "cm"))
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_SI_4.png"),
#'                 width = ggplot2::unit(8, "cm"))
#'
#'
#' ## Supplementary Data Figure 5
#'
#' plot_finetuning(result = RF_100, who = "rangers")
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_SI_5.pdf"),
#'                 width = ggplot2::unit(8, "cm"))
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_SI_5.png"),
#'                 width = ggplot2::unit(8, "cm"))
#'
#'
#' ## Supplementary Data Figure 6
#'
#' plot_PA_by_data_type(what = RF_100, data = data_rangers)
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_SI_6.pdf"),
#'                 width = ggplot2::unit(11, "cm"))
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_SI_6.png"),
#'                 width = ggplot2::unit(11, "cm"))
#'
#'
#'
#'
#' ###################################################### SUPPLEMENTARY DATA TABLES
#'
#' ## Supplementary Data Table 1
#'
#' readr::write_excel_csv(table_completeness_obs(data_rangers),
#'                        file = paste0(path_tables, "table_SI_1A.csv"))
#' readr::write_excel_csv(table_completeness_km2(data_rangers),
#'                        file = paste0(path_tables, "table_SI_1B.csv"))
#' readr::write_excel_csv(table_completeness_vars(data_rangers),
#'                        file = paste0(path_tables, "table_SI_1C.csv"))
#'
#'
#' ## Supplementary Data Table 2
#'
#' table_cor <- table_correlates(data_rangers)
#' readr::write_excel_csv(table_cor,
#'                        file = paste0(path_tables, "table_SI_2.csv"))
#'
#'
#' ## Supplementary Data Table 3
#'
#' training_info_initial <- table_training_initial(
#'                        list_results_LMM = list(LMM_000, LMM_025, LMM_050, LMM_075, LMM_100),
#'                        list_results_RF = list(RF_000, RF_025, RF_050, RF_075, RF_100),
#'                        data = data_rangers)
#'
#' readr::write_excel_csv(training_info_initial,
#'                        file = paste0(path_tables, "table_SI_3.csv"))
#'
#'
#' ## Supplementary Data Table 4
#'
#' training_info_final <- table_training_final(
#'                      list_results_LMM = list(LMM_000, LMM_025, LMM_050, LMM_075, LMM_100),
#'                      list_results_RF = list(RF_000, RF_025, RF_050, RF_075, RF_100),
#'                      data = data_rangers)
#'
#' readr::write_excel_csv(training_info_final,
#'                        file = paste0(path_tables, "table_SI_4.csv"))
#'
#'
#' ## Supplementary Data Table 5
#'
#' fine_tuning_selected <- table_tuning(
#'              list_results_LMM = list(LMM_000, LMM_025, LMM_050, LMM_075, LMM_100),
#'              list_results_RF = list(RF_000, RF_025, RF_050, RF_075, RF_100),
#'              data = data_rangers)
#'
#' readr::write_excel_csv(fine_tuning_selected,
#'                        file = paste0(path_tables, "table_SI_5.csv"))
#'
#'
#' ## Supplementary Data Table 6
#'
#' predictions_info <- table_predictions_data(
#'                       list_results_LMM = list(LMM_000, LMM_025, LMM_050, LMM_075, LMM_100),
#'                       list_results_RF = list(RF_000, RF_025, RF_050, RF_075, RF_100),
#'                       data = data_rangers)
#'
#' readr::write_excel_csv(predictions_info,
#'                        file = paste0(path_tables, "table_SI_6.csv"))
#'
#'
#'
#' ###################################################### OLD TABLES AND FIGURES NOT USED
#'
#' plot_projections(what = LMM_100, data = data_rangers)
#'
#' plot_map_reliability(data_rangers_with_geo)
#'
#' table_projections(what = LMM_100, data = data_rangers)
#'
#'
#'
#' ###################################################### COMPUTATIONS FOR TEXT
#'
#' ## Number of countries/territories surveyed:
#' nrow(table_raw_data(data_rangers))
#'
#'
#' ## Rounded numbers for all personnel:
#' table_projections <- table_projections(what = LMM_100, data = data_rangers)
#' table_projections %>%
#'   dplyr::group_by(.data$who) %>%
#'   dplyr::summarize(dplyr::across(tidyselect::contains("number") |
#'                                  tidyselect::contains("increase"),
#'                    .fns = ~ 1000*round(.x/1000)))
#'
#'
#' ## Rounded densities of all personnel:
#' table_projections %>%
#'   dplyr::group_by(.data$who) %>%
#'   dplyr::summarize(dplyr::across(tidyselect::contains("density"), .fns = ~ round(.x, digits = 2)))
#'
#'
#' ## Area covered by PAs:
#' table_raw_data(data_rangers) %>%
#'   dplyr::select(area_surveyed = .data$`Area of PA surveyed`,
#'                 total_area = .data$`Total PA in country/territory`) %>%
#'   dplyr::summarise(area_surveyed = sum(.data$area_surveyed),
#'                    prop_total_area = area_surveyed/sum(.data$total_area))
#'
#'
#' ## Percentage of land covered with PA in our study:
#' data_rangers %>%
#'   dplyr::filter(!is.na(staff_others) | !is.na(staff_rangers) | !is.na(staff_total)) %>%
#'   dplyr::summarise(area_total = sum(.data$area_PA_total),
#'                    prop_PA = .data$area_total / sum(.data$area_country)) -> coverage_PA
#' round(100*coverage_PA$prop_PA, digits = 2)
#'
#'
#' ## Numerical example to explain how imputation is done (in methods):
#'
#' data_rangers %>%
#'   dplyr::filter(countryname_eng == "Spain") %>%
#'   dplyr::select(.data$staff_rangers,
#'                 .data$PA_area_surveyed,
#'                 .data$PA_area_unsurveyed) -> data_spain_before_imputation
#'  data_spain_before_imputation
#'  fill_PA_area(data_spain_before_imputation, coef = 1)
#'
#'
#' ## Data reliability:
#'
#' summary(data_rangers$reliability)
#' sum(data_rangers$reliability >= 15, na.rm = TRUE)
#' length(na.omit(data_rangers$reliability))
#' round(mean(data_rangers$reliability, na.rm = TRUE), 2)
#' round(sd(data_rangers$reliability, na.rm = TRUE), 2)
#'
#'
#' ## Info on computing time:
#'
#' extract_results(list_results_LMM = list(LMM_000, LMM_025, LMM_050, LMM_075, LMM_100),
#'                 list_results_RF = list(RF_000, RF_025, RF_050, RF_075, RF_100)) %>%
#'                 dplyr::mutate(time = .data$run_time * .data$Ncpu) %>%
#'                 dplyr::summarize(total_time_h = sum(.data$time))
#'
#' }
