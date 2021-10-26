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
#' ## Data reliability
#' summary(data_rangers$reliability)
#' sum(data_rangers$reliability >= 15, na.rm = TRUE)
#' length(na.omit(data_rangers$reliability))
#' round(mean(data_rangers$reliability, na.rm = TRUE), 2)
#' round(sd(data_rangers$reliability, na.rm = TRUE), 2)
#'
#' ## Table S1 A
#'
#' readr::write_excel_csv(table_completeness_obs(data_rangers),
#'                        file = paste0(path_tables, "table_completeness_obs.csv"))
#'
#'
#' ## Table S1 B
#'
#' readr::write_excel_csv(table_completeness_km2(data_rangers),
#'                        file = paste0(path_tables, "table_completeness_km2.csv"))
#'
#'
#' ## Table S1 C
#'
#' readr::write_excel_csv(table_completeness_vars(data_rangers),
#'                        file = paste0(path_tables, "table_completeness_vars.csv"))
#'
#'
#' ## Figure 1
#'
#' plot_map_sampling(data_rangers_with_geo)
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_sampling.pdf"),
#'                 width = ggplot2::unit(15, "cm"))
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_sampling.png"),
#'                 width = ggplot2::unit(15, "cm"))
#'
#'
#' ## Figure S1
#'
#' plot_map_reliability(data_rangers_with_geo)
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_reliability.pdf"),
#'                 width = ggplot2::unit(15, "cm"))
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_reliability.png"),
#'                 width = ggplot2::unit(15, "cm"))
#'
#'
#' ## Figure XX
#'
#' plot_reliability_vs_sampling(data_rangers)
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_reliability_vs_design.pdf"),
#'                 width = ggplot2::unit(8, "cm"))
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_reliability_vs_design.png"),
#'                 width = ggplot2::unit(8, "cm"))
#'
#'
#' ## Figure XX
#'
#' plot_density_vs_sampling(data_rangers)
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_density_vs_sampling.pdf"),
#'                 width = ggplot2::unit(8, "cm"))
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_density_vs_sampling.png"),
#'                 width = ggplot2::unit(8, "cm"))
#'
#'
#' ## Numerical example to explain how imputation is done (in SI):
#'
#' data_rangers %>%
#'   dplyr::filter(countryname_eng == "Spain") %>%
#'   dplyr::select(.data$staff_rangers,
#'                 .data$PA_area_surveyed,
#'                 .data$PA_area_unsurveyed) -> data_spain_before_imputation
#'  data_spain_before_imputation
#'  fill_PA_area(data_spain_before_imputation, coef = 1)
#'  fill_PA_area(data_spain_before_imputation, coef = 0.5)
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
#' ## Info on computing time:
#'
#' extract_results(list_results_LMM = list(LMM_000, LMM_025, LMM_050, LMM_075, LMM_100),
#'                 list_results_RF = list(RF_000, RF_025, RF_050, RF_075, RF_100)) %>%
#'                 dplyr::mutate(time = .data$run_time * .data$Ncpu) %>%
#'                 dplyr::summarize(total_time_h = sum(.data$time))
#'
#' ## Table S2
#'
#' extract_training_info(which = "initial",
#'                       list_results_LMM = list(LMM_000, LMM_025, LMM_050, LMM_075, LMM_100),
#'                       list_results_RF = list(RF_000, RF_025, RF_050, RF_075, RF_100),
#'                       data = data_rangers) -> training_info_initial
#'
#' training_info_initial %>% # remove duplicates
#'   dplyr::group_by(who) %>%
#'   dplyr::slice(1) %>%
#'   dplyr::select(-.data$type, -.data$coef, -.data$ncol) %>%
#'   dplyr::ungroup() -> training_info_initial_clean
#'
#'  readr::write_excel_csv(training_info_initial_clean,
#'                         file = paste0(path_tables, "table_training_sets_initial.csv"))
#'
#'
#' ## Table S3
#'
#' extract_training_info(which = "final",
#'                       list_results_LMM = list(LMM_000, LMM_025, LMM_050, LMM_075, LMM_100),
#'                       list_results_RF = list(RF_000, RF_025, RF_050, RF_075, RF_100),
#'                       data = data_rangers) -> training_info_final
#'
#'
#' readr::write_excel_csv(training_info_final,
#'                        file = paste0(path_tables, "table_training_sets_final_temp.csv"))
#'
#'
#' ## Table S4
#' extract_finetuning(list_results_LMM = list(LMM_000, LMM_025, LMM_050, LMM_075, LMM_100),
#'                    list_results_RF = list(RF_000, RF_025, RF_050, RF_075, RF_100),
#'                    data = data_rangers) -> fine_tunning_selected
#'
#'  readr::write_excel_csv(fine_tunning_selected,
#'                         file = paste0(path_tables, "table_fine_tuning_temp.csv"))
#'
#'
#' ## Figure XX selected features
#'
#' plot_features_selected(list_results_LMM = list(LMM_000, LMM_025, LMM_050, LMM_075, LMM_100),
#'                        list_results_RF  = list(RF_000, RF_025, RF_050, RF_075, RF_100),
#'                        data = data_rangers)
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_features_selected_temp.pdf"),
#'                 width = ggplot2::unit(8, "cm"), height = ggplot2::unit(10, "cm"))
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_features_selected_temp.png"),
#'                 width = ggplot2::unit(8, "cm"), height = ggplot2::unit(10, "cm"))

#'
#' ## Figure XX feature selection (influence on RMSE)
#'
#' plot_features_selection(result = RF_100, who = "rangers")
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_features_selection_RF_temp.pdf"),
#'                 width = ggplot2::unit(8, "cm"))
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_features_selection_RF_temp.png"),
#'                 width = ggplot2::unit(8, "cm"))
#'
#' plot_features_selection(result = LMM_100, who = "rangers")
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_features_selection_LMM_temp.pdf"),
#'                 width = ggplot2::unit(8, "cm"))
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_features_selection_LMM_temp.png"),
#'                 width = ggplot2::unit(8, "cm"))
#'
#' ## Figure XX fine tuning
#'
#' plot_finetuning(result = RF_100, who = "rangers")
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_finetuning_temp.pdf"),
#'                 width = ggplot2::unit(8, "cm"))
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_finetuning_temp.png"),
#'                 width = ggplot2::unit(8, "cm"))

#' ## Table SXX
#'
#' table_predictions_per_method(list_results_LMM = list(LMM_000, LMM_025, LMM_050, LMM_075, LMM_100),
#'                 list_results_RF = list(RF_000, RF_025, RF_050, RF_075, RF_100),
#'                 data = data_rangers) -> table_predictions
#'
#'  readr::write_excel_csv(table_predictions,
#'                         file = paste0(path_tables, "table_predictions.csv"))
#'
#'
#' ## Table SXX
#'
#' table_predictions_per_method(list_results_LMM = list(LMM_000, LMM_025, LMM_050, LMM_075, LMM_100),
#'                 list_results_RF = list(RF_000, RF_025, RF_050, RF_075, RF_100),
#'                 data = data_rangers, density = TRUE) -> table_predictions_densities
#'
#'  readr::write_excel_csv(table_predictions_densities,
#'                         file = paste0(path_tables, "table_predictions_densities.csv"))
#'
#'
#' ## Table SXX
#'
#' table_predictions_per_continent <- table_predictions_per_continent(what = LMM_100, data = data_rangers)
#'
#' readr::write_excel_csv(table_predictions_per_continent,
#'                        file = paste0(path_tables, "table_predictions_per_continent.csv"))
#'
#'
#' ## Table 1
#'
#' table_predictions_main <- table_predictions_summary(what = LMM_100, data = data_rangers)
#' readr::write_excel_csv(table_predictions_main,
#'                        file = paste0(path_tables, "table_predictions_main.csv"))
#'
#'
#' ## Figure XX tallies per methods
#'
#' plot_tallies_across_methods(list_results_LMM = list(LMM_000, LMM_025, LMM_050, LMM_075, LMM_100),
#'                             list_results_RF = list(RF_000, RF_025, RF_050, RF_075, RF_100),
#'                             data = data_rangers)
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_tallies_across_methods_temp.pdf"),
#'                 width = ggplot2::unit(14, "cm"))
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_tallies_across_methods_temp.png"),
#'                 width = ggplot2::unit(14, "cm"))
#'
#'
#' ## Figure XX tallies per continent
#'
#' plot_tallies_across_continents(what = LMM_100, data = data_rangers)
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_tallies_across_continents_temp.pdf"),
#'                 width = ggplot2::unit(8, "cm"))
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_tallies_across_continents_temp.png"),
#'                 width = ggplot2::unit(8, "cm"))
#'
#'
#' ## Figure XX density rangers
#'
#' plot_density_staff(what = LMM_100, who = "rangers", data = data_rangers)
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_density_rangers.pdf"),
#'                  width = 15, height = 9, scale = 0.7)
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_density_rangers.png"),
#'                  width = 15, height = 9, scale = 0.7)
#'
#'
#' ## Figure XX density all
#'
#' plot_density_staff(what = LMM_100, who = "all", data = data_rangers)
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_density_all.pdf"),
#'                  width = 15, height = 9, scale = 0.7)
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_density_all.png"),
#'                  width = 15, height = 9, scale = 0.7)
#'
#'
#' ## Figure XX PA per data type
#'
#' plot_PA_by_data_type(what = LMM_100, data = data_rangers)
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_PA_by_data_type_temp.pdf"),
#'                 width = ggplot2::unit(11, "cm"))
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_PA_by_data_type_temp.png"),
#'                 width = ggplot2::unit(11, "cm"))
#'
#' ## Figure XX density vs PA
#' plot_density_vs_PA(data = data_rangers, who = "all", coef = 1)
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_density_vs_PA_all.pdf"),
#'                 width = ggplot2::unit(8, "cm"))
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_density_vs_PA_all.png"),
#'                 width = ggplot2::unit(8, "cm"))
#'
#' #' ## Figure XX density vs PA
#' plot_density_vs_PA(data = data_rangers, who = "rangers", coef = 1)
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_density_vs_PA_rangers.pdf"),
#'                 width = ggplot2::unit(8, "cm"))
#' ggplot2::ggsave(filename = paste0(path_figures, "figure_density_vs_PA_rangers.png"),
#'                 width = ggplot2::unit(8, "cm"))
#'
#' }
