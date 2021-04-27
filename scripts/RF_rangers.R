library(rangeRinPA)
library(dplyr)
library(tidyr)
library(ggplot2)
library(spaMM)
library(ranger)
library(randomForestExplainer)
library(patchwork)

rm(list = ls())

n_tests <- 1000
Ncpu <- 100

# Selecting non-spatial predictors for RF --------------------------------------

## we examine the importance of variable on the subset of data with no missing data
forest_full <- ranger(staff_rangers_log ~ . , data = data_test, importance = "impurity",
                      splitrule = "extratrees", replace = FALSE, sample.fraction = 1, mtry = function(n) n, seed = 123)
## Note: I used the recommended default settings from Geurts et al. 2006 extremely randomized trees
forest_full
tibble::as_tibble_row(importance(forest_full)) %>%
  pivot_longer(everything(), names_to = "Predictor", values_to = "Importance") %>%
  arrange(desc(Importance)) %>%
  mutate(Predictor = forcats::fct_inorder(Predictor)) %>%
  ggplot() +
    aes(y = Importance, x = Predictor) +
    geom_col(width = 0.2) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45))

## Note: since we are doing extremely randomized trees, we have no OOB observations!

accuracy_forest_full <- validate_RF(formula = staff_rangers_log ~ ., data = data_test,
                                    rep = n_tests, Ncpu = Ncpu,  method = "CV", spatial = FALSE, seed = 123,
                                    splitrule = "extratrees", replace = FALSE, sample.fraction = 1, mtry = function(n) n)

## let's test a forest using only the top 4 predictors
accuracy_forest_4top_pred <- validate_RF(formula = staff_rangers_log ~ area_country_log + PA_area_log + GDP_2019_log + pop_density_log, data = data_test,
                                         rep = n_tests, Ncpu = Ncpu,  method = "CV", spatial = FALSE, seed = 123,
                                         splitrule = "extratrees", replace = FALSE, sample.fraction = 1, mtry = function(n) n)

# since GDP_2019_log and pop_density_log are not available for all territory, let's check how much we loose by not having them:
accuracy_forest_no_GDP <- validate_RF(formula = staff_rangers_log ~ area_country_log + PA_area_log + pop_density_log, data = data_test,
                                      rep = n_tests, Ncpu = Ncpu,  method = "CV", spatial = FALSE, seed = 123,
                                      splitrule = "extratrees", replace = FALSE, sample.fraction = 1, mtry = function(n) n)

accuracy_forest_no_pop_dens <- validate_RF(formula = staff_rangers_log ~  area_country_log + PA_area_log + GDP_2019_log, data = data_test,
                                           rep = n_tests, Ncpu = Ncpu,  method = "CV", spatial = FALSE, seed = 123,
                                           splitrule = "extratrees", replace = FALSE, sample.fraction = 1, mtry = function(n) n)

accuracy_forest_no_GDP_no_pop_dens <- validate_RF(formula = staff_rangers_log ~  area_country_log + PA_area_log, data = data_test,
                                                  rep = n_tests, Ncpu = Ncpu,  method = "CV", spatial = FALSE, seed = 123,
                                                  splitrule = "extratrees", replace = FALSE, sample.fraction = 1, mtry = function(n) n)

cbind(fix = c("full", "top4", "top4_noGDP", "top4_noPopDens", "top4_noGDP_noPopDen"),
      rbind(aggregate_metrics(accuracy_forest_full),
            aggregate_metrics(accuracy_forest_4top_pred),
            aggregate_metrics(accuracy_forest_no_GDP),
            aggregate_metrics(accuracy_forest_no_pop_dens),
            aggregate_metrics(accuracy_forest_no_GDP_no_pop_dens)))

#                   fix type  rep     RMSE           ME       MAE        R2      RRSE       CCC      MoranI Moran_pv_freq
# 1                full   CV 1000 3.565140 -0.016761908 0.8235895 0.3817319 0.3588943 0.4208240 -0.06917688         0.081
# 2                top4   CV 1000 3.642382  0.001818198 0.8342861 0.3365033 0.3757961 0.4761208 -0.05968191         0.083
# 3          top4_noGDP   CV 1000 3.480530  0.005961123 0.7871275 0.3995374 0.3585841 0.5125900 -0.07226391         0.066
# 4      top4_noPopDens   CV 1000 4.077865 -0.012659761 0.9631045 0.1702652 0.4168843 0.3946634 -0.03582471         0.113
# 5 top4_noGDP_noPopDen   CV 1000 4.087240 -0.009332992 0.9730100 0.1828931 0.4084761 0.3902846 -0.05245354         0.082

# NOTE: we choose top4_noGDP

# Selecting spatial predictors for RF ------------------------------------------
## The issue with the approach above is that no obs is considered OOB in fit_Matern, so let's redo all using proper CV

accuracy_forest_no_spatial <- accuracy_forest_no_GDP

accuracy_forest_spatial_coord <- validate_RF(formula = staff_rangers_log ~ area_country_log + PA_area_log + pop_density_log, data = data_test,
                                             rep = n_tests, Ncpu = Ncpu, method = "CV", spatial = "coord", seed = 123,
                                             splitrule = "extratrees", replace = FALSE, sample.fraction = 1, mtry = function(n) n)

accuracy_forest_spatial_dist <- validate_RF(formula = staff_rangers_log ~ area_country_log + PA_area_log + pop_density_log, data = data_test,
                                            rep = n_tests, Ncpu = Ncpu, method = "CV", spatial = "dist", seed = 123,
                                            splitrule = "extratrees", replace = FALSE, sample.fraction = 1, mtry = function(n) n)

accuracy_forest_spatial_hybrid <- validate_RF(formula = staff_rangers_log ~ area_country_log + PA_area_log + pop_density_log, data = data_test,
                                              rep = n_tests, Ncpu = Ncpu, method = "CV", spatial = "hybrid", seed = 123,
                                              splitrule = "extratrees", replace = FALSE, sample.fraction = 1, mtry = function(n) n)

cbind(spatial = c("none", "coord", "dist", "hybrid"),
      rbind(aggregate_metrics(accuracy_forest_no_spatial),
            aggregate_metrics(accuracy_forest_spatial_coord),
            aggregate_metrics(accuracy_forest_spatial_dist),
            aggregate_metrics(accuracy_forest_spatial_hybrid)))

#   spatial type  rep     RMSE           ME       MAE        R2      RRSE       CCC      MoranI Moran_pv_freq
# 1    none   CV 1000 3.480530  0.005961123 0.7871275 0.3995374 0.3585841 0.5125900 -0.07226391         0.066
# 2   coord   CV 1000 3.455511 -0.002066693 0.7772497 0.4154516 0.3505110 0.4943840 -0.07387185         0.075
# 3    dist   CV 1000 3.708072 -0.011140640 0.8483120 0.3245788 0.3688680 0.4517223 -0.07089223         0.087
# 4  hybrid   CV 1000 3.466056  0.011697469 0.7993319 0.3826882 0.3482759 0.5690676 -0.07338786         0.065

# NOTE: accounting for spatialisation does not seem to help much...


# Selecting RF method ----------------------------------------------------------

accuracy_extra_trees <- accuracy_forest_no_GDP

accuracy_Breiman1 <- validate_RF(formula = staff_rangers_log ~ area_country_log + PA_area_log + pop_density_log, data = data_test,
                                 rep = n_tests, Ncpu = Ncpu,  method = "CV", spatial = FALSE, seed = 123,
                                 mtry = function(n) floor(n/3))

accuracy_Breiman2 <- validate_RF(formula = staff_rangers_log ~ area_country_log + PA_area_log + pop_density_log, data = data_test,
                                rep = n_tests, Ncpu = Ncpu,  method = "CV", spatial = FALSE, seed = 123,
                                mtry = function(n) n)

cbind(spatial = c("extra_trees", "Breiman1", "Breiman2"),
      rbind(aggregate_metrics(accuracy_extra_trees),
            aggregate_metrics(accuracy_Breiman1),
            aggregate_metrics(accuracy_Breiman2)))

#       spatial type  rep     RMSE          ME       MAE        R2      RRSE       CCC      MoranI Moran_pv_freq
# 1 extra_trees   CV 1000 3.480530 0.005961123 0.7871275 0.3995374 0.3585841 0.5125900 -0.07226391         0.066
# 2    Breiman1   CV 1000 3.572876 0.021856557 0.8126015 0.3595288 0.3642463 0.5190065 -0.07194014         0.067
# 3    Breiman2   CV 1000 3.479724 0.019608340 0.7909531 0.4071951 0.3551688 0.4662679 -0.06650214         0.070

forest_extra_trees <- ranger(staff_rangers_log ~ area_country_log + PA_area_log + pop_density_log, data = data_test,
                             seed = 123, splitrule = "extratrees", replace = FALSE, sample.fraction = 1, mtry = function(n) n)
forest_Breiman1 <- ranger(staff_rangers_log ~ area_country_log + PA_area_log + pop_density_log, data = data_test, seed = 123, mtry = function(n) floor(n/3))
p1 <- plot_predict_interaction(forest_extra_trees, data = data_test, variable1 = "area_country_log", variable2 = "PA_area_log")
p2 <- plot_predict_interaction(forest_Breiman1, data = data_test, variable1 = "area_country_log", variable2 = "PA_area_log")
p1 + p2

# NOTE: these results justify our approach based on extremely randomized trees!


## Fine tuning the parameters for the RF ---------------------------------------

### let's define a custom function allowing for fine tuning any parameter (one at a time):


### Testing effect of min.node.size in RF
test_nodesize <- fine_tune_RF(values_to_try = 1:20, param_to_tune = "min.node.size",
                              formula = staff_rangers_log ~ area_country_log + PA_area_log + pop_density_log,
                              data = data_test, rep = n_tests, Ncpu = Ncpu,
                              splitrule = "extratrees", replace = FALSE, sample.fraction = 1, mtry = function(n) n)

pdf("./figs/FT_nodesize.pdf", width = 10, height = 10)
test_nodesize$plot
dev.off()

