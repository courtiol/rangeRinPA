library(rangeRinPA)
library(tidyverse)

files_to_load <- c("LMM_000.Rdata", "LMM_025.Rdata", "LMM_050.Rdata", "LMM_075.Rdata", "LMM_100.Rdata")
sapply(paste0("scripts/results/", files_to_load), function(file) load(file, envir = .GlobalEnv))

files_to_load <- c("RF_000.Rdata", "RF_025.Rdata", "RF_050.Rdata", "RF_075.Rdata", "RF_100.Rdata")
sapply(paste0("scripts/results/", files_to_load), function(file) load(file, envir = .GlobalEnv))

res <- extract_results(list_results_LMM = list(LMM_000, LMM_025, LMM_050, LMM_075, LMM_100),
                       list_results_RF = list(RF_000, RF_025, RF_050, RF_075, RF_100))
res$who <- factor(res$who, levels = c("rangers", "others", "all"),
                  labels = c("Ranger staff", "Other staff", "All staff"))

res |>
  subset(select = c(who, type, coef, coverage, formula, spatial)) |>
  as.data.frame()


## Plot of retained predictors:
all_predictors <- c( "PA_area_log", "pop_density_log", "area_country_log", "long", "lat", "area_forest_pct","GDP_2019_log", "GDP_capita_log",
                    "GDP_growth", "unemployment_log", "EVI", "SPI", "EPI_2020", "IUCN_1_4_prop", "IUCN_1_2_prop")

res$predictor <- list(all_predictors)
res$predictor_included <- lapply(res$formula, \(x) all_predictors %in% all.vars(as.formula(x)[-2]))

res %>%
  unnest(c(predictor, predictor_included)) -> res_long

res_long$predictor <- factor(res_long$predictor, levels = rev(all_predictors))

ggplot(res_long) +
  aes(y = predictor, x = coef, shape = factor(predictor_included, levels = c("TRUE", "FALSE"))) +
  geom_point(size = 5) +
  scale_shape_manual(values = c("circle", "circle open")) +
  labs(x = "Relative density of staff in unsurveyed area", y = "Candidate predictor", shape = "Predictor selected") +
  facet_grid(who ~ type) +
  theme_bw()
ggsave("./scripts/figures/predictors_presence.pdf", scale = 0.7)

## Models for which spatial autocorrelation is selected:
res[res$spatial, ]

library(ggplot2)
library(scales)
library(ggsci)

ggplot(res) +
  aes(y = point_pred, x = as.factor(coef), fill = type, ymin = lwr, ymax = upr) +
  geom_col(position = "dodge", colour = "black", size = 0.2) +
  geom_linerange(position = position_dodge(width = 0.9), size = 0.5) +
  scale_y_continuous(breaks = (0:10) * 1e5, minor_breaks = (0:200) * 1e4, labels = scales::comma) +
  scale_fill_npg(guide = guide_legend(reverse = TRUE), alpha = 0.8) + # values = c("#52734D", "#FEFFDE", "#91C788")
  theme_minimal() +
  coord_flip() +
  labs(x = "Relative staff density in unsurveyed PA from surveyed country/territory",
       y = "Predicted number of staff",
       fill = "workforce") +
  facet_wrap(~ who, scales = "free")
