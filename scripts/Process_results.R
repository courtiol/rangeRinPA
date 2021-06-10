library(rangeRinPA)
library(tidyverse)
library(scales)
library(ggsci)


## Loading results:
files_to_load <- c("LMM_000.Rdata", "LMM_025.Rdata", "LMM_050.Rdata", "LMM_075.Rdata", "LMM_100.Rdata")
sapply(paste0("scripts/results/", files_to_load), function(file) load(file, envir = .GlobalEnv))

files_to_load <- c("RF_000.Rdata", "RF_025.Rdata", "RF_050.Rdata", "RF_075.Rdata", "RF_100.Rdata")
sapply(paste0("scripts/results/", files_to_load), function(file) load(file, envir = .GlobalEnv))

extract_results(list_results_LMM = list(LMM_000, LMM_025, LMM_050, LMM_075, LMM_100),
               list_results_RF = list(RF_000, RF_025, RF_050, RF_075, RF_100), data = data_rangers) %>%
    mutate(who = str_to_title(who),
           who = factor(who, levels = c("All", "Rangers", "Others"))) -> res

## Table showing selected predictors:
res |>
  subset(select = c(who, type, coef, formula, spatial)) |>
  as.data.frame()


## Plot showing distribution of PAs per continent:
res |>
  filter(type == "LMM", coef == 0) |> ## note: changes very little across type and coef
  subset(select = c(who, PA_areas)) |>
  unnest_wider(PA_areas) |>
  unnest(-who) -> PA_areas_breakdown

PA_areas_breakdown |>
  pivot_longer(cols = starts_with("PA")) |>
  mutate(name = str_remove(name, "PA_area_")) |>
  mutate(name = fct_inorder(name)) -> PA_areas_breakdown_long

PA_areas_breakdown_long |>
  group_by(who, continent) |>
  mutate(total = sum(value),
         value = value / total) |>
  ungroup() -> PA_areas_breakdown_long_pct

PA_areas_breakdown_long_pct |>
  ggplot() +
    aes(y = value, x = total/2, fill = name, width = total) +
    geom_bar(stat = "identity") +
    facet_grid(who ~ continent, switch = "y") +
    coord_polar(theta = "y", start = 0, direction = 1) +
    theme_void() +
    scale_fill_npg() +
    labs(fill = "Number of staff:",
         title = "Distribution of protected areas") +
    theme(plot.title.position = "plot",
          plot.title = element_text(hjust = 0.5, vjust = 10),
          #strip.background = element_rect(fill = "grey95", colour = NA),
          #plot.background = element_rect(fill = "grey95", colour = NA),
          legend.position = "bottom",
          plot.margin = margin(t = 1, l = 0.5, unit = "cm"))
ggsave("./scripts/figures/PA_area_per_continent.pdf", width = 10, height = 6)


## Plot showing distribution of PAs global:
PA_areas_breakdown_long |>
  group_by(who, name) |>
  summarise(value = sum(value)) -> PA_areas_breakdown_long_world

PA_areas_breakdown_long_world |>
  group_by(who) |>
  mutate(total = sum(value),
         value = value / total) |>
  ungroup() -> PA_areas_breakdown_long_pct_world

PA_areas_breakdown_long_pct_world |>
  ggplot() +
    aes(y = value, x = total/2, fill = name, width = total) +
    geom_bar(stat = "identity") +
    facet_wrap(~ who) +
    coord_polar(theta = "y", start = 0, direction = 1) +
    theme_void() +
    scale_fill_npg() +
    labs(fill = "Number of staff:",
         title = "Distribution of protected areas") +
    theme(plot.title.position = "plot",
          plot.title = element_text(hjust = 0.5, vjust = 10),
          #strip.background = element_rect(fill = "grey95", colour = NA),
          #plot.background = element_rect(fill = "grey95", colour = NA),
          legend.position = "bottom",
          plot.margin = margin(t = 1, l = 0.5, unit = "cm"))
ggsave("./scripts/figures/PA_area_earth.pdf", width = 10, height = 4.5)


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
  theme_bw() +
  theme(legend.position = "bottom")
ggsave("./scripts/figures/predictors_presence.pdf", scale = 1, height = 10, width = 8)


## Table of models for which spatial autocorrelation is selected:
res[res$spatial, ]


## Plot of tallies:
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
  facet_wrap(~ who, scales = "free") +
  theme(legend.position = "bottom")
ggsave("./scripts/figures/predictions_across_analyses.pdf", width = 14, height = 9)

