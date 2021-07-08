library(rangeRinPA)
library(tidyverse)
library(scales)
library(ggrepel)
library(ggsci)
library(rnaturalearth)
library(sf)


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

## Tables showing predicitons:
res |>
  unnest(pred_details) |>
  group_by(who, continent) |>
  summarize(known = mean(known, na.rm = TRUE),
            predicted = mean(predicted, na.rm = TRUE),
            total = known + predicted) |>
  ungroup() -> table_pred_breakdown

table_pred_breakdown |>
  group_by(who) |>
   summarize(known = sum(known, na.rm = TRUE),
             predicted = sum(predicted, na.rm = TRUE),
             total = known + predicted) |>
  mutate(continent = "Earth") |>
  full_join(table_pred_breakdown, by = c("who", "known", "predicted", "total", "continent")) |>
  relocate(continent, .before = 2) |>
  rename(Location = continent, Staff = who, Surveyed = known, Predicted = predicted, Total = total) |>
  mutate(Location = factor(Location,
                           levels = c("Earth", "Asia", "Europe", "Northern America", "Africa", "Latin America & Caribbean", "Oceania"))) |>
  arrange(Staff, Location) -> table_pred_breakdown_with_earth

print(table_pred_breakdown_with_earth, n = Inf)
write_csv(table_pred_breakdown_with_earth, "scripts/tables/table_prediction.csv")

table_pred_breakdown_with_earth |>
  select(-Surveyed, -Predicted) |>
  pivot_wider(names_from = Location, values_from = Total) -> table_pred_total_with_earth

print(table_pred_total_with_earth, n = Inf)
write_csv(table_pred_total_with_earth, "scripts/tables/table_prediction_simple.csv")

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
ggsave("./scripts/figures/PA_area_per_continent.png", width = 10, height = 6)

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
ggsave("./scripts/figures/PA_area_earth.png", width = 10, height = 4.5)



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
ggsave("./scripts/figures/predictors_presence.png", scale = 1, height = 10, width = 8)

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
ggsave("./scripts/figures/predictions_across_analyses.png", width = 14, height = 9)

## Plot predictors:
options("ggrepel.max.overlaps" = Inf)

data_rangers |>
  fill_PA_area(coef = 0.5) |>
  ggplot() +
  aes(y = staff_total, x = area_PA_total, label = countryname_iso, colour = country_UN_continent) +
  geom_text_repel(key_glyph = "point", alpha = 0.35, size = 3) +
  geom_point() +
  coord_trans(x = "log", y = "log") +
  scale_colour_npg() +
  scale_x_continuous(breaks = 10^(1:7), minor_breaks = NULL, limits = c(5, 0.3e7), labels = label_number(accuracy = 1)) +
  scale_y_continuous(breaks = 10^(0:5), minor_breaks = NULL, limits = c(1, 1e5), labels = label_number(accuracy = 1)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(y = "Total number of staff in surveyed countries/territories",
       x = expression(paste("Total surface of protected areas for the country/territory in km"^"2")),
       colour = "Continent:")
ggsave("./scripts/figures/predictor_PA_area.pdf", width = 13, height = 9, scale = 0.7)
ggsave("./scripts/figures/predictor_PA_area.png", width = 13, height = 9, scale = 0.7)

data_rangers |>
  fill_PA_area(coef = 0.5) |>
  ggplot() +
  aes(y = staff_total, x = area_country, label = countryname_iso, colour = country_UN_continent) +
  geom_text_repel(key_glyph = "point", alpha = 0.35, size = 3) +
  geom_point() +
  coord_trans(x = "log", y = "log") +
  scale_colour_npg() +
  scale_x_continuous(breaks = 10^(1:7), minor_breaks = NULL, limits = c(50, 0.2e8), labels = label_number(accuracy = 1)) +
  scale_y_continuous(breaks = 10^(0:5), minor_breaks = NULL, limits = c(1, 1e5), labels = label_number(accuracy = 1)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(y = "Total number of staff in surveyed countries/territories",
       x = expression(paste("Surface of the country/territory in km"^"2")),
       colour = "Continent:")
#ggsave("./scripts/figures/predictor_country_area.pdf", width = 13, height = 9, scale = 0.7) ## useless? -> driven by collinearity

data_rangers |>
  fill_PA_area(coef = 0.5) |>
  ggplot() +
  aes(y = area_PA_total / staff_total, x = area_PA_total, label = countryname_iso, colour = country_UN_continent) +
  geom_text_repel(key_glyph = "point", alpha = 0.35, size = 3) +
  geom_point() +
  coord_trans(y = "log", x = "log") +
  scale_colour_npg() +
  scale_x_continuous(breaks = 10^(1:7), minor_breaks = NULL, limits = c(5, 0.3e7), labels = label_number(accuracy = 1)) +
  scale_y_continuous(breaks = 10^(0:5), minor_breaks = NULL, limits = c(0.5, 1e5), labels = label_number(accuracy = 1)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(y = expression(paste("Surface of protected areas in km"^"2", " per staff member")),
       x = expression(paste("Total surface of protected areas for the country/territory in km"^"2")),
       colour = "Continent:")
ggsave("./scripts/figures/predictor_relative_PA_area.pdf", width = 13, height = 9, scale = 0.7)
ggsave("./scripts/figures/predictor_relative_PA_area.png", width = 13, height = 9, scale = 0.7)

data_rangers |>
  fill_PA_area(coef = 0.5) |>
  filter(!is.na(staff_total)) -> d

fit <- lm(log(staff_total + 1) ~ log(area_PA_total + 1), data = d)
confint(fit)
d$staff_total_resid <- resid(fit)

d |>
  ggplot() +
  aes(y = staff_total_resid, x = pop_density, label = countryname_iso, colour = country_UN_continent) +
  geom_text_repel(key_glyph = "point", alpha = 0.35, size = 3) +
  geom_point() +
  coord_trans(x = "log") +
  scale_colour_npg() +
  scale_x_continuous(breaks = 10^(0:4), minor_breaks = NULL, limits = c(0.1, 1e4), labels = label_number(accuracy = 1)) +
  scale_y_continuous(breaks = seq(-6, 4, 1), minor_breaks = NULL, limits = c(-6, 4), labels = label_number(accuracy = 2)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(y = "Residual number of staff in surveyed countries/territories\n(after accounting for the effect of the surface of protected areas)",
       x = expression(paste("Population density in people per km"^"2")),
       colour = "Continent:")
ggsave("./scripts/figures/predictor_pop_density.pdf", width = 13, height = 9, scale = 0.7)
ggsave("./scripts/figures/predictor_pop_density.png", width = 13, height = 9, scale = 0.7)

d |>
  ggplot() +
  aes(y = staff_total_resid, x = area_country, label = countryname_iso, colour = country_UN_continent) +
  geom_text_repel(key_glyph = "point", alpha = 0.35, size = 3) +
  geom_point() +
  coord_trans(x = "log") +
  scale_colour_npg() +
  scale_x_continuous(breaks = 10^(0:8), minor_breaks = NULL, limits = c(50, 4e7), labels = label_number(accuracy = 1)) +
  scale_y_continuous(breaks = seq(-6, 4, 1), minor_breaks = NULL, limits = c(-6, 4), labels = label_number(accuracy = 2)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(y = "Residual number of staff in surveyed countries/territories\n(after accounting for the effect of the surface of protected areas)",
       x = expression(paste("Total surface of country/territory in km"^"2")),
       colour = "Continent:")
ggsave("./scripts/figures/predictor_area_country.pdf", width = 13, height = 9, scale = 0.7)
ggsave("./scripts/figures/predictor_area_country.png", width = 13, height = 9, scale = 0.7)

# Map of the sampling

world_sf <- ne_countries(scale = "medium", returnclass = "sf")
i <- which(names(world_sf) != "geometry")
names(world_sf)[i] <- paste0("rne_", names(world_sf)[i])

## check locations not found in map (depend on scale defined above):
data_rangers %>%
 anti_join(world_sf, by = c(countryname_iso = "rne_iso_a3")) %>%
 pull(countryname_eng)

## only keep locations found in map:
data_rangers %>%
 right_join(world_sf, by = c(countryname_iso = "rne_iso_a3")) %>%
 st_as_sf() -> world_rangers

world_rangers %>%
  mutate(PA_area_surveyed = ifelse(PA_area_surveyed == 0, NA, PA_area_surveyed)) -> world_rangers

## applying projection:
world_rangers %>%
 st_transform(crs = "+proj=moll") -> world_rangers

ggplot() +
  geom_sf(mapping = aes(fill = PA_area_surveyed / (PA_area_surveyed + PA_area_unsurveyed)),
          data = world_rangers, colour = "black", size = 0.05) +
  scale_fill_fermenter(palette = 2,
                       direction = 1,
                       breaks = seq(0, 0.8, 0.2),
                       guide = guide_colorsteps(title = "Proportion of PA area surveyed",
                                                title.vjust = 1, barwidth = 10,
                                                label.theme = element_text(angle = 0),
                                                label.hjust = 0.5, label.vjust = 1)) +
  scale_x_continuous(breaks = seq(-180, 180, 30)) +
  theme_minimal() +
  theme(legend.position = "bottom", panel.grid = element_line(colour = "GREY", size = 0.3),
        plot.title = element_text(size = 20, hjust = 0.5))
ggsave("./scripts/figures/design.pdf", width = 13, height = 9, scale = 0.7)
ggsave("./scripts/figures/design.png", width = 13, height = 9, scale = 0.7)


## Exploration density rangers
data_rangers |>
#  filter(!countryname_eng %in% c("Greenland")) |>
  fill_PA_area(coef = 1) |>
  mutate(km2_per_staff = area_PA_total / staff_total) |>
  filter(!is.na(km2_per_staff)) |>
  select(countryname_iso, countryname_eng, km2_per_staff, country_UN_continent, area_country, area_PA_total) |>
  arrange(km2_per_staff) -> d

order_continents <- rev(c("World", "Latin America \n& Caribbean", "Africa", "Oceania", "Asia", "Europe", "Northern\n America")) #\n (incl. Greenland)

d_All <- d
d_All |>
  mutate(country_UN_continent = "World") |>
  bind_rows(d) |>
  mutate(country_UN_continent = ifelse(country_UN_continent == "Latin America & Caribbean", "Latin America \n& Caribbean", country_UN_continent)) |>
  mutate(country_UN_continent = ifelse(country_UN_continent == "Northern America", "Northern\n America", country_UN_continent)) |> #\n (incl. Greenland)
  mutate(country_UN_continent = factor(country_UN_continent, levels = order_continents)) -> dd

dd |>
  filter(!countryname_eng %in% c("Greenland")) |>
  group_by(country_UN_continent) |>
  summarise(mean = weighted.mean(km2_per_staff, area_PA_total),
            good = sum(area_PA_total[km2_per_staff <= 5]),
            bad = sum(area_PA_total[km2_per_staff > 5])) |>
  mutate(country_UN_continent = factor(country_UN_continent, levels =  order_continents)) -> dd_mean

dd_mean |>
  pivot_longer(cols = c("good", "bad")) |>
  group_nest(country_UN_continent) |>
  rowwise() |>
  mutate(gg = list(ggplot(data) +
    aes(y = value, x = "", fill = name) +
    geom_bar(stat = "identity", position = "stack", show.legend = FALSE) +
    coord_polar(theta = "y", start = 0, direction = 1) +
    theme_void())) -> data_pies

dd |>
  filter(countryname_eng %in% c("Greenland")) -> dd_green

dd |>
  filter(!countryname_eng %in% c("Greenland")) |>
  ggplot() +
  #geom_point(aes(x = km2_per_staff, y = country_UN_continent, size = area_PA_total),
  #           shape = 8, data = dd_green) +
  geom_segment(aes(y = mean, x = country_UN_continent, yend = 5, xend = country_UN_continent),
               arrow = arrow(length = unit(0.3, "cm")), data = dd_mean) +
  geom_point(aes(y = mean, x = country_UN_continent, fill = country_UN_continent),
             shape = 23, colour = "black", size = 3, data = dd_mean) +
  geom_text(aes(y = mean, x = country_UN_continent, label = round(mean)), nudge_x = 0.3, size = 6, data = dd_mean) +
  geom_jitter(aes(y = km2_per_staff, x = country_UN_continent, size = area_PA_total, alpha = km2_per_staff < 5,
                  colour = country_UN_continent, fill = country_UN_continent),
              shape = 21,
              position = position_jitter(seed = 1L, width = 0.15, height = 0)) +
  geom_hline(yintercept = 5, colour = "darkgreen") +
  scale_y_continuous(limits = c(5000, 0.01), breaks = c(5, 10^(0:3)), minor_breaks = NULL, labels = label_number(accuracy = 1), trans = "reverse") +
  scale_x_discrete(position = "top") +
  scale_colour_npg() +
  scale_fill_npg() +
  scale_alpha_discrete(range = c(0.3, 0.95)) +
  coord_trans(y = "pseudo_log") +
  labs(x = "Geographic area", y = expression(paste("Surface of protected area per individual staff (km"^"2", ")"))) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank()) +
  guides(colour = "none", size = "none", fill = "none", alpha = "none") -> main_plot

main_plot +
  patchwork::inset_element(data_pies$gg[[1]], 0, 0.025, 1/6, 0.1,                 align_to = "panel") +
  patchwork::inset_element(data_pies$gg[[2]], 0, 0.025, 1/6 + (1 - 1/6)*1/3, 0.1, align_to = "panel") +
  patchwork::inset_element(data_pies$gg[[3]], 0, 0.025, 1/6 + (1 - 1/6)*2/3, 0.1, align_to = "panel") +
  patchwork::inset_element(data_pies$gg[[4]], 0, 0.025, 1, 0.1,                   align_to = "panel") +
  patchwork::inset_element(data_pies$gg[[5]], 0, 0.025, 1 + (1 - 1/6)*1/3, 0.1,   align_to = "panel") +
  patchwork::inset_element(data_pies$gg[[6]], 0, 0.025, 1 + (1 - 1/6)*2/3, 0.1,   align_to = "panel") +
  patchwork::inset_element(data_pies$gg[[7]], 0, 0.025, 1 + (1 - 1/6), 0.1,       align_to = "panel")

ggsave("./scripts/figures/density.png", width = 15, height = 9, scale = 0.7)
ggsave("./scripts/figures/density.pdf", width = 15, height = 9, scale = 0.7)


dd %>%
  filter(country_UN_continent == "World") %>%
  summarise(p = 100 * sum(area_PA_total[km2_per_staff < 5]) / sum(area_PA_total))

