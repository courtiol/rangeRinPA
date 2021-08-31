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


## Tables showing predictions:
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



