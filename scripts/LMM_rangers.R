library(spaMM)

fit_full <- fitme(staff_rangers_log ~ pop_density_log + country_UN_subcontinent + PA_area_log + area_country_log + area_forest_pct + GDP_2019_log + GDP_capita_log +
                    GDP_growth + unemployment_log + EVI + SPI + EPI_2020 + IUCN_1_4_prop + IUCN_1_2_prop, data = data_test)
step(fit_full)
