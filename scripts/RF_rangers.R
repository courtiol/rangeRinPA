library(rangeRinPA)

options(width = 350)
Ncpu <- 120

## Run all RF workflows and save results:
RF_000 <- run_RF_workflow(data = data_rangers, Ncpu = Ncpu, coef = 0)
save(RF_000, file = "RF_000.Rdata")

RF_025 <- run_RF_workflow(data = data_rangers, Ncpu = Ncpu, coef = 0.25)
save(RF_025, file = "RF_025.Rdata")

RF_050 <- run_RF_workflow(data = data_rangers, Ncpu = Ncpu, coef = 0.50)
save(RF_050, file = "RF_050.Rdata")

RF_075 <- run_RF_workflow(data = data_rangers, Ncpu = Ncpu, coef = 0.75)
save(RF_075, file = "RF_075.Rdata")

RF_100 <- run_RF_workflow(data = data_rangers, Ncpu = Ncpu, coef = 1)
save(RF_100, file = "RF_100.Rdata")

## Load results from RF workflows:
files_to_load <- c("RF_000.Rdata", "RF_025.Rdata", "RF_050.Rdata", "RF_075.Rdata", "RF_100.Rdata")
sapply(files_to_load, function(file) load(file, envir = .GlobalEnv))

## Process results
res <- extract_results(list_results_RF = list(RF_000, RF_025, RF_050, RF_075, RF_100))

library(ggplot2)
library(scales)
library(ggsci)

ggplot(res) +
  aes(y = point_pred, x = as.factor(coef), fill = who, ymin = lwr, ymax = upr) +
  geom_col(position = "dodge", colour = "black", size = 0.2) +
  geom_linerange(position = position_dodge(width = 0.9), size = 0.5) +
  scale_y_continuous(breaks = (0:10) * 1e5, minor_breaks = (0:200) * 1e4, labels = scales::comma) +
  scale_fill_npg(guide = guide_legend(reverse = TRUE), alpha = 0.8) + # values = c("#52734D", "#FEFFDE", "#91C788")
  theme_minimal() +
  coord_flip() +
  labs(x = "Relative staff density in unsurveyed PA from surveyed country/territory",
       y = "Predicted number of staff",
       fill = "workforce")


