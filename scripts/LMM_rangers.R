library(rangeRinPA)

options(width = 350)
Ncpu <- 120

## Run all LMM workflows and save results:
LMM_000 <- run_LMM_workflow(data = data_rangers, Ncpu = Ncpu, coef = 0)
save(LMM_000, file = "LMM_000.Rdata")

LMM_025 <- run_LMM_workflow(data = data_rangers, Ncpu = Ncpu, coef = 0.25)
save(LMM_025, file = "LMM_025.Rdata")

LMM_050 <- run_LMM_workflow(data = data_rangers, Ncpu = Ncpu, coef = 0.50)
save(LMM_050, file = "LMM_050.Rdata")

LMM_075 <- run_LMM_workflow(data = data_rangers, Ncpu = Ncpu, coef = 0.75)
save(LMM_075, file = "LMM_075.Rdata")

LMM_100 <- run_LMM_workflow(data = data_rangers, Ncpu = Ncpu, coef = 1)
save(LMM_100, file = "LMM_100.Rdata")

## Load results from LMM workflows:
files_to_load <- c("LMM_000.Rdata", "LMM_025.Rdata", "LMM_050.Rdata", "LMM_075.Rdata", "LMM_100.Rdata")
sapply(files_to_load, function(file) load(file, envir = .GlobalEnv))

## Process results
res <- extract_results(list_results_LMM = list(LMM_000, LMM_025, LMM_050, LMM_075, LMM_100))

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
