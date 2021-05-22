library(rangeRinPA)

files_to_load <- c("LMM_000.Rdata", "LMM_025.Rdata", "LMM_050.Rdata", "LMM_075.Rdata", "LMM_100.Rdata")
sapply(files_to_load, function(file) load(file, envir = .GlobalEnv))
files_to_load <- c("RF_000.Rdata", "RF_025.Rdata", "RF_050.Rdata", "RF_075.Rdata", "RF_100.Rdata")
sapply(files_to_load, function(file) load(file, envir = .GlobalEnv))

res <- extract_results(list_results_LMM = list(LMM_000, LMM_025, LMM_050, LMM_075, LMM_100),
                       list_results_RF = list(RF_000, RF_025, RF_050, RF_075, RF_100))
res$who <- factor(res$who, levels = c("rangers", "others", "all"),
                  labels = c("Ranger staff", "Other staff", "All staff"))

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
