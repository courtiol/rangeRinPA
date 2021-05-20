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
extract_results <- function(what, who) {
  data.frame(type = "LMM",
    coef = what$meta$coef_population,
    rerank = what$meta$rerank,
    Ncpu = what$meta$Ncpu,
    run_time = what$meta$duration_h,
    point_pred = what[[who]]$tally_total,
    lwr = what[[who]]$lwr[[1]],
    upr = what[[who]]$upr[[1]],
    coverage = with(what[[who]], (PA_area_obs_or_imputed + PA_area_predict) / (PA_area_obs_or_imputed + PA_area_predict + PA_area_no_predict))
  )
}

extract_all_results <- function(list_results) {
  rangers_list <- lapply(list_results, function(x) extract_results(what = x, who = "rangers"))
  others_list  <- lapply(list_results, function(x) extract_results(what = x, who = "others"))
  all_list     <- lapply(list_results, function(x) extract_results(what = x, who = "all"))
  rbind(cbind(who = "rangers", as.data.frame(do.call("rbind", rangers_list))),
        cbind(who = "others",  as.data.frame(do.call("rbind", others_list))),
        cbind(who = "all",     as.data.frame(do.call("rbind", all_list)))) -> d
  d$who <- as.factor(d$who)
  d
}

res <- extract_all_results(list_results = list(LMM_000, LMM_025, LMM_050, LMM_075, LMM_100))

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
