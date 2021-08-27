library(rangeRinPA)
library(sf) ## to handle the geometry column in the data

## Set and create all directories to store files
path <- "inst/extdata/" ## set the path where you want to store all created files
if (!dir.exists(path)) stop("You must use an existing path")

path_tables <- paste0(path, "tables/")
if (!dir.exists(path_tables)) dir.create(path_tables)

path_figures <- paste0(path, "figures/")
if (!dir.exists(path_figures)) dir.create(path_figures)

path_predictions <- paste0(path, "predictions/")
if (!dir.exists(path_predictions)) dir.create(path_predictions)


Ncpu <- 120

LMM_100 <- run_LMM_workflow(data = data_rangers, Ncpu = Ncpu, coef = 1)
save(LMM_100, file = paste0(path_predictions, "LMM_100.Rdata"))

LMM_075 <- run_LMM_workflow(data = data_rangers, Ncpu = Ncpu, coef = 0.75)
save(LMM_075, file = paste0(path_predictions, "LMM_075.Rdata"))

LMM_050 <- run_LMM_workflow(data = data_rangers, Ncpu = Ncpu, coef = 0.50)
save(LMM_050, file = paste0(path_predictions, "LMM_050.Rdata"))

LMM_025 <- run_LMM_workflow(data = data_rangers, Ncpu = Ncpu, coef = 0.25)
save(LMM_025, file = paste0(path_predictions, "LMM_025.Rdata"))

LMM_000 <- run_LMM_workflow(data = data_rangers, Ncpu = Ncpu, coef = 0)
save(LMM_000, file = paste0(path_predictions, "LMM_000.Rdata"))



RF_100 <- run_RF_workflow(data = data_rangers, Ncpu = Ncpu, coef = 1)
save(RF_100, file = paste0(path_predictions, "RF_100.Rdata"))

RF_075 <- run_RF_workflow(data = data_rangers, Ncpu = Ncpu, coef = 0.75)
save(RF_075,file = paste0(path_predictions, "RF_075.Rdata"))

RF_050 <- run_RF_workflow(data = data_rangers, Ncpu = Ncpu, coef = 0.50)
save(RF_050, file = paste0(path_predictions, "RF_050.Rdata"))

RF_025 <- run_RF_workflow(data = data_rangers, Ncpu = Ncpu, coef = 0.25)
save(RF_025, file = paste0(path_predictions, "RF_025.Rdata"))

RF_000 <- run_RF_workflow(data = data_rangers, Ncpu = Ncpu, coef = 0)
save(RF_000, file = paste0(path_predictions, "RF_000.Rdata"))
