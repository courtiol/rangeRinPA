library(rangeRinPA)
#library(sf) ## to handle the geometry column in the data

## Set and create all directories to store files
path <- "inst/extdata/" ## set the path where you want to store all created files
if (!dir.exists(path)) stop("You must use an existing path")

path_tables <- paste0(path, "tables/")
if (!dir.exists(path_tables)) dir.create(path_tables)

path_figures <- paste0(path, "figures/")
if (!dir.exists(path_figures)) dir.create(path_figures)

path_predictions <- paste0(path, "predictions/")
if (!dir.exists(path_predictions)) dir.create(path_predictions)


Ncpu <- 50

LMM_100 <- run_LMM_workflow(data = data_rangers, Ncpu = Ncpu, coef = 1)
save(LMM_100, file = paste0(path_predictions, "LMM_100.Rdata"))
rm(LMM_100)
gc()

LMM_075 <- run_LMM_workflow(data = data_rangers, Ncpu = Ncpu, coef = 0.75)
save(LMM_075, file = paste0(path_predictions, "LMM_075.Rdata"))
rm(LMM_075)
gc()

LMM_050 <- run_LMM_workflow(data = data_rangers, Ncpu = Ncpu, coef = 0.50)
save(LMM_050, file = paste0(path_predictions, "LMM_050.Rdata"))
rm(LMM_050)
gc()

LMM_025 <- run_LMM_workflow(data = data_rangers, Ncpu = Ncpu, coef = 0.25)
save(LMM_025, file = paste0(path_predictions, "LMM_025.Rdata"))
rm(LMM_025)
gc()

LMM_000 <- run_LMM_workflow(data = data_rangers, Ncpu = Ncpu, coef = 0)
save(LMM_000, file = paste0(path_predictions, "LMM_000.Rdata"))
rm(LMM_000)
gc()

n_trees <- 2000

RF_100 <- run_RF_workflow(data = data_rangers, Ncpu = Ncpu, coef = 1, n_trees = n_trees)
save(RF_100, file = paste0(path_predictions, paste0("RF_100_", n_trees, ".Rdata")))
rm(RF_100)
gc()

RF_075 <- run_RF_workflow(data = data_rangers, Ncpu = Ncpu, coef = 0.75, n_trees = n_trees)
save(RF_075,file = paste0(path_predictions, paste0("RF_075_", n_trees, ".Rdata")))
rm(RF_075)
gc()

RF_050 <- run_RF_workflow(data = data_rangers, Ncpu = Ncpu, coef = 0.50, n_trees = n_trees)
save(RF_050, file = paste0(path_predictions, paste0("RF_050_", n_trees, ".Rdata")))
rm(RF_050)
gc()

RF_025 <- run_RF_workflow(data = data_rangers, Ncpu = Ncpu, coef = 0.25, n_trees = n_trees)
save(RF_025, file = paste0(path_predictions, paste0("RF_025_", n_trees, ".Rdata")))
rm(RF_025)
gc()

RF_000 <- run_RF_workflow(data = data_rangers, Ncpu = Ncpu, coef = 0, n_trees = n_trees)
save(RF_000, file = paste0(path_predictions, paste0("RF_000_", n_trees, ".Rdata")))
rm(RF_000)
gc()
