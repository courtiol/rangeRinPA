library(rangeRinPA)

set.seed(123)
Ncpu <- 200
n_trees <- 5000

RF_100 <- run_RF_workflow(data = data_rangers, Ncpu = Ncpu, coef = 1, n_trees = n_trees)
save(RF_100, file = paste0("RF_100_", n_trees, ".Rdata"))
rm(RF_100)
gc()

RF_075 <- run_RF_workflow(data = data_rangers, Ncpu = Ncpu, coef = 0.75, n_trees = n_trees)
save(RF_075,file = paste0("RF_075_", n_trees, ".Rdata"))
rm(RF_075)
gc()

RF_050 <- run_RF_workflow(data = data_rangers, Ncpu = Ncpu, coef = 0.50, n_trees = n_trees)
save(RF_050, file = paste0("RF_050_", n_trees, ".Rdata"))
rm(RF_050)
gc()

RF_025 <- run_RF_workflow(data = data_rangers, Ncpu = Ncpu, coef = 0.25, n_trees = n_trees)
save(RF_025, file = paste0("RF_025_", n_trees, ".Rdata"))
rm(RF_025)
gc()

RF_000 <- run_RF_workflow(data = data_rangers, Ncpu = Ncpu, coef = 0, n_trees = n_trees)
save(RF_000, file = paste0("RF_000_", n_trees, ".Rdata"))
rm(RF_000)
gc()
