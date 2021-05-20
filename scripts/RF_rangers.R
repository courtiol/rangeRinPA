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
