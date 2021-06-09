library(rangeRinPA)

options(width = 350)
Ncpu <- 120

## Run all LMM workflows and save results:
LMM_000 <- run_LMM_workflow(data = data_rangers, Ncpu = Ncpu, coef = 0)
save(LMM_000, file = "./scripts/results/LMM_000.Rdata")

LMM_025 <- run_LMM_workflow(data = data_rangers, Ncpu = Ncpu, coef = 0.25)
save(LMM_025, file = "./scripts/results/LMM_025.Rdata")

LMM_050 <- run_LMM_workflow(data = data_rangers, Ncpu = Ncpu, coef = 0.50)
save(LMM_050, file = "./scripts/results/LMM_050.Rdata")

LMM_075 <- run_LMM_workflow(data = data_rangers, Ncpu = Ncpu, coef = 0.75)
save(LMM_075, file = "./scripts/results/LMM_075.Rdata")

LMM_100 <- run_LMM_workflow(data = data_rangers, Ncpu = Ncpu, coef = 1)
save(LMM_100, file = "./scripts/results/LMM_100.Rdata")
