library(rangeRinPA)

options(width = 350)

LMM_000 <- run_LMM_workflow(data = data_rangers, Ncpu = 100, coef = 0)
save(LMM_000, file = "LMM_000.Rdata")

LMM_025 <- run_LMM_workflow(data = data_rangers, Ncpu = 100, coef = 0.25)
save(LMM_025, file = "LMM_025.Rdata")

LMM_050 <- run_LMM_workflow(data = data_rangers, Ncpu = 100, coef = 0.50)
save(LMM_050, file = "LMM_050.Rdata")

LMM_075 <- run_LMM_workflow(data = data_rangers, Ncpu = 100, coef = 0.75)
save(LMM_075, file = "LMM_075.Rdata")

LMM_100 <- run_LMM_workflow(data = data_rangers, Ncpu = 100, coef = 1)
save(LMM_100, file = "LMM_100.Rdata")

