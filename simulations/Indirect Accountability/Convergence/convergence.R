source("code/HTEBayes/functions_simulations.R")
source("code/HTEBayes/model_CDBMM.R")
source("code/HTEBayes/model_BART_BCF_CART.R")

library(parallel)
library(pbmcapply)

# Simulation set-up
n <- 500          # units for each sample
samples <- 100    # repetition of each setting (number of samples)


load("simulations/Indirect Accountability/IA_scenario1.RData")
load("simulations/Indirect Accountability/IA_scenario2.RData")

#CDBMM
R <- 3000
R_burnin <- 2000

L_0 <- 12
L_1 <- 12

CDBMM_1 <- pbmclapply(1:5, CDBMM_Gibbs, data_sample = scenario_1, n = n, chains= T, mc.cores = 10)
CDBMM_2 <- pbmclapply(1:5, CDBMM_Gibbs, data_sample = scenario_2, n = n, chains= T, mc.cores = 10)

#BCF
Sys.setenv(
  OMP_NUM_THREADS = "1",
  MKL_NUM_THREADS = "1",
  OPENBLAS_NUM_THREADS = "1",
  VECLIB_MAXIMUM_THREADS = "1"
)

cl <- makeCluster(5)
clusterEvalQ(cl, {
  library(bcf)
})

logfile <- "~/bcf_progress.log"

clusterExport(
  cl,
  varlist = c("BCF_sample", "scenario_1", "logfile"),
  envir = environment()
)

BCF_1 <- parLapply(
  cl,
  1:5,
  function(c) {
    cat(sprintf("START %d %s\n", c, Sys.time()),
        file = logfile, append = TRUE)
    
    res <- BCF_sample(c, scenario_1, is_parallel = TRUE, mean = F)
    
    cat(sprintf("END   %d %s\n", c, Sys.time()),
        file = logfile, append = TRUE)
    
    res
  }
)

stopCluster(cl)

Sys.setenv(
  OMP_NUM_THREADS = "1",
  MKL_NUM_THREADS = "1",
  OPENBLAS_NUM_THREADS = "1",
  VECLIB_MAXIMUM_THREADS = "1"
)

cl <- makeCluster(5)
clusterEvalQ(cl, {
  library(bcf)
})

logfile <- "~/bcf_progress.log"

clusterExport(
  cl,
  varlist = c("BCF_sample", "scenario_2", "logfile"),
  envir = environment()
)

BCF_2 <- parLapply(
  cl,
  1:5,
  function(c) {
    cat(sprintf("START %d %s\n", c, Sys.time()),
        file = logfile, append = TRUE)
    
    res <- BCF_sample(c, scenario_2, is_parallel = TRUE, mean = F)
    
    cat(sprintf("END   %d %s\n", c, Sys.time()),
        file = logfile, append = TRUE)
    
    res
  }
)

stopCluster(cl)
