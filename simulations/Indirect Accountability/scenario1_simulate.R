source("code/HTEBayes/functions_simulations.R")
source("code/HTEBayes/model_CDBMM.R")
source("code/HTEBayes/model_BART_BCF_CART.R")

library(parallel)
library(pbmcapply)

# Simulation set-up
n <- 500          # units for each sample
samples <- 100    # repetition of each setting (number of samples)

# Corresponds to scenario 5 in the paper
scenario_1 <- lapply(1:samples, function(c) 
  simulation_sample_5cov(seed = c,
                         eta_0 = c(2,2,3,4.5,6.5),
                         eta_1 = c(0,1,2.5,5,7.5),
                         sigma_0 = rep(0.2,5),
                         sigma_1 = rep(0.2,5),
                         n = n))

load('simulations/Indirect Accountability/IA_scenario1.RData')

# Run simulations
R <- 3000
R_burnin <- 2000

L_0 <- 12
L_1 <- 12

CDBMM_scenario_1 <- pbmclapply(1:samples, CDBMM_Gibbs, data_sample = scenario_1, n = n, mc.cores = 10)

Sys.setenv(
  OMP_NUM_THREADS = "1",
  MKL_NUM_THREADS = "1",
  OPENBLAS_NUM_THREADS = "1",
  VECLIB_MAXIMUM_THREADS = "1"
)

cl <- makeCluster(4)
clusterEvalQ(cl, {
  library(bcf)
})

logfile <- "~/bcf_progress.log"

clusterExport(
  cl,
  varlist = c("BCF_sample", "scenario_1", "logfile"),
  envir = environment()
)

BCF_scenario_1 <- parLapply(
  cl,
  1:samples,
  function(c) {
    cat(sprintf("START %d %s\n", c, Sys.time()),
        file = logfile, append = TRUE)
    
    res <- BCF_sample(c, scenario_1, is_parallel = TRUE)
    
    cat(sprintf("END   %d %s\n", c, Sys.time()),
        file = logfile, append = TRUE)
    
    res
  }
)

stopCluster(cl)

CART_scenario_1 <- pbmclapply(1:samples, CART, data_sample = scenario_1, estimated_Y = BCF_scenario_1, mc.cores = 8)

