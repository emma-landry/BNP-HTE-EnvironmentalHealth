source("simulations/Indirect Accountability/scenario2_function.R")
source("code/HTEBayes/model_CDBMM.R")
source("code/HTEBayes/model_BART_BCF_CART.R")

library(parallel)
library(pbmcapply)

# Simulation set-up
n <- 500          # units for each sample
samples <- 100    # repetition of each setting (number of samples)

# Corresponds to scenario 5 in the paper
scenario_2 <- lapply(1:samples, function(c) 
    generate_data(n = n,
                  p = 5,
                  tau = 'heterogeneous',
                  mu = 'nonlinear',
                  seed = c))

load('simulations/Indirect Accountability/IA_scenario2.RData')
# Run simulations
R <- 3000
R_burnin <- 2000

L_0 <- 12
L_1 <- 12

CDBMM_scenario_2 <- pbmclapply(1:samples, CDBMM_Gibbs, data_sample = scenario_2, n = n, mc.cores = 8)

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

logfile <- "~/bcf_progress_scenario2.log"

clusterExport(
  cl,
  varlist = c("BCF_sample", "scenario_2", "logfile"),
  envir = environment()
)

BCF_scenario_2 <- parLapply(
  cl,
  1:samples,
  function(c) {
    cat(sprintf("START %d %s\n", c, Sys.time()),
        file = logfile, append = TRUE)
    
    res <- BCF_sample(c, scenario_2, is_parallel = TRUE)
    
    cat(sprintf("END   %d %s\n", c, Sys.time()),
        file = logfile, append = TRUE)
    
    res
  }
)

save(BCF_scenario_2, file = '/Users/emmalandry/Documents/Falco GSR/Review Paper- CEHR/CausalBayes_Review/simulations/Indirect Accountability/BCF_scenario2.RData')

stopCluster(cl)


BCF_scenario_2 <- lapply(1:samples, function(s) {
  cat("Running sample", s, "\n")
  BCF_sample(s, data_sample = scenario_2)})

CART_scenario_2 <- pbmclapply(1:samples, CART, data_sample = scenario_1, estimated_Y = BCF_scenario_2, mc.cores = 8)