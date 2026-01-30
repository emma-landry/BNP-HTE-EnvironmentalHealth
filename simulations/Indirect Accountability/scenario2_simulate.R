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

# Run simulations
R <- 3000
R_burnin <- 2000

L_0 <- 12
L_1 <- 12

CDBMM_scenario_2 <- pbmclapply(1:samples, CDBMM_Gibbs, data_sample = scenario_2, n = n, mc.cores = 8)

BCF_scenario_2 <- lapply(1:samples, function(s) {
  cat("Running sample", s, "\n")
  BCF_sample(s, data_sample = scenario_2)})

CART_scenario_2 <- pbmclapply(1:samples, CART, data_sample = scenario_1, estimated_Y = BCF_scenario_2, mc.cores = 8)