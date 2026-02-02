library(Rcpp)
library(MCMCpack)
library(rootSolve)
library(pbmcapply)

source("simulations/Direct Accountability/scenario1_function.R")
source("simulations/Direct Accountability/BPCF_sample.R")

sourceCpp("code/BPCF/MCMC_main.cpp", rebuild = T, verbose = T)

# Simulation set-up
n <- 500          # units for each sample
samples <- 100    # repetition of each setting (number of samples)

# Scenario 1 from BPCF paper
scenario_1 <- lapply(1:samples, function(c) simulate_scenario1_DA(P = 7, n = n, seed = c))

BPCF_scenario1 <- pbmclapply(1:samples, function(c) {
  BPCF_sample(data_sample = scenario_1, n = n, seed = c) } ,
  mc.cores = 8)
  
  BPCF_sample(scenario_1, n = n, seed = 1)


