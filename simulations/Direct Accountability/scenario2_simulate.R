library(Rcpp)
library(MCMCpack)
library(rootSolve)
library(pbmcapply)

source("code/PSBayes/simulation_functions.R")
source("code/PSBayes/CASBAH.R")
source("simulations/Direct Accountability/BPCF_sample.R")

sourceCpp("code/BPCF/MCMC_main.cpp", rebuild = T, verbose = T)

# Simulation set-up
n <- 500          # units for each sample
samples <- 100    # repetition of each setting (number of samples)

# Scenario 2 from CASBAH paper
scenario_2 <- lapply(1:samples, function(s) 
                      prova=setup_sim_2cov(seed=s,
                       eta=c(1,2,3),
                       sigma_p=rep(0.05,3),
                       allocation_0=c(2,2,2),
                       allocation_1=c(1,2,3),
                       beta_0=c(1,2),
                       beta_1=c(1,1.2,-1,1),
                       sigma_y=c(-0.5,0.1)))


# BPCF
BPCF_scenario2 <- pbmclapply(1:samples, function(c) {
  BPCF_sample(data_sample = scenario_2, n = n, seed = c, scenario1 = F) },
  mc.cores = 10)

# CASBAH
R <- 3000
R_burnin <- 1500
n_cluster <- 10 # max number of clusters

CASBAH_scenario2 <- pbmclapply(1:samples, function(c) {
  Gibbs_CASDMM(c = c, sim = scenario_2, scenario1 = F) },
  mc.cores = 10)

