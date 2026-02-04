library(Rcpp)
library(MCMCpack)
library(rootSolve)
library(pbmcapply)

source("simulations/Direct Accountability/scenario1_function.R")
source("simulations/Direct Accountability/BPCF_sample.R")
source("code/PSBayes/CASBAH.R")


sourceCpp("code/BPCF/MCMC_main.cpp", rebuild = T, verbose = T)

# Simulation set-up
n <- 500          # units for each sample
samples <- 100    # repetition of each setting (number of samples)

# Scenario 1 from BPCF paper
scenario_1 <- lapply(1:samples, function(c) simulate_scenario1_DA_alt(P = 7, n = n, seed = c))

load("simulations/Direct Accountability/DA_scenario1_alt.RData")

BPCF_scenario1 <- pbmclapply(1:samples, function(c) {
  BPCF_sample(data_sample = scenario_1, n = n, seed = c, scenario1 = T) },
  mc.cores = 10)


# CASBAH
R <- 3000
R_burnin <- 1500
n_cluster <- 10 # max number of clusters

CASBAH_scenario1 <- pbmclapply(1:samples, function(c) {
  Gibbs_CASDMM(c = c, sim = scenario_1, scenario1 = T) },
  mc.cores = 10)
save(CASBAH_scenario1, file = "/Users/emmalandry/Documents/Falco_GSR/ReviewPaper_CEHR/CausalBayes_Review/simulations/Direct Accountability/CASBAH_scenario1.RData")



