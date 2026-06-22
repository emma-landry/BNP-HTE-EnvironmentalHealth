library(Rcpp)
library(MCMCpack)
library(rootSolve)
library(pbmcapply)

source("simulations/Direct Accountability/scenario1_function.R")
source("code/PSBayes/simulation_functions.R")
source("simulations/Direct Accountability/BPCF_sample.R")
source("code/PSBayes/CASBAH.R")

n <- 500          # units for each sample
samples <- 100    # repetition of each setting (number of samples)

load("simulations/Direct Accountability/DA_scenario1.RData")
load("simulations/Direct Accountability/DA_scenario2.RData")

BPCF_1 <- pbmclapply(1:5, function(c) {
                     BPCF_sample(data_sample = scenario_1, n = n, seed = c, scenario1 = T, median = F) },
                     mc.cores = 10)
BPCF_2 <- pbmclapply(1:5, function(c) {
                     BPCF_sample(data_sample = scenario_2, n = n, seed = c, scenario1 = F, median = F) },
                     mc.cores = 10)

R <- 3000
R_burnin <- 1500
n_cluster <- 10 

CASBAH_1 <- pbmclapply(1:5, function(c) {
                       Gibbs_CASDMM(c = c, sim = scenario_1, scenario1 = T, median = F) },
                       mc.cores = 10)
CASBAH_2 <- pbmclapply(1:5, function(c) {
                       Gibbs_CASDMM(c = c, sim = scenario_2, scenario1 = F, median = F) },
                       mc.cores = 10)
