# load('simulations/Indirect Accountability/IA_scenario1.RData')
# load('simulations/Indirect Accountability/IA_scenario2.RData')
# load('simulations/Indirect Accountability/CDBMM_scenario1.RData')
# load('simulations/Indirect Accountability/CDBMM_scenario2.RData')

compute_ATE <- function(tau_hat, tau_true) {
  ate_hat  <- mean(tau_hat)
  ate_true <- mean(tau_true)
  
  list(
    bias = ate_hat - ate_true,
    mse  = (ate_hat - ate_true)^2
  )
}

compute_CATE <- function(tau_hat, tau_true) {
  diff <- tau_hat - tau_true
  
  list(
    bias = mean(diff),
    mse  = mean(diff^2)
  )
}

compute_GATE <- function(tau_hat, tau_true, groups) {
  gate_hat  <- tapply(tau_hat,  groups, mean)
  gate_true <- tapply(tau_true, groups, mean)
  
  diff <- gate_hat - gate_true
  
  list(
    bias = mean(diff),
    mse  = mean(diff^2)
  )
}

# Simulated ITE
simulated_tau_1 <- sapply(1:samples, function(s) scenario_1[[s]]$data$Y[2,] - scenario_1[[s]]$data$Y[1,])
simulated_tau_2 <- sapply(1:samples, function(s) scenario_2[[s]]$data$Y[2,] - scenario_2[[s]]$data$Y[1,])

# ATE --------------------
# Bias for ATE
bias_ATE_CDBMM_1 <- sapply(1:samples, function(s)
  compute_ATE(
    tau_hat  = CDBMM_scenario_1[[s]]$tau,
    tau_true = simulated_tau_1[, s]
  )$bias
)

bias_ATE_CDBMM_2 <- sapply(1:samples, function(s)
  compute_ATE(
    tau_hat  = CDBMM_scenario_2[[s]]$tau,
    tau_true = simulated_tau_2[, s]
  )$bias
)

# MSE for ATE
mse_ATE_CDBMM_1 <- sapply(1:samples, function(s)
  compute_ATE(
    tau_hat  = CDBMM_scenario_1[[s]]$tau,
    tau_true = simulated_tau_1[, s]
  )$mse
)

mse_ATE_CDBMM_2 <- sapply(1:samples, function(s)
  compute_ATE(
    tau_hat  = CDBMM_scenario_2[[s]]$tau,
    tau_true = simulated_tau_2[, s]
  )$mse
)

# CATE ---------------
# Bias for CATE
bias_CATE_CDBMM_1 <- sapply(1:samples, function(s)
  compute_CATE(
    CDBMM_scenario_1[[s]]$tau,
    simulated_tau_1[, s]
  )$bias
)

# MSE for CATE
mse_CATE_CDBMM_1 <- sapply(1:samples, function(s)
  compute_CATE(
    CDBMM_scenario_1[[s]]$tau,
    simulated_tau_1[, s]
  )$mse
)


