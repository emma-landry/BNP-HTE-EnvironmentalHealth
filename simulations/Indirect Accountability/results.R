load('simulations/Indirect Accountability/IA_scenario1.RData')
load('simulations/Indirect Accountability/IA_scenario2.RData')
load('simulations/Indirect Accountability/CDBMM_scenario1.RData')
load('simulations/Indirect Accountability/CDBMM_scenario2.RData')
load('simulations/Indirect Accountability/BCF_scenario1.RData')
load('simulations/Indirect Accountability/BCF_scenario2.RData')
load('simulations/Indirect Accountability/CART_scenario1.RData')
load('simulations/Indirect Accountability/CART_scenario2.RData')

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

compute_CDBMM_groups <- function(partition) {
  stopifnot(
    is.matrix(partition),
    ncol(partition) == 2
  )
  
  interaction(
    partition[, 1],
    partition[, 2],
    drop = TRUE
  )
}

# Sizes
n <- 500
samples <- 100

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

bias_ATE_BCF_1 <- sapply(1:samples, function(s)
  compute_ATE(
    tau_hat  = BCF_scenario_1[[s]]$tau,
    tau_true = simulated_tau_1[, s]
  )$bias
)

bias_ATE_BCF_2 <- sapply(1:samples, function(s)
  compute_ATE(
    tau_hat  = BCF_scenario_2[[s]]$tau,
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

mse_ATE_BCF_1 <- sapply(1:samples, function(s)
  compute_ATE(
    tau_hat  = BCF_scenario_1[[s]]$tau,
    tau_true = simulated_tau_1[, s]
  )$mse
)

mse_ATE_BCF_2 <- sapply(1:samples, function(s)
  compute_ATE(
    tau_hat  = BCF_scenario_2[[s]]$tau,
    tau_true = simulated_tau_2[, s]
  )$mse
)

# Figures

library(ggplot2)

bias_df <- rbind(
  data.frame(
    value    = bias_ATE_CDBMM_1,
    Method   = "CDBMM",
    Scenario = "Scenario 1"
  ),
  data.frame(
    value    = bias_ATE_BCF_1,
    Method   = "BCF",
    Scenario = "Scenario 1"
  ),
  data.frame(
    value    = bias_ATE_CDBMM_2,
    Method   = "CDBMM",
    Scenario = "Scenario 2"
  ),
  data.frame(
    value    = bias_ATE_BCF_2,
    Method   = "BCF",
    Scenario = "Scenario 2"
  )
)

bias_df$Scenario <- ifelse(
  bias_df$Scenario == "Scenario 1",
  "Scenario 2",
  "Scenario 1"
)

bias_df$Scenario <- factor(bias_df$Scenario)


ggplot(bias_df, aes(x = Method, y = value, fill = Method)) +
  geom_boxplot(width = 0.6, outlier.size = 0.8) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.7) +
  facet_wrap(~ Scenario, scales = "free_y") +
  labs(
    y = "Bias (ATE)",
    x = NULL
  ) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "none",
    strip.text = element_text(size = 12)
  )
ggsave('/Users/emmalandry/Documents/Falco_GSR/ReviewPaper_CEHR/IA_bias_ATE.pdf', width = 10, height = 4.5)

rmse_df <- rbind(
  data.frame(
    value    = sqrt(mse_ATE_CDBMM_1),
    Method   = "CDBMM",
    Scenario = "Scenario 1"
  ),
  data.frame(
    value    = sqrt(mse_ATE_BCF_1),
    Method   = "BCF",
    Scenario = "Scenario 1"
  ),
  data.frame(
    value    = sqrt(mse_ATE_CDBMM_2),
    Method   = "CDBMM",
    Scenario = "Scenario 2"
  ),
  data.frame(
    value    = sqrt(mse_ATE_BCF_2),
    Method   = "BCF",
    Scenario = "Scenario 2"
  )
)

# Flip scenario labels (same as bias)
rmse_df$Scenario <- ifelse(
  rmse_df$Scenario == "Scenario 1",
  "Scenario 2",
  "Scenario 1"
)

rmse_df$Scenario <- factor(rmse_df$Scenario)

ggplot(rmse_df, aes(x = Method, y = value, fill = Method)) +
  geom_boxplot(width = 0.6, outlier.size = 0.8) +
  facet_wrap(~ Scenario, scales = "free_y") +
  labs(
    y = "RMSE (ATE)",
    x = NULL
  ) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "none",
    strip.text = element_text(size = 12)
  )

ggsave('/Users/emmalandry/Documents/Falco_GSR/ReviewPaper_CEHR/IA_rmse_ATE.pdf', width = 10, height = 4.5)
# CATE ---------------
# Bias for CATE
bias_CATE_CDBMM_1 <- sapply(1:samples, function(s)
  compute_CATE(
    CDBMM_scenario_1[[s]]$tau,
    simulated_tau_1[, s]
  )$bias
)

bias_CATE_CDBMM_2 <- sapply(1:samples, function(s)
  compute_CATE(
    CDBMM_scenario_2[[s]]$tau,
    simulated_tau_2[, s]
  )$bias
)

bias_CATE_BCF_1 <- sapply(1:samples, function(s)
  compute_CATE(
    BCF_scenario_1[[s]]$tau,
    simulated_tau_1[, s]
  )$bias
)

bias_CATE_BCF_2 <- sapply(1:samples, function(s)
  compute_CATE(
    BCF_scenario_2[[s]]$tau,
    simulated_tau_2[, s]
  )$bias
)



# MSE for CATE
mse_CATE_CDBMM_1 <- sapply(1:samples, function(s)
  compute_CATE(
    CDBMM_scenario_1[[s]]$tau,
    simulated_tau_1[, s]
  )$mse
)

mse_CATE_CDBMM_2 <- sapply(1:samples, function(s)
  compute_CATE(
    CDBMM_scenario_2[[s]]$tau,
    simulated_tau_2[, s]
  )$mse
)

mse_CATE_BCF_1 <- sapply(1:samples, function(s)
  compute_CATE(
    BCF_scenario_1[[s]]$tau,
    simulated_tau_1[, s]
  )$mse
)

mse_CATE_BCF_2 <- sapply(1:samples, function(s)
  compute_CATE(
    BCF_scenario_2[[s]]$tau,
    simulated_tau_2[, s]
  )$mse
)

# Figures
bias_CATE_df <- rbind(
  data.frame(
    value    = bias_CATE_CDBMM_1,
    Method   = "CDBMM",
    Scenario = "Scenario 1"
  ),
  data.frame(
    value    = bias_CATE_BCF_1,
    Method   = "BCF",
    Scenario = "Scenario 1"
  ),
  data.frame(
    value    = bias_CATE_CDBMM_2,
    Method   = "CDBMM",
    Scenario = "Scenario 2"
  ),
  data.frame(
    value    = bias_CATE_BCF_2,
    Method   = "BCF",
    Scenario = "Scenario 2"
  )
)

bias_CATE_df$Scenario <- ifelse(
  bias_CATE_df$Scenario == "Scenario 1",
  "Scenario 2",
  "Scenario 1"
)

bias_CATE_df$Scenario <- factor(bias_CATE_df$Scenario)

ggplot(bias_CATE_df, aes(x = Method, y = value, fill = Method)) +
  geom_boxplot(width = 0.6, outlier.size = 0.8) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.7) +
  facet_wrap(~ Scenario, scales = "free_y") +
  labs(
    y = "Bias (CATE)",
    x = NULL
  ) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "none",
    strip.text = element_text(size = 12)
  )

ggsave('/Users/emmalandry/Documents/Falco_GSR/ReviewPaper_CEHR/IA_bias_CATE.pdf', width = 10, height = 4.5)

rmse_CATE_df <- rbind(
  data.frame(
    value    = sqrt(mse_CATE_CDBMM_1),
    Method   = "CDBMM",
    Scenario = "Scenario 1"
  ),
  data.frame(
    value    = sqrt(mse_CATE_BCF_1),
    Method   = "BCF",
    Scenario = "Scenario 1"
  ),
  data.frame(
    value    = sqrt(mse_CATE_CDBMM_2),
    Method   = "CDBMM",
    Scenario = "Scenario 2"
  ),
  data.frame(
    value    = sqrt(mse_CATE_BCF_2),
    Method   = "BCF",
    Scenario = "Scenario 2"
  )
)

rmse_CATE_df$Scenario <- ifelse(
  rmse_CATE_df$Scenario == "Scenario 1",
  "Scenario 2",
  "Scenario 1"
)

rmse_CATE_df$Scenario <- factor(rmse_CATE_df$Scenario)

ggplot(rmse_CATE_df, aes(x = Method, y = value, fill = Method)) +
  geom_boxplot(width = 0.6, outlier.size = 0.8) +
  facet_wrap(~ Scenario, scales = "free_y") +
  labs(
    y = "RMSE (CATE)",
    x = NULL
  ) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "none",
    strip.text = element_text(size = 12)
  )

ggsave('/Users/emmalandry/Documents/Falco_GSR/ReviewPaper_CEHR/IA_rmse_CATE.pdf', width = 10, height = 4.5)

# GATE ------
# Bias for CATE
bias_GATE_CDBMM_1 <- sapply(1:samples, function(s)
  compute_GATE(
    CDBMM_scenario_1[[s]]$tau,
    simulated_tau_1[, s],
    compute_CDBMM_groups(CDBMM_scenario_1[[s]]$partition)
  )$bias
)

bias_GATE_CDBMM_2 <- sapply(1:samples, function(s)
  compute_GATE(
    CDBMM_scenario_2[[s]]$tau,
    simulated_tau_2[, s],
    compute_CDBMM_groups(CDBMM_scenario_2[[s]]$partition)
  )$bias
)

bias_GATE_BCF_1 <- sapply(1:samples, function(s)
  compute_GATE(
    BCF_scenario_1[[s]]$tau,
    simulated_tau_1[, s],
    CART_scenario_1[[s]]$partition
  )$bias
)

bias_GATE_BCF_2 <- sapply(1:samples, function(s)
  compute_GATE(
    BCF_scenario_2[[s]]$tau,
    simulated_tau_2[, s],
    CART_scenario_2[[s]]$partition
  )$bias
)

# MSE for GATE
mse_GATE_CDBMM_1 <- sapply(1:samples, function(s)
  compute_GATE(
    CDBMM_scenario_1[[s]]$tau,
    simulated_tau_1[, s],
    compute_CDBMM_groups(CDBMM_scenario_1[[s]]$partition)
  )$mse
)

mse_GATE_CDBMM_2 <- sapply(1:samples, function(s)
  compute_GATE(
    CDBMM_scenario_2[[s]]$tau,
    simulated_tau_2[, s],
    compute_CDBMM_groups(CDBMM_scenario_2[[s]]$partition)
  )$mse
)

mse_GATE_BCF_1 <- sapply(1:samples, function(s)
  compute_GATE(
    BCF_scenario_1[[s]]$tau,
    simulated_tau_1[, s],
    CART_scenario_1[[s]]$partition
  )$mse
)

mse_GATE_BCF_2 <- sapply(1:samples, function(s)
  compute_GATE(
    BCF_scenario_2[[s]]$tau,
    simulated_tau_2[, s],
    CART_scenario_2[[s]]$partition
  )$mse
)

# Figures

bias_GATE_df <- rbind(
  data.frame(
    value    = bias_GATE_CDBMM_1,
    Method   = "CDBMM",
    Scenario = "Scenario 1"
  ),
  data.frame(
    value    = bias_GATE_BCF_1,
    Method   = "BCF",
    Scenario = "Scenario 1"
  ),
  data.frame(
    value    = bias_GATE_CDBMM_2,
    Method   = "CDBMM",
    Scenario = "Scenario 2"
  ),
  data.frame(
    value    = bias_GATE_BCF_2,
    Method   = "BCF",
    Scenario = "Scenario 2"
  )
)

bias_GATE_df$Scenario <- ifelse(
  bias_GATE_df$Scenario == "Scenario 1",
  "Scenario 2",
  "Scenario 1"
)

bias_GATE_df$Scenario <- factor(bias_GATE_df$Scenario)
ggplot(bias_GATE_df, aes(x = Method, y = value, fill = Method)) +
  geom_boxplot(width = 0.6, outlier.size = 0.8) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.7) +
  facet_wrap(~ Scenario, scales = "free_y") +
  labs(
    y = "Bias (GATE)",
    x = NULL
  ) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "none",
    strip.text = element_text(size = 12)
  )

ggsave('/Users/emmalandry/Documents/Falco_GSR/ReviewPaper_CEHR/IA_bias_GATE.pdf', width = 10, height = 4.5)

rmse_GATE_df <- rbind(
  data.frame(
    value    = sqrt(mse_GATE_CDBMM_1),
    Method   = "CDBMM",
    Scenario = "Scenario 1"
  ),
  data.frame(
    value    = sqrt(mse_GATE_BCF_1),
    Method   = "BCF",
    Scenario = "Scenario 1"
  ),
  data.frame(
    value    = sqrt(mse_GATE_CDBMM_2),
    Method   = "CDBMM",
    Scenario = "Scenario 2"
  ),
  data.frame(
    value    = sqrt(mse_GATE_BCF_2),
    Method   = "BCF",
    Scenario = "Scenario 2"
  )
)

rmse_GATE_df$Scenario <- ifelse(
  rmse_GATE_df$Scenario == "Scenario 1",
  "Scenario 2",
  "Scenario 1"
)

rmse_GATE_df$Scenario <- factor(rmse_GATE_df$Scenario)

ggplot(rmse_GATE_df, aes(x = Method, y = value, fill = Method)) +
  geom_boxplot(width = 0.6, outlier.size = 0.8) +
  facet_wrap(~ Scenario, scales = "free_y") +
  labs(
    y = "RMSE (GATE)",
    x = NULL
  ) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "none",
    strip.text = element_text(size = 12)
  )

ggsave('/Users/emmalandry/Documents/Falco_GSR/ReviewPaper_CEHR/IA_rmse_GATE.pdf', width = 10, height = 4.5)


