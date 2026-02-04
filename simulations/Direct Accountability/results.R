library(ggplot2)

load('simulations/Direct Accountability/DA_scenario1.RData')
load('simulations/Direct Accountability/DA_scenario2.RData')
load('simulations/Direct Accountability/BPCF_scenario1.RData')
load('simulations/Direct Accountability/BPCF_scenario2.RData')
load('simulations/Direct Accountability/CASBAH_scenario1.RData')
load('simulations/Direct Accountability/CASBAH_scenario2.RData')

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

# Sizes
n <- 500
samples <- 100

# Outcome / Post Treatment ------

# Simulated 
simulated_tau_1 <- sapply(1:samples, function(s) scenario_1[[s]]$Y_1 - scenario_1[[s]]$Y_0)
simulated_tau_2 <- sapply(1:samples, function(s) scenario_2[[s]]$data$Y_1 - scenario_2[[s]]$data$Y_0)

simulate_pt_1 <- sapply(1:samples, function(s) scenario_1[[s]]$M_1 - scenario_1[[s]]$M_0)
simulate_pt_2 <- sapply(1:samples, function(s) scenario_2[[s]]$data$P_1 - scenario_2[[s]]$data$P_0)

# BPCF
BPCF_tau_1 <- sapply(1:samples, function(s) BPCF_scenario1[[s]]$Y1_med - BPCF_scenario1[[s]]$Y0_med)
BPCF_tau_2 <- sapply(1:samples, function(s) BPCF_scenario2[[s]]$Y1_med - BPCF_scenario2[[s]]$Y0_med)

BPCF_pt_1 <- sapply(1:samples, function(s) BPCF_scenario1[[s]]$M1_med - BPCF_scenario1[[s]]$M0_med)
BPCF_pt_2 <- sapply(1:samples, function(s) BPCF_scenario2[[s]]$M1_med - BPCF_scenario2[[s]]$M0_med)

# CASBAH
CASBAH_tau_1 <- sapply(1:samples, function(s) CASBAH_scenario1[[s]]$post_Y_1_imp - CASBAH_scenario1[[s]]$post_Y_0_imp)
CASBAH_tau_2 <- sapply(1:samples, function(s) CASBAH_scenario2[[s]]$post_Y_1_imp - CASBAH_scenario2[[s]]$post_Y_0_imp)

CASBAH_pt_1 <- sapply(1:samples, function(s) CASBAH_scenario1[[s]]$post_P_1_imp - CASBAH_scenario1[[s]]$post_P_0_imp)
CASBAH_pt_2 <- sapply(1:samples, function(s) CASBAH_scenario2[[s]]$post_P_1_imp - CASBAH_scenario2[[s]]$post_P_0_imp)

# Outcome ATE ---------
# Bias
bias_ATE_BPCF_1 <- sapply(1:samples, function(s)
  compute_ATE(BPCF_tau_1[, s], simulated_tau_1[, s])$bias
)

bias_ATE_BPCF_2 <- sapply(1:samples, function(s)
  compute_ATE(BPCF_tau_2[, s], simulated_tau_2[, s])$bias
)

bias_ATE_CASBAH_1 <- sapply(1:samples, function(s)
  compute_ATE(CASBAH_tau_1[, s], simulated_tau_1[, s])$bias
)

bias_ATE_CASBAH_2 <- sapply(1:samples, function(s)
  compute_ATE(CASBAH_tau_2[, s], simulated_tau_2[, s])$bias
)

# MSE
mse_ATE_BPCF_1 <- sapply(1:samples, function(s)
  compute_ATE(BPCF_tau_1[, s], simulated_tau_1[, s])$mse
)

mse_ATE_BPCF_2 <- sapply(1:samples, function(s)
  compute_ATE(BPCF_tau_2[, s], simulated_tau_2[, s])$mse
)

mse_ATE_CASBAH_1 <- sapply(1:samples, function(s)
  compute_ATE(CASBAH_tau_1[, s], simulated_tau_1[, s])$mse
)

mse_ATE_CASBAH_2 <- sapply(1:samples, function(s)
  compute_ATE(CASBAH_tau_2[, s], simulated_tau_2[, s])$mse
)

# Figures 
bias_df_outcome <- rbind(
  data.frame(value = bias_ATE_BPCF_1,   Method = "BPCF",   Scenario = "Scenario 1"),
  data.frame(value = bias_ATE_CASBAH_1, Method = "CASBAH", Scenario = "Scenario 1"),
  data.frame(value = bias_ATE_BPCF_2,   Method = "BPCF",   Scenario = "Scenario 2"),
  data.frame(value = bias_ATE_CASBAH_2, Method = "CASBAH", Scenario = "Scenario 2")
)

bias_df_outcome$Scenario <- factor(bias_df_outcome$Scenario)

ggplot(bias_df_outcome, aes(x = Method, y = value, fill = Method)) +
  geom_boxplot(width = 0.6, outlier.size = 0.8) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.7) +
  facet_wrap(~ Scenario, scales = "free_y") +
  labs(
    title = "Outcome",
    y = "Bias (ATE)",
    x = NULL
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "none"
  )
ggsave('/Users/emmalandry/Documents/Falco_GSR/ReviewPaper_CEHR/DA_bias_tau_ATE.pdf', width = 10, height = 4.5)


rmse_df_outcome <- rbind(
  data.frame(value = sqrt(mse_ATE_BPCF_1),   Method = "BPCF",   Scenario = "Scenario 1"),
  data.frame(value = sqrt(mse_ATE_CASBAH_1), Method = "CASBAH", Scenario = "Scenario 1"),
  data.frame(value = sqrt(mse_ATE_BPCF_2),   Method = "BPCF",   Scenario = "Scenario 2"),
  data.frame(value = sqrt(mse_ATE_CASBAH_2), Method = "CASBAH", Scenario = "Scenario 2")
)

rmse_df_outcome$Scenario <- factor(rmse_df_outcome$Scenario)

ggplot(rmse_df_outcome, aes(x = Method, y = value, fill = Method)) +
  geom_boxplot(width = 0.6, outlier.size = 0.8) +
  facet_wrap(~ Scenario, scales = "free_y") +
  labs(
    title = "Outcome",
    y = "RMSE (ATE)",
    x = NULL
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "none",
    strip.text = element_text(size = 12)
  )
ggsave('/Users/emmalandry/Documents/Falco_GSR/ReviewPaper_CEHR/DA_rmse_tau_ATE.pdf', width = 10, height = 4.5)

# Outcome CATE ------
# Bias
bias_CATE_BPCF_1 <- sapply(1:samples, function(s)
  compute_CATE(BPCF_tau_1[, s], simulated_tau_1[, s])$bias
)

bias_CATE_BPCF_2 <- sapply(1:samples, function(s)
  compute_CATE(BPCF_tau_2[, s], simulated_tau_2[, s])$bias
)

bias_CATE_CASBAH_1 <- sapply(1:samples, function(s)
  compute_CATE(CASBAH_tau_1[, s], simulated_tau_1[, s])$bias
)

bias_CATE_CASBAH_2 <- sapply(1:samples, function(s)
  compute_CATE(CASBAH_tau_2[, s], simulated_tau_2[, s])$bias
)

# MSE
mse_CATE_BPCF_1 <- sapply(1:samples, function(s)
  compute_CATE(BPCF_tau_1[, s], simulated_tau_1[, s])$mse
)

mse_CATE_BPCF_2 <- sapply(1:samples, function(s)
  compute_CATE(BPCF_tau_2[, s], simulated_tau_2[, s])$mse
)

mse_CATE_CASBAH_1 <- sapply(1:samples, function(s)
  compute_CATE(CASBAH_tau_1[, s], simulated_tau_1[, s])$mse
)

mse_CATE_CASBAH_2 <- sapply(1:samples, function(s)
  compute_CATE(CASBAH_tau_2[, s], simulated_tau_2[, s])$mse
)

# Figures 
bias_CATE_df_outcome <- rbind(
  data.frame(value = bias_CATE_BPCF_1,   Method = "BPCF",   Scenario = "Scenario 1"),
  data.frame(value = bias_CATE_CASBAH_1, Method = "CASBAH", Scenario = "Scenario 1"),
  data.frame(value = bias_CATE_BPCF_2,   Method = "BPCF",   Scenario = "Scenario 2"),
  data.frame(value = bias_CATE_CASBAH_2, Method = "CASBAH", Scenario = "Scenario 2")
)

bias_CATE_df_outcome$Scenario <- factor(bias_CATE_df_outcome$Scenario)

ggplot(bias_CATE_df_outcome, aes(x = Method, y = value, fill = Method)) +
  geom_boxplot(width = 0.6, outlier.size = 0.8) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.7) +
  facet_wrap(~ Scenario, scales = "free_y") +
  labs(
    title = "Outcome",
    y = "Bias (CATE)",
    x = NULL
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "none"
  )
ggsave('/Users/emmalandry/Documents/Falco_GSR/ReviewPaper_CEHR/DA_bias_tau_CATE.pdf', width = 10, height = 4.5)

rmse_CATE_df_outcome <- rbind(
  data.frame(value = sqrt(mse_CATE_BPCF_1),   Method = "BPCF",   Scenario = "Scenario 1"),
  data.frame(value = sqrt(mse_CATE_CASBAH_1), Method = "CASBAH", Scenario = "Scenario 1"),
  data.frame(value = sqrt(mse_CATE_BPCF_2),   Method = "BPCF",   Scenario = "Scenario 2"),
  data.frame(value = sqrt(mse_CATE_CASBAH_2), Method = "CASBAH", Scenario = "Scenario 2")
)

rmse_CATE_df_outcome$Scenario <- factor(rmse_CATE_df_outcome$Scenario)

ggplot(rmse_CATE_df_outcome, aes(x = Method, y = value, fill = Method)) +
  geom_boxplot(width = 0.6, outlier.size = 0.8) +
  facet_wrap(~ Scenario, scales = "free_y") +
  labs(
    title = "Outcome",
    y = "RMSE (CATE)",
    x = NULL
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "none",
    strip.text = element_text(size = 12)
  )
ggsave('/Users/emmalandry/Documents/Falco_GSR/ReviewPaper_CEHR/DA_rmse_tau_CATE.pdf', width = 10, height = 4.5)

# Post-treatment ATE -----
# Bias
bias_PT_BPCF_1 <- sapply(1:samples, function(s)
  compute_ATE(BPCF_pt_1[, s], simulate_pt_1[, s])$bias
)

bias_PT_BPCF_2 <- sapply(1:samples, function(s)
  compute_ATE(BPCF_pt_2[, s], simulate_pt_2[, s])$bias
)

bias_PT_CASBAH_1 <- sapply(1:samples, function(s)
  compute_ATE(CASBAH_pt_1[, s], simulate_pt_1[, s])$bias
)

bias_PT_CASBAH_2 <- sapply(1:samples, function(s)
  compute_ATE(CASBAH_pt_2[, s], simulate_pt_2[, s])$bias
)

# MSE
mse_PT_BPCF_1 <- sapply(1:samples, function(s)
  compute_ATE(BPCF_pt_1[, s], simulate_pt_1[, s])$mse
)

mse_PT_BPCF_2 <- sapply(1:samples, function(s)
  compute_ATE(BPCF_pt_2[, s], simulate_pt_2[, s])$mse
)

mse_PT_CASBAH_1 <- sapply(1:samples, function(s)
  compute_ATE(CASBAH_pt_1[, s], simulate_pt_1[, s])$mse
)

mse_PT_CASBAH_2 <- sapply(1:samples, function(s)
  compute_ATE(CASBAH_pt_2[, s], simulate_pt_2[, s])$mse
)

# Figures
bias_PT_df <- rbind(
  data.frame(value = bias_PT_BPCF_1,   Method = "BPCF",   Scenario = "Scenario 1"),
  data.frame(value = bias_PT_CASBAH_1, Method = "CASBAH", Scenario = "Scenario 1"),
  data.frame(value = bias_PT_BPCF_2,   Method = "BPCF",   Scenario = "Scenario 2"),
  data.frame(value = bias_PT_CASBAH_2, Method = "CASBAH", Scenario = "Scenario 2")
)

bias_PT_df$Scenario <- factor(bias_PT_df$Scenario)

ggplot(bias_PT_df, aes(x = Method, y = value, fill = Method)) +
  geom_boxplot(width = 0.6, outlier.size = 0.8) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.7) +
  facet_wrap(~ Scenario, scales = "free_y") +
  labs(
    title = "Post-treatment",
    y = "Bias (ATE)",
    x = NULL
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "none",
    strip.text = element_text(size = 12)
  )
ggsave('/Users/emmalandry/Documents/Falco_GSR/ReviewPaper_CEHR/DA_bias_pt_ATE.pdf', width = 10, height = 4.5)

rmse_PT_df <- rbind(
  data.frame(value = sqrt(mse_PT_BPCF_1),   Method = "BPCF",   Scenario = "Scenario 1"),
  data.frame(value = sqrt(mse_PT_CASBAH_1), Method = "CASBAH", Scenario = "Scenario 1"),
  data.frame(value = sqrt(mse_PT_BPCF_2),   Method = "BPCF",   Scenario = "Scenario 2"),
  data.frame(value = sqrt(mse_PT_CASBAH_2), Method = "CASBAH", Scenario = "Scenario 2")
)

rmse_PT_df$Scenario <- factor(rmse_PT_df$Scenario)

ggplot(rmse_PT_df, aes(x = Method, y = value, fill = Method)) +
  geom_boxplot(width = 0.6, outlier.size = 0.8) +
  facet_wrap(~ Scenario, scales = "free_y") +
  labs(
    title = "Post-treatment",
    y = "RMSE (ATE)",
    x = NULL
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "none",
    strip.text = element_text(size = 12)
  )
ggsave('/Users/emmalandry/Documents/Falco_GSR/ReviewPaper_CEHR/DA_rmse_pt_ATE.pdf', width = 10, height = 4.5)

# Post-treatment CATE ----
# Bias
bias_CATE_PT_BPCF_1 <- sapply(1:samples, function(s)
  compute_CATE(BPCF_pt_1[, s], simulate_pt_1[, s])$bias
)

bias_CATE_PT_BPCF_2 <- sapply(1:samples, function(s)
  compute_CATE(BPCF_pt_2[, s], simulate_pt_2[, s])$bias
)

bias_CATE_PT_CASBAH_1 <- sapply(1:samples, function(s)
  compute_CATE(CASBAH_pt_1[, s], simulate_pt_1[, s])$bias
)

bias_CATE_PT_CASBAH_2 <- sapply(1:samples, function(s)
  compute_CATE(CASBAH_pt_2[, s], simulate_pt_2[, s])$bias
)

# MSE
mse_CATE_PT_BPCF_1 <- sapply(1:samples, function(s)
  compute_CATE(BPCF_pt_1[, s], simulate_pt_1[, s])$mse
)

mse_CATE_PT_BPCF_2 <- sapply(1:samples, function(s)
  compute_CATE(BPCF_pt_2[, s], simulate_pt_2[, s])$mse
)

mse_CATE_PT_CASBAH_1 <- sapply(1:samples, function(s)
  compute_CATE(CASBAH_pt_1[, s], simulate_pt_1[, s])$mse
)

mse_CATE_PT_CASBAH_2 <- sapply(1:samples, function(s)
  compute_CATE(CASBAH_pt_2[, s], simulate_pt_2[, s])$mse
)

bias_CATE_PT_df <- rbind(
  data.frame(value = bias_CATE_PT_BPCF_1,   Method = "BPCF",   Scenario = "Scenario 1"),
  data.frame(value = bias_CATE_PT_CASBAH_1, Method = "CASBAH", Scenario = "Scenario 1"),
  data.frame(value = bias_CATE_PT_BPCF_2,   Method = "BPCF",   Scenario = "Scenario 2"),
  data.frame(value = bias_CATE_PT_CASBAH_2, Method = "CASBAH", Scenario = "Scenario 2")
)

bias_CATE_PT_df$Scenario <- factor(bias_CATE_PT_df$Scenario)

ggplot(bias_CATE_PT_df, aes(x = Method, y = value, fill = Method)) +
  geom_boxplot(width = 0.6, outlier.size = 0.8) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.7) +
  facet_wrap(~ Scenario, scales = "free_y") +
  labs(
    title = "Post-treatment",
    y = "Bias (CATE)",
    x = NULL
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "none",
    strip.text = element_text(size = 12)
  )
ggsave('/Users/emmalandry/Documents/Falco_GSR/ReviewPaper_CEHR/DA_bias_pt_CATE.pdf', width = 10, height = 4.5)

rmse_CATE_PT_df <- rbind(
  data.frame(value = sqrt(mse_CATE_PT_BPCF_1),   Method = "BPCF",   Scenario = "Scenario 1"),
  data.frame(value = sqrt(mse_CATE_PT_CASBAH_1), Method = "CASBAH", Scenario = "Scenario 1"),
  data.frame(value = sqrt(mse_CATE_PT_BPCF_2),   Method = "BPCF",   Scenario = "Scenario 2"),
  data.frame(value = sqrt(mse_CATE_PT_CASBAH_2), Method = "CASBAH", Scenario = "Scenario 2")
)

rmse_CATE_PT_df$Scenario <- factor(rmse_CATE_PT_df$Scenario)

ggplot(rmse_CATE_PT_df, aes(x = Method, y = value, fill = Method)) +
  geom_boxplot(width = 0.6, outlier.size = 0.8) +
  facet_wrap(~ Scenario, scales = "free_y") +
  labs(
    title = "Post-treatment",
    y = "RMSE (CATE)",
    x = NULL
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "none",
    strip.text = element_text(size = 12)
  )

ggsave('/Users/emmalandry/Documents/Falco_GSR/ReviewPaper_CEHR/DA_rmse_pt_CATE.pdf', width = 10, height = 4.5)



