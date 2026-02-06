library(ggplot2)

load('simulations/Direct Accountability/DA_scenario1_alt.RData')
load('simulations/Direct Accountability/DA_scenario2_alt.RData')
load('simulations/Direct Accountability/BPCF_scenario1_alt.RData')
load('simulations/Direct Accountability/BPCF_scenario2_alt.RData')
load('simulations/Direct Accountability/CASBAH_scenario1_alt.RData')
load('simulations/Direct Accountability/CASBAH_scenario2_alt.RData')

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
#ggsave('/Users/emmalandry/Documents/Falco_GSR/ReviewPaper_CEHR/DA_bias_tau_ATE.pdf', width = 10, height = 4.5)


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
#ggsave('/Users/emmalandry/Documents/Falco_GSR/ReviewPaper_CEHR/DA_rmse_tau_ATE.pdf', width = 10, height = 4.5)

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
#ggsave('/Users/emmalandry/Documents/Falco_GSR/ReviewPaper_CEHR/DA_bias_tau_CATE.pdf', width = 10, height = 4.5)

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
#ggsave('/Users/emmalandry/Documents/Falco_GSR/ReviewPaper_CEHR/DA_rmse_tau_CATE.pdf', width = 10, height = 4.5)

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
#ggsave('/Users/emmalandry/Documents/Falco_GSR/ReviewPaper_CEHR/DA_bias_pt_ATE.pdf', width = 10, height = 4.5)

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
#ggsave('/Users/emmalandry/Documents/Falco_GSR/ReviewPaper_CEHR/DA_rmse_pt_ATE.pdf', width = 10, height = 4.5)

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
#ggsave('/Users/emmalandry/Documents/Falco_GSR/ReviewPaper_CEHR/DA_bias_pt_CATE.pdf', width = 10, height = 4.5)

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

#ggsave('/Users/emmalandry/Documents/Falco_GSR/ReviewPaper_CEHR/DA_rmse_pt_CATE.pdf', width = 10, height = 4.5)

# Groups ------
# Scenario 1
group_labels_s1 <- lapply(1:samples, function(s) {
  S <- scenario_1[[s]]$S
  
  cuts <- quantile(S, probs = c(1/3, 2/3), type = 7)
  
  cut(
    S,
    breaks = c(-Inf, cuts, Inf),
    labels = c("Low", "Mid", "High"),
    include.lowest = TRUE
  )
})

true_ATE_group_s1 <- lapply(1:samples, function(s) {
  tau_true <- simulated_tau_1[, s]   # Y1 - Y0 from the DGP
  groups   <- group_labels_s1[[s]]   # Low / Mid / High from true S
  
  tapply(tau_true, groups, mean)
})


ATE_BPCF_group_s1 <- lapply(1:samples, function(s) {
  tau_hat <- BPCF_tau_1[, s]
  groups  <- group_labels_s1[[s]]
  
  tapply(tau_hat, groups, mean)
})

ATE_CASBAH_group_s1 <- lapply(1:samples, function(s) {
  tau_hat <- CASBAH_tau_1[, s]
  groups  <- group_labels_s1[[s]]
  
  tapply(tau_hat, groups, mean)
})

bias_ATE_group_BPCF_s1 <- do.call(
  rbind,
  lapply(1:samples, function(s) {
    ATE_BPCF_group_s1[[s]] - true_ATE_group_s1[[s]]
  })
)

bias_ATE_group_CASBAH_s1 <- do.call(
  rbind,
  lapply(1:samples, function(s) {
    ATE_CASBAH_group_s1[[s]] - true_ATE_group_s1[[s]]
  })
)

rmse_ATE_group_BPCF_s1_mat <- sqrt(bias_ATE_group_BPCF_s1^2)
rmse_ATE_group_CASBAH_s1_mat <- sqrt(bias_ATE_group_CASBAH_s1^2)

bias_group_df_s1 <- rbind(
  data.frame(
    value  = as.vector(bias_ATE_group_BPCF_s1),
    Group  = rep(colnames(bias_ATE_group_BPCF_s1), each = samples),
    Method = "BPCF"
  ),
  data.frame(
    value  = as.vector(bias_ATE_group_CASBAH_s1),
    Group  = rep(colnames(bias_ATE_group_CASBAH_s1), each = samples),
    Method = "CASBAH"
  )
)

bias_group_df_s1$Method <- factor(
  bias_group_df_s1$Method,
  levels = c("BPCF", "CASBAH")
)

bias_group_df_s1$Group <- factor(
  bias_group_df_s1$Group,
  levels = c("Low", "Mid", "High")
)

bias_group_df_s1$MethodGroup <- interaction(
  bias_group_df_s1$Method,
  bias_group_df_s1$Group,
  sep = "_"
)


# BPCF = blue shades
bpcf_cols <- c(
  BPCF_Low  = "#bdd7e7",
  BPCF_Mid  = "#6baed6",
  BPCF_High = "#2171b5"
)

# CASBAH = orange/red shades
casbah_cols <- c(
  CASBAH_Low  = "#fdd0a2",
  CASBAH_Mid  = "#fd8d3c",
  CASBAH_High = "#e6550d"
)

fill_cols <- c(bpcf_cols, casbah_cols)

ggplot(
  bias_group_df_s1,
  aes(
    x = Method,
    y = value,
    fill = MethodGroup
  )
) +
  geom_boxplot(
    width = 0.6,
    position = position_dodge(width = 0.75),
    outlier.size = 0.8
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.7
  ) +
  scale_fill_manual(values = fill_cols) +
  labs(
    title = "Scenario 1: Group-specific ATE Bias",
    y = "Bias",
    x = NULL
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "none"
  )

rmse_group_df_s1 <- rbind(
  data.frame(
    value  = as.vector(rmse_ATE_group_BPCF_s1_mat),
    Group  = rep(colnames(rmse_ATE_group_BPCF_s1_mat), each = samples),
    Method = "BPCF"
  ),
  data.frame(
    value  = as.vector(rmse_ATE_group_CASBAH_s1_mat),
    Group  = rep(colnames(rmse_ATE_group_CASBAH_s1_mat), each = samples),
    Method = "CASBAH"
  )
)

rmse_group_df_s1$Method <- factor(
  rmse_group_df_s1$Method,
  levels = c("BPCF", "CASBAH")
)

rmse_group_df_s1$Group <- factor(
  rmse_group_df_s1$Group,
  levels = c("Low", "Mid", "High")
)

rmse_group_df_s1$MethodGroup <- interaction(
  rmse_group_df_s1$Method,
  rmse_group_df_s1$Group,
  sep = "_"
)

ggplot(
  rmse_group_df_s1,
  aes(
    x = Method,
    y = value,
    fill = MethodGroup
  )
) +
  geom_boxplot(
    width = 0.6,
    position = position_dodge(width = 0.75),
    outlier.size = 0.8
  ) +
  scale_fill_manual(values = fill_cols) +
  labs(
    title = "Scenario 1: Group-specific ATE RMSE",
    y = "RMSE",
    x = NULL
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "none"
  )

# Scenario 2
group_labels_s2 <- lapply(1:samples, function(s) {
  factor(
    scenario_2[[s]]$clusters$S_strata,
    levels = c(0, 1, 2),
    labels = c("S(1) = S(0)", "S(1) > S(0)", "S(1) < S(0)")
  )
})

true_ATE_group_s2 <- lapply(1:samples, function(s) {
  tau_true <- simulated_tau_2[, s]
  groups <- group_labels_s2[[s]]
  
  tapply(tau_true, groups, mean)
})

ATE_BPCF_group_s2 <- lapply(1:samples, function(s) {
  tau_hat <- BPCF_tau_2[, s]
  groups <- group_labels_s2[[s]]
  
  tapply(tau_hat, groups, mean)
})

ATE_CASBAH_group_s2 <- lapply(1:samples, function(s) {
  tau_hat <- CASBAH_tau_2[, s]
  groups <- group_labels_s2[[s]]
  
  tapply(tau_hat, groups, mean)
})

bias_ATE_group_BPCF_s2 <- do.call(
  rbind,
  lapply(1:samples, function(s) {
    ATE_BPCF_group_s2[[s]] - true_ATE_group_s2[[s]]
  })
)

bias_ATE_group_CASBAH_s2 <- do.call(
  rbind,
  lapply(1:samples, function(s) {
    ATE_CASBAH_group_s2[[s]] - true_ATE_group_s2[[s]]
  })
)

rmse_ATE_group_BPCF_s2_mat <- sqrt(bias_ATE_group_BPCF_s2^2)
rmse_ATE_group_CASBAH_s2_mat <- sqrt(bias_ATE_group_CASBAH_s2^2)

bias_group_df_s2 <- rbind(
  data.frame(
    value = as.vector(bias_ATE_group_BPCF_s2),
    Group = rep(colnames(bias_ATE_group_BPCF_s2), each = samples),
    Method = "BPCF"
  ),
  data.frame(
    value = as.vector(bias_ATE_group_CASBAH_s2),
    Group = rep(colnames(bias_ATE_group_CASBAH_s2), each = samples),
    Method = "CASBAH"
  )
)

bias_group_df_s2$Method <- factor(
  bias_group_df_s2$Method,
  levels = c("BPCF", "CASBAH")
)

bias_group_df_s2$Group <- factor(
  bias_group_df_s2$Group,
  levels = c("S(1) = S(0)", "S(1) > S(0)", "S(1) < S(0)")
)

bias_group_df_s2$MethodGroup <- interaction(
  bias_group_df_s2$Method,
  bias_group_df_s2$Group,
  sep = "_"
)

rmse_group_df_s2 <- rbind(
  data.frame(
    value = as.vector(rmse_ATE_group_BPCF_s2_mat),
    Group = rep(colnames(rmse_ATE_group_BPCF_s2_mat), each = samples),
    Method = "BPCF"
  ),
  data.frame(
    value = as.vector(rmse_ATE_group_CASBAH_s2_mat),
    Group = rep(colnames(rmse_ATE_group_CASBAH_s2_mat), each = samples),
    Method = "CASBAH"
  )
)

rmse_group_df_s2$Method <- factor(
  rmse_group_df_s2$Method,
  levels = c("BPCF", "CASBAH")
)

rmse_group_df_s2$Group <- factor(
  rmse_group_df_s2$Group,
  levels = c("S(1) = S(0)", "S(1) > S(0)", "S(1) < S(0)")
)

rmse_group_df_s2$MethodGroup <- interaction(
  rmse_group_df_s2$Method,
  rmse_group_df_s2$Group,
  sep = "_"
)


for (df in list(bias_group_df_s2, rmse_group_df_s2)) {
  df$Method <- factor(df$Method, levels = c("BPCF", "CASBAH"))
  df$Group <- factor(
    df$Group,
    levels = c("S(1) = S(0)", "S(1) > S(0)", "S(1) < S(0)")
  )
  df$MethodGroup <- interaction(df$Method, df$Group, sep = "_")
}

fill_cols <- c(
  "BPCF_S(1) = S(0)" = "#bdd7e7",
  "BPCF_S(1) > S(0)" = "#6baed6",
  "BPCF_S(1) < S(0)" = "#2171b5",
  "CASBAH_S(1) = S(0)" = "#fdd0a2",
  "CASBAH_S(1) > S(0)" = "#fd8d3c",
  "CASBAH_S(1) < S(0)" = "#e6550d"
)

ggplot(
  bias_group_df_s2,
  aes(x = Method, y = value, fill = MethodGroup)
) +
  geom_boxplot(
    width = 0.6,
    position = position_dodge(width = 0.75),
    outlier.size = 0.8
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.7) +
  scale_fill_manual(values = fill_cols) +
  labs(
    title = "Scenario 2: Group-specific ATE Bias",
    y = "Bias",
    x = NULL
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "none"
  )

ggplot(
  rmse_group_df_s2,
  aes(x = Method, y = value, fill = MethodGroup)
) +
  geom_boxplot(
    width = 0.6,
    position = position_dodge(width = 0.75),
    outlier.size = 0.8
  ) +
  scale_fill_manual(values = fill_cols) +
  labs(
    title = "Scenario 2: Group-specific ATE RMSE",
    y = "RMSE",
    x = NULL
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "none"
  )

