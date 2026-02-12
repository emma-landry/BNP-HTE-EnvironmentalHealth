library(ggplot2)
library(patchwork)
library(grid)
library(tidyr)
library(dplyr)

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
bias_ATE_CDBMM_1 <- sapply(1:samples, function(s) compute_ATE(tau_hat  = CDBMM_scenario_1[[s]]$tau, tau_true = simulated_tau_1[, s])$bias)

bias_ATE_CDBMM_2 <- sapply(1:samples, function(s) compute_ATE(tau_hat  = CDBMM_scenario_2[[s]]$tau, tau_true = simulated_tau_2[, s])$bias)

bias_ATE_BCF_1 <- sapply(1:samples, function(s) compute_ATE(tau_hat  = BCF_scenario_1[[s]]$tau, tau_true = simulated_tau_1[, s])$bias)

bias_ATE_BCF_2 <- sapply(1:samples, function(s) compute_ATE(tau_hat  = BCF_scenario_2[[s]]$tau, tau_true = simulated_tau_2[, s])$bias)

# MSE for ATE
mse_ATE_CDBMM_1 <- sapply(1:samples, function(s) compute_ATE(tau_hat  = CDBMM_scenario_1[[s]]$tau, tau_true = simulated_tau_1[, s])$mse)

mse_ATE_CDBMM_2 <- sapply(1:samples, function(s) compute_ATE(tau_hat  = CDBMM_scenario_2[[s]]$tau, tau_true = simulated_tau_2[, s])$mse)

mse_ATE_BCF_1 <- sapply(1:samples, function(s) compute_ATE(tau_hat  = BCF_scenario_1[[s]]$tau, tau_true = simulated_tau_1[, s])$mse)

mse_ATE_BCF_2 <- sapply(1:samples, function(s) compute_ATE(tau_hat  = BCF_scenario_2[[s]]$tau, tau_true = simulated_tau_2[, s])$mse)

# Figures
bias_df <- rbind(data.frame(value = bias_ATE_CDBMM_1, Method = "CDBMM", Scenario = "Scenario 1"),
                 data.frame(value = bias_ATE_BCF_1, Method  = "BCF", Scenario = "Scenario 1"),
                 data.frame(value = bias_ATE_CDBMM_2, Method = "CDBMM", Scenario = "Scenario 2"),
                 data.frame(value = bias_ATE_BCF_2, Method = "BCF", Scenario = "Scenario 2"))

# Switching scenario 1 and scenario 2
bias_df$Scenario <- ifelse(bias_df$Scenario == "Scenario 1", "Scenario 2", "Scenario 1")
bias_df$Scenario <- factor(bias_df$Scenario)


ylims_df <- bias_df %>%
  group_by(Scenario) %>%
  summarise(lim = max(abs(value), na.rm = TRUE))

ggplot(bias_df, aes(x = Method, y = value, fill = Method,color = Method)) +
  geom_boxplot(width = 0.6, outlier.size = 0.8, alpha = 0.35, linewidth = 0.9) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.8, linetype = "dashed") +
  facet_wrap(~ Scenario, scales = "free_y") +
  scale_y_continuous(limits = function(x) {
      lim <- max(abs(x), na.rm = TRUE)
      c(-lim, lim)}) +
  scale_fill_manual(values = c("BCF"   = "#1F77B4", "CDBMM" = "#FF7F0E")) +
  scale_color_manual(values = c("BCF"   = "#1F77B4","CDBMM" = "#FF7F0E")) +
  labs(y = "Bias (ATE)", x = NULL) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "grey80", linewidth = 0.4),
        panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.25),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(color = "black",fill = NA,linewidth = 0.8),
        legend.position = "none",
        strip.text = element_text(size = 12))

rmse_df <- rbind(data.frame(value = sqrt(mse_ATE_CDBMM_1), Method = "CDBMM", Scenario = "Scenario 1"),
                 data.frame(value = sqrt(mse_ATE_BCF_1), Method = "BCF", Scenario = "Scenario 1"),
                 data.frame(value = sqrt(mse_ATE_CDBMM_2), Method = "CDBMM", Scenario = "Scenario 2"),
                 data.frame(value = sqrt(mse_ATE_BCF_2), Method = "BCF", Scenario = "Scenario 2"))

# Switching scenario 1 and scenario 2
rmse_df$Scenario <- ifelse(rmse_df$Scenario == "Scenario 1", "Scenario 2", "Scenario 1")
rmse_df$Scenario <- factor(rmse_df$Scenario)

ggplot(rmse_df, aes(x = Method, y = value, fill = Method, color = Method)) +
  geom_boxplot(width = 0.6, outlier.size = 0.8, alpha = 0.35, linewidth = 0.9) +
  facet_wrap(~ Scenario, scales = "free_y") +
  scale_fill_manual(values = c( "BCF"   = "#1F77B4", "CDBMM" = "#FF7F0E")) +
  scale_color_manual(values = c("BCF"   = "#1F77B4", "CDBMM" = "#FF7F0E")) +
  labs(y = "RMSE (ATE)", x = NULL) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "grey80", linewidth = 0.4),
        panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.25),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
        legend.position = "none",
        strip.text = element_text(size = 12))

# CATE ---------------
# Bias for CATE
bias_CATE_CDBMM_1 <- sapply(1:samples, function(s) compute_CATE(CDBMM_scenario_1[[s]]$tau, simulated_tau_1[, s])$bias)

bias_CATE_CDBMM_2 <- sapply(1:samples, function(s) compute_CATE(CDBMM_scenario_2[[s]]$tau, simulated_tau_2[, s])$bias)

bias_CATE_BCF_1 <- sapply(1:samples, function(s) compute_CATE(BCF_scenario_1[[s]]$tau, simulated_tau_1[, s])$bias)

bias_CATE_BCF_2 <- sapply(1:samples, function(s) compute_CATE(BCF_scenario_2[[s]]$tau, simulated_tau_2[, s])$bias)

# MSE for CATE
mse_CATE_CDBMM_1 <- sapply(1:samples, function(s) compute_CATE(CDBMM_scenario_1[[s]]$tau, simulated_tau_1[, s])$mse)

mse_CATE_CDBMM_2 <- sapply(1:samples, function(s) compute_CATE(CDBMM_scenario_2[[s]]$tau, simulated_tau_2[, s])$mse)

mse_CATE_BCF_1 <- sapply(1:samples, function(s) compute_CATE(BCF_scenario_1[[s]]$tau, simulated_tau_1[, s])$mse)

mse_CATE_BCF_2 <- sapply(1:samples, function(s) compute_CATE(BCF_scenario_2[[s]]$tau, simulated_tau_2[, s])$mse)

# Figures
bias_CATE_df <- rbind(data.frame(value = bias_CATE_CDBMM_1, Method = "CDBMM", Scenario = "Scenario 1"),
                      data.frame(value = bias_CATE_BCF_1, Method = "BCF", Scenario = "Scenario 1"),
                      data.frame(value = bias_CATE_CDBMM_2, Method   = "CDBMM", Scenario = "Scenario 2"),
                      data.frame(value = bias_CATE_BCF_2, Method   = "BCF", Scenario = "Scenario 2"))

# Switching scenario 1 and scenario 2
bias_CATE_df$Scenario <- ifelse(bias_CATE_df$Scenario == "Scenario 1", "Scenario 2","Scenario 1")
bias_CATE_df$Scenario <- factor(bias_CATE_df$Scenario)

ggplot(bias_CATE_df, aes(x = Method, y = value, fill = Method)) +
  geom_boxplot(width = 0.6, outlier.size = 0.8) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.7) +
  facet_wrap(~ Scenario, scales = "free_y") +
  labs(y = "Bias (CATE)", x = NULL) +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
        legend.position = "none",
        strip.text = element_text(size = 12))

rmse_CATE_df <- rbind(data.frame(value = sqrt(mse_CATE_CDBMM_1), Method = "CDBMM", Scenario = "Scenario 1"),
                      data.frame(value = sqrt(mse_CATE_BCF_1), Method = "BCF", Scenario = "Scenario 1"),
                      data.frame(value = sqrt(mse_CATE_CDBMM_2), Method = "CDBMM", Scenario = "Scenario 2"),
                      data.frame(value = sqrt(mse_CATE_BCF_2), Method = "BCF", Scenario = "Scenario 2"))

# Switching scenario 1 and scenario 2
rmse_CATE_df$Scenario <- ifelse(rmse_CATE_df$Scenario == "Scenario 1", "Scenario 2","Scenario 1")
rmse_CATE_df$Scenario <- factor(rmse_CATE_df$Scenario)

ggplot(rmse_CATE_df, aes(x = Method, y = value, fill = Method)) +
  geom_boxplot(width = 0.6, outlier.size = 0.8) +
  facet_wrap(~ Scenario, scales = "free_y") +
  labs(y = "RMSE (CATE)", x = NULL) +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
        legend.position = "none",strip.text = element_text(size = 12))

# GATE ------
# Bias for CATE
bias_GATE_CDBMM_1 <- sapply(1:samples, function(s)
  compute_GATE(CDBMM_scenario_1[[s]]$tau, simulated_tau_1[, s], compute_CDBMM_groups(CDBMM_scenario_1[[s]]$partition))$bias)

bias_GATE_CDBMM_2 <- sapply(1:samples, function(s)
  compute_GATE(CDBMM_scenario_2[[s]]$tau, simulated_tau_2[, s], compute_CDBMM_groups(CDBMM_scenario_2[[s]]$partition))$bias)

bias_GATE_BCF_1 <- sapply(1:samples, function(s)
  compute_GATE(BCF_scenario_1[[s]]$tau, simulated_tau_1[, s], CART_scenario_1[[s]]$partition)$bias)

bias_GATE_BCF_2 <- sapply(1:samples, function(s)
  compute_GATE(BCF_scenario_2[[s]]$tau, simulated_tau_2[, s], CART_scenario_2[[s]]$partition)$bias)

# MSE for GATE
mse_GATE_CDBMM_1 <- sapply(1:samples, function(s)
  compute_GATE(CDBMM_scenario_1[[s]]$tau, simulated_tau_1[, s], compute_CDBMM_groups(CDBMM_scenario_1[[s]]$partition))$mse)

mse_GATE_CDBMM_2 <- sapply(1:samples, function(s)
  compute_GATE(CDBMM_scenario_2[[s]]$tau, simulated_tau_2[, s], compute_CDBMM_groups(CDBMM_scenario_2[[s]]$partition))$mse)

mse_GATE_BCF_1 <- sapply(1:samples, function(s)
  compute_GATE(BCF_scenario_1[[s]]$tau, simulated_tau_1[, s], CART_scenario_1[[s]]$partition)$mse)

mse_GATE_BCF_2 <- sapply(1:samples, function(s)
  compute_GATE(BCF_scenario_2[[s]]$tau, simulated_tau_2[, s], CART_scenario_2[[s]]$partition)$mse)

# Figures

bias_GATE_df <- rbind(data.frame(value = bias_GATE_CDBMM_1, Method = "CDBMM", Scenario = "Scenario 1"),
                      data.frame(value = bias_GATE_BCF_1, Method = "BCF", Scenario = "Scenario 1"),
                      data.frame(value = bias_GATE_CDBMM_2, Method = "CDBMM", Scenario = "Scenario 2"),
                      data.frame(value = bias_GATE_BCF_2, Method = "BCF", Scenario = "Scenario 2"))

# Switching scenario 1 and scenario 2
bias_GATE_df$Scenario <- ifelse(bias_GATE_df$Scenario == "Scenario 1", "Scenario 2", "Scenario 1")
bias_GATE_df$Scenario <- factor(bias_GATE_df$Scenario)

ggplot(bias_GATE_df, aes(x = Method, y = value, fill = Method)) +
  geom_boxplot(width = 0.6, outlier.size = 0.8) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.7) +
  facet_wrap(~ Scenario, scales = "free_y") +
  labs(y = "Bias (GATE)", x = NULL) +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
        legend.position = "none",
        strip.text = element_text(size = 12))

rmse_GATE_df <- rbind(data.frame(value = sqrt(mse_GATE_CDBMM_1), Method = "CDBMM", Scenario = "Scenario 1"),
                      data.frame(value = sqrt(mse_GATE_BCF_1), Method = "BCF", Scenario = "Scenario 1"),
                      data.frame(value = sqrt(mse_GATE_CDBMM_2), Method = "CDBMM", Scenario = "Scenario 2"),
                      data.frame(value = sqrt(mse_GATE_BCF_2), Method = "BCF", Scenario = "Scenario 2"))

# Switching scenario 1 and scenario 2
rmse_GATE_df$Scenario <- ifelse(rmse_GATE_df$Scenario == "Scenario 1", "Scenario 2", "Scenario 1")
rmse_GATE_df$Scenario <- factor(rmse_GATE_df$Scenario)

ggplot(rmse_GATE_df, aes(x = Method, y = value, fill = Method)) +
  geom_boxplot(width = 0.6, outlier.size = 0.8) +
  facet_wrap(~ Scenario, scales = "free_y") +
  labs(y = "RMSE (GATE)", x = NULL) +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
        legend.position = "none",
        strip.text = element_text(size = 12))

# Group histograms -----
count_groups <- function(groups) {
  length(unique(groups))
}

ngroups_s1 <- data.frame(
  n_groups = c(sapply(1:samples, function(s) 
                      count_groups(compute_CDBMM_groups(CDBMM_scenario_1[[s]]$partition))),
              sapply(1:samples, function(s)
                     count_groups(CART_scenario_1[[s]]$partition))),
  Method   = rep(c("CDBMM", "BCF"), each = samples),
  Scenario = "Scenario 1"
)

ngroups_s2 <- data.frame(
  n_groups = c(sapply(1:samples, function(s)
                      count_groups(compute_CDBMM_groups(CDBMM_scenario_2[[s]]$partition))),
               sapply(1:samples, function(s)
                      count_groups(CART_scenario_2[[s]]$partition))),
  Method   = rep(c("CDBMM", "BCF"), each = samples),
  Scenario = "Scenario 2")

ngroups_df <- rbind(ngroups_s1, ngroups_s2)

ngroups_df$Scenario <- ifelse(
  ngroups_df$Scenario == "Scenario 1",
  "Scenario 2",
  "Scenario 1"
)

ngroups_df$Scenario <- factor(ngroups_df$Scenario)

ggplot(ngroups_df, aes(x = n_groups, fill = Method)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 1,
                 position = "identity",
                 alpha = 0.35,
                 color = NA) +
  geom_segment(data = data.frame(Scenario = "Scenario 2", x = 5),
               aes(x = x, xend = x, y = 0, yend = 1),
               inherit.aes = FALSE,
               linetype = "dashed",
               linewidth = 0.9,
               color = "black") +
  facet_wrap(~ Scenario, scales = "free") +
  scale_fill_manual(values = c("BCF"   = "#1F77B4","CDBMM" = "#FF7F0E")) +
  scale_x_continuous(breaks = function(x) pretty(x, n = 5)) +
  labs(x = "Number of groups", y = "Density", fill = "Method") +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "grey80", linewidth = 0.4),
        panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.25),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
        strip.text = element_text(size = 12),
        legend.position = "top")

# Threee panel figures -----
bias_s1 <- subset(bias_df, Scenario == "Scenario 1")
bias_s2 <- subset(bias_df, Scenario == "Scenario 2")

rmse_s1 <- subset(rmse_df, Scenario == "Scenario 1")
rmse_s2 <- subset(rmse_df, Scenario == "Scenario 2")

ngroups_s1 <- subset(ngroups_df, Scenario == "Scenario 1")
ngroups_s2 <- subset(ngroups_df, Scenario == "Scenario 2")

bias_s1$Panel <- "Bias"

bias_plot_s1 <- ggplot(bias_s1, aes(x = Method, y = value, fill = Method, color = Method)) +
  geom_boxplot(width = 0.6, outlier.size = 0.8, alpha = 0.35, linewidth = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.8) +
  scale_y_continuous(limits = function(x) {
    lim <- max(abs(x), na.rm = TRUE)
    c(-lim, lim)}) +
  scale_fill_manual(values = c("BCF" = "#1F77B4", "CDBMM" = "#FF7F0E")) +
  scale_color_manual(values = c("BCF" = "#1F77B4", "CDBMM" = "#FF7F0E")) +
  labs(x = NULL, y = NULL) +
  facet_wrap(~ Panel) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "grey80", linewidth = 0.4),
        panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.25),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
        legend.position = "none",
        strip.text = element_text(size = 13),
        strip.background = element_rect(
        fill = NA,
        color = "black",
        linewidth = 0.8))

rmse_s1$Panel <- "RMSE"

rmse_plot_s1 <- ggplot(rmse_s1, aes(x = Method, y = value, fill = Method, color = Method)) +
  geom_boxplot(width = 0.6, outlier.size = 0.8, alpha = 0.35, linewidth = 0.9) +
  scale_fill_manual(values = c("BCF" = "#1F77B4", "CDBMM" = "#FF7F0E")) +
  scale_color_manual(values = c("BCF" = "#1F77B4", "CDBMM" = "#FF7F0E")) +
  labs(x = NULL, y = NULL) +
  facet_wrap(~ Panel) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "grey80", linewidth = 0.4),
        panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.25),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
        legend.position = "none",
        strip.text = element_text(size = 13),
        strip.background = element_rect(fill = NA, color = "black", linewidth = 0.8))

ngroups_s1$Panel <- "Number of groups distribution"

ngroups_plot_s1 <- ggplot(ngroups_s1, aes(x = n_groups, fill = Method)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 1,
                 position = "identity",
                 alpha = 0.35,
                 color = NA)  +
  scale_fill_manual(values = c("BCF" = "#1F77B4", "CDBMM" = "#FF7F0E")) +
  scale_x_continuous(breaks = function(x) pretty(x, n = 5)) +
  labs(x = "", y = NULL, fill = NULL) +
  facet_wrap(~ Panel) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "grey80", linewidth = 0.4),
        panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.25),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
        legend.position = c(0.75, 0.8),
        legend.background = element_blank(),
        strip.text = element_text(size = 13),
        strip.background = element_rect(fill = NA, color = "black", linewidth = 0.8))

fig_scenario1 <- bias_plot_s1 | rmse_plot_s1 | ngroups_plot_s1
fig_scenario1

bias_s2$Panel <- "Bias"

bias_plot_s2 <- ggplot(bias_s2, aes(x = Method, y = value, fill = Method, color = Method)) +
  geom_boxplot(width = 0.6, outlier.size = 0.8, alpha = 0.35, linewidth = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.8) +
  scale_y_continuous(limits = function(x) {
    lim <- max(abs(x), na.rm = TRUE)
    c(-lim, lim)}) +
  scale_fill_manual(values = c("BCF" = "#1F77B4", "CDBMM" = "#FF7F0E")) +
  scale_color_manual(values = c("BCF" = "#1F77B4", "CDBMM" = "#FF7F0E")) +
  labs(x = NULL, y = NULL) +
  facet_wrap(~ Panel) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "grey80", linewidth = 0.4),
        panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.25),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
        legend.position = "none",
        strip.text = element_text(size = 13),
        strip.background = element_rect(fill = NA, color = "black", linewidth = 0.8))

rmse_s2$Panel <- "RMSE"

rmse_plot_s2 <- ggplot(rmse_s2, aes(x = Method, y = value, fill = Method, color = Method)) +
    geom_boxplot(width = 0.6, outlier.size = 0.8, alpha = 0.35, linewidth = 0.9) +
    scale_fill_manual(values = c("BCF" = "#1F77B4", "CDBMM" = "#FF7F0E")) +
    scale_color_manual(values = c("BCF" = "#1F77B4", "CDBMM" = "#FF7F0E")) +
    labs(x = NULL, y = NULL) +
    facet_wrap(~ Panel) +
    theme_classic() +
    theme(panel.grid.major.y = element_line(color = "grey80", linewidth = 0.4),
          panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.25),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
          legend.position = "none",
          strip.text = element_text(size = 13),
          strip.background = element_rect(fill = NA, color = "black", linewidth = 0.8))

ngroups_s2$Panel <- "Number of groups distribution"

ngroups_plot_s2 <- ggplot(ngroups_s2, aes(x = n_groups, fill = Method)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 1,
                 position = "identity",
                 alpha = 0.35,
                 color = NA) +
  annotate("segment", x = 5, xend = 5, y = 0, yend = 1, linetype = "dashed", linewidth = 0.9) +
  scale_fill_manual(values = c("BCF" = "#1F77B4", "CDBMM" = "#FF7F0E")) +
  scale_x_continuous(breaks = function(x) pretty(x, n = 5)) +
  labs(x = "", y = NULL, fill = NULL) +
  facet_wrap(~ Panel) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "grey80", linewidth = 0.4),
        panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.25),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
        legend.position = c(0.35, 0.8),
        legend.background = element_blank(),
        strip.text = element_text(size = 13),
        strip.background = element_rect(fill = NA, color = "black", linewidth = 0.8))

fig_scenario2 <- bias_plot_s2 | rmse_plot_s2 | ngroups_plot_s2
fig_scenario2

scenario_title <- function(label) {
  ggplot() +
  annotate("text", x = 0.5, y = 0.5, label = label, size = 5, fontface = "bold") +
  theme_void() +
  theme(plot.margin = margin(b = 6, t = 6))
}

row1 <- bias_plot_s1 | rmse_plot_s1 | ngroups_plot_s1
row2 <- bias_plot_s2 | rmse_plot_s2 | ngroups_plot_s2


fig_both <- scenario_title("Scenario 1") / row1 / scenario_title("Scenario 2") / row2 +
  plot_layout(heights = c(0.12, 1, 0.12, 1))

fig_both

