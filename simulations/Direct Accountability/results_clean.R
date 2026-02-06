library(ggplot2)
library(patchwork)

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

# Sizes
n <- 500
samples <- 100

# Figure style -------
paper_colors <- c(
  "BPCF"   = "#1F77B4",
  "CASBAH" = "#FF7F0E"
)

paper_theme <- function() {
  theme_classic() +
    theme(
      panel.grid.major.y = element_line(color = "grey80", linewidth = 0.4),
      panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.25),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
      legend.position = "none",
      strip.text = element_text(size = 12),
      plot.title = element_text(hjust = 0.5, size = 13)
    )
}

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

ggplot(
  bias_df_outcome,
  aes(x = Method, y = value, fill = Method, color = Method)
) +
  geom_boxplot(
    width = 0.6,
    outlier.size = 0.8,
    alpha = 0.35,
    linewidth = 0.9
  ) +
  geom_hline(
    yintercept = 0,
    color = "black",
    linewidth = 0.8,
    linetype = "dashed"
  ) +
  facet_wrap(~ Scenario, scales = "free_y") +
  scale_y_continuous(limits = function(x) {
    lim <- max(abs(x), na.rm = TRUE)
    c(-lim, lim)
  }) +
  scale_fill_manual(values = paper_colors) +
  scale_color_manual(values = paper_colors) +
  labs(
    y = "Bias",
    x = NULL,
    title = expression(paste(E, "[", Y(1) - Y(0), "]"))
  )+
  paper_theme() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14)
  )


ggsave('/Users/emmalandry/Documents/Falco_GSR/ReviewPaper_CEHR/DA_bias_Y_ATE.pdf', width = 8, height = 3.7)

rmse_df_outcome <- rbind(
  data.frame(value = sqrt(mse_ATE_BPCF_1),   Method = "BPCF",   Scenario = "Scenario 1"),
  data.frame(value = sqrt(mse_ATE_CASBAH_1), Method = "CASBAH", Scenario = "Scenario 1"),
  data.frame(value = sqrt(mse_ATE_BPCF_2),   Method = "BPCF",   Scenario = "Scenario 2"),
  data.frame(value = sqrt(mse_ATE_CASBAH_2), Method = "CASBAH", Scenario = "Scenario 2")
)

rmse_df_outcome$Scenario <- factor(rmse_df_outcome$Scenario)

ggplot(
  rmse_df_outcome,
  aes(x = Method, y = value, fill = Method, color = Method)
) +
  geom_boxplot(
    width = 0.6,
    outlier.size = 0.8,
    alpha = 0.35,
    linewidth = 0.9
  ) +
  facet_wrap(~ Scenario, scales = "free_y") +
  scale_fill_manual(values = paper_colors) +
  scale_color_manual(values = paper_colors) +
  labs(
    y = "RMSE",
    x = NULL,
    title = expression(paste(E, "[", Y(1) - Y(0), "]"))
  )+
  paper_theme() + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 14)
  )

ggsave('/Users/emmalandry/Documents/Falco_GSR/ReviewPaper_CEHR/DA_rmse_Y_ATE.pdf', width = 8, height = 3.7)

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

ggplot(
  bias_PT_df,
  aes(x = Method, y = value, fill = Method, color = Method)
) +
  geom_boxplot(
    width = 0.6,
    outlier.size = 0.8,
    alpha = 0.35,
    linewidth = 0.9
  ) +
  geom_hline(
    yintercept = 0,
    color = "black",
    linewidth = 0.8,
    linetype = "dashed"
  ) +
  facet_wrap(~ Scenario, scales = "free_y") +
  scale_y_continuous(limits = function(x) {
    lim <- max(abs(x), na.rm = TRUE)
    c(-lim, lim)
  }) +
  scale_fill_manual(values = paper_colors) +
  scale_color_manual(values = paper_colors) +
  labs(
    y = "Bias",
    x = NULL,
    title = expression(paste(E, "[", P(1) - P(0), "]"))
  )+
  paper_theme() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14)
  )

ggsave('/Users/emmalandry/Documents/Falco_GSR/ReviewPaper_CEHR/DA_bias_P_ATE.pdf', width = 8, height = 3.7)

rmse_PT_df <- rbind(
  data.frame(value = sqrt(mse_PT_BPCF_1),   Method = "BPCF",   Scenario = "Scenario 1"),
  data.frame(value = sqrt(mse_PT_CASBAH_1), Method = "CASBAH", Scenario = "Scenario 1"),
  data.frame(value = sqrt(mse_PT_BPCF_2),   Method = "BPCF",   Scenario = "Scenario 2"),
  data.frame(value = sqrt(mse_PT_CASBAH_2), Method = "CASBAH", Scenario = "Scenario 2")
)

rmse_PT_df$Scenario <- factor(rmse_PT_df$Scenario)

ggplot(
  rmse_PT_df,
  aes(x = Method, y = value, fill = Method, color = Method)
) +
  geom_boxplot(
    width = 0.6,
    outlier.size = 0.8,
    alpha = 0.35,
    linewidth = 0.9
  ) +
  facet_wrap(~ Scenario, scales = "free_y") +
  scale_fill_manual(values = paper_colors) +
  scale_color_manual(values = paper_colors) +
  labs(
    y = "RMSE",
    x = NULL,
    title = expression(paste(E, "[", P(1) - P(0), "]"))
  )+
  paper_theme() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14)
  )

ggsave('/Users/emmalandry/Documents/Falco_GSR/ReviewPaper_CEHR/DA_rmse_P_ATE.pdf', width = 8, height = 3.7)

# Group ----
# Get groups
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

group_labels_s2 <- lapply(1:samples, function(s) {
  factor(
    scenario_2[[s]]$clusters$S_strata,
    levels = c(0, 1, 2),
    labels = c("S(1) = S(0)", "S(1) > S(0)", "S(1) < S(0)")
  )
})

# Quantities
true_ATE_group_s1 <- lapply(1:samples, function(s) {
  tapply(simulated_tau_1[, s], group_labels_s1[[s]], mean)
})

true_ATE_group_s2 <- lapply(1:samples, function(s) {
  tapply(simulated_tau_2[, s], group_labels_s2[[s]], mean)
})

ATE_BPCF_group_s1 <- lapply(1:samples, function(s) {
  tapply(BPCF_tau_1[, s], group_labels_s1[[s]], mean)
})

ATE_BPCF_group_s2 <- lapply(1:samples, function(s) {
  tapply(BPCF_tau_2[, s], group_labels_s2[[s]], mean)
})

ATE_CASBAH_group_s1 <- lapply(1:samples, function(s) {
  tapply(CASBAH_tau_1[, s], group_labels_s1[[s]], mean)
})

ATE_CASBAH_group_s2 <- lapply(1:samples, function(s) {
  tapply(CASBAH_tau_2[, s], group_labels_s2[[s]], mean)
})

# Bias
bias_BPCF_s1 <- do.call(
  rbind,
  lapply(1:samples, function(s)
    ATE_BPCF_group_s1[[s]] - true_ATE_group_s1[[s]])
)

bias_CASBAH_s1 <- do.call(
  rbind,
  lapply(1:samples, function(s)
    ATE_CASBAH_group_s1[[s]] - true_ATE_group_s1[[s]])
)

bias_BPCF_s2 <- do.call(
  rbind,
  lapply(1:samples, function(s)
    ATE_BPCF_group_s2[[s]] - true_ATE_group_s2[[s]])
)

bias_CASBAH_s2 <- do.call(
  rbind,
  lapply(1:samples, function(s)
    ATE_CASBAH_group_s2[[s]] - true_ATE_group_s2[[s]])
)

# RMSE
rmse_BPCF_s1 <- sqrt(bias_BPCF_s1^2)
rmse_CASBAH_s1 <- sqrt(bias_CASBAH_s1^2)

rmse_BPCF_s2 <- sqrt(bias_BPCF_s2^2)
rmse_CASBAH_s2 <- sqrt(bias_CASBAH_s2^2)

# Figures
fill_cols <- c(
  # Scenario 1
  "BPCF_Low"    = "#bdd7e7",
  "BPCF_Mid"    = "#6baed6",
  "BPCF_High"   = "#2171b5",
  "CASBAH_Low"  = "#fdd0a2",
  "CASBAH_Mid"  = "#fd8d3c",
  "CASBAH_High" = "#e6550d",
  
  # Scenario 2
  "BPCF_S(1) = S(0)" = "#bdd7e7",
  "BPCF_S(1) > S(0)" = "#6baed6",
  "BPCF_S(1) < S(0)" = "#2171b5",
  "CASBAH_S(1) = S(0)" = "#fdd0a2",
  "CASBAH_S(1) > S(0)" = "#fd8d3c",
  "CASBAH_S(1) < S(0)" = "#e6550d"
)

levels_s1 <- c("Low", "Mid", "High")
levels_s2 <- c("S(1) < S(0)", "S(1) = S(0)", "S(1) > S(0)")

bias_group_df <- rbind(
  
  # Scenario 1
  data.frame(
    value    = as.vector(bias_BPCF_s1),
    Group    = rep(colnames(bias_BPCF_s1), each = samples),
    Method   = "BPCF",
    Scenario = "Scenario 1"
  ),
  data.frame(
    value    = as.vector(bias_CASBAH_s1),
    Group    = rep(colnames(bias_CASBAH_s1), each = samples),
    Method   = "CASBAH",
    Scenario = "Scenario 1"
  ),
  
  # Scenario 2
  data.frame(
    value    = as.vector(bias_BPCF_s2),
    Group    = rep(colnames(bias_BPCF_s2), each = samples),
    Method   = "BPCF",
    Scenario = "Scenario 2"
  ),
  data.frame(
    value    = as.vector(bias_CASBAH_s2),
    Group    = rep(colnames(bias_CASBAH_s2), each = samples),
    Method   = "CASBAH",
    Scenario = "Scenario 2"
  )
)

bias_group_df$Method   <- factor(bias_group_df$Method, levels = c("BPCF", "CASBAH"))
bias_group_df$Scenario <- factor(bias_group_df$Scenario)
bias_group_df$Group    <- factor(bias_group_df$Group)

bias_group_df$MethodGroup <- interaction(
  bias_group_df$Method,
  bias_group_df$Group,
  sep = "_"
)

ggplot(
  bias_group_df,
  aes(x = Method, y = value, fill = MethodGroup, color = MethodGroup)
) +
  geom_boxplot(
    width = 0.6,
    position = position_dodge(0.75),
    alpha = 0.35,
    linewidth = 0.9,
    outlier.size = 0.8
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.7) +
  facet_wrap(~ Scenario, scales = "free_y") +
  scale_fill_manual(values = fill_cols) +
  scale_color_manual(values = fill_cols) +
  scale_y_continuous(limits = function(x) {
    lim <- max(abs(x), na.rm = TRUE)
    c(-lim, lim)
  }) +
  labs(y = "Bias (ATE)", x = NULL) +
  paper_theme()


sym_limits <- function(x) {
  lim <- max(abs(x), na.rm = TRUE)
  c(-lim, lim)
}

bias_group_s1 <- subset(bias_group_df, Scenario == "Scenario 1")

bias_group_s1$Group <- factor(
  bias_group_s1$Group,
  levels = c("Low", "Mid", "High")
)
bias_group_s1$Panel <- "Scenario1"

bias_plot_s1 <- ggplot(
  bias_group_s1,
  aes(x = Group, y = value, fill = Method, color = Method)
) +
  geom_boxplot(
    width = 0.6,
    position = position_dodge(0.7),
    alpha = 0.35,
    linewidth = 0.9,
    outlier.size = 0.8
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.8) +
  scale_y_continuous(limits = sym_limits) +
  scale_fill_manual(values = paper_colors) +
  scale_color_manual(values = paper_colors) +
  labs(x = NULL, y = "Bias") +
  facet_wrap(~ Panel) +
  guides(fill = guide_legend(title = NULL),
         color = guide_legend(title = NULL)) +
  theme_classic() +
  theme(
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.4),
    panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.25),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    
    strip.text = element_text(size = 13),
    strip.background = element_rect(fill = NA, color = "black", linewidth = 0.8),
    
    legend.position = c(0.20, 0.9),
    legend.background = element_blank()
  )


bias_group_s2 <- subset(bias_group_df, Scenario == "Scenario 2")

bias_group_s2$Group <- factor(
  bias_group_s2$Group,
  levels = c("S(1) < S(0)", "S(1) = S(0)", "S(1) > S(0)")
)

bias_group_s2$Panel <- "Scenario 2"
bias_plot_s2 <- ggplot(
  bias_group_s2,
  aes(x = Group, y = value, fill = Method, color = Method)
) +
  geom_boxplot(
    width = 0.6,
    position = position_dodge(0.7),
    alpha = 0.35,
    linewidth = 0.9,
    outlier.size = 0.8
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.8) +
  scale_y_continuous(limits = sym_limits) +
  scale_fill_manual(values = paper_colors) +
  scale_color_manual(values = paper_colors) +
  labs(x = NULL, y = "Bias") +
  facet_wrap(~ Panel) +
  theme_classic() +
  guides(fill = guide_legend(title = NULL),
         color = guide_legend(title = NULL))+
  theme(
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.4),
    panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.25),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    
    strip.text = element_text(size = 13),
    strip.background = element_rect(fill = NA, color = "black", linewidth = 0.8),
    
    legend.position = c(0.82, 0.9),
    legend.background = element_blank()
  )
bias_plot_s1 | bias_plot_s2
ggsave('/Users/emmalandry/Documents/Falco_GSR/ReviewPaper_CEHR/DA_bias_groups_ATE.pdf', width = 8, height = 3.7)

rmse_group_df <- rbind(
  
  # Scenario 1
  data.frame(
    value    = as.vector(rmse_BPCF_s1),
    Group    = rep(colnames(rmse_BPCF_s1), each = samples),
    Method   = "BPCF",
    Scenario = "Scenario 1"
  ),
  data.frame(
    value    = as.vector(rmse_CASBAH_s1),
    Group    = rep(colnames(rmse_CASBAH_s1), each = samples),
    Method   = "CASBAH",
    Scenario = "Scenario 1"
  ),
  
  # Scenario 2
  data.frame(
    value    = as.vector(rmse_BPCF_s2),
    Group    = rep(colnames(rmse_BPCF_s2), each = samples),
    Method   = "BPCF",
    Scenario = "Scenario 2"
  ),
  data.frame(
    value    = as.vector(rmse_CASBAH_s2),
    Group    = rep(colnames(rmse_CASBAH_s2), each = samples),
    Method   = "CASBAH",
    Scenario = "Scenario 2"
  )
)

rmse_group_df$Method   <- factor(rmse_group_df$Method, levels = c("BPCF", "CASBAH"))
rmse_group_df$Scenario <- factor(rmse_group_df$Scenario)
rmse_group_df$Group    <- factor(rmse_group_df$Group)

rmse_group_df$MethodGroup <- interaction(
  rmse_group_df$Method,
  rmse_group_df$Group,
  sep = "_"
)


ggplot(
  rmse_group_df,
  aes(x = Method, y = value, fill = MethodGroup, color = MethodGroup)
) +
  geom_boxplot(
    width = 0.6,
    position = position_dodge(0.75),
    alpha = 0.35,
    linewidth = 0.9,
    outlier.size = 0.8
  ) +
  facet_wrap(~ Scenario, scales = "free_y") +
  scale_fill_manual(values = fill_cols) +
  scale_color_manual(values = fill_cols) +
  labs(y = "RMSE (ATE)", x = NULL) +
  paper_theme()

rmse_group_s1 <- subset(rmse_group_df, Scenario == "Scenario 1")

rmse_group_s1$Group <- factor(
  rmse_group_s1$Group,
  levels = c("Low", "Mid", "High")
)

rmse_group_s1$Panel <- "Scenario 1"

rmse_plot_s1 <- ggplot(
  rmse_group_s1,
  aes(x = Group, y = value, fill = Method, color = Method)
) +
  geom_boxplot(
    width = 0.6,
    position = position_dodge(0.7),
    alpha = 0.35,
    linewidth = 0.9,
    outlier.size = 0.8
  ) +
  scale_fill_manual(values = paper_colors) +
  scale_color_manual(values = paper_colors) +
  labs(x = NULL, y = "RMSE") +
  facet_wrap(~ Panel) +
  guides(
    fill  = guide_legend(title = NULL),
    color = guide_legend(title = NULL)
  ) +
  theme_classic() +
  theme(
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.4),
    panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.25),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    
    strip.text = element_text(size = 13),
    strip.background = element_rect(fill = NA, color = "black", linewidth = 0.8),
    
    legend.position = c(0.20, 0.9),
    legend.background = element_blank()
  )

rmse_group_s2 <- subset(rmse_group_df, Scenario == "Scenario 2")

rmse_group_s2$Group <- factor(
  rmse_group_s2$Group,
  levels = c("S(1) < S(0)", "S(1) = S(0)", "S(1) > S(0)")
)

rmse_group_s2$Panel <- "Scenario 2"

rmse_plot_s2 <- ggplot(
  rmse_group_s2,
  aes(x = Group, y = value, fill = Method, color = Method)
) +
  geom_boxplot(
    width = 0.6,
    position = position_dodge(0.7),
    alpha = 0.35,
    linewidth = 0.9,
    outlier.size = 0.8
  ) +
  scale_fill_manual(values = paper_colors) +
  scale_color_manual(values = paper_colors) +
  labs(x = NULL, y = "RMSE") +
  facet_wrap(~ Panel) +
  guides(
    fill  = guide_legend(title = NULL),
    color = guide_legend(title = NULL)
  ) +
  theme_classic() +
  theme(
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.4),
    panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.25),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    
    strip.text = element_text(size = 13),
    strip.background = element_rect(fill = NA, color = "black", linewidth = 0.8),
    
    legend.position = c(0.82, 0.9),
    legend.background = element_blank()
  )

rmse_plot_s1 | rmse_plot_s2
ggsave('/Users/emmalandry/Documents/Falco_GSR/ReviewPaper_CEHR/DA_rmse_groups_ATE.pdf', width = 8, height = 3.7)

# Three panels
bias_Y_s1 <- subset(bias_df_outcome, Scenario == "Scenario 1")
bias_Y_s1$Panel <- "E[Y(1) - Y(0)]"

bias_Y_plot_s1 <- ggplot(
  bias_Y_s1,
  aes(x = Method, y = value, fill = Method, color = Method)
) +
  geom_boxplot(width = 0.6, alpha = 0.35, linewidth = 0.9, outlier.size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.8) +
  scale_y_continuous(limits = sym_limits) +
  scale_fill_manual(values = paper_colors) +
  scale_color_manual(values = paper_colors) +
  labs(x = NULL, y = "Bias") +
  facet_wrap(~ Panel) +
  paper_theme()

bias_P_s1 <- subset(bias_PT_df, Scenario == "Scenario 1")
bias_P_s1$Panel <- "E[P(1) - P(0)]"

bias_P_plot_s1 <- ggplot(
  bias_P_s1,
  aes(x = Method, y = value, fill = Method, color = Method)
) +
  geom_boxplot(width = 0.6, alpha = 0.35, linewidth = 0.9, outlier.size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.8) +
  scale_y_continuous(limits = sym_limits) +
  scale_fill_manual(values = paper_colors) +
  scale_color_manual(values = paper_colors) +
  labs(x = NULL, y = "Bias") +
  facet_wrap(~ Panel) +
  paper_theme()


bias_G_s1 <- subset(bias_group_df, Scenario == "Scenario 1")
bias_G_s1$Group <- factor(bias_G_s1$Group, levels = c("Low", "Mid", "High"))
bias_G_s1$Panel <- "E[Y(1) - Y(0) | strata]"

bias_G_plot_s1 <- ggplot(
  bias_G_s1,
  aes(x = Group, y = value, fill = Method, color = Method)
) +
  geom_boxplot(
    width = 0.6,
    position = position_dodge(0.7),
    alpha = 0.35,
    linewidth = 0.9,
    outlier.size = 0.8
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.8) +
  scale_y_continuous(limits = sym_limits) +
  scale_fill_manual(values = paper_colors) +
  scale_color_manual(values = paper_colors) +
  labs(x = NULL, y = "Bias") +
  facet_wrap(~ Panel) +
  guides(
    fill  = guide_legend(title = NULL),
    color = guide_legend(title = NULL)
  )+
  theme_classic() +
  theme(
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.4),
    panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.25),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    strip.text = element_text(size = 13),
    strip.background = element_rect(fill = NA, color = "black", linewidth = 0.8),
    legend.position = c(0.25, 0.85),
    legend.background = element_blank()
  )

row_s1 <- bias_Y_plot_s1 | bias_P_plot_s1 | bias_G_plot_s1

bias_Y_s2 <- subset(bias_df_outcome, Scenario == "Scenario 2")
bias_Y_s2$Panel <- "E[Y(1) - Y(0)]"

bias_Y_plot_s2 <- ggplot(
  bias_Y_s2,
  aes(x = Method, y = value, fill = Method, color = Method)
) +
  geom_boxplot(width = 0.6, alpha = 0.35, linewidth = 0.9, outlier.size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.8) +
  scale_y_continuous(limits = sym_limits) +
  scale_fill_manual(values = paper_colors) +
  scale_color_manual(values = paper_colors) +
  labs(x = NULL, y = "Bias") +
  facet_wrap(~ Panel) +
  paper_theme()

bias_P_s2 <- subset(bias_PT_df, Scenario == "Scenario 2")
bias_P_s2$Panel <- "E[P(1) - P(0)]"

bias_P_plot_s2 <- ggplot(
  bias_P_s2,
  aes(x = Method, y = value, fill = Method, color = Method)
) +
  geom_boxplot(width = 0.6, alpha = 0.35, linewidth = 0.9, outlier.size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.8) +
  scale_y_continuous(limits = sym_limits) +
  scale_fill_manual(values = paper_colors) +
  scale_color_manual(values = paper_colors) +
  labs(x = NULL, y = "Bias") +
  facet_wrap(~ Panel) +
  paper_theme()

bias_G_s2 <- subset(bias_group_df, Scenario == "Scenario 2")
bias_G_s2$Group <- factor(
  bias_G_s2$Group,
  levels = c("S(1) < S(0)", "S(1) = S(0)", "S(1) > S(0)")
)
bias_G_s2$Panel <- "E[Y(1) - Y(0) | strata]"

bias_G_plot_s2 <- ggplot(
  bias_G_s2,
  aes(x = Group, y = value, fill = Method, color = Method)
) +
  geom_boxplot(
    width = 0.6,
    position = position_dodge(0.7),
    alpha = 0.35,
    linewidth = 0.9,
    outlier.size = 0.8
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.8) +
  scale_y_continuous(limits = sym_limits) +
  scale_fill_manual(values = paper_colors) +
  scale_color_manual(values = paper_colors) +
  labs(x = NULL, y = "Bias") +
  facet_wrap(~ Panel) +
  guides(
    fill  = guide_legend(title = NULL),
    color = guide_legend(title = NULL)
  ) +
  theme_classic() +
  theme(
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.4),
    panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.25),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    strip.text = element_text(size = 13),
    strip.background = element_rect(fill = NA, color = "black", linewidth = 0.8),
    legend.position = c(0.75, 0.85),
    legend.background = element_blank()
  )

row_s2 <- bias_Y_plot_s2 | bias_P_plot_s2 | bias_G_plot_s2

scenario_title <- function(label) {
  ggplot() +
    annotate(
      "text",
      x = 0.5, y = 0.5,
      label = label,
      size = 5,
      fontface = "bold"
    ) +
    theme_void() +
    theme(plot.margin = margin(b = 6, t = 6))
}

fig_final <-
  scenario_title("Scenario 1") /
  row_s1 /
  scenario_title("Scenario 2") /
  row_s2 +
  plot_layout(heights = c(0.12, 1, 0.12, 1))

fig_final

ggsave('/Users/emmalandry/Documents/Falco_GSR/ReviewPaper_CEHR/DA_bias_panel.pdf', width = 12, height = 9)


