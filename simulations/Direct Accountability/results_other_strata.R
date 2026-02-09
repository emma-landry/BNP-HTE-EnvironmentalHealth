library(ggplot2)
library(patchwork)

load('simulations/Direct Accountability/DA_scenario1_alt.RData')
load('simulations/Direct Accountability/DA_scenario2_alt.RData')
load('simulations/Direct Accountability/BPCF_scenario1_alt.RData')
load('simulations/Direct Accountability/BPCF_scenario2_alt.RData')
load('simulations/Direct Accountability/CASBAH_scenario1_alt.RData')
load('simulations/Direct Accountability/CASBAH_scenario2_alt.RData')

# Sizes
n <- 500
samples <- 100

# Helper functions -------
compute_ATE <- function(tau_hat, tau_true) {
  ate_hat  <- mean(tau_hat)
  ate_true <- mean(tau_true)
  
  list(
    bias = ate_hat - ate_true,
    mse  = (ate_hat - ate_true)^2
  )
}

sym_limits <- function(x) {
  lim <- max(abs(x), na.rm = TRUE)
  c(-lim, lim)
}

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

# Strata definition ----
# True
strata_true_s1 <- lapply(1:samples, function(s) {
  S <- scenario_1[[s]]$M_1 - scenario_1[[s]]$M_0
  cut(
    S,
    breaks = quantile(S, probs = c(0, 1/3, 2/3, 1)),
    labels = c("Low","Mid","High"),
    include.lowest = TRUE
  )
})

strata_true_s2 <- lapply(1:samples, function(s) {
  factor(
    scenario_2[[s]]$clusters$S_strata,
    levels = c(0,1,2),
    labels = c("S(1) < S(0)", "S(1) = S(0)", "S(1) > S(0)")
  )
})

# BPCF 
strata_BPCF_s1 <- lapply(1:samples, function(s) {
  S_hat <- BPCF_scenario1[[s]]$M1_med - BPCF_scenario1[[s]]$M0_med
  cut(
    S_hat,
    breaks = quantile(S_hat, probs = c(0, 1/3, 2/3, 1)),
    labels = c("Low","Mid","High"),
    include.lowest = TRUE
  )
})

strata_BPCF_s2 <- lapply(1:samples, function(s) {
  S_hat <- BPCF_scenario2[[s]]$M1_med - BPCF_scenario2[[s]]$M0_med
  cut(
    S_hat,
    breaks = quantile(S_hat, probs = c(0, 1/3, 2/3, 1)),
    labels = c("S(1) < S(0)", "S(1) = S(0)", "S(1) > S(0)"),
    include.lowest = TRUE
  )
})

# CASBAH
strata_CASBAH_s1 <- lapply(1:samples, function(s) {
  factor(
    CASBAH_scenario1[[s]]$S_strata_cluster,
    levels = c(1,2,3),
    labels = c("Low","Mid","High")
  )
})

strata_CASBAH_s2 <- lapply(1:samples, function(s) {
  factor(
    CASBAH_scenario2[[s]]$S_strata_cluster,
    levels = c(1, 2, 3),
    labels = c("S(1) < S(0)", "S(1) = S(0)", "S(1) > S(0)")
  )
})

make_EP_strata <- function(pt_hat, strata, method, scenario) {
  do.call(rbind, lapply(1:samples, function(s) {
    data.frame(
      value = tapply(pt_hat[, s], strata[[s]], mean),
      Group = levels(strata[[s]]),
      Method = method,
      Scenario = scenario,
      Rep = s
    )
  }))
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

# Bias ----
# Bias: marginal P
bias_P_df <- rbind(
  data.frame(
    value = sapply(1:samples, function(s)
      compute_ATE(BPCF_pt_1[, s], simulate_pt_1[, s])$bias),
    Method = "BPCF", Scenario = "Scenario 1"
  ),
  data.frame(
    value = sapply(1:samples, function(s)
      compute_ATE(CASBAH_pt_1[, s], simulate_pt_1[, s])$bias),
    Method = "CASBAH", Scenario = "Scenario 1"
  ),
  data.frame(
    value = sapply(1:samples, function(s)
      compute_ATE(BPCF_pt_2[, s], simulate_pt_2[, s])$bias),
    Method = "BPCF", Scenario = "Scenario 2"
  ),
  data.frame(
    value = sapply(1:samples, function(s)
      compute_ATE(CASBAH_pt_2[, s], simulate_pt_2[, s])$bias),
    Method = "CASBAH", Scenario = "Scenario 2"
  )
)

# Bias: marginal Y
bias_Y_df <- rbind(
  data.frame(
    value = sapply(1:samples, function(s)
      compute_ATE(BPCF_tau_1[, s], simulated_tau_1[, s])$bias),
    Method = "BPCF", Scenario = "Scenario 1"
  ),
  data.frame(
    value = sapply(1:samples, function(s)
      compute_ATE(CASBAH_tau_1[, s], simulated_tau_1[, s])$bias),
    Method = "CASBAH", Scenario = "Scenario 1"
  ),
  data.frame(
    value = sapply(1:samples, function(s)
      compute_ATE(BPCF_tau_2[, s], simulated_tau_2[, s])$bias),
    Method = "BPCF", Scenario = "Scenario 2"
  ),
  data.frame(
    value = sapply(1:samples, function(s)
      compute_ATE(CASBAH_tau_2[, s], simulated_tau_2[, s])$bias),
    Method = "CASBAH", Scenario = "Scenario 2"
  )
)

# Stratify -----
make_bias_P_strata <- function(pt_hat, pt_true, strata, method, scenario) {
  do.call(rbind, lapply(1:samples, function(s) {
    data.frame(
      value = tapply(pt_hat[, s] - pt_true[, s], strata[[s]], mean),
      Group = levels(strata[[s]]),
      Method = method,
      Scenario = scenario
    )
  }))
}

bias_P_strata_df <- rbind(
  make_bias_P_strata(BPCF_pt_1, simulate_pt_1, strata_BPCF_s1, "BPCF", "Scenario 1"),
  make_bias_P_strata(CASBAH_pt_1, simulate_pt_1, strata_CASBAH_s1, "CASBAH", "Scenario 1"),
  make_bias_P_strata(BPCF_pt_2, simulate_pt_2, strata_BPCF_s2, "BPCF", "Scenario 2"),
  make_bias_P_strata(CASBAH_pt_2, simulate_pt_2, strata_CASBAH_s2, "CASBAH", "Scenario 2")
)


make_bias_P_strata <- function(pt_hat, pt_true, strata, method, scenario) {
  do.call(rbind, lapply(1:samples, function(s) {
    data.frame(
      value = tapply(pt_hat[, s] - pt_true[, s], strata[[s]], mean),
      Group = levels(strata[[s]]),
      Method = method,
      Scenario = scenario
    )
  }))
}

bias_P_strata_df <- rbind(
  make_bias_P_strata(BPCF_pt_1, simulate_pt_1, strata_BPCF_s1, "BPCF", "Scenario 1"),
  make_bias_P_strata(CASBAH_pt_1, simulate_pt_1, strata_CASBAH_s1, "CASBAH", "Scenario 1"),
  make_bias_P_strata(BPCF_pt_2, simulate_pt_2, strata_BPCF_s2, "BPCF", "Scenario 2"),
  make_bias_P_strata(CASBAH_pt_2, simulate_pt_2, strata_CASBAH_s2, "CASBAH", "Scenario 2")
)

make_bias_Y_strata <- function(tau_hat, tau_true, strata, method, scenario) {
  do.call(rbind, lapply(1:samples, function(s) {
    data.frame(
      value = tapply(tau_hat[, s] - tau_true[, s], strata[[s]], mean),
      Group = levels(strata[[s]]),
      Method = method,
      Scenario = scenario
    )
  }))
}

bias_Y_strata_df <- rbind(
  make_bias_Y_strata(BPCF_tau_1, simulated_tau_1, strata_BPCF_s1, "BPCF", "Scenario 1"),
  make_bias_Y_strata(CASBAH_tau_1, simulated_tau_1, strata_CASBAH_s1, "CASBAH", "Scenario 1"),
  make_bias_Y_strata(BPCF_tau_2, simulated_tau_2, strata_BPCF_s2, "BPCF", "Scenario 2"),
  make_bias_Y_strata(CASBAH_tau_2, simulated_tau_2, strata_CASBAH_s2, "CASBAH", "Scenario 2")
)

EP_strata_df <- rbind(
  make_EP_strata(BPCF_pt_1,   strata_BPCF_s1,   "BPCF",   "Scenario 1"),
  make_EP_strata(CASBAH_pt_1, strata_CASBAH_s1, "CASBAH", "Scenario 1"),
  make_EP_strata(BPCF_pt_2,   strata_BPCF_s2,   "BPCF",   "Scenario 2"),
  make_EP_strata(CASBAH_pt_2, strata_CASBAH_s2, "CASBAH", "Scenario 2")
)


# Scenario 1 ----
# Column 1
EP_true_s1 <- do.call(rbind, lapply(1:samples, function(s) {
  data.frame(
    value = tapply(simulate_pt_1[, s], strata_true_s1[[s]], mean),
    Group = levels(strata_true_s1[[s]]),
    Scenario = "Scenario 1",
    Rep = s
  )
}))

p1_s1 <- ggplot(
  subset(EP_strata_df, Scenario == "Scenario 1"),
  aes(Group, value, fill = Method, color = Method)
) +
  geom_boxplot(position = position_dodge(0.7), alpha = 0.35) +
  labs(x = NULL, y = "Value") +
  facet_wrap(~"E[P(1) - P(0) | strata]") +
  scale_fill_manual(values = paper_colors) +
  scale_color_manual(values = paper_colors) +
  paper_theme()

# Column 2
p2_s1 <- ggplot(subset(bias_P_df, Scenario == "Scenario 1"),
                aes(Method, value, fill = Method, color = Method)) +
  geom_boxplot(alpha = 0.35) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = sym_limits) +
  labs(x = NULL, y = "Bias") +
  facet_wrap(~"E[P(1) - P(0)]") +
  scale_fill_manual(values = paper_colors) +
  scale_color_manual(values = paper_colors) +
  paper_theme()

# Column 3
p3_s1 <- ggplot(subset(bias_Y_strata_df, Scenario == "Scenario 1"),
                aes(Group, value, fill = Method, color = Method)) +
  geom_boxplot(position = position_dodge(0.7), alpha = 0.35) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = sym_limits) +
  labs(x = NULL, y = "Bias") +
  facet_wrap(~"E[Y(1) - Y(0) | strata]") +
  scale_fill_manual(values = paper_colors) +
  scale_color_manual(values = paper_colors) +
  paper_theme()

# Column 4
p4_s1 <- ggplot(subset(bias_Y_df, Scenario == "Scenario 1"),
                aes(Method, value, fill = Method, color = Method)) +
  geom_boxplot(alpha = 0.35) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = sym_limits) +
  labs(x = NULL, y = "Bias") +
  facet_wrap(~"E[Y(1) - Y(0)]") +
  scale_fill_manual(values = paper_colors) +
  scale_color_manual(values = paper_colors) +
  paper_theme()

row_s1 <- p1_s1 | p2_s1 | p3_s1 | p4_s1

# Scenario 2 ----
EP_true_s2 <- do.call(rbind, lapply(1:samples, function(s) {
  data.frame(
    value = tapply(simulate_pt_2[, s], strata_true_s2[[s]], mean),
    Group = levels(strata_true_s2[[s]]),
    Scenario = "Scenario 2"
  )
}))

EP_true_s2 <- aggregate(value ~ Group + Scenario, EP_true_s2, mean)

p1_s2 <- ggplot(
  subset(EP_strata_df, Scenario == "Scenario 2"),
  aes(Group, value, fill = Method, color = Method)
) +
  geom_boxplot(position = position_dodge(0.7), alpha = 0.35) +
  geom_hline(
    data = EP_true_s2,
    aes(yintercept = value),
    linetype = "dashed",
    linewidth = 0.8,
    inherit.aes = FALSE
  ) +
  labs(x = NULL, y = "Value") +
  facet_wrap(~"E[P(1) - P(0) | strata]") +
  scale_fill_manual(values = paper_colors) +
  scale_color_manual(values = paper_colors) +
  paper_theme()

#p1_s2 <- p1_s1 %+% subset(bias_P_strata_df, Scenario == "Scenario 2")
p2_s2 <- p2_s1 %+% subset(bias_P_df,        Scenario == "Scenario 2")
p3_s2 <- p3_s1 %+% subset(bias_Y_strata_df, Scenario == "Scenario 2")
p4_s2 <- p4_s1 %+% subset(bias_Y_df,        Scenario == "Scenario 2")

row_s2 <- p1_s2 | p2_s2 | p3_s2 | p4_s2

scenario_title <- function(label) {
  ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = label,
             size = 5, fontface = "bold") +
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
