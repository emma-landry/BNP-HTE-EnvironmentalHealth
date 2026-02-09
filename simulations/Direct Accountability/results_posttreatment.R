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

strata_true_s1 <- lapply(1:samples, function(s) {
  S_true <- scenario_1[[s]]$M_1 - scenario_1[[s]]$M_0
  
  cut(
    S_true,
    breaks = quantile(S_true, probs = c(0, 1/3, 2/3, 1)),
    labels = c("Low", "Mid", "High"),
    include.lowest = TRUE
  )
})


strata_BPCF_s1 <- lapply(1:samples, function(s) {
  S_hat <- BPCF_scenario1[[s]]$M1_med - BPCF_scenario1[[s]]$M0_med
  
  cut(
    S_hat,
    breaks = quantile(S_hat, probs = c(0, 1/3, 2/3, 1)),
    labels = c("Low", "Mid", "High"),
    include.lowest = TRUE
  )
})

strata_CASBAH_s1 <- lapply(1:samples, function(s) {
  factor(
    CASBAH_scenario1[[s]]$S_strata_cluster,
    levels = c(1, 2, 3),
    labels = c("Low", "Mid", "High")
  )
})

make_S_value_df <- function(S_mat, strata, method, scenario) {
  do.call(rbind, lapply(1:samples, function(s) {
    data.frame(
      value    = tapply(S_mat[, s], strata[[s]], mean),
      Group    = levels(strata[[s]]),
      Method   = method,
      Scenario = scenario
    )
  }))
}

S_true_1 <- sapply(1:samples, function(s)
  scenario_1[[s]]$M_1 - scenario_1[[s]]$M_0
)

# BPCF
S_BPCF_1 <- sapply(1:samples, function(s)
  BPCF_scenario1[[s]]$M1_med - BPCF_scenario1[[s]]$M0_med
)

# CASBAH
S_CASBAH_1 <- sapply(1:samples, function(s)
  CASBAH_scenario1[[s]]$post_P_1_imp - CASBAH_scenario1[[s]]$post_P_0_imp
)

S_value_s1_df <- rbind(
  make_S_value_df(S_true_1,   strata_true_s1,   "Simulation", "Scenario 1"),
  make_S_value_df(S_BPCF_1,   strata_BPCF_s1,   "BPCF",       "Scenario 1"),
  make_S_value_df(S_CASBAH_1, strata_CASBAH_s1, "CASBAH",     "Scenario 1")
)


make_S_value_df <- function(S_mat, strata, method, scenario) {
  do.call(rbind, lapply(1:samples, function(s) {
    
    tmp <- tapply(S_mat[, s], strata[[s]], mean)
    
    data.frame(
      value    = as.numeric(tmp),
      Group    = names(tmp),
      Method   = method,
      Scenario = scenario
    )
  }))
}

plot_colors <- c(
  "Simulation" = "grey60",
  "BPCF"       = "#1F77B4",
  "CASBAH"     = "#FF7F0E"
)

S_value_s1_df$Group <- factor(
  S_value_s1_df$Group,
  levels = c("Low", "Mid", "High")
)

ggplot(
  subset(S_value_s1_df, Scenario == "Scenario 1"),
  aes(x = Group, y = value, fill = Method, color = Method)
) +
  geom_boxplot(
    position = position_dodge(width = 0.7),
    alpha = 0.35,
    outlier.size = 0.8
  ) +
  labs(
    x = NULL,
    y = expression(E[P(1) - P(0) ~ "|" ~ stratum]),
    title = "Scenario 1"
  ) +
  scale_fill_manual(values = plot_colors) +
  scale_color_manual(values = plot_colors) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA),
    legend.title = element_blank()
  )

