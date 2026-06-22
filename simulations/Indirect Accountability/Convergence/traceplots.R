library(ggplot2)
library(dplyr)
library(ggh4x)

load('simulations/Indirect Accountability/Convergence/BCF_1.RData')
load('simulations/Indirect Accountability/Convergence/BCF_2.RData')
load('simulations/Indirect Accountability/Convergence/CDBMM_1.RData')
load('simulations/Indirect Accountability/Convergence/CDBMM_2.RData')

ibm_cols <- c("#648FFF", "#785EF0", "#DC267F", "#FE6100", "#FFB000")

# BCF Scenario 1
BCF_2_df <- bind_rows(lapply(seq_along(BCF_2), function(ch) {
  ate <- rowMeans(BCF_2[[ch]]$tau)
  data.frame(iter = seq_along(ate),ATE = ate, chain = factor(ch)) }))

ggplot(BCF_2_df, aes(iter, ATE, color = chain)) +
  geom_line(linewidth = 0.3) +
  scale_color_manual(values = ibm_cols) +
  ggh4x::facet_wrap2(~ chain, ncol = 1, scales = "free_y", axes = "all") +
  theme_classic() +
  labs(title = "BCF - Scenario 1", x = "Iteration", y = "") +
  scale_x_continuous(limits = c(min(BCF_2_df$iter), max(BCF_2_df$iter)),
                     expand = expansion(mult = c(0, 0.02))) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        panel.spacing.y = unit(0.8, "lines"))

ggsave(file = '/Users/emmalandry/Documents/Falco_GSR/ReviewPaper_CEHR/Trace plots/BCF_scenario1.pdf', width = 4.5, height = 8 )

# BCF Scenario 2
BCF_1_df <- bind_rows(lapply(seq_along(BCF_1), function(ch) {
    ate <- rowMeans(BCF_1[[ch]]$tau)
    data.frame(iter = seq_along(ate),ATE = ate, chain = factor(ch)) }))

ggplot(BCF_1_df, aes(iter, ATE, color = chain)) +
  geom_line(linewidth = 0.3) +
  scale_color_manual(values = ibm_cols) +
  ggh4x::facet_wrap2(~ chain, ncol = 1, scales = "free_y", axes = "all") +
  theme_classic() +
  labs(title = "BCF - Scenario 2", x = "Iteration", y = "") +
  scale_x_continuous(limits = c(min(BCF_1_df$iter), max(BCF_1_df$iter)),
                     expand = expansion(mult = c(0, 0.02))) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        panel.spacing.y = unit(0.8, "lines"))

ggsave(file = '/Users/emmalandry/Documents/Falco_GSR/ReviewPaper_CEHR/Trace plots/BCF_scenario2.pdf', width = 4.5, height = 8 )

# CDBMM Scenario 1
CDBMM_2_df <- bind_rows(lapply(seq_along(CDBMM_2), function(ch) {
  ate <- colMeans(CDBMM_2[[ch]]$Y1 - CDBMM_2[[ch]]$Y0)
  data.frame(iter = seq_along(ate), ATE = ate, chain = factor(ch))}))


ggplot(CDBMM_2_df, aes(iter, ATE, color = chain)) +
  geom_line(linewidth = 0.3) +
  scale_color_manual(values = ibm_cols) +
  ggh4x::facet_wrap2(~ chain, ncol = 1, scales = "free_y", axes = "all") +
  theme_classic() +
  labs(title = "CDBMM - Scenario 1", x = "Iteration", y = "") +
  scale_x_continuous(limits = c(min(CDBMM_2_df$iter), max(CDBMM_2_df$iter)),
                     expand = expansion(mult = c(0, 0.02))) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        panel.spacing.y = unit(0.8, "lines"))

ggsave(file = '/Users/emmalandry/Documents/Falco_GSR/ReviewPaper_CEHR/Trace plots/CDBMM_scenario1.pdf', width = 4.5, height = 8 )

# CDBMM Scenario 2
CDBMM_1_df <- bind_rows(lapply(seq_along(CDBMM_1), function(ch) {
  ate <- colMeans(CDBMM_1[[ch]]$Y1 - CDBMM_1[[ch]]$Y0)
  data.frame(iter = seq_along(ate), ATE = ate, chain = factor(ch))}))


ggplot(CDBMM_1_df, aes(iter, ATE, color = chain)) +
  geom_line(linewidth = 0.3) +
  scale_color_manual(values = ibm_cols) +
  ggh4x::facet_wrap2(~ chain, ncol = 1, scales = "free_y", axes = "all") +
  theme_classic() +
  labs(title = "CDBMM - Scenario 2", x = "Iteration", y = "") +
  scale_x_continuous(limits = c(min(CDBMM_1_df$iter), max(CDBMM_1_df$iter)),
                     expand = expansion(mult = c(0, 0.02))) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        panel.spacing.y = unit(0.8, "lines"))

ggsave(file = '/Users/emmalandry/Documents/Falco_GSR/ReviewPaper_CEHR/Trace plots/CDBMM_scenario2.pdf', width = 4.5, height = 8 )

