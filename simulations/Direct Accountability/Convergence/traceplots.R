library(ggplot2)
library(dplyr)
library(ggh4x)

load('simulations/Direct Accountability/Convergence/BPCF_1.RData')
load('simulations/Direct Accountability/Convergence/BPCF_2.RData')
load('simulations/Direct Accountability/Convergence/CASBAH_1.RData')
load('simulations/Direct Accountability/Convergence/CASBAH_2.RData')

ibm_cols <- c("#648FFF", "#785EF0", "#DC267F", "#FE6100", "#FFB000")

# BPCF Scenario 1
BPCF_1_df <- bind_rows(lapply(seq_along(BPCF_1), function(ch) {
  ate <- rowMeans(BPCF_1[[ch]]$Y1_med - BPCF_1[[ch]]$Y0_med)
  data.frame(iter = seq_along(ate), ATE = ate, chain = factor(ch))}))


ggplot(BPCF_1_df, aes(iter, ATE, color = chain)) +
  geom_line(linewidth = 0.3) +
  scale_color_manual(values = ibm_cols) +
  ggh4x::facet_wrap2(~ chain, ncol = 1, scales = "free_y", axes = "all") +
  theme_classic() +
  labs(title = "BPCF - Scenario 1", x = "Iteration", y = "") +
  scale_x_continuous(limits = c(min(BPCF_1_df$iter), max(BPCF_1_df$iter)),
                     expand = expansion(mult = c(0, 0.02))) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        panel.spacing.y = unit(0.8, "lines"))

ggsave(file = '/Users/emmalandry/Documents/Falco_GSR/ReviewPaper_CEHR/Trace plots/BPCF_scenario1.pdf', width = 4.5, height = 8 )


# BPCF Scenario 2
BPCF_2_df <- bind_rows(lapply(seq_along(BPCF_2), function(ch) {
  ate <- rowMeans(BPCF_2[[ch]]$Y1_med - BPCF_2[[ch]]$Y0_med)
  data.frame(iter = seq_along(ate), ATE = ate, chain = factor(ch))}))


ggplot(BPCF_2_df, aes(iter, ATE, color = chain)) +
  geom_line(linewidth = 0.3) +
  scale_color_manual(values = ibm_cols) +
  ggh4x::facet_wrap2(~ chain, ncol = 1, scales = "free_y", axes = "all") +
  theme_classic() +
  labs(title = "BPCF - Scenario 2", x = "Iteration", y = "") +
  scale_x_continuous(limits = c(min(BPCF_2_df$iter), max(BPCF_2_df$iter)),
                     expand = expansion(mult = c(0, 0.02))) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        panel.spacing.y = unit(0.8, "lines"))

ggsave(file = '/Users/emmalandry/Documents/Falco_GSR/ReviewPaper_CEHR/Trace plots/BPCF_scenario2.pdf', width = 4.5, height = 8 )


# CASBAH Scenario 1
CASBAH_1_df <- bind_rows(lapply(seq_along(CASBAH_1), function(ch) {
  ate <- colMeans(CASBAH_1[[ch]]$post_Y_1_imp - CASBAH_1[[ch]]$post_Y_0_imp)
  data.frame(iter = seq_along(ate), ATE = ate, chain = factor(ch))}))


ggplot(CASBAH_1_df, aes(iter, ATE, color = chain)) +
  geom_line(linewidth = 0.3) +
  scale_color_manual(values = ibm_cols) +
  ggh4x::facet_wrap2(~ chain, ncol = 1, scales = "free_y", axes = "all") +
  theme_classic() +
  labs(title = "CASBAH - Scenario 1", x = "Iteration", y = "") +
  scale_x_continuous(limits = c(min(CASBAH_1_df$iter), max(CASBAH_1_df$iter)), expand = expansion(mult = c(0, 0.02))) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        panel.spacing.y = unit(0.8, "lines"))

ggsave(file = '/Users/emmalandry/Documents/Falco_GSR/ReviewPaper_CEHR/Trace plots/CASBAH_scenario1.pdf', width = 4.5, height = 8 )


# CASBAH Scenario 2
CASBAH_2_df <- bind_rows(lapply(seq_along(CASBAH_2), function(ch) {
  ate <- colMeans(CASBAH_2[[ch]]$post_Y_1_imp - CASBAH_2[[ch]]$post_Y_0_imp)
  data.frame(iter = seq_along(ate), ATE = ate, chain = factor(ch))}))


ggplot(CASBAH_2_df, aes(iter, ATE, color = chain)) +
  geom_line(linewidth = 0.3) +
  scale_color_manual(values = ibm_cols) +
  ggh4x::facet_wrap2(~ chain, ncol = 1, scales = "free_y", axes = "all") +
  theme_classic() +
  labs(title = "CASBAH - Scenario 2", x = "Iteration", y = "") +
  scale_x_continuous(limits = c(min(CASBAH_2_df$iter), max(CASBAH_2_df$iter)), expand = expansion(mult = c(0, 0.02))) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        panel.spacing.y = unit(0.8, "lines"))

ggsave(file = '/Users/emmalandry/Documents/Falco_GSR/ReviewPaper_CEHR/Trace plots/CASBAH_scenario2.pdf', width = 4.5, height = 8 )


