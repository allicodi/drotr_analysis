
here::i_am("05_compare_results.R")

library(drotr)
library(dplyr)

# Original

gs_res_seed_1 <- readRDS(here::here("results_csv/results_gs_seed_1.Rds"))
host_res_seed_1 <- readRDS(here::here("results_csv/results_host_seed_1.Rds"))

compare.otr_results(gs_res_seed_1, host_res_seed_1, threshold = 0.06, "se", "se")
compare.otr_results(gs_res_seed_1, host_res_seed_1, threshold = 0.08, "se", "se")
compare.otr_results(gs_res_seed_1, host_res_seed_1, threshold = 0.10, "se", "se")

compare.otr_results(gs_res_seed_1, host_res_seed_1, threshold = 0.06, "se_dZ0", "se_dZ0")
compare.otr_results(gs_res_seed_1, host_res_seed_1, threshold = 0.08, "se_dZ0", "se_dZ0")
compare.otr_results(gs_res_seed_1, host_res_seed_1, threshold = 0.10, "se_dZ0", "se_dZ0")

compare.otr_results(gs_res_seed_1, host_res_seed_1, threshold = 0.06, "te", "te")
compare.otr_results(gs_res_seed_1, host_res_seed_1, threshold = 0.08, "te", "te")
compare.otr_results(gs_res_seed_1, host_res_seed_1, threshold = 0.10, "te", "te")
 
#--------------------------------------------

# Repeat for day3diar

day3diar_gs_res_seed_1 <- readRDS(here::here("results_csv/results_day3diar_gs_seed1.Rds"))
day3diar_host_res_seed_1 <- readRDS(here::here("results_csv/results_day3diar_host_seed1.Rds"))

compare.otr_results(day3diar_gs_res_seed_1, day3diar_host_res_seed_1, threshold = -0.06, "se", "se")
compare.otr_results(day3diar_gs_res_seed_1, day3diar_host_res_seed_1, threshold = -0.08, "se", "se")
compare.otr_results(day3diar_gs_res_seed_1, day3diar_host_res_seed_1, threshold = -0.10, "se", "se")

compare.otr_results(day3diar_gs_res_seed_1, day3diar_host_res_seed_1, threshold = -0.06, "se_dZ0", "se_dZ0")
compare.otr_results(day3diar_gs_res_seed_1, day3diar_host_res_seed_1, threshold = -0.08, "se_dZ0", "se_dZ0")
compare.otr_results(day3diar_gs_res_seed_1, day3diar_host_res_seed_1, threshold = -0.10, "se_dZ0", "se_dZ0")

compare.otr_results(day3diar_gs_res_seed_1, day3diar_host_res_seed_1, threshold = -0.06, "te", "te")
compare.otr_results(day3diar_gs_res_seed_1, day3diar_host_res_seed_1, threshold = -0.08, "te", "te")
compare.otr_results(day3diar_gs_res_seed_1, day3diar_host_res_seed_1, threshold = -0.10, "te", "te")

##############################################################################################################

# Update to average results rather than reporting single seed
summarize_otr_results <- function(outcome, gs_prefix, host_prefix, thresholds, seeds = 1:25) {
  
  res_df <- data.frame()
  
  for(seed in seeds) {
    
    gs_res <- readRDS(here::here(paste0("results_csv/", gs_prefix, seed, ".Rds")))
    host_res <- readRDS(here::here(paste0("results_csv/", host_prefix, seed, ".Rds")))
    
    for(threshold in thresholds) {
      
      res_se <- compare.otr_results(gs_res, host_res, threshold = threshold, "se", "se")
      res_te <- compare.otr_results(gs_res, host_res, threshold = threshold, "te", "te")
      res_se_dZ0 <- compare.otr_results(gs_res, host_res, threshold = threshold, "se_dZ0", "se_dZ0")
      
      res_df <- rbind(
        res_df,
        data.frame(
          outcome = outcome,
          seed = seed,
          threshold = threshold,
          atrt = res_se$expected_val_of_comparison,
          atrt_variance = res_se$var_of_comparison,
          atnrt = res_se_dZ0$expected_val_of_comparison,
          atnrt_variance = res_se_dZ0$var_of_comparison,
          atr = res_te$expected_val_of_comparison,
          atr_variance = res_te$var_of_comparison
        )
      )
    }
  }
  
  res_df %>%
    mutate(
      seed_group = paste0(
        5 * floor((seed - 1) / 5) + 1,
        "-",
        5 * floor((seed - 1) / 5) + 5
      )
    )
}

laz_res_df <- summarize_otr_results(
  outcome = "laz",
  gs_prefix = "results_gs_seed_",
  host_prefix = "results_host_seed_",
  thresholds = c(0.06, 0.08, 0.10)
)

day3diar_res_df <- summarize_otr_results(
  outcome = "day3diar",
  gs_prefix = "results_day3diar_gs_seed_",
  host_prefix = "results_day3diar_host_seed_",
  thresholds = c(-0.06, -0.08, -0.10)
)

all_res_df <- bind_rows(laz_res_df, day3diar_res_df)

summary_res_df <- all_res_df %>%
  group_by(outcome, seed_group, threshold) %>%
  summarise(
    avg_atrt = mean(atrt, na.rm = TRUE),
    se_atrt = sqrt(mean(atrt_variance, na.rm = TRUE)),
    avg_atnrt = mean(atnrt, na.rm = TRUE),
    se_atnrt = sqrt(mean(atnrt_variance, na.rm = TRUE)),
    avg_atr = mean(atr, na.rm = TRUE),
    se_atr = sqrt(mean(atr_variance, na.rm = TRUE)),
    .groups = "drop"
  )

# Main results for seeds 1-5
main_res_df <- all_res_df %>%
  filter(seed %in% 1:5) %>%
  group_by(outcome, threshold) %>%
  summarise(
    avg_atrt = mean(atrt, na.rm = TRUE),
    se_atrt = sqrt(mean(atrt_variance, na.rm = TRUE)),
    lower_atrt = avg_atrt - 1.96 * se_atrt,
    upper_atrt = avg_atrt + 1.96 * se_atrt,
    
    avg_atnrt = mean(atnrt, na.rm = TRUE),
    se_atnrt = sqrt(mean(atnrt_variance, na.rm = TRUE)),
    lower_atnrt = avg_atnrt - 1.96 * se_atnrt,
    upper_atnrt = avg_atnrt + 1.96 * se_atnrt,
    
    avg_atr = mean(atr, na.rm = TRUE),
    se_atr = sqrt(mean(atr_variance, na.rm = TRUE)),
    lower_atr = avg_atr - 1.96 * se_atr,
    upper_atr = avg_atr + 1.96 * se_atr,
    
    .groups = "drop"
  )

# ------------------------------------------------------------------------

# Figures for seed comparion

library(tidyr)
library(ggplot2)

# Labels for plotting
plot_res_df <- plot_res_df %>%
  mutate(
    estimand = factor(
      estimand,
      levels = c("ATRT", "ATNRT", "ATR"),
      labels = c(
        "bar(ATRT)[C]-bar(ATRT)[H]",
        "bar(ATNRT)[C]-bar(ATNRT)[H]",
        "bar(ATR)[C]-bar(ATR)[H]"
      )
    )
  )

plot_summary_df <- plot_summary_df %>%
  mutate(
    estimand = factor(
      estimand,
      levels = c("ATRT", "ATNRT", "ATR"),
      labels = c(
        "bar(ATRT)[C]-bar(ATRT)[H]",
        "bar(ATNRT)[C]-bar(ATNRT)[H]",
        "bar(ATR)[C]-bar(ATR)[H]"
      )
    )
  )

# ------------------------------------------------------------------------
# LAZ

ggplot(
  filter(plot_summary_df, outcome == "laz"),
  aes(x = seed_group, y = avg_estimate)
) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    width = 0.15
  ) +
  geom_point(size = 3) +
  geom_jitter(
    data = filter(plot_res_df, outcome == "laz"),
    aes(x = seed_group, y = estimate),
    width = 0.10,
    height = 0,
    alpha = 0.35,
    size = 1.5,
    inherit.aes = FALSE
  ) +
  facet_grid(
    estimand ~ threshold,
    scales = "free_y",
    labeller = label_parsed
  ) +
  labs(
    x = "Seed group",
    y = "Average comparison of treatment rules based on comprehensive and host-only covariates",
    title = "LAZ rule comparison"
  ) +
  theme_bw()

# ------------------------------------------------------------------------
# Day-3 diarrhea

ggplot(
  filter(plot_summary_df, outcome == "day3diar"),
  aes(x = seed_group, y = avg_estimate)
) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    width = 0.15
  ) +
  geom_point(size = 3) +
  geom_jitter(
    data = filter(plot_res_df, outcome == "day3diar"),
    aes(x = seed_group, y = estimate),
    width = 0.10,
    height = 0,
    alpha = 0.35,
    size = 1.5,
    inherit.aes = FALSE
  ) +
  facet_grid(
    estimand ~ threshold,
    scales = "free_y",
    labeller = label_parsed
  ) +
  labs(
    x = "Seed group",
    y = "Average comparison of treatment rules based on comprehensive and host-only covariates",
    title = "Day-3 diarrhea rule comparison"
  ) +
  theme_bw()




##########

# Individual seed results
seed_plot_df <- plot_res_df %>%
  filter(
    outcome == "day3diar",
    threshold == -0.06
  ) %>%
  mutate(
    lower = estimate - 1.96 * sqrt(variance),
    upper = estimate + 1.96 * sqrt(variance),
    type = "Individual seed"
  )

# Five-group summaries
group_plot_df <- plot_summary_df %>%
  filter(
    outcome == "day3diar",
    threshold == -0.06
  ) %>%
  mutate(
    seed_midpoint = case_when(
      seed_group == "1-5" ~ 3,
      seed_group == "6-10" ~ 8,
      seed_group == "11-15" ~ 13,
      seed_group == "16-20" ~ 18,
      seed_group == "21-25" ~ 23
    ),
    type = "5-seed average"
  )

# Common y-axis limits across all panels
y_lim <- range(
  c(
    seed_plot_df$lower,
    seed_plot_df$upper,
    group_plot_df$lower,
    group_plot_df$upper
  ),
  na.rm = TRUE
)

pad <- 0.05 * diff(y_lim)

seed_plot <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  # Individual seed estimates
  geom_errorbar(
    data = seed_plot_df,
    aes(
      x = seed,
      ymin = lower,
      ymax = upper,
      color = type
    ),
    width = 0.15,
    alpha = 0.45
  ) +
  geom_point(
    data = seed_plot_df,
    aes(
      x = seed,
      y = estimate,
      color = type
    ),
    size = 2,
    alpha = 0.7
  ) +
  
  # Five-seed averages
  geom_errorbar(
    data = group_plot_df,
    aes(
      x = seed_midpoint,
      ymin = lower,
      ymax = upper,
      color = type
    ),
    width = 0.5,
    linewidth = 0.9
  ) +
  geom_point(
    data = group_plot_df,
    aes(
      x = seed_midpoint,
      y = avg_estimate,
      color = type
    ),
    size = 3.8
  ) +
  
  facet_wrap(
    ~ estimand,
    nrow = 1,
    scales = "fixed",
    labeller = label_parsed
  ) +
  
  coord_cartesian(
    ylim = c(y_lim[1] - pad, y_lim[2] + pad)
  ) +
  
  scale_x_continuous(
    breaks = c(1, 5, 10, 15, 20, 25),
    limits = c(0.5, 25.5)
  ) +
  
  scale_color_manual(
    values = c(
      "Individual seed" = "#00468B99",
      "5-seed average" = "#ED0000FF"
    )
  ) +
  
  labs(
    x = "Random seed",
    y = "Estimated difference (Comprehensive − Host-only)",
    color = NULL,
    title = "Day three diarrhea rule comparison",
    subtitle = "Threshold = -0.06"
  ) +
  
  theme_bw() +
  theme(
    legend.position = "bottom"
  )

ggsave(here::here("figures/avg_seed_comparison.png"), plot = seed_plot, width = 9, height = 6)

