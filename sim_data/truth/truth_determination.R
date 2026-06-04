here::i_am("sim_data/truth/truth_determination.R")

library(tidyverse)

source("00_simulate_data.R")

truth_df_gs <- data.frame()
truth_df_host <- data.frame()

n <- 1e6
n_seeds <- 5

t_list_gs <- seq(0.05, 0.35, 0.1)
t_list_host <- seq(0.025, 0.1, 0.025)

get_truth <- function(abcd_data, true_cate, t_list, seed) {
  
  truth_df <- data.frame()
  
  for (threshold in t_list) {
    
    d_Z <- ifelse(true_cate >= threshold, 1, 0)
    
    treated <- abcd_data[d_Z == 1, ]
    untreated <- abcd_data[d_Z == 0, ]
    
    EY_A1_dZ1 <- mean(treated$lazd90_mu1, na.rm = TRUE)
    EY_A0_dZ1 <- mean(treated$lazd90_mu0, na.rm = TRUE)
    
    EY_A1_dZ0 <- mean(untreated$lazd90_mu1, na.rm = TRUE)
    EY_A0_dZ0 <- mean(untreated$lazd90_mu0, na.rm = TRUE)
    
    E_dZ1 <- mean(d_Z)
    
    subgroup_effect_treated <- EY_A1_dZ1 - EY_A0_dZ1
    subgroup_effect_untreated <- EY_A1_dZ0 - EY_A0_dZ0
    
    treatment_effect <- subgroup_effect_treated * E_dZ1
    
    subgroup_difference <- subgroup_effect_treated - subgroup_effect_untreated
    
    truth_df <- rbind(
      truth_df,
      data.frame(
        seed = seed,
        threshold = threshold,
        EY_A1_dZ1 = EY_A1_dZ1,
        EY_A0_dZ1 = EY_A0_dZ1,
        EY_A1_dZ0 = EY_A1_dZ0,
        EY_A0_dZ0 = EY_A0_dZ0,
        E_dZ1 = E_dZ1,
        subgroup_effect_treated = subgroup_effect_treated,
        subgroup_effect_untreated = subgroup_effect_untreated,
        treatment_effect = treatment_effect,
        subgroup_difference = subgroup_difference
      )
    )
  }
  
  return(truth_df)
}

for (seed in 1:n_seeds) {
  
  set.seed(seed)
  
  abcd_data <- generate_abcd(n = n, potential_outcomes = TRUE)
  
  # Gold-standard true CATE from counterfactual means
  true_cate_gs <- abcd_data$lazd90_mu1 - abcd_data$lazd90_mu0
  
  # Host true CATE marginalizes over pathogen distribution
  true_cate_host <- 0.1940622 * mean(abcd_data$shigella_bin) +
    -0.007401625 * mean(abcd_data$shigella_bin) * abcd_data$lfazscore +
    -0.0025 * mean(abcd_data$shigella_bin) * abcd_data$agemchild * abcd_data$lfazscore
  
  truth_df_gs <- rbind(
    truth_df_gs,
    get_truth(
      abcd_data = abcd_data,
      true_cate = true_cate_gs,
      t_list = t_list_gs,
      seed = seed
    )
  )
  
  truth_df_host <- rbind(
    truth_df_host,
    get_truth(
      abcd_data = abcd_data,
      true_cate = true_cate_host,
      t_list = t_list_host,
      seed = seed
    )
  )
}

write.csv(truth_df_gs, "truth/truth_df_gs.csv", row.names = FALSE)
write.csv(truth_df_host, "truth/truth_df_host.csv", row.names = FALSE)

average_truth_gs <- truth_df_gs %>%
  group_by(threshold) %>%
  summarise(
    across(-seed, mean, na.rm = TRUE),
    .groups = "drop"
  )

average_truth_host <- truth_df_host %>%
  group_by(threshold) %>%
  summarise(
    across(-seed, mean, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(average_truth_gs, "truth/average_truth_gs.csv", row.names = FALSE)
write.csv(average_truth_host, "truth/average_truth_host.csv", row.names = FALSE)

# -------------------
# Truth for comparison at threshold 0.05

truth_df_gs_0.05 <- truth_df_gs %>%
  filter(threshold == 0.05) %>%
  arrange(seed)

truth_df_host_0.05 <- truth_df_host %>%
  filter(threshold == 0.05) %>%
  arrange(seed)

true_dif_df <- truth_df_gs_0.05 %>%
  select(
    seed,
    threshold,
    gs_subgroup_effect_treated = subgroup_effect_treated,
    gs_subgroup_effect_untreated = subgroup_effect_untreated,
    gs_treatment_effect = treatment_effect
  ) %>%
  left_join(
    truth_df_host_0.05 %>%
      select(
        seed,
        threshold,
        host_subgroup_effect_treated = subgroup_effect_treated,
        host_subgroup_effect_untreated = subgroup_effect_untreated,
        host_treatment_effect = treatment_effect
      ),
    by = c("seed", "threshold")
  ) %>%
  mutate(
    true_dif_atrt = gs_subgroup_effect_treated - host_subgroup_effect_treated,
    true_dif_atnrt = gs_subgroup_effect_untreated - host_subgroup_effect_untreated,
    true_dif_atr = gs_treatment_effect - host_treatment_effect
  ) %>%
  select(seed, threshold, true_dif_atrt, true_dif_atnrt, true_dif_atr)

average_dif <- true_dif_df %>%
  summarise(
    threshold = 0.05,
    across(starts_with("true_dif"), mean, na.rm = TRUE)
  )

write.csv(true_dif_df, "truth/truth_df_dif_0.05.csv", row.names = FALSE)
write.csv(average_dif, "truth/truth_df_avg_dif.csv", row.names = FALSE)
