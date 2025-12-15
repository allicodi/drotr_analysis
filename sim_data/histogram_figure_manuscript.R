# ---------------------------------------------------------------------------
# Make figure with CATE distributions under each rule
# ---------------------------------------------------------------------------

here::i_am("sim_data/histogram_figure_manuscript.R")

source("sim_data/00_simulate_data.R")

library(ggplot2)
library(patchwork)

# ---------------------------------------------------------------------------
# Simulate large dataset and true CATEs for each rule

abcd_data <- generate_abcd(n = 1e6)

# True CATE GS = E[Qbar(1, W) - Qbar(0, W) | gold-standard Z]
# pathogen quantity * (beta_pathogen_x_treatment + 
#                      beta_pathogen_x_treatment_x_lfazscore * lfazscore +
#                      beta_pathogen_x_treatment_x_lfazscore_x_age * lfazscore * age)

# True CATE Host = E[Qbar(1, W) - Qbar(0, W) | host Z]
# E[pathogen quantity] * (beta_pathogen_x_treatment + 
#                      beta_pathogen_x_treatment_x_lfazscore * lfazscore +
#                      beta_pathogen_x_treatment_x_lfazscore_x_age * lfazscore * age)

abcd_data$true_cate_gs <- 0.1940622*abcd_data$shigella_bin +
  -0.007401625*abcd_data$shigella_bin*abcd_data$lfazscore +
  -0.0025*abcd_data$shigella_bin*abcd_data$agemchild*abcd_data$lfazscore

abcd_data$true_cate_host <- 0.1940622*mean(abcd_data$shigella_bin) +
  -0.007401625*mean(abcd_data$shigella_bin)*abcd_data$lfazscore +
  -0.0025*mean(abcd_data$shigella_bin)*abcd_data$agemchild*abcd_data$lfazscore

# ---------------------------------------------------------------------------
# Histogram for gold standard

t_list_gs <- seq(0.05, 0.35, 0.1)
# pt_gs <- sapply(t_list_gs, function(t){
#   (length(which(true_cate_gs > t)) / 1e6)*100
# })

# hard code so matches table in manuscript with results from 5 seeds
pt_gs <- c(19.01, 18.63, 8.45, 0.82)

# Gold standard histogram
# ggplot(data = abcd_data, aes(x = true_cate_gs)) +
#   geom_histogram(binwidth = 0.01, fill = "lightblue", color = "black") +
#   geom_vline(xintercept = t_list_gs, color = "red", linetype = "dashed", size = 0.8) +
#   annotate("text", x = t_list_gs+0.03, y = 75000, label = sprintf("t = %.2f", t_list_gs),
#             color = "red", vjust = -0.5, size = 4) +
#   annotate("text", x = t_list_gs+0.03, y = 72000, label = paste0(sprintf("%.1f", pt_gs), "%"),
#            color = "red", vjust = -0.5, size = 4) +
#   labs(title = "Relative Frequency Histogram of CATE under comprehensive rule",
#        x = "CATE",
#        y = "Frequency") +
#   coord_cartesian(ylim = c(0, 1e5)) +  
#   theme_minimal(base_size = 14) +
#   theme(plot.title = element_text(hjust = 0.5))


comp_hist <- ggplot(data = abcd_data, aes(x = true_cate_gs)) +
  geom_histogram(aes(y = (..count..) / sum(..count..) * 100),
                 binwidth = 0.01, fill = "coral", color = "black") +
  geom_vline(xintercept = t_list_gs, color = "red", linetype = "dashed", size = 0.8) +
  annotate("text", x = t_list_gs+0.03, y = 11.5, label = sprintf("t = %.2f", t_list_gs),
           color = "red", vjust = -0.5, size = 4) +
  annotate("text", x = t_list_gs+0.03, y = 11, label = paste0(sprintf("%.2f", pt_gs), "%"),
           color = "red", vjust = -0.5, size = 4) +
  labs(title = "Relative frequency histogram of CATE under comprehensive rule",
       x = "CATE",
       y = "Percentage (%)") +
  coord_cartesian(ylim = c(0, 11.5), xlim = c(0, 0.5)) +  
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))

# ---------------------------------------------------------------------------
# Histogram for host

t_list_host <- seq(0.025, 0.1, 0.025)
# pt_host <- sapply(t_list_host, function(t){
#   (length(which(true_cate_host > t)) / 1e6)*100
# })

pt_host <- c(99.11, 34.75, 1.23, 0.01)

host_hist <- ggplot(data = abcd_data, aes(x = true_cate_host)) +
  geom_histogram(aes(y = (..count..) / sum(..count..) * 100),
                 binwidth = 0.0025, fill = "lightblue", color = "black",) +
  geom_vline(xintercept = t_list_host, color = "red", linetype = "dashed", size = 0.8) +
  annotate("text", x = t_list_host+0.01, y = 11.5, label = sprintf("t = %.3f", t_list_host),
           color = "red", vjust = -0.5, size = 4) +
  annotate("text", x = t_list_host+0.008, y = 11, label = paste0(sprintf("%.2f", pt_host), "%"),
           color = "red", vjust = -0.5, size = 4) +
  labs(title = "Relative frequency histogram of CATE under host rule",
       x = "CATE",
       y = "Percentage (%)") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))

comp_hist + host_hist

# Save the side-by-side plots to a file
ggsave(here::here("sim_data/combined_histograms.png"), comp_hist + host_hist, width = 14, height = 7, dpi = 300)

# ---------------------------------------------------------------------------
# Histogram for shigella (supplement)

abcd_data$true_cate_shig <- 0.1940622*abcd_data$shigella_bin +
  -0.007401625*abcd_data$shigella_bin*mean(abcd_data$lfazscore) +
  -0.0025*abcd_data$shigella_bin*mean(abcd_data$agemchild)*mean(abcd_data$lfazscore)

t_shig <- 0.1
pt_shig <- length(which(abcd_data$true_cate_shig > t_shig)) / 1e6 *100

shig_hist <- ggplot(data = abcd_data, aes(x = true_cate_shig)) +
  geom_histogram(aes(y = (..count..) / sum(..count..) * 100),
                 binwidth = 0.02, fill = "lightgreen", color = "black",) +
  geom_vline(xintercept = t_shig, color = "red", linetype = "dashed", size = 0.8) +
  annotate("text", x = t_shig+0.01, y = 80, label = sprintf("t = %.1f", t_shig),
           color = "red", vjust = -0.5, size = 4) +
  annotate("text", x = t_shig+0.01, y = 78, label = paste0(sprintf("%.1f", pt_shig), "%"),
           color = "red", vjust = -0.5, size = 4) +
  labs(title = "Relative frequency histogram of CATE under binary Shigella rule",
       x = "CATE",
       y = "Percentage (%)") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))

# Save the side-by-side plots to a file
ggsave(here::here("sim_data/shigella_histogram.png"), shig_hist, width = 14, height = 7, dpi = 300)

