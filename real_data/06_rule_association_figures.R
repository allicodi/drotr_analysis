# -------------------------------------------------------------------------------------------------------
# Create figures similar to Sara's supplementary ones for association between CATE and covariate
# -------------------------------------------------------------------------------------------------------

here::i_am("06_rule_association_figures.R")

library(drotr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(labelled)

# Read in original data & reformat factors
source(here::here("00_load_and_prep_data.R"))

# Read in results objects
results_laz_comp <- readRDS(here::here("results_csv/results_gs_seed_1.Rds"))
results_laz_host <- readRDS(here::here("results_csv/results_host_seed_1.Rds"))
results_day3diar_comp <- readRDS(here::here("results_csv/results_day3diar_gs_seed1.Rds"))
results_day3diar_host <- readRDS(here::here("results_csv/results_day3diar_host_seed1.Rds"))

# Get decision dataframes
decision_df_laz_comp <- results_laz_comp$`threshold =  0.06`$decision_df %>%
  select(id, CATE_pred, decision) %>%
  rename('CATE_laz_comp' = CATE_pred,
         'decision_laz_comp_0.06' = decision,
         'pid' = id)

decision_df_laz_host <- results_laz_host$`threshold =  0.06`$decision_df %>%
  select(id, CATE_pred, decision) %>%
  rename('CATE_laz_host' = CATE_pred,
         'decision_laz_host_0.06' = decision,
         'pid' = id)

decision_df_day3diar_comp <- results_day3diar_comp$`threshold =  -0.06`$decision_df%>%
  select(id, CATE_pred, decision) %>%
  rename('CATE_day3diar_comp' = CATE_pred,
         'decision_day3diar_comp_0.06' = decision,
         'pid' = id)

decision_df_day3diar_host <- results_day3diar_host$`threshold =  -0.06`$decision_df %>%
  select(id, CATE_pred, decision) %>%
  rename('CATE_day3diar_host' = CATE_pred,
         'decision_day3diar_host_0.06' = decision,
         'pid' = id)

# Merge original data with CATE predictions to make single dataframe with col for cate preds under different rules
all_decision_df <- left_join(abcd_data, decision_df_laz_comp, by = "pid") %>%
  left_join(decision_df_laz_host, by = "pid") %>%
  left_join(decision_df_day3diar_comp, by = "pid") %>%
  left_join(decision_df_day3diar_host, by = "pid")

# Make plots like this below for the following covariates x
# rotavirus_new, norovirus_new,  adenovirus_new, sapovirus_new, astrovirus_new, 
# st_etec_new, shigella_new,  campylobacter_new,  tepec_new,  v_cholerae_new, 
# salmonella_new, cryptosporidium_new, dy1_scrn_vomitall, dy1_scrn_lstools, 
# dy1_scrn_sstools, dy1_scrn_diardays, dy1_scrn_dehydr, avemuac, wfazscore, 
# lfazscore, wflzscore, dy1_ant_sex, agemchild, an_ses_quintile, an_tothhlt5 

comp_covariates <- c(
  "rotavirus_new", "norovirus_new", "adenovirus_new", "sapovirus_new", "astrovirus_new", 
  "st_etec_new", "shigella_new", "campylobacter_new", "tepec_new", "v_cholerae_new", 
  "salmonella_new", "cryptosporidium_new", "dy1_scrn_vomitall", "dy1_scrn_lstools", 
  "dy1_scrn_sstools", "dy1_scrn_diardays", "dy1_scrn_dehydr", "avemuac", "wfazscore", 
  "lfazscore", "wflzscore", "dy1_ant_sex", "agemchild", "an_ses_quintile", "an_tothhlt5"
)

host_covariates <- c(
  "avemuac", "wfazscore", "wflzscore", 
  "lfazscore", "dy1_ant_sex", "agemchild", 
  "an_ses_quintile", "an_tothhlt5"
)

# Add variables labels for covariates to display on plot
all_decision_df <- all_decision_df %>%
  set_variable_labels(rotavirus_new = "Rotavirus TAC quantity",
                      norovirus_new = "Norovirus TAC quantity",
                      adenovirus_new = "Adenovirus TAC quantity",
                      sapovirus_new = "Sapovirus TAC quantity",
                      astrovirus_new = "Astrovirus TAC quantity",
                      st_etec_new = "ST ETEC TAC quantity",
                      shigella_new = "Shigella TAC quantity",
                      campylobacter_new = "Campylobacter TAC quantity",
                      tepec_new = "tEPEC TAC quantity",
                      v_cholerae_new = "V Cholera TAC quantity",
                      salmonella_new = "Salmonella TAC quantity",
                      cryptosporidium_new = "Cryptosporidium TAC quantity",
                      dy1_scrn_vomitall = "Child vomits everything",
                      dy1_scrn_lstools = "Number of loose stools",
                      dy1_scrn_sstools = "Number of solid stools",
                      dy1_scrn_diardays = "Number of days of diarrhea",
                      dy1_scrn_dehydr = "Dehydration level",
                      avemuac = "Baseline mid-upper arm circumference",
                      wfazscore = "Baseline weight-for-age z-score",
                      lfazscore = "Baseline length-for-age z-score",
                      wflzscore = "Baseline weight-for-length z-score",
                      dy1_ant_sex = "Sex",
                      agemchild = "Age (Months)",
                      an_ses_quintile = "Socioeconomic quintile",
                      an_tothhlt5 = "Number of children in household <5years of age")

make_plot <- function(df, x_var, y_var, decision_var, legend = FALSE){
  
  df$decision <- factor(df[[decision_var]], levels=0:1, labels = c("Not recommended", "Recommended"))

  p <- ggplot(df, aes_string(x = x_var, y = y_var)) +
    geom_point(size=2, alpha=0.3, aes(color = decision)) +
    geom_smooth(method=lm) +
    labs(x=var_label(df[[x_var]]), y = "CATE", color = "Treatment recommendation") +
    scale_color_brewer(palette="Pastel1") + theme_minimal() 
  
  if(legend){
    p <- p + theme(legend.position = "bottom")
  } else{
    p <- p + theme(legend.position = "none")
  }
  
  # get correlation between x and CATE
  if(is.numeric(df[[x_var]])){
    cor_x_cate <- cor(df[[x_var]], df[[y_var]])
    cor_label <- paste0("r = ", round(cor_x_cate, 2))
    
    p <- p + annotate("text", x = Inf, y = Inf, label = cor_label, hjust = 1.1, vjust = 1.5, size = 4, color = "black")
  }
  
  return(p)
}

plot_laz_comp <- lapply(comp_covariates, function(x) make_plot(all_decision_df, x, "CATE_laz_comp", "decision_laz_comp_0.06"))
plot_day3diar_comp <- lapply(comp_covariates, function(x) make_plot(all_decision_df, x, "CATE_day3diar_comp", "decision_day3diar_comp_0.06"))
plot_laz_host <- lapply(host_covariates, function(x) make_plot(all_decision_df, x, "CATE_laz_host", "decision_laz_host_0.06"))
plot_day3diar_host <- lapply(host_covariates, function(x) make_plot(all_decision_df, x, "CATE_day3diar_host", "decision_day3diar_host_0.06"))

combo_plot_laz_comp <- wrap_plots(plot_laz_comp, ncol = 5) +
  plot_annotation(title = "Comprehensive Rule, LAZ Outcome - Threshold = 0.06")

combo_plot_day3diar_comp <- wrap_plots(plot_day3diar_comp, ncol = 5) +
  plot_annotation(title = "Comprehensive Rule, Day 3 Diarrhea Outcome - Threshold = 0.06")

combo_plot_laz_host <- wrap_plots(plot_laz_host, ncol = 3) +
  plot_annotation(title = "Host Rule, LAZ Outcome - Threshold = 0.06")

combo_plot_day3diar_host <- wrap_plots(plot_day3diar_host, ncol = 3) +
  plot_annotation(title = "Host Rule, Day 3 Diarrhea Outcome - Threshold = 0.06")

ggsave(here::here("figures/laz_comp_all.png"), combo_plot_laz_comp, width = 12, height = 12, dpi = 300)
ggsave(here::here("figures/laz_host_all.png"), combo_plot_laz_host, width = 12, height = 12, dpi = 300)
ggsave(here::here("figures/day3diar_comp_all.png"), combo_plot_day3diar_comp, width = 12, height = 12, dpi = 300)
ggsave(here::here("figures/day3diar_host_all.png"), combo_plot_day3diar_host, width = 12, height = 12, dpi = 300)

# select top 3 strongest correlation from each rule
# combo_plot_laz_comp = shigella (0.17), solid stool (0.17), number days diarrhea (-0.15)
top_3_laz_comp <- c("shigella_new", "dy1_scrn_sstools", "dy1_scrn_diardays")

# combo_plot_laz_host = baseline muac (-0.13), baseline wflz (-0.13), age (-0.09)
top_3_laz_host <- c("avemuac", "wflzscore", "agemchild")

# combo_plot_day3diar_comp = rota (.34), shigella (-0.23), salmonella (-0.17)
top_3_day3diar_comp <- c("rotavirus_new", "shigella_new", "salmonella_new")

# combo_plot_day3diar_host = muac (0.3), lfaz (0.23), wfaz (0.17)
top_3_day3diar_host <- c("avemuac", "lfazscore", "wfazscore")

top_3_laz_comp_plot <- lapply(top_3_laz_comp, function(x) make_plot(all_decision_df, x, "CATE_laz_comp", "decision_laz_comp_0.06"))
top_3_day3diar_comp_plot <- lapply(top_3_day3diar_comp, function(x) make_plot(all_decision_df, x, "CATE_day3diar_comp", "decision_day3diar_comp_0.06"))
top_3_laz_host_plot <- lapply(top_3_laz_host, function(x) make_plot(all_decision_df, x, "CATE_laz_host", "decision_laz_host_0.06"))
top_3_day3diar_host_plot <- lapply(top_3_day3diar_host , function(x) make_plot(all_decision_df, x, "CATE_day3diar_host", "decision_day3diar_host_0.06"))

# Add titles to each row using plot_annotation and wrap_plots
# row1 <- wrap_plots(top_3_laz_host_plot, ncol = 3) +
#   plot_annotation(title = "LAZ, Host-Only Rule")
# 
# row2 <- wrap_plots(top_3_laz_comp_plot, ncol = 3) +
#   plot_annotation(title = "LAZ, Comprehensive Rule")
# 
# row3 <- wrap_plots(top_3_day3diar_host_plot, ncol = 3) +
#   plot_annotation(title = "Day three diarrhea, Host-Only Rule")
# 
# row4 <- wrap_plots(top_3_day3diar_comp_plot, ncol = 3) +
#   plot_annotation(title = "Day three diarrha, Comprehensive Rule")

# original combo figure

# plot_list <- c(top_3_laz_host_plot, top_3_laz_comp_plot, top_3_day3diar_host_plot, top_3_day3diar_comp_plot)
# plot_list[[11]] <- make_plot(all_decision_df, "shigella_new", "CATE_day3diar_comp", "decision_day3diar_comp_0.06", legend = TRUE)
# all_rows <- wrap_plots(plot_list, ncol = 3) 

# separate for LAZ and day three diarrhea
plot_list_laz <- c(top_3_laz_host_plot, top_3_laz_comp_plot)
plot_list_day_3 <- c(top_3_day3diar_host_plot, top_3_day3diar_comp_plot)

plot_list_laz[[5]] <- make_plot(all_decision_df, "dy1_scrn_sstools", "CATE_laz_comp", "decision_laz_comp_0.06", legend = TRUE)
plot_list_day_3[[5]] <- make_plot(all_decision_df, "shigella_new", "CATE_day3diar_comp", "decision_day3diar_comp_0.06", legend = TRUE)

all_rows_laz <- wrap_plots(plot_list_laz, ncol = 3) 
all_rows_day_3 <- wrap_plots(plot_list_day_3, ncol = 3) 



# Combine all rows into one figure
combo_rule_plot_laz <- (all_rows_laz  ) + 
  plot_annotation(
    tag_levels = list(c(
      "", "A: Host-only, t = 0.06 rule for LAZ at day ninety", "", 
      "", "B: Comprehensive, t = 0.06 rule for LAZ at day ninety", ""
    ))) & 
  theme(
    plot.tag = element_text(size = 14, face = "bold"), 
    plot.tag.position = "top"
  )

combo_rule_plot_day_3 <- (all_rows_day_3  ) + 
  plot_annotation(
    tag_levels = list(c(
      "", "A: Host-only, t = 0.06 rule for day three diarrhea", "", 
      "", "B: Comprehensive, t = 0.06 rule for day three diarrhea", ""
    ))) & 
  theme(
    plot.tag = element_text(size = 14, face = "bold"), 
    plot.tag.position = "top"
  )

#ggsave(here::here("figures/top_three_all_rules.png"), combo_rule_plot, width = 9, height = 9, dpi = 300)
ggsave(here::here("figures/top_three_laz.png"), combo_rule_plot_laz, width = 12, height = 9, dpi = 300)
ggsave(here::here("figures/top_three_day_3.png"), combo_rule_plot_day_3, width = 12, height = 9, dpi = 300)


# 
# library(ggpubr)
test <- ggarrange(all_rows, nrows = 4, common.legend = TRUE, legend = "bottom", widths = c(2,2,2))
ggsave(here::here("figures/top_three_all_rules_with_legend.jpeg"), test, width = 12, height = 12, dpi = 300)
