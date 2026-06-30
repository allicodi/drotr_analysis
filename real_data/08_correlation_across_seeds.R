library(tidyverse)
library(epiR)

here::i_am("08_correlation_across_seeds.R")

seeds <- 1:5
threshold_name <- "threshold =  0.06"
seed_names <- paste0("seed_", seeds)

axis_lims <- c(-0.5, 0.5)
axis_breaks <- c(-0.5, 0, 0.5)

# ---------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------

get_ccc_ci <- function(x, y) {
  keep <- complete.cases(x, y)
  
  if (sum(keep) < 2) {
    return(tibble(ccc = NA_real_, lower = NA_real_, upper = NA_real_))
  }
  
  out <- epiR::epi.ccc(
    x[keep],
    y[keep],
    ci = "z-transform",
    conf.level = 0.95
  )$rho.c
  
  tibble(
    ccc = as.numeric(out$est),
    lower = as.numeric(out$lower),
    upper = as.numeric(out$upper)
  )
}

read_seed_results <- function(seed) {
  readRDS(here::here(
    paste0("results_csv/results_gs_seed_", seed, ".Rds")
  ))
}

get_seed_cate_overall <- function(seed) {
  res <- read_seed_results(seed)
  
  res[[threshold_name]]$decision_df %>%
    group_by(id) %>%
    summarise(
      CATE_pred = mean(CATE_pred, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(!!paste0("seed_", seed) := CATE_pred)
}

# ---------------------------------------------------------------
# Make wide CATE data
# ---------------------------------------------------------------

cate_by_seed_overall <- map(seeds, get_seed_cate_overall) %>%
  reduce(full_join, by = "id")

# ---------------------------------------------------------------
# Build lower-triangle scatter data
# ---------------------------------------------------------------

pair_grid <- expand_grid(
  x_seed = seed_names,
  y_seed = seed_names
) %>%
  mutate(
    x_index = match(x_seed, seed_names),
    y_index = match(y_seed, seed_names),
    panel_type = case_when(
      y_index > x_index ~ "scatter",
      y_index < x_index ~ "text",
      TRUE ~ "diag"
    )
  )

scatter_dat <- pair_grid %>%
  filter(panel_type == "scatter") %>%
  pmap_dfr(function(x_seed, y_seed, x_index, y_index, panel_type) {
    cate_by_seed_overall %>%
      transmute(
        x_seed = x_seed,
        y_seed = y_seed,
        x = .data[[x_seed]],
        y = .data[[y_seed]]
      )
  })

# ---------------------------------------------------------------
# Build upper-triangle CCC text data
# ---------------------------------------------------------------

text_dat <- pair_grid %>%
  filter(panel_type == "text") %>%
  pmap_dfr(function(x_seed, y_seed, x_index, y_index, panel_type) {
    
    ccc <- get_ccc_ci(
      cate_by_seed_overall[[x_seed]],
      cate_by_seed_overall[[y_seed]]
    )
    
    tibble(
      x_seed = x_seed,
      y_seed = y_seed,
      x = 0,
      y = 0,
      label = sprintf(
        "CCC\n%.3f\n(%.3f, %.3f)",
        ccc$ccc,
        ccc$lower,
        ccc$upper
      )
    )
  })

# ---------------------------------------------------------------
# Build diagonal density data
# ---------------------------------------------------------------

density_dat <- map_dfr(seed_names, function(s) {
  
  dens <- density(
    cate_by_seed_overall[[s]],
    na.rm = TRUE,
    from = axis_lims[1],
    to = axis_lims[2]
  )
  
  tibble(
    x_seed = s,
    y_seed = s,
    x = dens$x,
    density_raw = dens$y,
    y = scales::rescale(dens$y, to = c(axis_lims[1], axis_lims[2]))
  )
})

# ---------------------------------------------------------------
# Plot
# ---------------------------------------------------------------

axis_lims <- c(-0.5, 0.5)
axis_breaks <- c(-0.5, 0, 0.5)

seed_labs <- setNames(
  paste("Seed", 1:5),
  paste0("seed_", 1:5)
)

pairs_style_plot <- ggplot() +
  geom_point(
    data = scatter_dat,
    aes(x = x, y = y),
    size = 0.5,
    alpha = 0.25,
    color = "black"
  ) +
  geom_abline(
    data = pair_grid %>% filter(panel_type == "scatter"),
    aes(intercept = 0, slope = 1),
    color = "darkgray",
    linetype = 3
  ) +
  geom_line(
    data = density_dat,
    aes(x = x, y = y),
    color = "black",
    linewidth = 0.5
  ) +
  geom_text(
    data = text_dat,
    aes(x = x, y = y, label = label),
    size = 3
  ) +
  facet_grid(
    y_seed ~ x_seed,
    drop = FALSE,
    labeller = labeller(
      x_seed = seed_labs,
      y_seed = seed_labs
    )
  ) +
  scale_x_continuous(
    limits = axis_lims,
    breaks = axis_breaks
  ) +
  scale_y_continuous(
    limits = axis_lims,
    breaks = axis_breaks
  ) +
  coord_equal() +
  theme_minimal() +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme(
    panel.border = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 10),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7),
    panel.spacing = unit(0.5, "lines")
  )

ggsave(
  filename = here::here("figures/cate_seed_agreement.png"),
  plot = pairs_style_plot,
  width = 10,
  height = 10,
  units = "in",
  dpi = 300,
  bg = "white"
)

