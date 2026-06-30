# ---------------------------------------------------------------
# Missingness table by outcome
# ---------------------------------------------------------------

here::i_am("07_missingness_table.R")

library(here)
library(dplyr)
library(stringr)
library(gtsummary)
library(gt)
library(kableExtra)
library(tidyr)
library(purrr)
library(knitr)


source(here::here("00_load_and_prep_data.R"))

make_missingness_tbl <- function(data, group_var, variable_label) {
  group_var <- rlang::ensym(group_var)
  
  data %>%
    group_by(!!group_var) %>%
    summarise(
      N = n(),
      Day3_missing_n = sum(is.na(day3diar)),
      Day3_missing_pct = 100 * mean(is.na(day3diar)),
      LAZ_missing_n = sum(is.na(lazd90)),
      LAZ_missing_pct = 100 * mean(is.na(lazd90)),
      .groups = "drop"
    ) %>%
    mutate(
      Variable = variable_label,
      Level = as.character(!!group_var)
    ) %>%
    select(
      Variable, Level, N,
      Day3_missing_n, Day3_missing_pct,
      LAZ_missing_n, LAZ_missing_pct
    )
}

missingness_tbl <- bind_rows(
  make_missingness_tbl(abcd_data, site, "Site"),
  make_missingness_tbl(abcd_data, month_en, "Enrollment month")
)

overall_tbl <- abcd_data %>%
  summarise(
    Variable = "Overall",
    Level = "Overall",
    N = n(),
    Day3_missing_n = sum(is.na(day3diar)),
    Day3_missing_pct = 100 * mean(is.na(day3diar)),
    LAZ_missing_n = sum(is.na(lazd90)),
    LAZ_missing_pct = 100 * mean(is.na(lazd90))
  )

missingness_tbl <- bind_rows(overall_tbl, missingness_tbl)

missingness_latex <- missingness_tbl %>%
  mutate(
    `Day-3 diarrhea missing` =
      sprintf("%d (%.1f\\%%)", Day3_missing_n, Day3_missing_pct),
    `90-day LAZ missing` =
      sprintf("%d (%.1f\\%%)", LAZ_missing_n, LAZ_missing_pct)
  ) %>%
  select(
    Variable,
    Level,
    N,
    `Day-3 diarrhea missing`,
    `90-day LAZ missing`
  )

kable(
  missingness_latex,
  format = "latex",
  booktabs = TRUE,
  longtable = FALSE,
  escape = FALSE,
  caption = "Outcome missingness by site and enrollment month."
) %>%
  collapse_rows(columns = 1, latex_hline = "major") 
# missingness_gt <- missingness_tbl %>%
#   mutate(
#     `Day-3 diarrhea missing` = sprintf("%d (%.1f%%)", Day3_missing_n, Day3_missing_pct),
#     `90-day LAZ missing` = sprintf("%d (%.1f%%)", LAZ_missing_n, LAZ_missing_pct)
#   ) %>%
#   select(
#     Variable, Level, N,
#     `Day-3 diarrhea missing`,
#     `90-day LAZ missing`
#   ) %>%
#   gt(groupname_col = "Variable") %>%
#   cols_label(
#     Level = "",
#     N = "Total N"
#   ) %>%
#   tab_header(
#     title = "Outcome missingness by site and enrollment month"
#   )

# ---------------------------------------------------------------------------

# Version with all covariates
W_list <- c(
  "rotavirus_bin", "norovirus_bin", "adenovirus_bin", "sapovirus_bin",
  "astrovirus_bin", "st_etec_bin", "shigella_bin", "campylobacter_bin",
  "tepec_bin", "v_cholerae_bin", "salmonella_bin", "cryptosporidium_bin",
  "dy1_scrn_vomitall", "dy1_scrn_dehydr", "dy1_ant_sex",
  "an_ses_quintile", "rotaseason"
)

abcd_data <- abcd_data %>%
  mutate(
    avemuac_lt_12 = factor(
      if_else(avemuac < 12, 1, 0, missing = NA_real_),
      levels = c(0, 1),
      labels = c(">= 12", "< 12")
    ),
    wfaz_lt_neg1 = factor(
      if_else(wfazscore < -1, 1, 0, missing = NA_real_),
      levels = c(0, 1),
      labels = c(">= -1", "< -1")
    ),
    lfaz_lt_neg1 = factor(
      if_else(lfazscore < -1, 1, 0, missing = NA_real_),
      levels = c(0, 1),
      labels = c(">= -1", "< -1")
    ),
    wflz_lt_neg1 = factor(
      if_else(wflzscore < -1, 1, 0, missing = NA_real_),
      levels = c(0, 1),
      labels = c(">= -1", "< -1")
    ),
    age_lt_12mo = factor(
      if_else(agemchild < 12, 1, 0, missing = NA_real_),
      levels = c(0, 1),
      labels = c(">= 12 months", "< 12 months")
    )
  ) %>%
  mutate(
    across(
      all_of(grep("_bin$", W_list, value = TRUE)),
      ~ factor(.x, levels = c(0, 1), labels = c("Not detected", "Detected"))
    ),
    rotaseason = factor(rotaseason)
  )

extra_group_vars <- c(
  W_list,
  "avemuac_lt_12",
  "wfaz_lt_neg1",
  "lfaz_lt_neg1",
  "wflz_lt_neg1",
  "age_lt_12mo"
)

missingness_tbl <- bind_rows(
  make_missingness_tbl(abcd_data, site, "Site"),
  make_missingness_tbl(abcd_data, month_en, "Enrollment month"),
  lapply(extra_group_vars, function(v) {
    make_missingness_tbl(
      abcd_data,
      !!rlang::sym(v),
      v
    )
  }) %>%
    bind_rows()
)

pretty_names <- c(
  rotavirus_bin = "Rotavirus detected",
  norovirus_bin = "Norovirus detected",
  adenovirus_bin = "Adenovirus detected",
  sapovirus_bin = "Sapovirus detected",
  astrovirus_bin = "Astrovirus detected",
  st_etec_bin = "ST-ETEC detected",
  shigella_bin = "Shigella detected",
  campylobacter_bin = "Campylobacter detected",
  tepec_bin = "tEPEC detected",
  v_cholerae_bin = "V. cholerae detected",
  salmonella_bin = "Salmonella detected",
  cryptosporidium_bin = "Cryptosporidium detected",
  dy1_scrn_vomitall = "Vomiting",
  dy1_scrn_dehydr = "Dehydration",
  dy1_ant_sex = "Sex",
  an_ses_quintile = "SES quintile",
  rotaseason = "Rotavirus season",
  avemuac_lt_neg1 = "MUAC < -1",
  wfaz_lt_neg1 = "WFAZ < -1",
  lfaz_lt_neg1 = "LFAZ < -1",
  wflz_lt_neg1 = "WFLZ < -1",
  age_lt_12mo = "Age < 12 months",
  site = "Site",
  month_en = "Enrollment month"
)

latex_tbl <- missingness_tbl %>%
  mutate(
    Variable = dplyr::recode(Variable, !!!pretty_names),
    `Day-3 diarrhea missing` =
      sprintf("%d (%.1f\\%%)", Day3_missing_n, Day3_missing_pct),
    `90-day LAZ missing` =
      sprintf("%d (%.1f\\%%)", LAZ_missing_n, LAZ_missing_pct)
  ) %>%
  select(
    Variable,
    Level,
    N,
    `Day-3 diarrhea missing`,
    `90-day LAZ missing`
  )

latex_code <- latex_tbl %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    longtable = TRUE,
    escape = FALSE,
    align = c("l", "l", "r", "r", "r"),
    caption = paste(
      "Outcome missingness by site, enrollment month,",
      "and baseline characteristics."
    )
  ) %>%
  collapse_rows(
    columns = 1,
    latex_hline = "major"
  ) %>%
  kable_styling(
    latex_options = c("repeat_header")
  )

#################################################################################


# ---------------------------------------------------------------
# Variables
# ---------------------------------------------------------------

W_list <- c(
  "rotavirus_bin",
  "norovirus_bin",
  "adenovirus_bin",
  "sapovirus_bin",
  "astrovirus_bin",
  "st_etec_bin",
  "shigella_bin",
  "campylobacter_bin",
  "tepec_bin",
  "v_cholerae_bin",
  "salmonella_bin",
  "cryptosporidium_bin",
  "dy1_scrn_vomitall",
  "dy1_scrn_dehydr",
  "dy1_ant_sex",
  "an_ses_quintile",
  "rotaseason"
)

categorical_vars <- c(
  "site",
  "month_en",
  W_list
)

continuous_vars <- c(
  "agemchild",
  "avemuac",
  "wfazscore",
  "lfazscore",
  "wflzscore"
)

pretty_names <- c(
  site = "Site",
  month_en = "Enrollment month",
  rotavirus_bin = "Rotavirus detected",
  norovirus_bin = "Norovirus detected",
  adenovirus_bin = "Adenovirus detected",
  sapovirus_bin = "Sapovirus detected",
  astrovirus_bin = "Astrovirus detected",
  st_etec_bin = "ST-ETEC detected",
  shigella_bin = "Shigella detected",
  campylobacter_bin = "Campylobacter detected",
  tepec_bin = "tEPEC detected",
  v_cholerae_bin = "V. cholerae detected",
  salmonella_bin = "Salmonella detected",
  cryptosporidium_bin = "Cryptosporidium detected",
  dy1_scrn_vomitall = "Vomiting",
  dy1_scrn_dehydr = "Dehydration",
  dy1_ant_sex = "Sex",
  an_ses_quintile = "SES quintile",
  rotaseason = "Rotavirus season",
  agemchild = "Age, months",
  avemuac = "MUAC, cm",
  wfazscore = "WFAZ",
  lfazscore = "LFAZ",
  wflzscore = "WFLZ"
)

# ---------------------------------------------------------------
# Format variables
# ---------------------------------------------------------------

abcd_data <- abcd_data %>%
  mutate(
    across(
      all_of(grep("_bin$", W_list, value = TRUE)),
      ~ factor(
        .x,
        levels = c(0, 1),
        labels = c("Not detected", "Detected")
      )
    ),
    rotaseason = factor(rotaseason)
  )

# ---------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------

make_categorical_summary <- function(
    data,
    variable,
    missing_indicator,
    variable_label
) {
  
  variable <- rlang::ensym(variable)
  missing_indicator <- rlang::ensym(missing_indicator)
  
  data %>%
    mutate(
      outcome_status = if_else(
        !!missing_indicator,
        "Outcome missing",
        "Outcome not missing"
      ),
      level = as.character(!!variable),
      level = if_else(is.na(level), "Missing", level)
    ) %>%
    group_by(outcome_status) %>%
    mutate(denominator = n()) %>%
    group_by(outcome_status, level, denominator) %>%
    summarise(
      n = n(),
      pct = 100 * n / first(denominator),
      .groups = "drop"
    ) %>%
    mutate(
      value = sprintf("%d (%.1f\\%%)", n, pct)
    ) %>%
    select(outcome_status, level, value) %>%
    pivot_wider(
      names_from = outcome_status,
      values_from = value
    ) %>%
    mutate(
      Variable = variable_label,
      Level = level
    ) %>%
    select(
      Variable,
      Level,
      `Outcome missing`,
      `Outcome not missing`
    )
}

make_continuous_summary <- function(
    data,
    variable,
    missing_indicator,
    variable_label,
    digits = 1
) {
  
  variable <- rlang::ensym(variable)
  missing_indicator <- rlang::ensym(missing_indicator)
  
  summary_tbl <- data %>%
    mutate(
      outcome_status = if_else(
        !!missing_indicator,
        "Outcome missing",
        "Outcome not missing"
      )
    ) %>%
    group_by(outcome_status) %>%
    summarise(
      median = median(!!variable, na.rm = TRUE),
      q1 = quantile(!!variable, probs = 0.25, na.rm = TRUE),
      q3 = quantile(!!variable, probs = 0.75, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      value = sprintf(
        paste0("%.", digits, "f (%.", digits, "f, %.", digits, "f)"),
        median,
        q1,
        q3
      )
    ) %>%
    select(outcome_status, value) %>%
    pivot_wider(
      names_from = outcome_status,
      values_from = value
    ) %>%
    mutate(
      Variable = variable_label,
      Level = "Median (Q1, Q3)"
    ) %>%
    select(
      Variable,
      Level,
      `Outcome missing`,
      `Outcome not missing`
    )
  
  missing_tbl <- data %>%
    mutate(
      outcome_status = if_else(
        !!missing_indicator,
        "Outcome missing",
        "Outcome not missing"
      )
    ) %>%
    group_by(outcome_status) %>%
    summarise(
      denominator = n(),
      n = sum(is.na(!!variable)),
      pct = 100 * mean(is.na(!!variable)),
      .groups = "drop"
    ) %>%
    filter(n > 0) %>%
    mutate(
      value = sprintf("%d (%.1f\\%%)", n, pct)
    ) %>%
    select(outcome_status, value) %>%
    pivot_wider(
      names_from = outcome_status,
      values_from = value
    )
  
  if (nrow(missing_tbl) > 0) {
    missing_tbl <- missing_tbl %>%
      mutate(
        Variable = variable_label,
        Level = "Missing"
      ) %>%
      select(
        Variable,
        Level,
        `Outcome missing`,
        `Outcome not missing`
      )
    
    summary_tbl <- bind_rows(summary_tbl, missing_tbl)
  }
  
  summary_tbl
}

make_outcome_table <- function(
    data,
    outcome,
    outcome_label
) {
  
  outcome <- rlang::ensym(outcome)
  
  data_table <- data %>%
    mutate(
      outcome_missing = is.na(!!outcome)
    )
  
  group_n <- data_table %>%
    summarise(
      outcome_missing_n = sum(outcome_missing),
      outcome_not_missing_n = sum(!outcome_missing)
    )
  
  categorical_tbl <- map_dfr(
    categorical_vars,
    function(variable) {
      make_categorical_summary(
        data = data_table,
        variable = !!rlang::sym(variable),
        missing_indicator = outcome_missing,
        variable_label = unname(pretty_names[variable])
      )
    }
  )
  
  continuous_tbl <- map_dfr(
    continuous_vars,
    function(variable) {
      make_continuous_summary(
        data = data_table,
        variable = !!rlang::sym(variable),
        missing_indicator = outcome_missing,
        variable_label = unname(pretty_names[variable]),
        digits = 1
      )
    }
  )
  
  table_body <- bind_rows(
    categorical_tbl,
    continuous_tbl
  ) %>%
    rename(
      !!paste0(
        "Outcome missing, N = ",
        group_n$outcome_missing_n
      ) := `Outcome missing`,
      !!paste0(
        "Outcome not missing, N = ",
        group_n$outcome_not_missing_n
      ) := `Outcome not missing`
    )
  
  table_body %>%
    kable(
      format = "latex",
      booktabs = TRUE,
      longtable = TRUE,
      escape = FALSE,
      align = c("l", "l", "r", "r"),
      caption = paste0(
        "Baseline characteristics by missingness of the ",
        outcome_label,
        " outcome."
      )
    ) %>%
    collapse_rows(
      columns = 1,
      latex_hline = "major"
    ) %>%
    kable_styling(
      latex_options = "repeat_header"
    )
}

# ---------------------------------------------------------------
# Day-3 diarrhea table
# ---------------------------------------------------------------

day3_missingness_latex <- make_outcome_table(
  data = abcd_data,
  outcome = day3diar,
  outcome_label = "Day-3 diarrhea"
)

day3_missingness_latex

# ---------------------------------------------------------------
# 90-day LAZ table
# ---------------------------------------------------------------

laz_missingness_latex <- make_outcome_table(
  data = abcd_data,
  outcome = lazd90,
  outcome_label = "90-day LAZ"
)

laz_missingness_latex