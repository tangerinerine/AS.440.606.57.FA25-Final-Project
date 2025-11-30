# All used libraries
library("knitr")
library("rmarkdown")
library("renv")
library("tidyverse")
library("knitr")
library("devtools")
library("viridis")
library("sjPlot")
library(rsconnect)
library(packrat)
library(kableExtra)

# ---  Load Data ---
ajr_data <- read_dta("/Users/qiujiechen/Desktop/AE606_Econometrics/colonial_origins/maketable1/maketable1.dta")

# ---  Prepare Data Samples ---
base_sample <- ajr_data %>% filter(baseco == 1)
quartile_sample <- base_sample %>%
  filter(!is.na(extmort4)) %>%
  mutate(quartile = ntile(extmort4, 4))

# ---  Define Helper Functions to Calculate Stats ---
get_means <- function(df, vars) {
  df %>% summarise(across(all_of(vars), ~mean(.x, na.rm = TRUE)))
}
get_sds <- function(df, vars) {
  df %>% summarise(across(all_of(vars), ~sd(.x, na.rm = TRUE)))
}

# ---  Calculate All Statistics ---

all_vars <- c("logpgp95", "loghjypl", "avexpr", "cons00a",
              "cons1", "democ00a", "euro1900", "logem4")

# Calculate means and sds for all groups
means_w <- get_means(ajr_data, all_vars)
sds_w <- get_sds(ajr_data, all_vars)
means_b <- get_means(base_sample, all_vars)
sds_b <- get_sds(base_sample, all_vars)
means_q <- quartile_sample %>% group_by(quartile) %>% get_means(all_vars)
sds_q <- quartile_sample %>% group_by(quartile) %>% get_sds(all_vars)

# Get observation counts
obs_w <- nrow(ajr_data)
obs_b <- nrow(base_sample)
obs_q <- quartile_sample %>% group_by(quartile) %>% count()

# ---  Assemble the Final Table ---
final_table <- tibble(
  ` ` = c("Log GDP per capita (PPP) in 1995", "(Std. Dev.)",
          "Log output per worker in 1988", "(Std. Dev.)",
          "Average protection against expropriation risk, 1985-1995", "(Std. Dev.)",
          "Constraint on executive in 1900", "(Std. Dev.)",
          "Constraint on executive in first year of independence", "(Std. Dev.)",
          "Democracy in 1900", "(Std. Dev.)",
          "European settlements in 1900", "(Std. Dev.)",
          "Log European settler mortality", "(Std. Dev.)",
          "Number of observations", ""),
  `Whole world` = c(means_w$logpgp95, sds_w$logpgp95, means_w$loghjypl, sds_w$loghjypl, means_w$avexpr, sds_w$avexpr,
                    means_w$cons00a, sds_w$cons00a, means_w$cons1, sds_w$cons1, means_w$democ00a, sds_w$democ00a,
                    means_w$euro1900, sds_w$euro1900, means_w$logem4, sds_w$logem4, obs_w, NA),
  `Base sample` = c(means_b$logpgp95, sds_b$logpgp95, means_b$loghjypl, sds_b$loghjypl, means_b$avexpr, sds_b$avexpr,
                    means_b$cons00a, sds_b$cons00a, means_b$cons1, sds_b$cons1, means_b$democ00a, sds_b$democ00a,
                    means_b$euro1900, sds_b$euro1900, means_b$logem4, sds_b$logem4, obs_b, NA),
  `(1)` = c(means_q$logpgp95[1], sds_q$logpgp95[1], means_q$loghjypl[1], sds_q$loghjypl[1], means_q$avexpr[1], sds_q$avexpr[1],
            means_q$cons00a[1], sds_q$cons00a[1], means_q$cons1[1], sds_q$cons1[1], means_q$democ00a[1], sds_q$democ00a[1],
            means_q$euro1900[1], sds_q$euro1900[1], means_q$logem4[1], sds_q$logem4[1], obs_q$n[1], NA),
  `(2)` = c(means_q$logpgp95[2], sds_q$logpgp95[2], means_q$loghjypl[2], sds_q$loghjypl[2], means_q$avexpr[2], sds_q$avexpr[2],
            means_q$cons00a[2], sds_q$cons00a[2], means_q$cons1[2], sds_q$cons1[2], means_q$democ00a[2], sds_q$democ00a[2],
            means_q$euro1900[2], sds_q$euro1900[2], means_q$logem4[2], sds_q$logem4[2], obs_q$n[2], NA),
  `(3)` = c(means_q$logpgp95[3], sds_q$logpgp95[3], means_q$loghjypl[3], sds_q$loghjypl[3], means_q$avexpr[3], sds_q$avexpr[3],
            means_q$cons00a[3], sds_q$cons00a[3], means_q$cons1[3], sds_q$cons1[3], means_q$democ00a[3], sds_q$democ00a[3],
            means_q$euro1900[3], sds_q$euro1900[3], means_q$logem4[3], sds_q$logem4[3], obs_q$n[3], NA),
  `(4)` = c(means_q$logpgp95[4], sds_q$logpgp95[4], means_q$loghjypl[4], sds_q$loghjypl[4], means_q$avexpr[4], sds_q$avexpr[4],
            means_q$cons00a[4], sds_q$cons00a[4], means_q$cons1[4], sds_q$cons1[4], means_q$democ00a[4], sds_q$democ00a[4],
            means_q$euro1900[4], sds_q$euro1900[4], means_q$logem4[4], sds_q$logem4[4], obs_q$n[4], NA)
)

# ---  Generate and Style the HTML Table ---
final_table %>%
  # Use a single, robust mutate with case_when to format all numbers
  mutate(across(where(is.numeric), ~ case_when(
    # Condition 0: If the value is NA, make it a blank string first.
    is.na(.) ~ "",
    
    # Condition 1: Handle the "Number of observations" row
    ` `[row_number()] == "Number of observations" ~ sprintf("%.0f", .),
    
    # Condition 2: Handle the standard deviation rows
    grepl("Std. Dev.", ` `[row_number()]) ~ sprintf("(%.2f)", .),
    
    # Condition 3 (Default): Handle all other numeric rows (the means)
    TRUE ~ sprintf("%.2f", .)
  ))) %>%
  
  # Replace any other NA values with a blank string (mostly for safety)
  mutate(across(everything(), ~ replace_na(., ""))) %>%
  
  # Generate the kable table 
  kable("html", caption = "<b>TABLE 1—DESCRIPTIVE STATISTICS</b>", align = "r") %>%
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE) %>%
  add_header_above(c(" " = 3, "By quartiles of mortality" = 4)) %>%
  column_spec(1, extra_css = "padding-left: 2em;") %>%
  row_spec(seq(1, 16, 2), bold = TRUE)