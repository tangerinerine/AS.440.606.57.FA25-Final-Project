# --- Load Packages ---
library(haven)
library(dplyr)
library(magrittr) 
library(fixest)

# --- Load Data ---
ajr_dta <- read_dta("/Users/xavier/Downloads/colonial_origins/maketable4/maketable4.dta") %>%
  as.data.frame()   # fixest prefer data.frame

# --- Create Base Sample ---
base_sample <- ajr_dta %>%
  filter(baseco == 1) %>%
  mutate(
    other_cont = if_else(shortnam %in% c("AUS", "MLT", "NZL"), 1, 0)
  )

# --- Create Subsamples ---
no_neo_europes_sample <- base_sample %>% filter(rich4 != 1)
no_africa_sample <- base_sample %>% filter(africa != 1)

# --- IV Regression Models ---
iv_models <- list(
  "(1)" = feols(logpgp95 ~ 1 | avexpr ~ logem4, data = base_sample),
  "(2)" = feols(logpgp95 ~ lat_abst | avexpr ~ logem4, data = base_sample),
  "(3)" = feols(logpgp95 ~ 1 | avexpr ~ logem4, data = no_neo_europes_sample),
  "(4)" = feols(logpgp95 ~ lat_abst | avexpr ~ logem4, data = no_neo_europes_sample),
  "(5)" = feols(logpgp95 ~ 1 | avexpr ~ logem4, data = no_africa_sample),
  "(6)" = feols(logpgp95 ~ lat_abst | avexpr ~ logem4, data = no_africa_sample),
  "(7)" = feols(logpgp95 ~ africa + asia + other_cont | avexpr ~ logem4, data = base_sample),
  "(8)" = feols(logpgp95 ~ lat_abst + africa + asia + other_cont | avexpr ~ logem4, data = base_sample),
  "(9)" = feols(loghjypl ~ 1 | avexpr ~ logem4, data = base_sample)
)
