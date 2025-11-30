library(haven)

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

# --- Load and Prepare Data ---
ajr_dta <- read_dta("/Users/jiasizhe/Downloads/colonial_origins/maketable3/maketable3.dta")

# Create the main sample used in most regressions for Table 3
main_sample <- ajr_dta %>%
  filter(excolony == 1, !is.na(extmort4)) %>%
  mutate(euro1900 = euro1900 / 100)

# Create the smaller subsample for regressions that also require log GDP data
lpgp_sample <- main_sample %>%
  filter(!is.na(logpgp95))

# --- Run Regressions for Panel A ---
panel_A_models <- list(
  "(1)" = feols(avexpr ~ cons00a, data = main_sample),
  "(2)" = feols(avexpr ~ cons00a + lat_abst, data = main_sample),
  "(3)" = feols(avexpr ~ democ00a, data = main_sample),
  "(4)" = feols(avexpr ~ democ00a + lat_abst, data = main_sample),
  "(5)" = feols(avexpr ~ indtime + cons1, data = main_sample),
  "(6)" = feols(avexpr ~ indtime + cons1 + lat_abst, data = main_sample),
  "(7)" = feols(avexpr ~ euro1900, data = main_sample),
  "(8)" = feols(avexpr ~ euro1900 + lat_abst, data = main_sample),
  "(9)" = feols(avexpr ~ logem4, data = lpgp_sample), # Uses smaller sample
  "(10)" = feols(avexpr ~ logem4 + lat_abst, data = lpgp_sample) # Uses smaller sample
)