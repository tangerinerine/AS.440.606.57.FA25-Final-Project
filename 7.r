# ---  Load and Prepare Data ---
ajr_dta <- read_dta("/Users/xavier/Downloads/colonial_origins/maketable5/maketable5.dta")

# Create the base sample
base_sample <- ajr_dta %>% filter(baseco == 1)

# Create the British colonies subsample
brit_colonies_sample <- base_sample %>% filter(f_brit == 1)

# ---  Run IV and OLS Regressions ---
# Run IV Models (for Panels A and B)
iv_models <- list(
  "(1)" = feols(logpgp95 ~ f_brit + f_french | avexpr ~ logem4, data = base_sample),
  "(2)" = feols(logpgp95 ~ lat_abst + f_brit + f_french | avexpr ~ logem4, data = base_sample),
  "(3)" = feols(logpgp95 ~ 1 | avexpr ~ logem4, data = brit_colonies_sample),
  "(4)" = feols(logpgp95 ~ lat_abst | avexpr ~ logem4, data = brit_colonies_sample),
  "(5)" = feols(logpgp95 ~ sjlofr | avexpr ~ logem4, data = base_sample),
  "(6)" = feols(logpgp95 ~ lat_abst + sjlofr | avexpr ~ logem4, data = base_sample),
  "(7)" = feols(logpgp95 ~ catho80 + muslim80 + no_cpm80 | avexpr ~ logem4, data = base_sample),
  "(8)" = feols(logpgp95 ~ lat_abst + catho80 + muslim80 + no_cpm80 | avexpr ~ logem4, data = base_sample),
  "(9)" = feols(logpgp95 ~ f_french + sjlofr + catho80 + muslim80 + no_cpm80 | avexpr ~ logem4, data = base_sample),
  "(10)" = feols(logpgp95 ~ lat_abst + f_french + sjlofr + catho80 + muslim80 + no_cpm80 | avexpr ~ logem4, data = base_sample)
)

# Run OLS Models (for Panel C)
ols_models <- list(
  "(1)" = feols(logpgp95 ~ avexpr + f_brit + f_french, data = base_sample),
  "(2)" = feols(logpgp95 ~ avexpr + lat_abst + f_brit + f_french, data = base_sample),
  "(3)" = feols(logpgp95 ~ avexpr, data = brit_colonies_sample),
  "(4)" = feols(logpgp95 ~ avexpr + lat_abst, data = brit_colonies_sample),
  "(5)" = feols(logpgp95 ~ avexpr + sjlofr, data = base_sample),
  "(6)" = feols(logpgp95 ~ avexpr + lat_abst + sjlofr, data = base_sample),
  "(7)" = feols(logpgp95 ~ avexpr + catho80 + muslim80 + no_cpm80, data = base_sample),
  "(8)" = feols(logpgp95 ~ avexpr + lat_abst + catho80 + muslim80 + no_cpm80, data = base_sample),
  "(9)" = feols(logpgp95 ~ avexpr + lat_abst + f_french + sjlofr + catho80 + muslim80 + no_cpm80, data = base_sample)
)

# ---  Generate Tables for Each Panel ---

# Panel A: 2SLS with Additional Controls
modelsummary(
  iv_models,
  output = "gt",
  title = "Table 5, Panel A: IV Regressions of Log GDP Per Capita with Additional Controls",
  coef_map = c("fit_avexpr" = "Average Expropriation Risk", "lat_abst" = "Latitude",
               "f_brit" = "British Colony Dummy", "f_french" = "French Colony Dummy",
               "sjlofr" = "French Legal Origin", "catho80" = "Catholic Religion Dummy",
               "muslim80" = "Muslim Religion Dummy", "no_cpm80" = "Other Religion Dummy"),
  gof_map = "nobs",
  stars = TRUE
)

# Panel B: First Stage

first_stage_models <- purrr::map(iv_models, ~.$iv_first_stage$avexpr)
# ---------------------------------

modelsummary(
  first_stage_models,
  output = "gt",
  title = "Table 5, Panel B: First-Stage Regressions",
  coef_map = c("logem4" = "Log Settler Mortality", "lat_abst" = "Latitude",
               "f_brit" = "British Colony Dummy", "f_french" = "French Colony Dummy",
               "sjlofr" = "French Legal Origin", "catho80" = "Catholic Religion Dummy",
               "muslim80" = "Muslim Religion Dummy", "no_cpm80" = "Other Religion Dummy",
               "(Intercept)" = "Constant"),
  gof_map = list(list("raw" = "nobs", "clean" = "Num. Obs.", "fmt" = 0),
                 list("raw" = "r.squared", "clean" = "R-squared", "fmt" = 3)),
  stars = TRUE
)
