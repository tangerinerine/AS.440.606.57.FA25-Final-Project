# ---  Load and Prepare Data ---
ajr_dta <- read_dta("/Users/xavier/Downloads/colonial_origins/maketable8/maketable8.dta")

# Create the base sample for the analysis
base_sample <- ajr_dta %>% filter(baseco == 1)

# ---  Run All Regression Models ---

# Panel A & B Models: IV with alternative instruments
panel_AB_models <- list(
  "(1)" = feols(logpgp95 ~ 1 | avexpr ~ euro1900, data = base_sample),
  "(2)" = feols(logpgp95 ~ lat_abst | avexpr ~ euro1900, data = base_sample),
  "(3)" = feols(logpgp95 ~ 1 | avexpr ~ cons00a, data = base_sample),
  "(4)" = feols(logpgp95 ~ lat_abst | avexpr ~ cons00a, data = base_sample),
  "(5)" = feols(logpgp95 ~ 1 | avexpr ~ democ00a, data = base_sample),
  "(6)" = feols(logpgp95 ~ lat_abst | avexpr ~ democ00a, data = base_sample),
  "(7)" = feols(logpgp95 ~ indtime | avexpr ~ cons1, data = base_sample),
  "(8)" = feols(logpgp95 ~ lat_abst + indtime | avexpr ~ cons1, data = base_sample),
  "(9)" = feols(logpgp95 ~ indtime | avexpr ~ democ1, data = base_sample),
  "(10)" = feols(logpgp95 ~ lat_abst + indtime | avexpr ~ democ1, data = base_sample)
)
# Panel C Models: Overidentification tests (multiple instruments)
panel_C_models <- list(
  "(1)" = feols(logpgp95 ~ 1 | avexpr ~ euro1900 + logem4, data = base_sample),
  "(2)" = feols(logpgp95 ~ lat_abst | avexpr ~ euro1900 + logem4, data = base_sample),
  "(3)" = feols(logpgp95 ~ 1 | avexpr ~ cons00a + logem4, data = base_sample),
  "(4)" = feols(logpgp95 ~ lat_abst | avexpr ~ cons00a + logem4, data = base_sample),
  "(5)" = feols(logpgp95 ~ 1 | avexpr ~ democ00a + logem4, data = base_sample),
  "(6)" = feols(logpgp95 ~ lat_abst | avexpr ~ democ00a + logem4, data = base_sample),
  "(7)" = feols(logpgp95 ~ indtime | avexpr ~ cons1 + logem4, data = base_sample),
  "(8)" = feols(logpgp95 ~ lat_abst + indtime | avexpr ~ cons1 + logem4, data = base_sample),
  "(9)" = feols(logpgp95 ~ indtime | avexpr ~ democ1 + logem4, data = base_sample),
  "(10)" = feols(logpgp95 ~ lat_abst + indtime | avexpr ~ democ1 + logem4, data = base_sample)
)
# Panel D Models: IV with mortality as an exogenous control
panel_D_models <- list(
  "(1)" = feols(logpgp95 ~ logem4 | avexpr ~ euro1900, data = base_sample),
  "(2)" = feols(logpgp95 ~ logem4 + lat_abst | avexpr ~ euro1900, data = base_sample),
  "(3)" = feols(logpgp95 ~ logem4 | avexpr ~ cons00a, data = base_sample),
  "(4)" = feols(logpgp95 ~ logem4 + lat_abst | avexpr ~ cons00a, data = base_sample),
  "(5)" = feols(logpgp95 ~ logem4 | avexpr ~ democ00a, data = base_sample),
  "(6)" = feols(logpgp95 ~ logem4 + lat_abst | avexpr ~ democ00a, data = base_sample),
  "(7)" = feols(logpgp95 ~ logem4 + indtime | avexpr ~ cons1, data = base_sample),
  "(8)" = feols(logpgp95 ~ logem4 + lat_abst + indtime | avexpr ~ cons1, data = base_sample),
  "(9)" = feols(logpgp95 ~ logem4 + indtime | avexpr ~ democ1, data = base_sample),
  "(10)" = feols(logpgp95 ~ logem4 + lat_abst + indtime | avexpr ~ democ1, data = base_sample)
)
# ---  Generate Tables for Each Panel ---

# Panel A: 2SLS with Alternative Instruments
modelsummary(
  panel_AB_models, output = "gt", title = "Table 8, Panel A: 2SLS with Alternative Instruments",
  coef_map = c("fit_avexpr" = "Average Expropriation Risk", "lat_abst" = "Latitude", "indtime" = "Date of Independence"),
  gof_map = "nobs", stars = TRUE
)

# Panel B: First Stage for Alternative Instruments

first_stage_AB <- purrr::map(panel_AB_models, ~.$iv_first_stage$avexpr)
# ---------------------------------
modelsummary(
  first_stage_AB, output = "gt", title = "Table 8, Panel B: First Stage for Alternative Instruments",
  coef_map = c("euro1900" = "European Settlements in 1900", "cons00a" = "Constraint on Executive in 1900",
               "democ00a" = "Democracy in 1900", "cons1" = "Constraint on Executive at Independence",
               "democ1" = "Democracy at Independence", "lat_abst" = "Latitude", "indtime" = "Date of Independence",
               "(Intercept)" = "Constant"),
  gof_map = list(list("raw" = "nobs", "clean" = "Num. Obs.", "fmt" = 0),
                 list("raw" = "r.squared", "clean" = "R-squared", "fmt" = 3)),
  stars = TRUE
)

# Panel C: Overidentification Tests
gof_map_sargan <- list(
  list("raw" = "nobs", "clean" = "Num. Obs.", "fmt" = 0),
  list("raw" = "iv_sargan_p", "clean" = "Sargan Test p-value", "fmt" = 3)
)
modelsummary(
  panel_C_models, output = "gt", title = "Table 8, Panel C: Overidentification Tests",
  coef_map = c("fit_avexpr" = "Average Expropriation Risk", "lat_abst" = "Latitude", "indtime" = "Date of Independence"),
  gof_map = gof_map_sargan, stars = TRUE
)

# Panel D: 2SLS with Mortality as Exogenous Control
modelsummary(
  panel_D_models, output = "gt", title = "Table 8, Panel D: 2SLS with Log Mortality as Exogenous Variable",
  coef_map = c("fit_avexpr" = "Average Expropriation Risk", "logem4" = "Log Settler Mortality",
               "lat_abst" = "Latitude", "indtime" = "Date of Independence"),
  gof_map = "nobs", stars = TRUE
)