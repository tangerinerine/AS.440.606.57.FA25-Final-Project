install.packages("haven")   
install.packages("modelsummary")
install.packages("fixest")

# All used libraries
library("knitr")
library("renv")
library("tidyverse")
library("knitr")
library("devtools")
library("viridis")
library("sjPlot")
library(rsconnect)
library(packrat)
 
library(haven)
library(fixest)
library(modelsummary)
library(gt)



# --- Load and Prepare Data ---
ajr_dta <- read_dta("~/Desktop/finalproject/maketable3.dta")

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

# --- Generate Table for Panel A ---
modelsummary(
  panel_A_models,
  output = "gt",
  title = "Table 3, Panel A: Determinants of Institutions",
  coef_map = c("cons00a" = "Constraint on Executive in 1900", "democ00a" = "Democracy in 1900",
               "cons1" = "Constraint on Executive at Independence", "indtime" = "Date of Independence",
               "euro1900" = "European Settlements in 1900", "logem4" = "Log Settler Mortality",
               "lat_abst" = "Distance from Equator"),
  gof_map = list(list("raw" = "nobs", "clean" = "Num. Obs.", "fmt" = 0),
                 list("raw" = "r.squared", "clean" = "R-squared", "fmt" = 3)),
  stars = TRUE,
  notes = "Notes: Dependent Variable is Average Expropriation Risk, 1985-95. Standard errors in parentheses."
)

# --- Run Regressions for Panel B ---
panel_B_models <- list(
  # Dep Var: Constraint on Executive in 1900
  "(1)" = feols(cons00a ~ euro1900, data = lpgp_sample),
  "(2)" = feols(cons00a ~ euro1900 + lat_abst, data = lpgp_sample),
  "(3)" = feols(cons00a ~ logem4, data = main_sample),
  "(4)" = feols(cons00a ~ logem4 + lat_abst, data = main_sample),
  # Dep Var: Democracy in 1900
  "(5)" = feols(democ00a ~ euro1900, data = lpgp_sample),
  "(6)" = feols(democ00a ~ euro1900 + lat_abst, data = lpgp_sample),
  "(7)" = feols(democ00a ~ logem4, data = lpgp_sample),
  "(8)" = feols(democ00a ~ logem4 + lat_abst, data = lpgp_sample),
  # Dep Var: European Settlements in 1900
  "(9)" = feols(euro1900 ~ logem4, data = lpgp_sample),
  "(10)" = feols(euro1900 ~ logem4 + lat_abst, data = lpgp_sample)
)

# --- Generate Table for Panel B ---
modelsummary(
  panel_B_models,
  output = "gt",
  title = "Table 3, Panel B: Determinants of Early Institutions",
  coef_map = c("euro1900" = "European Settlements in 1900", "logem4" = "Log Settler Mortality",
               "lat_abst" = "Distance from Equator"),
  gof_map = list(list("raw" = "nobs", "clean" = "Num. Obs.", "fmt" = 0),
                 list("raw" = "r.squared", "clean" = "R-squared", "fmt" = 3)),
  stars = TRUE,
  notes = "Notes: Standard errors in parentheses."
) %>%
  # Add spanners to clarify the dependent variable for each set of columns
  tab_spanner(label = "Dep. Var: Constraint on Executive in 1900", columns = 2:5) %>%
  tab_spanner(label = "Dep. Var: Democracy in 1900", columns = 6:9) %>%
  tab_spanner(label = "Dep. Var: European Settlements in 1900", columns = 10:11)




# part 8.1

# ---  Load and Prepare Data ---
ajr_dta <- read_dta("~/Desktop/finalproject/maketable7.dta")

# Create the base sample and the `other_cont` dummy variable
base_sample <- ajr_dta %>%
  filter(baseco == 1) %>%
  mutate(other_cont = if_else(shortnam %in% c("AUS", "MLT", "NZL"), 1, 0))

# Create subsamples for specific regressions
ols_sample_789 <- base_sample %>%
  filter(!is.na(logem4), !is.na(latabs), !is.na(lt100km), !is.na(meantemp))

ols_sample_1011 <- base_sample %>%
  filter(!is.na(yellow))

# ---  Run All Regression Models ---

# Run IV Models (for Panels A and B)
iv_models <- list(
  "(1)" = feols(logpgp95 ~ malfal94 | avexpr ~ logem4, data = base_sample),
  "(2)" = feols(logpgp95 ~ lat_abst + malfal94 | avexpr ~ logem4, data = base_sample),
  "(3)" = feols(logpgp95 ~ leb95 | avexpr ~ logem4, data = base_sample),
  "(4)" = feols(logpgp95 ~ lat_abst + leb95 | avexpr ~ logem4, data = base_sample),
  "(5)" = feols(logpgp95 ~ imr95 | avexpr ~ logem4, data = base_sample),
  "(6)" = feols(logpgp95 ~ lat_abst + imr95 | avexpr ~ logem4, data = base_sample),
  "(7)" = feols(logpgp95 ~ 1 | avexpr + malfal94 ~ logem4 + latabs + lt100km + meantemp, data = base_sample),
  "(8)" = feols(logpgp95 ~ 1 | avexpr + leb95 ~ logem4 + latabs + lt100km + meantemp, data = base_sample),
  "(9)" = feols(logpgp95 ~ 1 | avexpr + imr95 ~ logem4 + latabs + lt100km + meantemp, data = base_sample),
  "(10)" = feols(logpgp95 ~ 1 | avexpr ~ yellow, data = base_sample),
  "(11)" = feols(logpgp95 ~ africa + asia + other_cont | avexpr ~ yellow, data = base_sample)
)

# Run OLS Models (for Panel C)
ols_models <- list(
  "(1)" = feols(logpgp95 ~ avexpr + malfal94, data = base_sample),
  "(2)" = feols(logpgp95 ~ avexpr + lat_abst + malfal94, data = base_sample),
  "(3)" = feols(logpgp95 ~ avexpr + leb95, data = base_sample),
  "(4)" = feols(logpgp95 ~ avexpr + lat_abst + leb95, data = base_sample),
  "(5)" = feols(logpgp95 ~ avexpr + imr95, data = base_sample),
  "(6)" = feols(logpgp95 ~ avexpr + lat_abst + imr95, data = base_sample),
  "(7)" = feols(logpgp95 ~ avexpr + malfal94, data = ols_sample_789),
  "(8)" = feols(logpgp95 ~ avexpr + leb95, data = ols_sample_789),
  "(9)" = feols(logpgp95 ~ avexpr + imr95, data = ols_sample_789),
  "(10)" = feols(logpgp95 ~ avexpr, data = ols_sample_1011),
  "(11)" = feols(logpgp95 ~ avexpr + africa + asia + other_cont, data = ols_sample_1011)
)


# ---  Generate Tables for Each Panel ---

# Panel A: 2SLS with Health Variables
modelsummary(
  iv_models,
  output = "gt",
  title = "Table 7, Panel A: 2SLS Regressions with Geography and Health Variables",
  coef_map = c("fit_avexpr" = "Average Expropriation Risk", "fit_malfal94" = "Malaria Index",
               "fit_leb95" = "Life Expectancy", "fit_imr95" = "Infant Mortality",
               "lat_abst" = "Latitude", "malfal94" = "Malaria Index",
               "leb95" = "Life Expectancy", "imr95" = "Infant Mortality",
               "africa" = "Africa Dummy", "asia" = "Asia Dummy", "other_cont" = "Other Continent Dummy"),
  gof_map = "nobs",
  stars = TRUE
)

# Panel B: First Stage
first_stage_models_avexpr <- purrr::map(iv_models, ~.$iv_first_stage$avexpr)
# ---------------------------------

modelsummary(
  first_stage_models_avexpr,
  output = "gt",
  title = "Table 7, Panel B: First-Stage Regressions for Expropriation Risk",
  coef_map = c("logem4" = "Log Settler Mortality", "yellow" = "Yellow Fever Dummy",
               "latabs" = "Latitude (abs)", "lt100km" = "Coastal Dummy",
               "meantemp" = "Mean Temperature", "lat_abst" = "Latitude",
               "malfal94" = "Malaria Index", "leb95" = "Life Expectancy", "imr95" = "Infant Mortality",
               "africa" = "Africa Dummy", "asia" = "Asia Dummy", "other_cont" = "Other Continent Dummy",
               "(Intercept)" = "Constant"),
  gof_map = list(list("raw" = "nobs", "clean" = "Num. Obs.", "fmt" = 0),
                 list("raw" = "r.squared", "clean" = "R-squared", "fmt" = 3)),
  stars = TRUE
)


# Panel C: OLS Regressions
modelsummary(
  ols_models,
  output = "gt",
  title = "Table 7, Panel C: OLS Regressions with Geography and Health Variables",
  coef_map = c("avexpr" = "Average Expropriation Risk", "lat_abst" = "Latitude",
               "malfal94" = "Malaria Index", "leb95" = "Life Expectancy",
               "imr95" = "Infant Mortality", "africa" = "Africa Dummy",
               "asia" = "Asia Dummy", "other_cont" = "Other Continent Dummy"),
  gof_map = list(list("raw" = "nobs", "clean" = "Num. Obs.", "fmt" = 0),
                 list("raw" = "r.squared", "clean" = "R-squared", "fmt" = 3)),
  stars = TRUE
)



