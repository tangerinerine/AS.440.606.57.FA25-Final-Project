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
library(haven)

#Part 3

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



#Part 8

library(fixest)
library(broom)
library(dplyr)
library(purrr)
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
library(haven)
library(gt)

# ---  Load and Prepare Data ---
ajr_dta <- read_dta("/Users/qiujiechen/Desktop/AE606_Econometrics/colonial_origins/maketable6/maketable6.dta")
base_sample <- ajr_dta %>% filter(baseco == 1)


# ---  Define Control Variable Sets ---
temp_humid_controls <- "temp1 + temp2 + humid1 + humid2"
resource_controls <- "steplow + deslow + stepmid + desmid + drystep + drywint + goldm + iron + silv + zinc + oilres + landlock"
all_controls <- paste("lat_abst", temp_humid_controls, "edes1975", "avelf", resource_controls, sep = " + ")

# ---  Run All Regression Models ---

# Run IV Models (for Panels A and B)
iv_models <- list(
  "(1)" = feols(logpgp95 ~ 1 | avexpr ~ logem4, data = base_sample),
  "(2)" = feols(as.formula(paste("logpgp95 ~ lat_abst +", temp_humid_controls, "| avexpr ~ logem4")), data = base_sample),
  "(3)" = feols(logpgp95 ~ edes1975 | avexpr ~ logem4, data = base_sample),
  "(4)" = feols(logpgp95 ~ lat_abst + edes1975 | avexpr ~ logem4, data = base_sample),
  "(5)" = feols(as.formula(paste("logpgp95 ~", resource_controls, "| avexpr ~ logem4")), data = base_sample),
  "(6)" = feols(as.formula(paste("logpgp95 ~ lat_abst +", resource_controls, "| avexpr ~ logem4")), data = base_sample),
  "(7)" = feols(logpgp95 ~ avelf | avexpr ~ logem4, data = base_sample),
  "(8)" = feols(logpgp95 ~ lat_abst + avelf | avexpr ~ logem4, data = base_sample),
  "(9)" = feols(as.formula(paste("logpgp95 ~", all_controls, "| avexpr ~ logem4")), data = base_sample)
)

# Run OLS Models (for Panel C)
ols_models <- list(
  "(1)" = feols(as.formula(paste("logpgp95 ~ avexpr +", temp_humid_controls)), data = base_sample),
  "(2)" = feols(as.formula(paste("logpgp95 ~ avexpr + lat_abst +", temp_humid_controls)), data = base_sample),
  "(3)" = feols(logpgp95 ~ avexpr + edes1975, data = base_sample),
  "(4)" = feols(logpgp95 ~ avexpr + lat_abst + edes1975, data = base_sample),
  "(5)" = feols(as.formula(paste("logpgp95 ~ avexpr +", resource_controls)), data = base_sample),
  "(6)" = feols(as.formula(paste("logpgp95 ~ avexpr + lat_abst +", resource_controls)), data = base_sample),
  "(7)" = feols(logpgp95 ~ avexpr + avelf, data = base_sample),
  "(8)" = feols(logpgp95 ~ avexpr + lat_abst + avelf, data = base_sample),
  "(9)" = feols(as.formula(paste("logpgp95 ~ avexpr +", all_controls)), data = base_sample)
)


# ---  Generate Table for Panel A ---
# This section manually builds the complex layout for Panel A

# Extract coefficients and p-values
tidy_results <- map_dfr(iv_models, tidy, .id = "model") %>%
  mutate(estimate_str = sprintf("%.2f", estimate), se_str = sprintf("(%.2f)", std.error))

get_p_value <- function(model, vars) { sprintf("[%.2f]", wald(model, vars)[[4]]) }
temp_vars <- c("temp1", "temp2"); humid_vars <- c("humid1", "humid2")
soil_vars <- c("steplow", "deslow", "stepmid", "desmid", "drystep", "drywint")
resource_vars <- c("goldm", "iron", "silv", "zinc", "oilres")

p_values <- list(
  temp2 = get_p_value(iv_models[['(2)']], temp_vars), humid2 = get_p_value(iv_models[['(2)']], humid_vars),
  soil5 = get_p_value(iv_models[['(5)']], soil_vars), resource5 = get_p_value(iv_models[['(5)']], resource_vars),
  soil6 = get_p_value(iv_models[['(6)']], soil_vars), resource6 = get_p_value(iv_models[['(6)']], resource_vars),
  temp9 = get_p_value(iv_models[['(9)']], temp_vars), humid9 = get_p_value(iv_models[['(9)']], humid_vars),
  soil9 = get_p_value(iv_models[['(9)']], soil_vars), resource9 = get_p_value(iv_models[['(9)']], resource_vars)
)


## Wald test, H0: joint nullity of temp1 and temp2
##  stat = 0.161882, p-value = 0.850931, on 2 and 57 DoF, VCOV: IID.Wald test, H0: joint nullity of humid1 and humid2
##  stat = 0.025921, p-value = 0.974423, on 2 and 57 DoF, VCOV: IID.Wald test, H0: joint nullity of steplow, deslow, stepmid, desmid, drystep and drywint
##  stat = 0.490958, p-value = 0.812014, on 6 and 50 DoF, VCOV: IID.Wald test, H0: joint nullity of goldm, iron, silv, zinc and oilres
##  stat = 0.472412, p-value = 0.79502, on 5 and 50 DoF, VCOV: IID.Wald test, H0: joint nullity of steplow, deslow, stepmid, desmid, drystep and drywint
##  stat = 0.398411, p-value = 0.876478, on 6 and 49 DoF, VCOV: IID.Wald test, H0: joint nullity of goldm, iron, silv, zinc and oilres
##  stat = 0.395728, p-value = 0.84939, on 5 and 49 DoF, VCOV: IID.Wald test, H0: joint nullity of temp1 and temp2
##  stat = 0.699118, p-value = 0.502585, on 2 and 43 DoF, VCOV: IID.Wald test, H0: joint nullity of humid1 and humid2
##  stat = 0.424181, p-value = 0.657014, on 2 and 43 DoF, VCOV: IID.Wald test, H0: joint nullity of steplow, deslow, stepmid, desmid, drystep and drywint
##  stat = 1.2792, p-value = 0.28702, on 6 and 43 DoF, VCOV: IID.Wald test, H0: joint nullity of goldm, iron, silv, zinc and oilres
##  stat = 0.388541, p-value = 0.85393, on 5 and 43 DoF, VCOV: IID.


nobs <- map_chr(iv_models, ~as.character(nobs(.)))
find_val <- function(m, t, type) { tidy_results %>% filter(model == m, term == t) %>% pull({{type}}) %>% first() }

# Manually construct the table data frame
final_table_A <- tribble(
  ~term, ~`(1)`, ~`(2)`, ~`(3)`, ~`(4)`, ~`(5)`, ~`(6)`, ~`(7)`, ~`(8)`, ~`(9)`,
  "Average protection against expropriation risk, 1985-1995", find_val("(1)", "fit_avexpr", "estimate_str"), find_val("(2)", "fit_avexpr", "estimate_str"), find_val("(3)", "fit_avexpr", "estimate_str"), find_val("(4)", "fit_avexpr", "estimate_str"), find_val("(5)", "fit_avexpr", "estimate_str"), find_val("(6)", "fit_avexpr", "estimate_str"), find_val("(7)", "fit_avexpr", "estimate_str"), find_val("(8)", "fit_avexpr", "estimate_str"), find_val("(9)", "fit_avexpr", "estimate_str"),
  "", find_val("(1)", "fit_avexpr", "se_str"), find_val("(2)", "fit_avexpr", "se_str"), find_val("(3)", "fit_avexpr", "se_str"), find_val("(4)", "fit_avexpr", "se_str"), find_val("(5)", "fit_avexpr", "se_str"), find_val("(6)", "fit_avexpr", "se_str"), find_val("(7)", "fit_avexpr", "se_str"), find_val("(8)", "fit_avexpr", "se_str"), find_val("(9)", "fit_avexpr", "se_str"),
  "Latitude", NA, find_val("(2)", "lat_abst", "estimate_str"), NA, find_val("(4)", "lat_abst", "estimate_str"), NA, find_val("(6)", "lat_abst", "estimate_str"), NA, find_val("(8)", "lat_abst", "estimate_str"), find_val("(9)", "lat_abst", "estimate_str"),
  "", NA, find_val("(2)", "lat_abst", "se_str"), NA, find_val("(4)", "lat_abst", "se_str"), NA, find_val("(6)", "lat_abst", "se_str"), NA, find_val("(8)", "lat_abst", "se_str"), find_val("(9)", "lat_abst", "se_str"),
  "p-value for temperature variables", NA, p_values$temp2, NA, NA, NA, NA, NA, NA, p_values$temp9,
  "p-value for humidity variables", NA, p_values$humid2, NA, NA, NA, NA, NA, NA, p_values$humid9,
  "Percent of European descent in 1975", NA, NA, find_val("(3)", "edes1975", "estimate_str"), find_val("(4)", "edes1975", "estimate_str"), NA, NA, NA, NA, find_val("(9)", "edes1975", "estimate_str"),
  "", NA, NA, find_val("(3)", "edes1975", "se_str"), find_val("(4)", "edes1975", "se_str"), NA, NA, NA, NA, find_val("(9)", "edes1975", "se_str"),
  "p-value for soil quality", NA, NA, NA, NA, p_values$soil5, p_values$soil6, NA, NA, p_values$soil9,
  "p-value for natural resources", NA, NA, NA, NA, p_values$resource5, p_values$resource6, NA, NA, p_values$resource9,
  "Dummy for being landlocked", NA, NA, NA, NA, find_val("(5)", "landlock", "estimate_str"), find_val("(6)", "landlock", "estimate_str"), NA, NA, find_val("(9)", "landlock", "estimate_str"),
  "", NA, NA, NA, NA, find_val("(5)", "landlock", "se_str"), find_val("(6)", "landlock", "se_str"), NA, NA, find_val("(9)", "landlock", "se_str"),
  "Ethnolinguistic fragmentation", NA, NA, NA, NA, NA, NA, find_val("(7)", "avelf", "estimate_str"), find_val("(8)", "avelf", "estimate_str"), find_val("(9)", "avelf", "estimate_str"),
  "", NA, NA, NA, NA, NA, NA, find_val("(7)", "avelf", "se_str"), find_val("(8)", "avelf", "se_str"), find_val("(9)", "avelf", "se_str"),
  "Number of observations", nobs[1], nobs[2], nobs[3], nobs[4], nobs[5], nobs[6], nobs[7], nobs[8], nobs[9]
)

# Render Panel A with gt

gt(final_table_A, rowname_col = "term") %>%
  tab_header(title = "Table 6, Panel A: Two-Stage Least Squares") %>%
  sub_missing(missing_text = "") %>%
  cols_align(align = "center", columns = -term) %>%
  tab_options(table.border.top.style = "none",
              column_labels.border.bottom.style = "solid",
              column_labels.border.bottom.width = px(2),
              table_body.border.bottom.style = "none")