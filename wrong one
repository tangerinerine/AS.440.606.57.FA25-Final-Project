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
install.packages("fixest")
library(fixest)

# ---  Load Data ---
ajr_dta <- read_dta("/Users/jiasizhe/Downloads/colonial_origins/maketable2/maketable2.dta")

# ---  Create Data Subsets ---
base_sample <- ajr_dta %>% filter(baseco == 1)

# ---  Run All Regressions ---
# Note on results: The public data has minor differences from the paper's,
# so coefficients may not match perfectly.
model_list <- list(
  "(1)" = feols(logpgp95 ~ avexpr, data = ajr_dta, se = "hetero"),
  "(2)" = feols(logpgp95 ~ avexpr, data = base_sample, se = "hetero"),
  "(3)" = feols(logpgp95 ~ avexpr + lat_abst, data = ajr_dta, se = "hetero"),
  "(4)" = feols(logpgp95 ~ avexpr + lat_abst + africa + asia + other, data = ajr_dta, se = "hetero"),
  "(5)" = feols(logpgp95 ~ avexpr + lat_abst, data = base_sample, se = "hetero"),
  "(6)" = feols(logpgp95 ~ avexpr + lat_abst + africa + asia + other, data = base_sample, se = "hetero"),
  "(7)" = feols(loghjypl ~ avexpr, data = ajr_dta, se = "hetero"),
  "(8)" = feols(loghjypl ~ avexpr, data = base_sample, se = "hetero")
)

install.packages("gt")
library(gt)
install.packages("modelsummary")
library(modelsummary)

# ---  Define Table Components ---
gof_map <- list(
  list("raw" = "nobs", "clean" = "Num. Obs.", "fmt" = 0),
  list("raw" = "r.squared", "clean" = "R-squared", "fmt" = 3)
)


coef_map <- c("avexpr"    = "Average Expropriation Risk",
              "lat_abst"  = "Distance from Equator",
              "africa"    = "Africa",
              "asia"      = "Asia",
              "other"     = "Other continents")


modelsummary(
  model_list,
  output = "gt",
  title = "Table 2: OLS Regressions",
  coef_map = coef_map,
  gof_map = gof_map,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  add_rows = tribble(
    ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)", ~"(7)", ~"(8)",
    "Base Sample", "No", "Yes", "No", "No", "Yes", "Yes", "No", "Yes",
    "Continent Dummies", "No", "No", "No", "Yes", "No", "Yes", "No", "No"
  ),
  notes = "Notes: Robust standard errors are in parentheses."
) %>%
  # Use gt's tab_spanner to create the correct headers
  tab_spanner(
    label = "Dependent variable: Log GDP per capita, 1995",
    columns = 2:7 # Selects columns for models (1) through (6)
  ) %>%
  tab_spanner(
    label = "Dependent variable: Log output per worker, 1988",
    columns = 8:9 # Selects columns for models (7) and (8)
  )

base_sample <- base_sample %>%
  mutate(region = case_when(
    africa == 1 ~ "Africa",
    asia == 1 ~ "Asia",
    TRUE ~ "Other"
  ))

region_colors <- c(
  "Africa" = "firebrick",
  "Asia" = "steelblue",
  "Other" = "darkgreen"
)

regions <- unique(base_sample$region)

for (r in regions) {
  p <- ggplot(
    base_sample %>% filter(region == r),
    aes(x = avexpr, y = logpgp95, label = shortnam)
  ) +
    geom_point(size = 3, color = region_colors[r]) +  # Different color each plot
    geom_smooth(method = "lm", se = FALSE, linewidth = 1, color = "black") +
    geom_text_repel(size = 3) +
    theme_minimal(base_size = 13) +
    labs(
      title = paste("Figure 1 -", r, ": Institutions and Development"),
      subtitle = "OLS trend line without confidence band",
      x = "Institution Quality (avexpr)",
      y = "Log GDP per capita, 1995"
    )
  
  print(p)
}
