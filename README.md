# AS.440.606.57.FA25-Final-Project
This is the final group project of Jiechen Qiu, Wenjing Ding, Zexi Xu, Sizhe Jia.

This GitHub repo keeps track of our replication of the main empirical results from **AJR (2001)** and incorporates insights from their **Reply (2012)** to Albouy.

---

## 1. Data
All data come from AJR replication files. Data link: https://www.dropbox.com/scl/fi/tgs2zy2x6g9cnmberwdno/colonial_origins.zip?rlkey=kumc7exsfl95vv5ejj1w1agup&e=1&dl=0

Key variables:
- **Instrument**: logem4 (settler mortality)
- **Institutions**: avexpr, cons00a, cons1, democ00a
- **Outcomes**: logpgp95 (GDP), loghjypl (output per worker)
- **Controls**: lat_abst, africa, asia, other_cont

---

## 2. Methods
We replicate AJR’s Table 1–4 using:

- **Descriptive statistics**  
- **OLS regressions**  
- **Determinants of institutions regressions**  
- **First stage & IV (2SLS)**  
- **Robustness checks** (dropping Africa / Neo-Europes, adding continent dummies)

---

## 3. Results

Institutions strongly predict GDP (OLS and IV)

Settler mortality is a strong and valid instrument

Historical institutions (1900) significantly predict today's institutions

IV > OLS, consistent with measurement error attenuation

Findings remain stable across multiple robustness checks

Overall, our replication supports AJR (2001) and does not validate Albouy’s critique.

Our final results are presented in Final.Rmd: https://github.com/tangerinerine/AS.440.606.57.FA25-Final-Project/blob/main/Final.Rmd and FinalProject.pdf: https://github.com/tangerinerine/AS.440.606.57.FA25-Final-Project/blob/main/FinalProject.pdf.

---

## 4. Extentions

**Figure 1 — Institutions and Economic Development by Region**

This visualization explores the relationship between institutional quality and economic development across former colonies, based on data from Acemoglu, Johnson, and Robinson (2001).

The figure splits the sample into **three distinct geographic regions**:
- **Africa**
- **Asia**
- **Other former colonies**

For each region, a separate scatter plot is generated with:
- Country labels  
- Region-specific point colors  
- OLS fitted regression line without confidence band  
- Clear academic-style formatting  


**Purpose of this Visualization**

This analysis supports the empirical results from AJR Table 2 by showing:

> Countries with stronger protection against expropriation (higher avexpr) tend to have significantly higher income levels (log GDP per capita, 1995).

Furthermore, the relationship is **consistently positive** across all regions, suggesting that the institutional effect is not driven only by Africa or any single subset of the data.



**Code Used to Generate the Visualization**

```r
# Create region label
base_sample <- base_sample %>%
  mutate(region = case_when(
    africa == 1 ~ "Africa",
    asia == 1 ~ "Asia",
    TRUE ~ "Other"
  ))

# Define custom colors for each region
region_colors <- c(
  "Africa" = "firebrick",
  "Asia" = "steelblue",
  "Other" = "darkgreen"
)

# Generate 3 separate plots
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
      y = "Log GDP per Capita, 1995"
    )

  print(p)
}

```
---

## 5. Summary

This project replicates AJR’s key results and confirms that better institutions strongly promote higher income. IV estimates support a causal link from settler mortality to institutions and modern development. Historical measures also show persistent institutional effects. Overall, the replication deepens understanding of identification, IV methods, and long-run development analysis.

A one-page reflection summary can be found here: https://github.com/tangerinerine/AS.440.606.57.FA25-Final-Project/blob/main/Reflection%20summary.pdf 
