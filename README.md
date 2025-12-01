# AS.440.606.57.FA25-Final-Project
This is the final group project of Jiechen Qiu, Wenjing Ding, Zexi Xu, Sizhe Jia.

This GitHub repo keeps track of our replication of the main empirical results from **AJR (2001)** and incorporates insights from their **Reply (2012)** to Albouy.


## 1. Data
All data come from AJR replication files.

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

Render the full report:

```r
rmarkdown::render("FinalProject.Rmd")

## 3. Results (Summary)

Institutions strongly predict GDP (OLS and IV)

Settler mortality is a strong and valid instrument

Historical institutions (1900) significantly predict today's institutions

IV > OLS, consistent with measurement error attenuation

Findings remain stable across multiple robustness checks

Overall, our replication supports AJR (2001) and does not validate Albouy’s critique.

