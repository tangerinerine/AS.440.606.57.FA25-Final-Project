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