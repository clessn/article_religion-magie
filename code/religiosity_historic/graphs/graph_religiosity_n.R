library(dplyr)
library(ggplot2)

# Assuming df is your dataframe already loaded
df <- readRDS("_SharedFolder_article_religion-magie/Data/religiosity_historic/mart/data.rds")

df$weighted_value <- df$choice * df$value

# Filter the dataset for the 'importance' variable and calculate the weighted average importance for each year and subgroup
df_avg_importance <- df %>%
  filter(variable_id == "importance") %>%
  filter(!is.na(subgroup)) %>%
  filter(!is.na(choice)) %>%
  group_by(survey_id, subgroup, year, n) %>%
  summarize(actual_value = sum(weighted_value), .groups = 'drop')

df_weighted_year <- df_avg_importance %>%
  group_by(subgroup, year) %>%
  summarize(weighted_avg_value = sum(actual_value * n) / sum(n), .groups = 'drop')

# Create the plot
ggplot(df_weighted_year, aes(x = as.numeric(year), y = weighted_avg_value, color = subgroup)) +
  geom_line(linewidth = 1.2, alpha = 0.4) +
  clessnize::theme_clean_light(base_size = 15) +
  scale_color_manual(values = c("qc" = "#003DA5", "can" = "#D80621"),
                     labels = c("qc" = "Quebec", "can" = "Rest of Canada")) +
  labs(x = "\nYears",
       y = "Importance of Religion\n",
       color = "Subgroup") +
  scale_x_continuous(breaks = unique(as.numeric(df_weighted_year$year))) + 
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  theme(plot.title = element_text(hjust = 0.5))




# Save the plot
ggsave("_SharedFolder_article_religion-magie/figures/graphs/religiosity_weighted.png", width = 10, height = 6, dpi = 300)

