library(dplyr)
library(ggplot2)

df <- readRDS("_SharedFolder_article_religion-magie/Data/religiosity_historic/mart/data.rds")

df$weighted_value <- df$choice * df$value

df_avg_religious <- df %>%
  filter(variable_id == "religious_bin") %>%
  filter(!is.na(subgroup)) %>%
  filter(!is.na(choice)) %>%
  group_by(survey_id, subgroup, year, n) %>%
  summarize(actual_value = sum(weighted_value), .groups = 'drop')

df_weighted_year <- df_avg_religious %>%
  group_by(subgroup, year) %>%
  summarize(weighted_avg_value = sum(actual_value * n) / sum(n), .groups = 'drop')

ggplot(df_weighted_year, aes(x = as.numeric(year), y = weighted_avg_value, color = subgroup)) +
  geom_line(linewidth = 1.2, alpha = 0.4) +
  scale_color_manual(values = c("qc" = "#003DA5", "can" = "#D80621"),
                    labels = c("qc" = "Québec", "can"= "Reste du Canada")) +
  labs(x = "\nAnnées",
       y = "Proportion de religieux\n",
       color = "Subgroup") +
  scale_x_continuous(breaks = unique(as.numeric(df_weighted_year$year))) + 
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  clessnize::theme_clean_light() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("_SharedFolder_article_religion-magie/figures/graphs/religious_bin_weighted.png", width = 10, height = 6, dpi = 300)

