library(dplyr)
library(ggplot2)

# Assuming df is your dataframe already loaded
df <- readRDS("_SharedFolder_article_religion-magie/Data/religiosity_historic/mart/data.rds")

# Filter the dataset for the 'importance' variable and calculate the average importance for each year and subgroup
df_avg_importance <- df %>%
  filter(variable_id == "importance") %>%
  group_by(year, subgroup) %>%
  summarise(avg_importance = sum(choice * value, na.rm = TRUE) / sum(value, na.rm = TRUE))

# Create the plot
ggplot(df_avg_importance, aes(x = as.numeric(year), y = avg_importance, color = subgroup)) +
  geom_line(linewidth = 1.2, alpha = 0.4) +
  scale_color_manual(values = c("qc" = "#003DA5", "can" = "#D80621"),
                    labels = c("qc" = "Québec", "can"= "Reste du Canada")) +
  labs(x = "\nAnnées",
       y = "Importance de la religion\n",
       color = "Subgroup") +
  scale_x_continuous(breaks = unique(as.numeric(df_avg_importance$year))) + 
  clessnize::theme_clean_light() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("_SharedFolder_article_religion-magie/figures/graphs/religiosity.png", width = 10, height = 6, dpi = 300)
