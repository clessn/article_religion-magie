library(dplyr)
library(ggplot2)
library(tidyr)

df <- readRDS("_SharedFolder_article_religion-magie/Data/religiosity_historic/mart/data.rds")

# Filter the dataset for the specific variable and choice, and calculate the mean value for each year and subgroup
df_avg <- df %>% 
  filter(variable_id == "religious_bin") %>% 
  filter(choice == 1) %>%
  group_by(year, subgroup) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>% 
  filter(!is.na(value), !is.na(subgroup)) 

# Create the plot
ggplot(df_avg, aes(x = as.numeric(year), y = value, color = subgroup)) +
  geom_line(size = 1.2, alpha = 0.4) + 
  scale_color_manual(values = c("qc" = "#003DA5", "can" = "#D80621"),
                     labels = c("qc" = "Québec", "can" = "Reste du Canada")) +
  labs(x = "\nAnnées",
       y = "Proportion des répondants qui s'identifient à une religion\n",
       color = "Subgroup") +
  scale_x_continuous(breaks = unique(as.numeric(df_avg$year))) + 
  clessnize::theme_clean_light() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("_SharedFolder_article_religion-magie/figures/graphs/religious_bin.png", width = 10, height = 6, dpi = 300)

