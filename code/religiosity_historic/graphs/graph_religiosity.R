df <- readRDS("_SharedFolder_article_religion-magie/Data/religiosity_historic/mart/data.rds")

# Filter the dataset for the specific variable and choice, and calculate the mean value for each year and subgroup
df_avg <- df %>% 
  filter(variable_id == "importance") %>% 
  group_by(year, subgroup) %>%
  summarise(value = mean(value, na.rm = TRUE))

# Create the plot
ggplot(df_avg, aes(x = as.numeric(year), y = value, color = subgroup)) +
  geom_line(size = 1.2, alpha = 0.7) + 
  scale_color_manual(values = c("qc" = "blue", "can" = "red")) +
  labs(title = "Time Series of Religious Bin by Year",
       x = "Year",
       y = "Proportion",
       color = "Region") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))