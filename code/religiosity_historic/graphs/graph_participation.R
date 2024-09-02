library(dplyr)
library(ggplot2)

# Assuming df is your dataframe already loaded
df <- readRDS("_SharedFolder_article_religion-magie/Data/religiosity_historic/mart/data.rds")

# Filter the dataset for the 'importance' variable and calculate the average importance for each year and subgroup
df_avg_participation <- df %>%
  filter(variable_id == "participation") %>%
  group_by(year, subgroup) %>%
  summarise(avg_participation = sum(choice * value, na.rm = TRUE) / sum(value, na.rm = TRUE))

# Create the plot
ggplot(df_avg_participation, aes(x = as.numeric(year), y = avg_participation, color = subgroup)) +
  geom_line(linewidth = 1.2, alpha = 0.7) + 
  labs(title = "Average Participation by Year and Subgroup",
       x = "Year",
       y = "Average Participation",
       color = "Subgroup") +
  scale_x_continuous(breaks = unique(as.numeric(df_avg_participation$year))) + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
