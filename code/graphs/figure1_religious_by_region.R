# Packages ----------------------------------------------------------------
library(ggplot2)
library(dplyr)

### religiosity
### peur mort
### qc-can

# Data --------------------------------------------------------------------

data <- readRDS("_SharedFolder_article_religion-magie/Data/data_clean/data_religion_magie.rds") %>% 
  mutate(region = ifelse(ses_province == "qc", "qc", "roc"))

str(data)

## religion_attached_to_church_all
## religion_attached_to_church_religious
## covid_afraid_of_dying
## ses_province

# Graph -------------------------------------------------------------------

z <- 1.96
graph_data <- data %>% 
  group_by(region, religion_bin) %>%
  summarise(n = n()) %>% 
  group_by(region) %>%
  mutate(prop = n / sum(n) * 100,
         margin_error = z * sqrt((prop / 100) * (1 - prop / 100) / sum(n)) * 100,
         conf_low = prop - margin_error,
         conf_high = prop + margin_error) %>% 
  filter(religion_bin == 1)


ggplot(graph_data, aes(x = region, y = prop, fill = religion_bin)) +
  geom_col(fill = "grey30") +
  geom_linerange(aes(ymin = conf_low, ymax = conf_high),
                 linewidth = 1, color = "black") +
  clessnize::theme_clean_light(base_size = 15) +
  ylab("Proportion of Respondents Identifying as \nMembers of a Religious Community (%)") +
  xlab("") +
  labs(caption = "Error bars represent 95% confidence intervals.") +
  scale_x_discrete(labels = c("Quebec", "Rest of Canada"),
                   breaks = c("qc", "roc"))


ggsave("_SharedFolder_article_religion-magie/figures/figure1_religious_by_region.png",
       width = 8, height = 6, dpi = 300)
