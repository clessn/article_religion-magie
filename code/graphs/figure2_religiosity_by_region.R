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
  filter(religion_bin == 1) %>% 
  group_by(region, religion_attached_to_church_religious) %>%
  summarise(n = n()) %>% 
  group_by(region) %>%
  mutate(prop = n / sum(n) * 100)

ggplot(graph_data, aes(x = religion_attached_to_church_religious)) +
  geom_density(aes(group = region,
                   fill = region,
                   weight = prop),
               color = NA,
               alpha = 0.4,
               bw = 0.1) +
  clessnize::theme_clean_light(base_size = 15) +
  scale_fill_manual(values = c("#003DA5", "#D80621"),
                    labels = c("Quebec", "Rest of Canada")) +
  ylab("Density\n") +
  xlab("\nAttachment to Religion") +
  scale_x_continuous(breaks = c(0.2, 0.8),
                     labels = c("Less attached", "More attached")) +
  labs(caption = "Among respondents identifying as members of a religious community") 
  #clessnverse::theme_clean_light()
ggsave("_SharedFolder_article_religion-magie/figures/figure2_religiosity_by_region.png",
       width = 9, height = 6, dpi = 300)
