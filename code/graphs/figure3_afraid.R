# Packages ----------------------------------------------------------------
library(ggplot2)
library(dplyr)

### religiosity
### peur mort
### qc-can

# Data --------------------------------------------------------------------

data <- readRDS("_SharedFolder_article_religion-science/Data/data_clean/data_religion_magie.rds") %>% 
  mutate(region = ifelse(ses_province == "qc", "qc", "roc"))


# Graph -------------------------------------------------------------------

data %>%
  group_by(region, covid_afraid_of_dying) %>% 
  summarise(n = n()) %>% 
  group_by(region) %>% 
  mutate(prop = n / sum(n) * 100) %>%
  ggplot(aes(x = covid_afraid_of_dying)) +
  geom_density(aes(group = region,
                   fill = region,
                   weight = prop),
               color = NA,
               alpha = 0.4,
               bw = 0.1) +
  clessnize::theme_clean_light(base_size = 15) +
  scale_x_continuous(breaks = c(0.2, 0.8),
                     labels = c("Moins peur", "Plus peur")) +
  scale_fill_manual(values = c("#003DA5", "#D80621"),
                    labels = c("Québec", "Reste du Canada")) +
  labs(x = "\nPeur de mourir de la COVID-19",
       y = "Densité\n")

ggsave("_SharedFolder_article_religion-science/figures/figure3_afraid.png",
       width = 10, height = 6, dpi = 300)
