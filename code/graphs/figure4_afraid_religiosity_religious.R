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

graph_data <- data %>% 
  filter(religion_bin == 1) %>%
  tidyr::drop_na(region, religion_attached_to_church_religious, covid_afraid_of_dying) %>%
  mutate(region = ifelse(region == "qc", "Québec", "Reste du Canada"))

graph_data %>%
  group_by(region, religion_attached_to_church_religious, covid_afraid_of_dying) %>% 
  summarise(n = n()) %>% 
  group_by(region) %>% 
  mutate(prop = n / sum(n) * 100) %>%
  ggplot(aes(y = religion_attached_to_church_religious,
             x = covid_afraid_of_dying,
             color = region, fill = region)) +
  facet_wrap(~region) +
  geom_tile(aes(alpha = prop),
            color = NA,
            width = 0.225, height = 0.225) +
  geom_smooth(data = graph_data, method = "lm",
              se = FALSE, alpha = 0.1) +
  clessnize::theme_clean_light(base_size = 15) +
  scale_y_continuous(breaks = c(0.2, 0.8),
                     labels = c("Moins attaché", "Plus attaché")) +
  scale_x_continuous(breaks = c(0.2, 0.8),
                     labels = c("Moins peur", "Plus peur")) +
  scale_fill_manual(values = c("#003DA5", "#D80621"),
                    labels = c("Québec", "Reste du Canada")) +
  scale_color_manual(values = c("#003DA5", "#D80621"),
                     labels = c("Québec", "Reste du Canada")) +
  scale_alpha_continuous(range = c(0.02, 0.8),
                         name = "Proportion (%)") +
  labs(y = "Attachement à\nune religion\n",
       x = "\nPeur de mourir de la COVID-19",
       caption = "Parmi les répondants qui sont attachés à une religion.") +
  guides(color = "none", fill = "none") +
  theme(legend.title = element_text(),
        axis.text.y = element_text(angle = 90, hjust = 0.5))

ggsave("_SharedFolder_article_religion-science/figures/figure4_afraid_religiosity_religious.png",
       width = 10, height = 6, dpi = 300)
