library(dplyr)
library(ggplot2)

source("code/cleaning/cleaning_quorum_1.R")

data <- readRDS("_SharedFolder_article_religion-magie/Data/data_clean/data_quorum_1.rds")

data_scale <- data %>%
    select(starts_with("covid")) %>% 
    na.omit()

sondr::topdown_fa(data_scale)

sum(table(data$covid_pandemic_more_serious))
sum(table(data$covid_only_vulnerable_people))
sum(table(data$covid_not_afraid_of_dying))
sum(table(data$covid_limiting_freedom_is_necessary))
sum(table(data$covid_feeling))

data$covid_fear_scale <- NA
data$covid_fear_scale <- (data$covid_pandemic_more_serious +
                          data$covid_only_vulnerable_people +
                          data$covid_not_afraid_of_dying +
                          data$covid_limiting_freedom_is_necessary +
                          data$covid_feeling) / 6


ggplot(data, aes(x = covid_fear_scale)) +
    geom_histogram(bins = 15) +
    clessnize::theme_clean_light()
ggsave("_SharedFolder_article_religion-magie/Data/graphs/histogram_covid_fear_scale.png")


saveRDS(data, "_SharedFolder_article_religion-magie/Data/data_clean/data_quorum_1.rds")

