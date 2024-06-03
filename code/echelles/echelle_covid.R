library(dplyr)
library(ggplot2)

source("code/cleaning/cleaning_quorum_1.R")

data <- readRDS("_SharedFolder_article_religion-magie/Data/data_clean/data_quorum_1.rds")

data_scale <- data %>%
    select(starts_with("covid")) %>% 
    na.omit()

sondr::topdown_fa(data_scale)

data_scale_2 <- data_scale %>%
    select(covid_afraid_of_dying, covid_feeling, covid_pandemic_more_serious)

sondr::topdown_fa(data_scale_2)

sum(table(data$covid_pandemic_more_serious))
sum(table(data$covid_only_vulnerable_people))
sum(table(data$covid_afraid_of_dying))
sum(table(data$covid_limiting_freedom_is_necessary))
sum(table(data$covid_feeling))

data$covid_fear_scale <- NA
data$covid_fear_scale <- (data$covid_pandemic_more_serious +
                          data$covid_only_vulnerable_people +
                          data$covid_afraid_of_dying +
                          data$covid_limiting_freedom_is_necessary +
                          data$covid_feeling) / 5

data$covid_fear_scale_small <- NA
data$covid_fear_scale_small <- (data$covid_pandemic_more_serious +
                                data$covid_afraid_of_dying +
                                data$covid_feeling) / 3


saveRDS(data, "_SharedFolder_article_religion-magie/Data/data_clean/data_quorum_1.rds")

