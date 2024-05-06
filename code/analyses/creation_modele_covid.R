library(dplyr)

data <- readRDS("_SharedFolder_article_religion-magie/Data/data_clean/data_religion_magie.RDS")

data_quorum <- data  %>% 
    filter(survey_name == "quorum_1")

mod_covid <- lm(covid_fear_scale ~ )