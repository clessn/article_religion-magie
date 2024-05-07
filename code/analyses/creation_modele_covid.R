library(dplyr)

data <- readRDS("_SharedFolder_article_religion-magie/Data/data_clean/data_religion_magie.rds")

data_quorum <- data  %>% 
    filter(survey_name == "quorum_1")

mod_covid <- lm(covid_fear_scale ~ religion_attached_to_church + ses_province + ses_female + ses_age_group + ses_language + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity + how_many_immigrants, data = data_quorum)

summary(mod_covid)

