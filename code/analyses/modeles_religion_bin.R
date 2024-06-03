library(dplyr)
library(ggplot2)

data <- readRDS("_SharedFolder_article_religion-magie/Data/data_clean/data_religion_magie.rds")

data_quorum <- data  %>% 
    filter(survey_name == "quorum_1")

data_quorum_roc <- data_quorum %>% 
    filter(ses_province != "qc")

data_quorum_qc <- data_quorum %>%
    filter(ses_province == "qc")

    
m1 <- glm(religion_bin ~ covid_afraid_of_dying + ses_female + ses_age_group + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity, data = data_quorum, family = "binomial")

m2 <- glm(religion_bin ~ covid_afraid_of_dying + ses_female + ses_age_group + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity, data = data_quorum_qc, family = "binomial")

m3 <- glm(religion_bin ~ covid_afraid_of_dying + ses_female + ses_age_group + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity, data = data_quorum_roc, family = "binomial")

summary(m1)
summary(m2)
summary(m3)
