library(dplyr)
library(ggplot2)

data <- readRDS("_SharedFolder_article_religion-magie/Data/data_clean/data_religion_magie.rds")

data_quorum <- data  %>% 
    filter(survey_name == "quorum_1")

data_quorum_roc <- data_quorum %>% 
    filter(ses_province != "qc")

data_quorum_qc <- data_quorum %>%
    filter(ses_province == "qc")


hist(data_quorum_roc$ses_age, breaks = 20, main = "Histogramme de l'âge des répondants", xlab = "Âge", col = "lightblue")

