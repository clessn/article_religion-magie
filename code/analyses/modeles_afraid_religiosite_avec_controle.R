library(dplyr)
library(ggplot2)
library(gt)
library(webshot2)

source("code/functions/reg_tables_to_png.R")

data <- readRDS("_SharedFolder_article_religion-magie/Data/data_clean/data_religion_magie.rds")

data_quorum <- data  %>% 
  filter(survey_name == "quorum_1")

data_quorum_roc <- data_quorum %>% 
  filter(ses_province != "qc")

data_quorum_qc <- data_quorum %>%
  filter(ses_province == "qc")


# Convert religion_bin to binary numeric variable
data_quorum$religion_bin <- as.numeric(data_quorum$religion_bin)
data_quorum_qc$religion_bin <- as.numeric(data_quorum_qc$religion_bin)
data_quorum_roc$religion_bin <- as.numeric(data_quorum_roc$religion_bin)

models_list <- list(
  '\\textbf{Only religious respondents}' = list(
    "Canada" = survey::svyglm(religion_attached_to_church_religious ~ covid_afraid_of_dying + ses_female + ses_age_group + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity, design = survey::svydesign(ids = ~1, data = data_quorum, weights = ~weight)),
    "Quebec" = survey::svyglm(religion_attached_to_church_religious ~ covid_afraid_of_dying + ses_female + ses_age_group + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity, design = survey::svydesign(ids = ~1, data = data_quorum_qc, weights = ~weight)),
    "Rest of Canada" = survey::svyglm(religion_attached_to_church_religious ~ covid_afraid_of_dying + ses_female + ses_age_group + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity, design = survey::svydesign(ids = ~1, data = data_quorum_roc, weights = ~weight))
  ),
  '\\textbf{All respondents}' = list(
    "Canada" = survey::svyglm(religion_attached_to_church_all ~ covid_afraid_of_dying + ses_female + ses_age_group + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity, design = survey::svydesign(ids = ~1, data = data_quorum, weights = ~weight)),
    "Quebec" = survey::svyglm(religion_attached_to_church_all ~ covid_afraid_of_dying + ses_female + ses_age_group + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity, design = survey::svydesign(ids = ~1, data = data_quorum_qc, weights = ~weight)),
    "Rest of Canada" = survey::svyglm(religion_attached_to_church_all ~ covid_afraid_of_dying + ses_female + ses_age_group + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity, design = survey::svydesign(ids = ~1, data = data_quorum_roc, weights = ~weight))
  )
)

# Translation of "Peur de la mort" into English
cm <- c("covid_afraid_of_dying" = "Fear of death")

gm <- c('r.squared', 'nobs')

# Model summary code
modelsummary::modelsummary(models_list,
                           output = "code/analyses/reg_table_afraid_avec_controle.tex", 
                           stars = TRUE,
                           coef_map = cm, 
                           gof_map = gm,
                           shape = "rbind",
                           title = "Relationship between fear of death during COVID-19 and religiosity",
                           notes = "Note: The controls used in these models are as follows: sex, age group, marital status, place of birth, education, sexual orientation, occupation, and ethnicity.")

reg_table_path <- latex_table_to_png("code/analyses/reg_table_afraid_avec_controle.tex", output_path = "_SharedFolder_article_religion-magie/Data/tables/reg_table.png", is_file = TRUE, dpi = 600)
