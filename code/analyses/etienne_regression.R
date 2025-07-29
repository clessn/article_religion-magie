library(dplyr)
library(ggplot2)
library(gt)
library(webshot2)

source("code/functions/reg_tables_to_png.R")

data <- readRDS("_SharedFolder_article_religion-magie/Data/data_clean/data_religion_magie.rds")

data_quorum <- data  %>% 
  filter(survey_name == "quorum_1")

# Create Quebec and ROC datasets (using original filter, no weight modification needed)
data_quorum_roc <- data_quorum %>% 
  filter(ses_province != "qc")
data_quorum_qc <- data_quorum %>%
  filter(ses_province == "qc")

# Population proportions
pop_quebec <- 0.2298
pop_roc <- 1 - pop_quebec

# Sample sizes
sample_quebec <- nrow(data_quorum %>% filter(ses_province == "qc"))
sample_roc <- nrow(data_quorum %>% filter(ses_province != "qc"))
total_sample <- sample_quebec + sample_roc

# Calculate province weights
weight_quebec <- (pop_quebec * total_sample) / sample_quebec
weight_roc <- (pop_roc * total_sample) / sample_roc

# Modify weights only for data_quorum
data_quorum <- data_quorum %>%
  mutate(
    # Calculate provincial weight factor
    province_weight = ifelse(ses_province == "qc", weight_quebec, weight_roc),
    # Combine weights by multiplication and rescale
    weight = weight * province_weight * (n() / sum(weight * province_weight))
  )

# Convert religion_bin to binary numeric variable
data_quorum$religion_bin <- as.numeric(data_quorum$religion_bin)
data_quorum_qc$religion_bin <- as.numeric(data_quorum_qc$religion_bin)
data_quorum_roc$religion_bin <- as.numeric(data_quorum_roc$religion_bin)

models_list <- list(
  '\\textbf{Only religious respondents}' = list(
    "Canada" = survey::svyglm(covid_afraid_of_dying ~ religion_attached_to_church_religious + ses_female + ses_age_group + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity, design = survey::svydesign(ids = ~1, data = data_quorum, weights = ~weight)),
    "Quebec" = survey::svyglm(covid_afraid_of_dying ~ religion_attached_to_church_religious + ses_female + ses_age_group + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity, design = survey::svydesign(ids = ~1, data = data_quorum_qc, weights = ~weight)),
    "Rest of Canada" = survey::svyglm(covid_afraid_of_dying ~ religion_attached_to_church_religious + ses_female + ses_age_group + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity, design = survey::svydesign(ids = ~1, data = data_quorum_roc, weights = ~weight))
  ),
  '\\textbf{All respondents}' = list(
    "Canada" = survey::svyglm(covid_afraid_of_dying ~ religion_attached_to_church_all + ses_female + ses_age_group + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity, design = survey::svydesign(ids = ~1, data = data_quorum, weights = ~weight)),
    "Quebec" = survey::svyglm(covid_afraid_of_dying ~ religion_attached_to_church_all + ses_female + ses_age_group + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity, design = survey::svydesign(ids = ~1, data = data_quorum_qc, weights = ~weight)),
    "Rest of Canada" = survey::svyglm(covid_afraid_of_dying ~ religion_attached_to_church_all + ses_female + ses_age_group + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity, design = survey::svydesign(ids = ~1, data = data_quorum_roc, weights = ~weight))
  )
)

# Translation of "Attachement religieux" into English
cm <- c("religion_attached_to_church_religious" = "Religious attachment (religious respondents)",
        "religion_attached_to_church_all" = "Religious attachment (all respondents)")
gm <- c('r.squared', 'nobs')

# Model summary code
modelsummary::modelsummary(models_list,
                           output = "code/analyses/reg_table_religious_avec_controle.tex", 
                           stars = TRUE,
                           coef_map = cm, 
                           gof_map = gm,
                           shape = "rbind",
                           title = "Relationship between religiosity and fear of death during COVID-19",
                           notes = "Note: The controls used in these models are as follows: sex, age group, marital status, place of birth, education, sexual orientation, occupation, and ethnicity.")

reg_table_path <- latex_table_to_png("code/analyses/reg_table_religious_avec_controle.tex", output_path = "_SharedFolder_article_religion-magie/Data/tables/reg_table.png", is_file = TRUE, dpi = 600)
