library(dplyr)
library(ggplot2)

data <- readRDS("_SharedFolder_article_religion-magie/Data/data_clean/data_religion_magie.rds")

data_quorum <- data  %>% 
    filter(survey_name == "quorum_1")

data_quorum_roc <- data_quorum %>% 
    filter(ses_province != "qc")

data_quorum_qc <- data_quorum %>%
    filter(ses_province == "qc")

ggplot(data_quorum, aes(x = religion_attached_to_church)) +
    geom_histogram(fill = "lightblue", color = "black") +
    labs(title = "Distribution of the variable religion_attached_to_church",
         x = "Religion attached to church",
         y = "Frequency") +
    theme_minimal()

ggplot(data_quorum, aes(x = covid_fear_scale, y = religion_attached_to_church_religious)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    labs(title = "Scatterplot of covid_fear_scale and religion_attached_to_church",
         x = "Covid fear scale",
         y = "Religion attached to church") +
    theme_classic()


models_list <- list(
  'Tous les répondants' = list(
    "Tout le Canada" = lm(religion_attached_to_church_all ~ covid_fear_scale + ses_female + ses_age_group + ses_language + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity + how_many_immigrants, data = data_quorum),
    "Québec" = lm(religion_attached_to_church_all ~ covid_fear_scale + ses_female + ses_age_group + ses_language + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity + how_many_immigrants, data = data_quorum_qc),
    "ROC" = lm(religion_attached_to_church_all ~ covid_fear_scale + ses_female + ses_age_group + ses_language + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity + how_many_immigrants, data = data_quorum_roc)
  ),
  'Seulement les répondants religieux' = list(
    "Tout le Canada" = lm(religion_attached_to_church_religious ~ covid_fear_scale + ses_female + ses_age_group + ses_language + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity + how_many_immigrants, data = data_quorum),
    "Québec" = lm(religion_attached_to_church_religious ~ covid_fear_scale + ses_female + ses_age_group + ses_language + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity + how_many_immigrants, data = data_quorum_qc),
    "ROC" = lm(religion_attached_to_church_religious ~ covid_fear_scale + ses_female + ses_age_group + ses_language + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity + how_many_immigrants, data = data_quorum_roc)
  ),
  'Religion binaire' = list(
    "Tout le Canada" = glm(religion_bin ~ covid_fear_scale + ses_female + ses_age_group + ses_language + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity + how_many_immigrants, data = data_quorum, family = "binomial"),
    "Québec" = glm(religion_bin ~ covid_fear_scale + ses_female + ses_age_group + ses_language + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity + how_many_immigrants, data = data_quorum_qc, family = "binomial"),
    "ROC" = glm(religion_bin ~ covid_fear_scale + ses_female + ses_age_group + ses_language + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity + how_many_immigrants, data = data_quorum_roc, family = "binomial")
  ), 
  'Peur de la mort' = list(
    "Tout le Canada" = lm(religion_attached_to_church_all ~ covid_not_afraid_of_dying + ses_female + ses_age_group + ses_language + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity + how_many_immigrants, data = data_quorum),
    "Québec" = lm(religion_attached_to_church_all ~ covid_not_afraid_of_dying + ses_female + ses_age_group + ses_language + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity + how_many_immigrants, data = data_quorum_qc),
    "ROC" = lm(religion_attached_to_church_all ~ covid_not_afraid_of_dying + ses_female + ses_age_group + ses_language + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity + how_many_immigrants, data = data_quorum_roc)
  )
)

fixed_effects <- tibble::tribble(
  ~Term, ~ToutCanada, ~Quebec, ~Canada,
  "Years Fixed Effects", "$\\checkmark$", "", "$\\checkmark$",
  "Sources Fixed Effects", "", "$\\checkmark$", "$\\checkmark$"
)

# Model summary code
modelsummary::modelsummary(models_list,
             output = "code/analyses/creation_modele_covid.tex", 
             stars = TRUE,
             coef_map = c("covid_fear_scale", "covid_not_afraid_of_dying"),  # Omit year coefficients
             gof_omit = 'DF|Deviance|AIC|BIC|Log|RMSE|adj.r.squared',
             shape = "rbind",
             add_rows = fixed_effects)
