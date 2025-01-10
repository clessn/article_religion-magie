library(dplyr)
library(ggplot2)

data <- readRDS("_SharedFolder_article_religion-magie/Data/data_clean/data_religion_magie.rds")

data_quorum <- data  %>% 
    filter(survey_name == "quorum_1")

data_quorum_roc <- data_quorum %>% 
    filter(ses_province != "qc")

data_quorum_qc <- data_quorum %>%
    filter(ses_province == "qc")

# Population proportions
pop_quebec <- 0.2298
pop_roc <- 1 - pop_quebec

# Sample sizes
sample_quebec <- nrow(data_quorum_qc)
sample_roc <- nrow(data_quorum_roc)

# Total sample size
total_sample <- sample_quebec + sample_roc

# Calculate weights
weight_quebec <- (pop_quebec * total_sample) / sample_quebec
weight_roc <- (pop_roc * total_sample) / sample_roc

# Convert religion_bin to binary numeric variable
data_quorum$religion_bin <- as.numeric(data_quorum$religion_bin)
data_quorum_qc$religion_bin <- as.numeric(data_quorum_qc$religion_bin)
data_quorum_roc$religion_bin <- as.numeric(data_quorum_roc$religion_bin)

# Assuming `data_quorum` is your dataframe
data_quorum$weight <- ifelse(data_quorum$ses_province == "qc", weight_quebec, weight_roc)

# Define survey design
survey_design <- survey::svydesign(ids = ~1, data = data_quorum, weights = ~weight)

models_list <- list(
  'Seulement les répondants religieux' = list(
    "Canada" = lm(religion_attached_to_church_religious ~ covid_afraid_of_dying + ses_female + ses_age_group + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity, data = data_quorum),
    "Canada (Pondéré)" = survey::svyglm(religion_attached_to_church_religious ~ covid_afraid_of_dying + ses_female + ses_age_group + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity, design = survey_design),
    "Québec" = lm(religion_attached_to_church_religious ~ covid_afraid_of_dying + ses_female + ses_age_group + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity, data = data_quorum_qc),
    "ROC" = lm(religion_attached_to_church_religious ~ covid_afraid_of_dying + ses_female + ses_age_group + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity, data = data_quorum_roc)
  ),
  'Tous les répondants' = list(
    "Canada" = lm(religion_attached_to_church_all ~ covid_afraid_of_dying + ses_female + ses_age_group + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity, data = data_quorum),
    "Canada (Pondéré)" = survey::svyglm(religion_attached_to_church_all ~ covid_afraid_of_dying + ses_female + ses_age_group + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity, design = survey_design),
    "Québec" = lm(religion_attached_to_church_all ~ covid_afraid_of_dying + ses_female + ses_age_group + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity, data = data_quorum_qc),
    "ROC" = lm(religion_attached_to_church_all ~ covid_afraid_of_dying + ses_female + ses_age_group + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity, data = data_quorum_roc)
  )
)

cm <- c("covid_afraid_of_dying" = "Peur de la mort")

gm <- c('r.squared', 'nobs')

# Model summary code
modelsummary::modelsummary(models_list,
             output = "code/analyses/reg_table_afraid_avec_controle.tex", 
             stars = TRUE,
             coef_map = cm, 
             gof_map = gm,
             shape = "rbind",
             title = "Relation entre la peur de la mort durant la COVID-19 et la religiosité",
             notes = "Notes: Les contrôles utilisés dans les modèles sont les suivants: sexe, groupe d'âge, état civil, lieu de naissance, éducation, orientation sexuelle, occupation, et ethnie. Les données de pondération proviennent du recensement.")

# Model plot --------------------------------------------------------------

### create a list containing each model

models_list_plot <- as.list(unlist(models_list, recursive = FALSE))

### use modelplot without drawing the plot. (draw = FALSE)
modelsummary::modelplot(models_list_plot,
                        draw = FALSE) %>% 
  ### create variables for facet_wrap
  tidyr::separate(model, into = c("respondents", "model"), sep = "\\.") %>% 
  filter(term == "covid_afraid_of_dying") %>%
  mutate() %>% 
  ggplot(aes(x = estimate, y = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey75") +
  facet_wrap(~respondents) +
  geom_point() +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high)) +
  labs(caption = "Notes:\n   Les lignes autour des points présentent les intervalles de confiance à 95%.\n   Les contrôles utilisés dans les modèles sont les suivants: sexe, groupe d'âge, état civil, lieu de naissance,\n    éducation, orientation sexuelle, occupation, et ethnie.\n   Les données de pondération proviennent du recensement.",
       x = "<br>Coefficient de régression linéaire<br>Peur de la mort &rarr; Religiosité<br>") +
  clessnize::theme_clean_light() +
  theme(axis.title.y = element_blank(),
        plot.caption.position = "plot",
        axis.title.x = ggtext::element_markdown())

ggsave("_SharedFolder_article_religion-science/figures/figure5_model.png",
       width = 6.5, height = 4, dpi = 500)
