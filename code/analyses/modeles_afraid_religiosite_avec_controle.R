library(dplyr)
library(ggplot2)
library(gt)
library(webshot2)

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
  'Only religious respondents' = list(
    "Canada" = lm(religion_attached_to_church_religious ~ covid_afraid_of_dying + ses_female + ses_age_group + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity, data = data_quorum),
    "Canada (Weighted)" = survey::svyglm(religion_attached_to_church_religious ~ covid_afraid_of_dying + ses_female + ses_age_group + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity, design = survey_design),
    "Quebec" = lm(religion_attached_to_church_religious ~ covid_afraid_of_dying + ses_female + ses_age_group + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity, data = data_quorum_qc),
    "Rest of Canada" = lm(religion_attached_to_church_religious ~ covid_afraid_of_dying + ses_female + ses_age_group + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity, data = data_quorum_roc)
  ),
  'All respondents' = list(
    "Canada" = lm(religion_attached_to_church_all ~ covid_afraid_of_dying + ses_female + ses_age_group + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity, data = data_quorum),
    "Canada (Weighted)" = survey::svyglm(religion_attached_to_church_all ~ covid_afraid_of_dying + ses_female + ses_age_group + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity, design = survey_design),
    "Quebec" = lm(religion_attached_to_church_all ~ covid_afraid_of_dying + ses_female + ses_age_group + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity, data = data_quorum_qc),
    "Rest of Canada" = lm(religion_attached_to_church_all ~ covid_afraid_of_dying + ses_female + ses_age_group + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity, data = data_quorum_roc)
  )
)

# Translation of "Peur de la mort" into English
cm <- c("covid_afraid_of_dying" = "Fear of death")

gm <- c('r.squared', 'nobs')

# Model summary code
modelsummary::modelsummary(models_list,
                           output = "code/analyses/reg_table_afraid_avec_controle.png", 
                           stars = TRUE,
                           coef_map = cm, 
                           gof_map = gm,
                           shape = "rbind",
                           title = "Relationship between fear of death during COVID-19 and religiosity",
                           notes = "Notes: The controls used in these models are as follows: sex, age group, marital status, place of birth, education, sexual orientation, occupation, and ethnicity. The weighting data come from the census.")


# Model plot --------------------------------------------------------------

### create a list containing each model
models_list_plot <- as.list(unlist(models_list, recursive = FALSE))

### use modelplot without drawing the plot (draw = FALSE).
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
  clessnize::theme_clean_light(base_size = 15) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high)) +
  labs(
    caption = "Notes:\n   The lines around the points represent the 95% confidence intervals.\n   The controls used in these models are as follows: sex, age group, marital status, place of birth,\n    education, sexual orientation, occupation, and ethnicity.\n   The weighting data comes from the census.",
    x = "<br>Coefficients"
  ) +
  theme(
    axis.title.y = element_blank(),
    plot.caption.position = "plot",
    axis.title.x = ggtext::element_markdown()
  )

ggsave("_SharedFolder_article_religion-magie/figures/figure5_model.png",
       width = 6.5, height = 4, dpi = 500)



