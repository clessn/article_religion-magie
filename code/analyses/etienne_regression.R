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

# Pour les modèles "Only religious respondents"
data_quorum_religious <- data_quorum %>% 
  filter(religion_bin == 1)  # Garde seulement les religieux

data_quorum_religious_qc <- data_quorum_religious %>%
  filter(ses_province == "qc")

data_quorum_religious_roc <- data_quorum_religious %>%
  filter(ses_province != "qc")

# Puis utilisez ces données filtrées dans vos modèles :
models_list <- list(
  '\\textbf{Only religious respondents}' = list(
    "Canada" = survey::svyglm(covid_afraid_of_dying ~ religion_attached_to_church_religious + ses_female + ses_age_group + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity, 
                              design = survey::svydesign(ids = ~1, data = data_quorum_religious, weights = ~weight)),
    "Quebec" = survey::svyglm(covid_afraid_of_dying ~ religion_attached_to_church_religious + ses_female + ses_age_group + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity, 
                              design = survey::svydesign(ids = ~1, data = data_quorum_religious_qc, weights = ~weight)),
    "Rest of Canada" = survey::svyglm(covid_afraid_of_dying ~ religion_attached_to_church_religious + ses_female + ses_age_group + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity, 
                                      design = survey::svydesign(ids = ~1, data = data_quorum_religious_roc, weights = ~weight))
  ),
  # Les modèles "All respondents" restent avec data_quorum complet
  '\\textbf{All respondents}' = list(
    "Canada" = survey::svyglm(covid_afraid_of_dying ~ religion_attached_to_church_all + ses_female + ses_age_group + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity, 
                              design = survey::svydesign(ids = ~1, data = data_quorum, weights = ~weight)),
    "Quebec" = survey::svyglm(covid_afraid_of_dying ~ religion_attached_to_church_all + ses_female + ses_age_group + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity, 
                              design = survey::svydesign(ids = ~1, data = data_quorum_qc, weights = ~weight)),
    "Rest of Canada" = survey::svyglm(covid_afraid_of_dying ~ religion_attached_to_church_all + ses_female + ses_age_group + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity, 
                                      design = survey::svydesign(ids = ~1, data = data_quorum_roc, weights = ~weight))
  )
)

# Translation of "Attachement religieux" into English
cm <- c("religion_attached_to_church_religious" = "Religious attachment (religious respondents)",
        "religion_attached_to_church_all" = "Religious attachment (all respondents)")
gm <- c('r.squared', 'nobs')

# Assurez-vous que l'option est bien prise en compte
options("modelsummary_format_numeric_latex" = "plain")

# Recréez complètement le tableau
modelsummary::modelsummary(models_list,
                          output = "code/analyses/reg_table_religious_avec_controle.tex", 
                          stars = TRUE,
                          coef_map = cm, 
                          gof_map = gm,
                          shape = "rbind",
                          notes = "Note: The controls used in these models are as follows: sex, age group, marital status, place of birth, education, sexual orientation, occupation, and ethnicity.")

# Puis conversion
reg_table_path <- latex_table_to_png(
  "code/analyses/reg_table_religious_avec_controle.tex",
  output_path = "_SharedFolder_article_religion-magie/Data/tables/reg_table.png",
  is_file = TRUE,
  dpi = 300
)

# =============================================================================
# TABLEAU SÉPARÉ POUR L'INTERACTION 
# =============================================================================

# Ajouter la variable dummy Quebec à tes données (après ton code existant)
data_quorum <- data_quorum %>%
  mutate(quebec_dummy = ifelse(ses_province == "qc", 1, 0))

data_quorum_religious <- data_quorum_religious %>%
  mutate(quebec_dummy = ifelse(ses_province == "qc", 1, 0))

# =============================================================================
# MODÈLES D'INTERACTION POUR TESTER LES DIFFÉRENCES RÉGIONALES
# =============================================================================

models_interaction_only <- list(
  "Religious respondents" = survey::svyglm(covid_afraid_of_dying ~ religion_attached_to_church_religious + quebec_dummy + religion_attached_to_church_religious:quebec_dummy + ses_female + ses_age_group + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity, 
                                          design = survey::svydesign(ids = ~1, data = data_quorum_religious, weights = ~weight)),
  
  "All respondents" = survey::svyglm(covid_afraid_of_dying ~ religion_attached_to_church_all + quebec_dummy + religion_attached_to_church_all:quebec_dummy + ses_female + ses_age_group + ses_marital_status + ses_born_canada + ses_education + ses_sexual_orientation + ses_occupation + ses_ethnicity, 
                                     design = survey::svydesign(ids = ~1, data = data_quorum, weights = ~weight))
)

# Mapping des coefficients pour le tableau d'interaction
cm_interaction <- c("religion_attached_to_church_religious" = "Religious attachment",
                   "religion_attached_to_church_all" = "Religious attachment", 
                   "quebec_dummy" = "Quebec (vs ROC)",
                   "religion_attached_to_church_religious:quebec_dummy" = "Religious attachment × Quebec",
                   "religion_attached_to_church_all:quebec_dummy" = "Religious attachment × Quebec")

gm <- c('r.squared', 'nobs')

# Tableau d'interaction séparé - OPTION 2 (Recommandée)
modelsummary::modelsummary(models_interaction_only,
                          output = "code/analyses/reg_table_interaction_test.tex", 
                          stars = TRUE,
                          coef_map = cm_interaction, 
                          gof_map = gm,
                          notes = "Note: Models include the same controls as in Table 1. The interaction term tests whether the effect of religious attachment differs significantly between Quebec and Rest of Canada.")

# Conversion en PNG
interaction_table_path <- latex_table_to_png(
  "code/analyses/reg_table_interaction_test.tex",
  output_path = "_SharedFolder_article_religion-magie/Data/tables/interaction_test_table.png",
  is_file = TRUE,
  dpi = 300
)

# =============================================================================
# RÉSUMÉ POUR INTERPRÉTATION
# =============================================================================

cat("=== RÉSULTATS DU TEST D'INTERACTION ===\n")
cat("Tableau principal: reg_table.png (tes modèles originaux)\n")
cat("Tableau d'interaction: interaction_test_table.png (nouveau tableau)\n\n")

# Extraire les résultats de l'interaction pour le texte
summary_religious <- summary(models_interaction_only[["Religious respondents"]])
summary_all <- summary(models_interaction_only[["All respondents"]])

# Coefficients d'interaction
interaction_religious <- summary_religious$coefficients["religion_attached_to_church_religious:quebec_dummy", ]
interaction_all <- summary_all$coefficients["religion_attached_to_church_all:quebec_dummy", ]

cat("Modèle 'Religious respondents':\n")
cat("• Interaction coefficient:", round(interaction_religious["Estimate"], 4), "\n")
cat("• P-value:", round(interaction_religious["Pr(>|t|)"], 4), "\n")

cat("\nModèle 'All respondents':\n") 
cat("• Interaction coefficient:", round(interaction_all["Estimate"], 4), "\n")
cat("• P-value:", round(interaction_all["Pr(>|t|)"], 4), "\n")

# Suggestion de texte pour l'article
cat("\n=== TEXTE SUGGÉRÉ POUR L'ARTICLE ===\n")
cat("À ajouter après ton tableau principal:\n\n")
cat("'To formally assess whether these regional differences are statistically significant, we estimated additional models including interaction terms between religious attachment and a Quebec dummy variable (Table Y). ")

if(interaction_religious["Pr(>|t|)"] < 0.05) {
  cat("The interaction term for religious respondents is statistically significant (β = ", round(interaction_religious["Estimate"], 3), ", p = ", round(interaction_religious["Pr(>|t|)"], 3), "), confirming that the effect of religious attachment on COVID-19 death fear differs significantly between Quebec and the rest of Canada.'")
} else if(interaction_religious["Pr(>|t|)"] < 0.1) {
  cat("The interaction term for religious respondents shows a marginally significant effect (β = ", round(interaction_religious["Estimate"], 3), ", p = ", round(interaction_religious["Pr(>|t|)"], 3), "), suggesting regional differences in the relationship between religious attachment and COVID-19 death fear.'")
} else {
  cat("While the separate models show different effect sizes between regions, the interaction term is not statistically significant (β = ", round(interaction_religious["Estimate"], 3), ", p = ", round(interaction_religious["Pr(>|t|)"], 3), "), indicating that we cannot formally reject the null hypothesis of no regional difference.'")
}