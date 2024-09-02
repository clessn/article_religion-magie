# Packages ---------------------------------------------------------------
library(dplyr)

# Load Raw Data -------------------------------------------------------------------

raw_data <- sondr::read_any_csv("_SharedFolder_article_religion-magie/Data/religiosity_historic/lake/ces2021.csv")

# Create Clean Data ------------------------------------------------------

clean_data <- data.frame(
  id = 1:nrow(raw_data)
)

# Clean relevant variables -----------------------------------------------


## Quebec-Canada -----------------------------------------------------------------

### name this variable "subgroup"
#### categories : qc, can

table(raw_data$cps21_province, useNA = "always")
clean_data$subgroup <- NA
clean_data$subgroup[raw_data$cps21_province == "Quebec"] <- "qc"
clean_data$subgroup[raw_data$cps21_province != "Quebec"] <- "can"
table(clean_data$subgroup, useNA = "always")

## Religion ---------------------------------------------------------------

table(raw_data$cps21_religion, useNA = "always")
clean_data$religious_bin <- NA
clean_data$religious_bin[raw_data$cps21_religion == "Agnostic" | raw_data$cps21_religion == "None/ Don't have one/ Atheist" | raw_data$cps21_religion == "Other (please specify)"] <- 0
clean_data$religious_bin[raw_data$cps21_religion != "Agnostic" & raw_data$cps21_religion != "None/ Don't have one/ Atheist" & raw_data$cps21_religion != "Other (please specify)"] <- 1
clean_data$religious_bin[raw_data$cps21_religion == "Don't know/ Prefer not to answer"] <- NA
table(clean_data$religious_bin, useNA = "always")

## Importance of religiosity ----------------------------------------------

table(raw_data$cps21_rel_imp)
clean_data$importance <- NA
clean_data$importance[raw_data$cps21_rel_imp == "Very important"] <- 1
clean_data$importance[raw_data$cps21_rel_imp == "Somewhat important"] <- 0.66
clean_data$importance[raw_data$cps21_rel_imp == "Not very important"] <- 0.33
clean_data$importance[raw_data$cps21_rel_imp == "Not important at all"] <- 0
clean_data$importance[raw_data$cps21_rel_imp == "Don't know/ Prefer not to answer"] <- NA
table(clean_data$importance)


# Aggregate --------------------------------------------------------------

#### inclure ici entre guillemets les noms des variables qui nous intéressent (exemple: importance, attend, etc.)
variables <- c("religious_bin", "importance")

output <- clean_data |> 
  tidyr::pivot_longer(
    cols = variables,
    names_to = "variable_id",
    values_to = "choice"
  ) |> 
  tidyr::drop_na(choice) |> 
  group_by(subgroup, variable_id, choice) |> 
  summarise(
    n = n()
  ) |> 
  group_by(subgroup, variable_id) |> 
  mutate(value = n / sum(n)) |> 
  select(-n)


# Save -------------------------------------------------------------------

### fill the survey_id variable
survey_id <- "ces2021"

### save it in the warehouse
saveRDS(output, paste0("_SharedFolder_article_religion-magie/Data/religiosity_historic/warehouse/individual/", survey_id, ".rds"))
