# Packages ---------------------------------------------------------------
library(dplyr)

# Load Raw Data -------------------------------------------------------------------

raw_data <- haven::read_sav("_SharedFolder_article_religion-magie/Data/religiosity_historic/lake/general_social_survey_2010.sav")

# Create Clean Data ------------------------------------------------------

clean_data <- data.frame(
  id = 1:nrow(raw_data)
)

# Clean relevant variables -----------------------------------------------

## Quebec-Canada -----------------------------------------------------------------

### name this variable "subgroup"
#### categories : qc, can

table(raw_data$PRV, useNA = "always")
attributes(raw_data$PRV)
clean_data$subgroup <- NA
clean_data$subgroup[raw_data$PRV == 24] <- "qc"
clean_data$subgroup[raw_data$PRV != 24] <- "can"
table(clean_data$subgroup)

## Importance of religiosity ----------------------------------------------

table(raw_data$RLR_Q110, useNA = "always")
attributes(raw_data$RLR_Q110)
clean_data$importance <- NA
clean_data$importance[raw_data$RLR_Q110 == 1] <- 1
clean_data$importance[raw_data$RLR_Q110 == 2] <- 0.67
clean_data$importance[raw_data$RLR_Q110 == 3] <- 0.33
clean_data$importance[raw_data$RLR_Q110 == 4] <- 0
clean_data$importance[raw_data$RLR_Q110 == 8 | raw_data$RLR_Q110 == 9] <- NA
clean_data$importance[is.na(raw_data$RLR_Q110)] <- NA
table(clean_data$importance)

## participate

table(raw_data$RELIGATT, useNA = "always")
attributes(raw_data$RELIGATT)
clean_data$participate <- NA
clean_data$participate[raw_data$RELIGATT == 1] <- 1
clean_data$participate[raw_data$RELIGATT == 2] <- 0.75
clean_data$participate[raw_data$RELIGATT == 3] <- 0.50
clean_data$participate[raw_data$RELIGATT == 4] <- 0.25
clean_data$participate[raw_data$RELIGATT == 5] <- 0
clean_data$participate[raw_data$RELIGATT == 8 | raw_data$RELIGATT == 9] <- NA
clean_data$participate[is.na(raw_data$RELIGATT)] <- NA
table(clean_data$participate)

## Religious BIN

table(raw_data$RELIG6, useNA = "always")
attributes(raw_data$RELIG6)
clean_data$religious_bin <- NA
clean_data$religious_bin[raw_data$RELIG6 != 1] <- 1
clean_data$religious_bin[raw_data$RELIG6 == 1] <- 0
clean_data$religious_bin[raw_data$RELIG6 == 8 | raw_data$RELIG6 == 5 | raw_data$RELIG6 == 9] <- NA
table(clean_data$religious_bin)

# Aggregate --------------------------------------------------------------

#### inclure ici entre guillemets les noms des variables qui nous intéressent (exemple: importance, participate, etc.)
variables <- c("importance", "participate", "religious_bin")

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
survey_id <- "general_social_survey_2010"
output$survey_id <- survey_id

### save it in the warehouse
saveRDS(output, paste0("_SharedFolder_article_religion-magie/Data/religiosity_historic/warehouse/individual/", survey_id, ".rds"))

