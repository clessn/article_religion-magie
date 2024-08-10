# Packages ---------------------------------------------------------------
library(dplyr)

# Load Raw Data -------------------------------------------------------------------

raw_data <- haven::read_sav("_SharedFolder_article_religion-magie/Data/religiosity_historic/lake/general_social_survey_2011.sav")

# Create Clean Data ------------------------------------------------------

clean_data <- data.frame(
  id = 1:nrow(raw_data)
)

# Clean relevant variables -----------------------------------------------

## Quebec-Canada -----------------------------------------------------------------

### name this variable "subgroup"
#### categories : qc, can

table(raw_data$REGION, useNA = "always")
attributes(raw_data$REGION)
clean_data$subgroup <- NA
clean_data$subgroup[raw_data$REGION == 2] <- "qc"
clean_data$subgroup[raw_data$REGION != 2] <- "can"
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

## Attend

table(raw_data$RELIGATT, useNA = "always")
attributes(raw_data$RELIGATT)
clean_data$attend <- NA
clean_data$attend[raw_data$RELIGATT == 1] <- 1
clean_data$attend[raw_data$RELIGATT == 2] <- 0.75
clean_data$attend[raw_data$RELIGATT == 3] <- 0.50
clean_data$attend[raw_data$RELIGATT == 4] <- 0.25
clean_data$attend[raw_data$RELIGATT == 5] <- 0
clean_data$attend[raw_data$RELIGATT == 8 | raw_data$RELIGATT == 9] <- NA
clean_data$attend[is.na(raw_data$RELIGATT)] <- NA
table(clean_data$attend)

## Religious BIN

table(raw_data$RELIG6C, useNA = "always")
attributes(raw_data$RELIG6C)
clean_data$religious_bin <- NA
clean_data$religious_bin[raw_data$RELIG6C != 1] <- 1
clean_data$religious_bin[raw_data$RELIG6C == 1] <- 0
clean_data$religious_bin[raw_data$RELIG6C == 8 | raw_data$RELIG6C == 9] <- NA
clean_data$religious_bin[is.na(raw_data$RELIG6C)] <- NA
table(clean_data$religious_bin)

# Aggregate --------------------------------------------------------------

#### inclure ici entre guillemets les noms des variables qui nous intéressent (exemple: importance, attend, etc.)
variables <- c("importance", "attend", "religious_bin")

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
survey_id <- "general_social_survey_2011"

### save it in the warehouse
saveRDS(output, paste0("_SharedFolder_article_religion-magie/Data/religiosity_historic/warehouse/individual/", survey_id, ".rds"))
