# Packages ---------------------------------------------------------------
library(dplyr)

# Load Raw Data -------------------------------------------------------------------

raw_data <- haven::read_sav("_SharedFolder_article_religion-magie/Data/religiosity_historic/lake/cora-cpep.sav")

# Create Clean Data ------------------------------------------------------

clean_data <- data.frame(
  id = 1:nrow(raw_data)
)

# Clean relevant variables -----------------------------------------------

## Quebec-Canada -----------------------------------------------------------------

### name this variable "subgroup"
#### categories : qc, can

table(raw_data$Province2, useNA = "always")
attributes(raw_data$Province2)
clean_data$subgroup <- NA
clean_data$subgroup[raw_data$Province2 == 7] <- "qc"
clean_data$subgroup[raw_data$Province2 != 7] <- "can"
clean_data$subgroup[is.na(raw_data$Province2)] <- NA
table(clean_data$subgroup)

## Importance of religiosity ----------------------------------------------

table(raw_data$q79, useNA = "always")
attributes(raw_data$q79)
clean_data$importance <- NA
clean_data$importance[raw_data$q79 == 1] <- 1
clean_data$importance[raw_data$q79 == 2] <- 0.67
clean_data$importance[raw_data$q79 == 3] <- 0.33
clean_data$importance[raw_data$q79 == 4] <- 0
table(clean_data$importance)

## Religious bin ---------------------------------------------------------

table(raw_data$q78, useNA = "always")
attributes(raw_data$q78)
clean_data$religious_bin <- NA
clean_data$religious_bin[raw_data$q78 == 9] <- 0
clean_data$religious_bin[raw_data$q78 != 9] <- 1
clean_data$religious_bin[raw_data$q78 == 10] <- NA
table(clean_data$religious_bin, useNA = "always")

# Aggregate --------------------------------------------------------------

#### inclure ici entre guillemets les noms des variables qui nous intéressent (exemple: importance, attend, etc.)
variables <- c("importance", "religious_bin")

output <- clean_data |> 
  tidyr::pivot_longer(
    cols = variables,
    names_to = "variable_id",
    values_to = "choice"
  ) |> 
  group_by(subgroup, variable_id, choice) |> 
  summarise(
    n = n()
  ) |> 
  group_by(subgroup, variable_id) |> 
  mutate(value = n / sum(n)) |> 
  select(-n)

# Save -------------------------------------------------------------------

### fill the survey_id variable
survey_id <- "cora-cpep"
output$survey_id <- survey_id

### save it in the warehouse
saveRDS(output, paste0("_SharedFolder_article_religion-magie/Data/religiosity_historic/warehouse/individual/", survey_id, ".rds"))
