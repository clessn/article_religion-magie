# Packages ---------------------------------------------------------------
library(dplyr)

# Load Raw Data -------------------------------------------------------------------

raw_data <- haven::read_sav("_SharedFolder_article_religion-magie/Data/religiosity_historic/lake/democracy_checkup_2022.sav", encoding = "latin1")

# Create Clean Data ------------------------------------------------------

clean_data <- data.frame(
  id = 1:nrow(raw_data)
)

# Clean relevant variables -----------------------------------------------

## Quebec-Canada -----------------------------------------------------------------

### name this variable "subgroup"
#### categories : qc, can

attributes(raw_data$dc22_province)
table(raw_data$dc22_province, useNA = "always")
clean_data$subgroup <- NA
clean_data$subgroup[raw_data$dc22_province == 11] <- "qc"
clean_data$subgroup[raw_data$dc22_province != 11] <- "can"
table(clean_data$subgroup)

## Importance of religiosity ----------------------------------------------

attributes(raw_data$dc22_religion_import)
table(raw_data$dc22_religion_import, useNA = "always")
clean_data$importance <- NA
clean_data$importance[raw_data$dc22_religion_import == 1] <- 1
clean_data$importance[raw_data$dc22_religion_import == 2] <- 0.67
clean_data$importance[raw_data$dc22_religion_import == 3] <- 0.33
clean_data$importance[raw_data$dc22_religion_import == 4] <- 0
table(clean_data$importance)


# Aggregate --------------------------------------------------------------

#### inclure ici entre guillemets les noms des variables qui nous intéressent (exemple: importance, attend, etc.)
variables <- c("importance")

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
survey_id <- "democracy_checkup_2022"

### save it in the warehouse
saveRDS(output, paste0("_SharedFolder_article_religion-magie/Data/religiosity_historic/warehouse/individual/", survey_id, ".rds"))
