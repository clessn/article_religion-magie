# Packages ---------------------------------------------------------------
library(dplyr)

# Load Raw Data -------------------------------------------------------------------

raw_data <- haven::read_sav("_SharedFolder_article_religion-magie/Data/religiosity_historic/lake/general_social_survey_2014.sav")


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
clean_data$subgroup[raw_data$PRV == "24"] <- "qc"
clean_data$subgroup[raw_data$PRV != "24"] <- "can"
clean_data$subgroup[raw_data$PRV %in% c("96", "97", "98", "99")] <- NA
clean_data$subgroup[is.na(raw_data$PRV)] <- NA
table(clean_data$subgroup)

## Importance ----------------------------------------------

table(raw_data$RLR_110, useNA = "always")
attributes(raw_data$RLR_110)
clean_data$importance <- NA
clean_data$importance[raw_data$RLR_110 == 1] <- 1
clean_data$importance[raw_data$RLR_110 == 2] <- 0.67
clean_data$importance[raw_data$RLR_110 == 3] <- 0.33
clean_data$importance[raw_data$RLR_110 == 4] <- 0
table(clean_data$importance)

## Frequence ----------------------------------------------------------

table(raw_data$REE_03, useNA = "always")
attributes(raw_data$REE_03)
clean_data$participation <- NA
clean_data$participation[raw_data$REE_03 == 1] <- 1
clean_data$participation[raw_data$REE_03 == 2] <- 0.8
clean_data$participation[raw_data$REE_03 == 3] <- 0.6
clean_data$participation[raw_data$REE_03 == 4] <- 0.4
clean_data$participation[raw_data$REE_03 == 5] <- 0.2
clean_data$participation[raw_data$REE_03 == 6] <- 0
table(clean_data$participation)

# Aggregate --------------------------------------------------------------

#### inclure ici entre guillemets les noms des variables qui nous intéressent (exemple: importance, attend, etc.)
variables <- c("importance", "participation")

output <- clean_data |> 
  tidyr::pivot_longer(
    cols = variables,
    names_to = "variable_id",
    values_to = "choice"
  ) |> 
  tidyr::drop_na(choice) %>% 
  group_by(subgroup, variable_id, choice) |> 
  summarise(
    n = n()
  ) |> 
  group_by(subgroup, variable_id) |> 
  mutate(value = n / sum(n)) |> 
  select(-n)


# Save -------------------------------------------------------------------

### fill the survey_id variable
survey_id <- "general_social_survey_2014"

### save it in the warehouse
saveRDS(output, paste0("_SharedFolder_article_religion-magie/Data/religiosity_historic/warehouse/individual/", survey_id, ".rds"))
