# Packages ---------------------------------------------------------------
library(dplyr)

# Load Raw Data -------------------------------------------------------------------

raw_data <- haven::read_sav("_SharedFolder_article_religion-magie/Data/religiosity_historic/lake/provincial_diversity_2014.sav")

# Create Clean Data ------------------------------------------------------

clean_data <- data.frame(
  id = 1:nrow(raw_data)
)

# Clean relevant variables -----------------------------------------------


## Quebec-Canada -----------------------------------------------------------------

### name this variable "subgroup"
#### categories : qc, can

table(raw_data$PROV, useNA = "always")
attributes(raw_data$PROV)
clean_data$subgroup <- NA
clean_data$subgroup[raw_data$PROV == 5] <- "qc"
clean_data$subgroup[raw_data$PROV != 5] <- "can"
table(clean_data$subgroup)

## Religious Bin --------------------------------------------------------

table(raw_data$Q98T, useNA = "always")
attributes(raw_data$Q98T)
clean_data$religious_bin <- NA
clean_data$religious_bin[raw_data$Q98T == 23] <- 0
clean_data$religious_bin[raw_data$Q98T != 23] <- 1
clean_data$religious_bin[raw_data$Q98T == 24] <- NA
table(clean_data$religious_bin)

## Importance of religiosity ----------------------------------------------

## Importance -----------------------------------------------------------------

table(raw_data$Q46B, useNA = "always")
attributes(raw_data$Q46B)
clean_data$importance <- NA
clean_data$importance[raw_data$Q46B == 1] <- 1
clean_data$importance[raw_data$Q46B == 2] <- 0.67
clean_data$importance[raw_data$Q46B == 3] <- 0.33
clean_data$importance[raw_data$Q46B == 4] <- 0
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
survey_id <- "provincial_diversity_2014"

### save it in the warehouse
saveRDS(output, paste0("_SharedFolder_article_religion-magie/Data/religiosity_historic/warehouse/individual/", survey_id, ".rds"))


