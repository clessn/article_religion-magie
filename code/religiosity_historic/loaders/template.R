# Packages ---------------------------------------------------------------
library(dplyr)

# Load Raw Data -------------------------------------------------------------------

raw_data


# Create Clean Data ------------------------------------------------------

clean_data <- data.frame(
  id = 1:nrow(raw_data)
)

# Clean relevant variables -----------------------------------------------


## Quebec-Canada -----------------------------------------------------------------

### name this variable "subgroup"
#### categories : qc, can


## Importance of religiosity ----------------------------------------------




# Aggregate --------------------------------------------------------------

#### inclure ici entre guillemets les noms des variables qui nous intéressent (exemple: importance, attend, etc.)
variables <- c()

output <- clean_data |> 
  tidyr::pivot_longer(
    cols = variables,
    names_to = "variable",
    values_to = "value"
  ) |> 
  group_by(subgroup, variable, value) |> 
  summarise(
    n = n()
  ) |> 
  group_by(subgroup, variable) |> 
  mutate(prop = n / sum(n))


# Save -------------------------------------------------------------------

### fill the survey_id variable
survey_id <- ""

### save it in the warehouse
saveRDS(output, paste0("_SharedFolder_article_religion-magie/Data/religiosity_historic/warehouse/individual/", survey_id, ".rds"))
