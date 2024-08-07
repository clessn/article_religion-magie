# Packages ---------------------------------------------------------------
library(dplyr)

# Load Raw Data -------------------------------------------------------------------

raw_data <- haven::read_sav("_SharedFolder_article_religion-magie/Data/religiosity_historic/lake/democracy_checkup_2019.sav")

# Create Clean Data ------------------------------------------------------

clean_data <- data.frame(
  id = 1:nrow(raw_data)
)

# Clean relevant variables -----------------------------------------------

## Quebec-Canada -----------------------------------------------------------------

### name this variable "subgroup"
#### categories : qc, can

attributes(raw_data$province)
table(raw_data$province)
clean_data$subgroup <- NA
clean_data$subgroup[raw_data$province == 24] <- "qc"
clean_data$subgroup[raw_data$province %in% c(14:23, 25:26)] <- "can"
table(clean_data$subgroup)

## Importance of religiosity ----------------------------------------------

attributes(raw_data$religion_import)


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
