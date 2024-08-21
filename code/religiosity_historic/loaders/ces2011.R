# Packages ---------------------------------------------------------------
library(dplyr)

# Load Raw Data -------------------------------------------------------------------

raw_data <- haven::read_sav("_SharedFolder_article_religion-magie/Data/religiosity_historic/lake/ces2011.sav")

# Create Clean Data ------------------------------------------------------

clean_data <- data.frame(
  id = 1:nrow(raw_data)
)

# Clean relevant variables -----------------------------------------------


## Quebec-Canada -----------------------------------------------------------------

### name this variable "subgroup"
#### categories : qc, can

attributes(raw_data$PROVINCE11)
table(raw_data$PROVINCE11, useNA = "always")
clean_data$subgroup <- NA
clean_data$subgroup[raw_data$PROVINCE11 == 24] <- "qc"
clean_data$subgroup[raw_data$PROVINCE11 != 24] <- "can"
table(clean_data$subgroup)

## Religion ---------------------------------------------------------------

attributes(raw_data$CPS11_80)



## Importance of religiosity ----------------------------------------------



# Aggregate --------------------------------------------------------------

#### inclure ici entre guillemets les noms des variables qui nous intéressent (exemple: importance, attend, etc.)
variables <- c()

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
survey_id <- ""

### save it in the warehouse
saveRDS(output, paste0("_SharedFolder_article_religion-magie/Data/religiosity_historic/warehouse/individual/", survey_id, ".rds"))
