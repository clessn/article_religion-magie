# Packages ---------------------------------------------------------------
library(dplyr)

# Load Raw Data -------------------------------------------------------------------

raw_data <- read.csv("_SharedFolder_article_religion-magie/Data/religiosity_historic/lake/ces2015.csv")

# Create Clean Data ------------------------------------------------------

clean_data <- data.frame(
  id = 1:nrow(raw_data)
)

# Clean relevant variables -----------------------------------------------


## Quebec-Canada -----------------------------------------------------------------

### name this variable "subgroup"
#### categories : qc, can

table(raw_data$province, useNA = "always")
clean_data$subgroup <- NA
clean_data$subgroup[raw_data$province == 24] <- "qc"
clean_data$subgroup[raw_data$province != 24] <- "can"
clean_data$subgroup[raw_data$province == 1000] <- NA
table(clean_data$subgroup, useNA = "always")

## Religion ---------------------------------------------------------------

table(raw_data$religion, useNA = "always")
clean_data$religious_bin <- NA
clean_data$religious_bin[raw_data$religion == 0] <- 0
clean_data$religious_bin[raw_data$religion != 0] <- 1
clean_data$religious_bin[raw_data$religion == 97 | raw_data$religion == 98 | raw_data$religion == 99 | raw_data$religion == 1000] <- NA
table(clean_data$religious_bin, useNA = "always")

## Importance of religiosity ----------------------------------------------

table(raw_data$relig_imp)
clean_data$importance <- NA
clean_data$importance[raw_data$relig_imp==1] <- 1
clean_data$importance[raw_data$relig_imp==3] <- 0.66
clean_data$importance[raw_data$relig_imp==5] <- 0.33
clean_data$importance[raw_data$relig_imp==7] <- 0
clean_data$importance[raw_data$relig_imp==98 | raw_data$relig_imp==99] <- NA
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
survey_id <- "ces2015"

### save it in the warehouse
saveRDS(output, paste0("_SharedFolder_article_religion-magie/Data/religiosity_historic/warehouse/individual/", survey_id, ".rds"))
