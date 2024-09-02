# Packages ---------------------------------------------------------------
library(dplyr)

# Load Raw Data -------------------------------------------------------------------

raw_data <- read.csv("_SharedFolder_article_religion-magie/Data/religiosity_historic/lake/ces2019.csv")

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

table(raw_data$CPS11_80)
attributes(raw_data$CPS11_80)
clean_data$religious_bin <- NA
clean_data$religious_bin[raw_data$CPS11_80 != 0] <- 1
clean_data$religious_bin[raw_data$CPS11_80 == 0 | raw_data$CPS11_80 == 97] <- 0
clean_data$religious_bin[raw_data$CPS11_80 == 99 | raw_data$CPS11_80 == 98] <- NA
table(clean_data$religious_bin)

## Importance of religiosity ----------------------------------------------

table(raw_data$CPS11_82)
attributes(raw_data$CPS11_82)
clean_data$importance <- NA
clean_data$importance[raw_data$CPS11_82 == 1] <- 1
clean_data$importance[raw_data$CPS11_82 == 3] <- 0.67
clean_data$importance[raw_data$CPS11_82 == 5] <- 0.33
clean_data$importance[raw_data$CPS11_82 == 7] <- 0
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
survey_id <- "ces2011"

### save it in the warehouse
saveRDS(output, paste0("_SharedFolder_article_religion-magie/Data/religiosity_historic/warehouse/individual/", survey_id, ".rds"))
