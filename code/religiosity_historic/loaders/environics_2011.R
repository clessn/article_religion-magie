# Packages ---------------------------------------------------------------
library(dplyr)

# Load Raw Data -------------------------------------------------------------------

raw_data <- haven::read_sav("_SharedFolder_article_religion-magie/Data/religiosity_historic/lake/environics_2011.sav")


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
clean_data$subgroup[raw_data$REGION == 5] <- "qc"
clean_data$subgroup[raw_data$REGION != 5] <- "can"
table(clean_data$subgroup)

## Religious Bin --------------------------------------------------------

table(raw_data$Q52_F1)
attributes(raw_data$Q52_F1)
clean_data$religious_bin <- NA
clean_data$religious_bin[raw_data$Q52_F1 != 9] <- 1
clean_data$religious_bin[raw_data$Q52_F1 == 9] <- 0
clean_data$religious_bin[raw_data$Q52_F1 == 99] <- NA
table(clean_data$religious_bin)

## Importance of religiosity ----------------------------------------------

## Frequence --------------------------------------------------------------

table(raw_data$Q53, useNA = "always")
attributes(raw_data$Q53)
clean_data$participation <- NA
clean_data$participation[raw_data$Q53 == 1] <- 1
clean_data$participation[raw_data$Q53 == 2] <- 0.8
clean_data$participation[raw_data$Q53 == 3] <- 0.6
clean_data$participation[raw_data$Q53 == 4] <- 0.4
clean_data$participation[raw_data$Q53 == 5] <- 0.2
clean_data$participation[raw_data$Q53 == 6] <- 0
table(clean_data$participation)

## Importance -----------------------------------------------------------------

table(raw_data$Q54, useNA = "always")
attributes(raw_data$Q54)
clean_data$importance <- NA
clean_data$importance[raw_data$Q54 == 1] <- 1
clean_data$importance[raw_data$Q54 == 2] <- 0.67
clean_data$importance[raw_data$Q54 == 3] <- 0.33
clean_data$importance[raw_data$Q54 == 4] <- 0
table(clean_data$importance)


# Aggregate --------------------------------------------------------------

#### inclure ici entre guillemets les noms des variables qui nous intéressent (exemple: importance, attend, etc.)
variables <- c("religious_bin", "participation", "importance")

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
survey_id <- "environics_2011"

### save it in the warehouse
saveRDS(output, paste0("_SharedFolder_article_religion-magie/Data/religiosity_historic/warehouse/individual/", survey_id, ".rds"))
