ource("code/cleaning/cleaning_quorum_1.R")
source("code/cleaning/cleaning_2014.R")

source("code/echelles/echelle_covid.R")

data_quorum_1 <- readRDS("_SharedFolder_article_religion-magie/Data/data_clean/data_quorum_1.rds")
data_2014 <- readRDS("_SharedFolder_article_religion-magie/Data/data_clean/data_2014.rds")

data_religion_magie <- dplyr::bind_rows(data_quorum_1, data_2014)

saveRDS(data_religion_magie, "_SharedFolder_article_religion-magie/Data/data_clean/data_religion_magie.rds")

test <- readRDS("_SharedFolder_article_religion-magie/Data/data_clean/data_religion_magie.rds")
