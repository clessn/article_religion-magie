data_quorum_1 <- readRDS("_SharedFolder_article_religion-magie/Data/data_clean/data_quorum_1.RDS")
data_2014 <- readRDS("_SharedFolder_article_religion-magie/Data/data_clean/data_2014.RDS")

source()

data_religion_magie <- dplyr::bind_rows(data_quorum_1, data_2014)

saveRDS(data_religion_magie, "_SharedFolder_article_religion-magie/Data/data_clean/data_religion_magie.RDS")

