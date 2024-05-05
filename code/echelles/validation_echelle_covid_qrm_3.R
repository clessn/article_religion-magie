library(dplyr)

data <- haven::read_sav("_SharedFolder_article_religion-magie/Data/quorum_3/ULA003.Sav", encoding = "latin1")

codebook <- sondr::sav_to_codebook(data)
sondr::codebook_to_catalog(codebook, "codebook_quorum2.md", "Quorum 2")

attributes(data$Q2_A3)$label
attributes(data$Q5_A3)$label
attributes(data$Q5_A4)$label

data_facanal <- data %>%
    select(Q2_A3, Q5_A3, Q5_A4) %>% 
    na.omit()

sondr::topdown_fa(data_facanal)
