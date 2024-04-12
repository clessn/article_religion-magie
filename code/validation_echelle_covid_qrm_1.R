library(dplyr)

data <- haven::read_sav("_SharedFolder_article_religion-magie/Data/quorum_1/ULAQUORUM.Sav", encoding = "latin1")

codebook <- sondr::sav_to_codebook(data)
sondr::codebook_to_catalog(codebook, "_SharedFolder_article_religion-magie/Data/quorum_1/codebook_qrm_1.md", "Quorum 1")

attributes(data$Q2B_5)$label

attributes(data$Q12_4)$label
data$Q12_4_inv <- sondr::finverser(data$Q12_4)

attributes(data$Q17_3)$label
data$Q17_3_inv <- sondr::finverser(data$Q17_3)

attributes(data$Q17_7)$label
attributes(data$Q23)$label
attributes(data$Q35)$label
attributes(data$Q37_2)$label


data_facanal <- data %>%
    select(Q2B_5, Q12_4_inv, Q17_3_inv, Q17_7, Q23, Q35, Q37_2) %>% 
    na.omit()

sondr::topdown_fa(data_facanal)
