
library(dplyr)

data <- haven::read_sav("_SharedFolder_article_religion-magie/Data/quorum_1/ULAQUORUM.Sav", encoding = "latin1")

codebook <- sondr::sav_to_codebook(data)

data <- data %>% 
  select(PROV, CP, SEXE, YOB, AGE, LANGU, Q47, Q48, Q51, Q52, Q54, Q10B_A1, Q4, Q61, Q62, Q49, Q2B_5, Q12_4_inv, Q17_3_inv, Q17_7, Q23, Q35, Q37_2)
