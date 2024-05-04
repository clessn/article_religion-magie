library(dplyr)

data <- haven::read_sav("_SharedFolder_article_religion-magie/Data/quorum_1/ULAQUORUM.Sav", encoding = "latin1")

codebook <- sondr::sav_to_codebook(data)

data_raw <- data %>% 
  select(PROV, REG_ADM, SEXE, YOB, AGE, LANGU, Q47, Q48, Q51, Q52, Q54, Q10B_A1, Q4, Q61, Q62, Q49, Q2B_5, Q12_4, Q17_3, Q17_7, Q23, Q35, Q37_2)

data_clean <- data.frame(id = 1:nrow(data_raw))

# PROV

attributes(data_raw$PROV)
table(data_raw$PROV)
data_clean$ses_province <- NA
data_clean$ses_province[data_raw$PROV == 1] <- "ab"
data_clean$ses_province[data_raw$PROV == 2] <- "bc"
data_clean$ses_province[data_raw$PROV == 3] <- "mb"
data_clean$ses_province[data_raw$PROV == 4] <- "nb"
data_clean$ses_province[data_raw$PROV == 5] <- "nl"
data_clean$ses_province[data_raw$PROV == 6] <- "ns"
data_clean$ses_province[data_raw$PROV == 7] <- "nt"
data_clean$ses_province[data_raw$PROV == 8] <- "nu"
data_clean$ses_province[data_raw$PROV == 9] <- "on"
data_clean$ses_province[data_raw$PROV == 10] <- "pe"
data_clean$ses_province[data_raw$PROV == 11] <- "qc"
data_clean$ses_province[data_raw$PROV == 12] <- "sk"
data_clean$ses_province[data_raw$PROV == 13] <- "yt"
table(data_clean$ses_province)

# REG_ADM

attributes(data_raw$REG_ADM)
table(data_raw$REG_ADM)
data_clean$ses_postal_code <- NA
data_clean$ses_postal_code <- data_raw$REG_ADM
table(data_clean$ses_postal_code)

# SEXE

attributes(data_raw$SEXE)
table(data_raw$SEXE)
data_clean$ses_female <- NA
data_clean$ses_female[data_raw$SEXE == 2] <- 1
data_clean$ses_female[data_raw$SEXE != 2] <- 0
table(data_clean$ses_female)

# YOB

attributes(data_raw$YOB)
table(data_raw$YOB)
data_clean$ses_age <- NA
data_clean$ses_age <- 2024 - data_raw$YOB
table(data_clean$ses_age)

## Age group

data_clean$ses_age_group <- NA
data_clean$ses_age_group[data_raw$YOB > 2006] <- "under_18"
data_clean$ses_age_group[(2024 - data_raw$YOB) %in% 18:24] <- "18_24"
data_clean$ses_age_group[(2024 - data_raw$YOB) %in% 25:34] <- "25_34"
data_clean$ses_age_group[(2024 - data_raw$YOB) %in% 35:44] <- "35_44"
data_clean$ses_age_group[(2024 - data_raw$YOB) %in% 45:54] <- "45_54"
data_clean$ses_age_group[(2024 - data_raw$YOB) %in% 55:59] <- "55_59"
data_clean$ses_age_group[(2024 - data_raw$YOB) %in% 60:64] <- "60_64"
data_clean$ses_age_group[(2024 - data_raw$YOB) %in% 65:74] <- "65_74"
data_clean$ses_age_group[(2024 - data_raw$YOB) %in% 75:84] <- "75_84"
data_clean$ses_age_group[(2024 - data_raw$YOB) %in% 85:125] <- "over_85"
data_clean$ses_age_group <- factor(data_clean$ses_age_group, levels = c("under_18",
                                                                        "18_24",
                                                                        "25_34",
                                                                        "35_44",
                                                                        "45_54",
                                                                        "55_59",
                                                                        "60_64",
                                                                        "65_74",
                                                                        "75_84",
                                                                        "over_85"))
table(data_clean$ses_age_group)