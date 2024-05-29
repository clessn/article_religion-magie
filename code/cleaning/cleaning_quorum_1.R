library(dplyr)

data <- haven::read_sav("_SharedFolder_article_religion-magie/Data/quorum_1/ULAQUORUM.Sav", encoding = "latin1")

codebook <- sondr::sav_to_codebook(data)

data_raw <- data %>% 
  select(PROV, SEXE, YOB, AGE, LANGU, Q47, Q48, Q51, Q52, Q54, Q10B_A1, Q4, Q61, Q62, Q49, Q2B_5, Q12_4, Q17_3, Q17_7, Q23, Q35, Q37_2)

data_clean <- data.frame(id = 1:nrow(data_raw))

## PROV ---------------------------- province ----------------------------------

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
sum(table(data_clean$ses_province))

## SEXE ---------------------------- sexe --------------------------------------

attributes(data_raw$SEXE)
table(data_raw$SEXE)
data_clean$ses_female <- NA
data_clean$ses_female[data_raw$SEXE == 2] <- 1
data_clean$ses_female[data_raw$SEXE != 2] <- 0
table(data_clean$ses_female)
sum(table(data_clean$ses_female))

## YOB ------------------------------- yob -------------------------------------

attributes(data_raw$YOB)
table(data_raw$YOB)
data_clean$ses_age <- NA
year_map <- setNames(as.character(1900:2002), 1:103)
data_raw$yob_label <- year_map[as.character(data_raw$YOB)]
data_raw$yob_label <- factor(data_raw$yob_label)
data_clean$ses_age <- as.numeric(data_raw$yob_label)
table(data_clean$ses_age)
sum(table(data_clean$ses_age))

## Age group ------------------------ Age group --------------------------------

data_clean$ses_age_group <- NA
data_clean$ses_age_group[data_raw$YOB > 2006] <- "under_18"
data_clean$ses_age_group[as.numeric(data_raw$yob_label) %in% 18:24] <- "18_24"
data_clean$ses_age_group[as.numeric(data_raw$yob_label) %in% 25:34] <- "25_34"
data_clean$ses_age_group[as.numeric(data_raw$yob_label) %in% 35:44] <- "35_44"
data_clean$ses_age_group[as.numeric(data_raw$yob_label) %in% 45:54] <- "45_54"
data_clean$ses_age_group[as.numeric(data_raw$yob_label) %in% 55:59] <- "55_59"
data_clean$ses_age_group[as.numeric(data_raw$yob_label) %in% 60:64] <- "60_64"
data_clean$ses_age_group[as.numeric(data_raw$yob_label) %in% 65:74] <- "65_74"
data_clean$ses_age_group[as.numeric(data_raw$yob_label) %in% 75:84] <- "75_84"
data_clean$ses_age_group[as.numeric(data_raw$yob_label) %in% 85:125] <- "over_85"
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
sum(table(data_clean$ses_age_group))

## LANGU ---------------------------- Language ---------------------------------

attributes(data_raw$LANGU)
table(data_raw$LANGU)
data_clean$ses_language <- NA
data_clean$ses_language[data_raw$LANGU == 1] <- "fr"
data_clean$ses_language[data_raw$LANGU == 2] <- "en"
data_clean$ses_language[data_raw$LANGU == 3] <- "other"
table(data_clean$ses_language)
sum(table(data_clean$ses_language))

## ---------------------------- marital status ---------------------------------

# Q47

attributes(data_raw$Q47)
table(data_raw$Q47)
data_clean$ses_marital_status <- NA
data_clean$ses_marital_status[data_raw$Q47 == 1] <- "married"
data_clean$ses_marital_status[data_raw$Q47 == 2] <- "living_with_partner"
data_clean$ses_marital_status[data_raw$Q47 == 3] <- "single"
data_clean$ses_marital_status[data_raw$Q47 == 4] <- "divorced"
data_clean$ses_marital_status[data_raw$Q47 == 5] <- "separated"
data_clean$ses_marital_status[data_raw$Q47 == 6] <- "widowed"
data_clean$ses_marital_status[data_raw$Q47 == 7] <- "never_married"
data_clean$ses_marital_status[data_raw$Q47 == 8] <- "dk"
data_clean$ses_marital_status <- factor(data_clean$ses_marital_status, levels = c("married", "living_with_partner", "single", "divorced", "separated", "widowed", "never_married", "dk"))
table(data_clean$ses_marital_status)
sum(table(data_clean$ses_marital_status))

## ------------------------ born in canada -------------------------------------

# Q48

attributes(data_raw$Q48)
table(data_raw$Q48)
data_clean$ses_born_canada <- NA
data_clean$ses_born_canada[data_raw$Q48 == 1] <- 1
data_clean$ses_born_canada[data_raw$Q48 != 1] <- 0
table(data_clean$ses_born_canada)
sum(table(data_clean$ses_born_canada))

## ---------------------------- education --------------------------------------

# Q51

attributes(data_raw$Q51)
table(data_raw$Q51)
data_clean$ses_education <- NA
data_clean$ses_education[data_raw$Q51 == 1] <- "before_high_school"
data_clean$ses_education[data_raw$Q51 == 2] <- "before_high_school"
data_clean$ses_education[data_raw$Q51 == 3] <- "before_high_school"
data_clean$ses_education[data_raw$Q51 == 4] <- "before_high_school"
data_clean$ses_education[data_raw$Q51 == 5] <- "high_school"
data_clean$ses_education[data_raw$Q51 == 6] <- "high_school"
data_clean$ses_education[data_raw$Q51 == 7] <- "cegep_college"
data_clean$ses_education[data_raw$Q51 == 8] <- "cegep_college"
data_clean$ses_education[data_raw$Q51 == 9] <- "undergraduate"
data_clean$ses_education[data_raw$Q51 == 10] <- "graduate"
data_clean$ses_education[data_raw$Q51 == 11] <- "graduate"
data_clean$ses_education <- factor(data_clean$ses_education, levels = c("before_high_school", "high_school", "cegep_college", "undergraduate", "graduate"))
table(data_clean$ses_education)
sum(table(data_clean$ses_education))

## ---------------------------- sexual orientation -----------------------------

# Q52

attributes(data_raw$Q52)
table(data_raw$Q52)
data_clean$ses_sexual_orientation <- NA
data_clean$ses_sexual_orientation[data_raw$Q52 == 1] <- "heterosexual"
data_clean$ses_sexual_orientation[data_raw$Q52 == 2] <- "bisexual"
data_clean$ses_sexual_orientation[data_raw$Q52 == 3] <- "homosexual"
data_clean$ses_sexual_orientation[data_raw$Q52 == 4 | data_raw$Q52 == 5] <- "other"
data_clean$ses_sexual_orientation <- factor(data_clean$ses_sexual_orientation, levels = c("heterosexual", "bisexual", "homosexual", "other")) 
table(data_clean$ses_sexual_orientation)
sum(table(data_clean$ses_sexual_orientation))

## -------------------------------- occupation ---------------------------------

# Q54

attributes(data_raw$Q54)
table(data_raw$Q54)
sum(table(data_raw$Q54))
data_clean$ses_occupation <- NA
data_clean$ses_occupation[data_raw$Q54 == 1] <- "employed"
data_clean$ses_occupation[data_raw$Q54 == 2] <- "employed"
data_clean$ses_occupation[data_raw$Q54 == 3] <- "employed"
data_clean$ses_occupation[data_raw$Q54 == 4] <- "employed"
data_clean$ses_occupation[data_raw$Q54 == 5] <- "employed"
data_clean$ses_occupation[data_raw$Q54 == 6] <- "employed"
data_clean$ses_occupation[data_raw$Q54 == 7] <- "unemployed"
data_clean$ses_occupation[data_raw$Q54 == 8] <- "retired"
data_clean$ses_occupation[data_raw$Q54 == 9] <- "student"
data_clean$ses_occupation[data_raw$Q54 == 10] <- "employed"
data_clean$ses_occupation <- factor(data_clean$ses_occupation, levels = c("employed", "unemployed", "retired", "student"))
table(data_clean$ses_occupation)
sum(table(data_clean$ses_occupation))

## ---------------------------- how many immigrants ----------------------------

# Q4

attributes(data_raw$Q4)
table(data_raw$Q4)
data_clean$how_many_immigrants <- NA
data_clean$how_many_immigrants <- (data_raw$Q4 - 1) / 4
table(data_clean$how_many_immigrants)
sum(table(data_clean$how_many_immigrants))

## --------------------------- attachment to church ----------------------------

# Q62

attributes(data_raw$Q62)
attributes(data_raw$Q61)
table(data_raw$Q62)
sum(table(data_raw$Q62))
data_clean$religion_attached_to_church_all <- NA
data_clean$religion_attached_to_church_all <- (data_raw$Q62 - 1) / 4
data_clean$religion_attached_to_church_all[data_raw$Q61 == 2] <- 0
table(data_clean$religion_attached_to_church_all)
sum(table(data_clean$religion_attached_to_church_all))

## --------------------------- attachment to church ----------------------------

data_clean$religion_attached_to_church_religious <- NA
data_clean$religion_attached_to_church_religious <- (data_raw$Q62 - 1) / 4
table(data_clean$religion_attached_to_church_religious)
sum(table(data_clean$religion_attached_to_church_religious))

## --------------------------- Religious bin -----------------------------------

data_clean$religion_bin <- NA
data_clean$religion_bin[data_raw$Q61 == 2] <- 0
data_clean$religion_bin[data_raw$Q61 !=2] <- 1
table(data_clean$religion_bin)
sum(table(data_clean$religion_bin))

## --------------------------- ethnicity ---------------------------------------

# Q49

attributes(data_raw$Q49)
table(data_raw$Q49)
sum(table(data_raw$Q49))
data_clean$ses_ethnicity <- NA
data_clean$ses_ethnicity[data_raw$Q49 == 1] <- "white"
data_clean$ses_ethnicity[data_raw$Q49 == 2] <- "black"
data_clean$ses_ethnicity[data_raw$Q49 == 3] <- "native"
data_clean$ses_ethnicity[data_raw$Q49 == 4] <- "asian"
data_clean$ses_ethnicity[data_raw$Q49 == 5] <- "hispanic"
data_clean$ses_ethnicity[data_raw$Q49 == 6] <- "arab"
data_clean$ses_ethnicity <- factor(data_clean$ses_ethnicity)
table(data_clean$ses_ethnicity)
sum(table(data_clean$ses_ethnicity))

## ---------------- Covid limiting freedom is necessary ------------------------

# Q2B_5

attributes(data_raw$Q2B_5)
table(data_raw$Q2B_5)
data_clean$covid_limiting_freedom_is_necessary <- NA
data_clean$covid_limiting_freedom_is_necessary <- (data_raw$Q2B_5 - 1) / 4
table(data_clean$covid_limiting_freedom_is_necessary)
sum(table(data_clean$covid_limiting_freedom_is_necessary))

## ------------------ Covid only dangerous for vulnerable people ---------------

# Q12_4

attributes(data_raw$Q12_4)
table(data_raw$Q12_4)
data_clean$covid_only_vulnerable_people <- NA
data_clean$covid_only_vulnerable_people <- (data_raw$Q12_4 - 1) / 4
data_clean$covid_only_vulnerable_people <- sondr::finverser(data_clean$covid_only_vulnerable_people)
table(data_clean$covid_only_vulnerable_people)
sum(table(data_clean$covid_only_vulnerable_people))

## -------------------- Not afraid of dying of covid ---------------------------

# Q17_3

attributes(data_raw$Q17_3)
table(data_raw$Q17_3)
data_clean$covid_not_afraid_of_dying <- NA
data_clean$covid_not_afraid_of_dying <- (data_raw$Q17_3 - 1) / 4
data_clean$covid_not_afraid_of_dying <- sondr::finverser(data_clean$covid_not_afraid_of_dying)
table(data_clean$covid_not_afraid_of_dying)
sum(table(data_clean$covid_not_afraid_of_dying))

## ---------------------- Afraid of dying of covid -----------------------------

data_clean$afraid_of_dying <- NA
data_clean$afraid_of_dying <- sondr::clean_likert_numeric_vector(data_raw$Q17_3)
table(data_clean$afraid_of_dying)

## ------------------------ Covid feeling --------------------------------------

# Q23

attributes(data_raw$Q23)
table(data_raw$Q23)
data_clean$covid_feeling <- NA
data_clean$covid_feeling <- data_raw$Q23 / 10
table(data_clean$covid_feeling)
sum(table(data_clean$covid_feeling))

## ----------------------- Covid pandemic more serious -------------------------

# Q37_2

attributes(data_raw$Q37_2)
table(data_raw$Q37_2)
data_clean$covid_pandemic_more_serious <- NA
data_clean$covid_pandemic_more_serious <- (data_raw$Q37_2 - 1) / 4
table(data_clean$covid_pandemic_more_serious)
sum(table(data_clean$covid_pandemic_more_serious))

# covid ----------------------------- covid ------------------------------------

data_clean$during_covid <- 1

# survey ----------------- which survey ----------------------------------------

data_clean$survey_name <- "quorum_1"

# Save data

saveRDS(data_clean, "_SharedFolder_article_religion-magie/Data/data_clean/data_quorum_1.rds")






































