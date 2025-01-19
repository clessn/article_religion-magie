library(dplyr)

data <- haven::read_sav("_SharedFolder_article_religion-magie/Data/MJU001 - PONDÉRÉ N=2000.Sav")

codebook <- sondr::sav_to_codebook(data)

data_raw <- data %>% 
  select(PROV, SEXE, YOB, AGE, LANGU, Q2_5, Q23, Q28, Q29, Q32, Q33, Q34, Q48, Q36, Q39)

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

## SEXE ---------------------------- sexe --------------------------------------

attributes(data_raw$SEXE)
table(data_raw$SEXE)
data_clean$ses_female <- NA
data_clean$ses_female[data_raw$SEXE == 2] <- 1
data_clean$ses_female[data_raw$SEXE != 2] <- 0
table(data_clean$ses_female)

## YOB ------------------------------- yob -------------------------------------

attributes(data_raw$YOB)
table(data_raw$YOB)
data_clean$ses_age <- NA
year_map <- setNames(as.character(1900:2002), 1:103)
data_raw$yob_label <- year_map[as.character(data_raw$YOB)]
data_raw$yob_label <- factor(data_raw$yob_label)
data_clean$ses_age <- as.numeric(data_raw$yob_label)
table(data_clean$ses_age)

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


## LANGU ---------------------------- Language ---------------------------------

attributes(data_raw$LANGU)
table(data_raw$LANGU)
data_clean$ses_language <- NA
data_clean$ses_language[data_raw$LANGU == 1] <- "fr"
data_clean$ses_language[data_raw$LANGU == 2] <- "en"
data_clean$ses_language[data_raw$LANGU == 3] <- "other"
table(data_clean$ses_language)

## Q23 ------------------------ how many immigrants ----------------------------

clean_immigrants <- sondr::load_variable("_SharedFolder_article_religion-magie/Data/data_clean/data_quorum_1.rds", "how_many_immigrants")
attributes(clean_immigrants)
sondr::find_quantile_range(c(0, 0.25, 0.5, 0.75, 1), clean_immigrants)

quantile(data_raw$Q23, prob = c(0, 0.23))
quantile(data_raw$Q23, prob = c(0.24, 0.45))
quantile(data_raw$Q23, prob = c(0.46, 0.80))
quantile(data_raw$Q23, prob = c(0.81, 0.93))
quantile(data_raw$Q23, prob = c(0.94, 1.0))

attributes(data_raw$Q23)
table(data_raw$Q23)
data_clean$how_many_immigrants <- NA
data_clean$how_many_immigrants[data_raw$Q23 <= 100000] <- 0
data_clean$how_many_immigrants[data_raw$Q23 > 100000 & data_raw$Q23 < 208100] <- 0.25
data_clean$how_many_immigrants[data_raw$Q23 >= 208100 & data_raw$Q23 < 350000] <- 0.5
data_clean$how_many_immigrants[data_raw$Q23 >= 350000 & data_raw$Q23 <= 500000] <- 0.75
data_clean$how_many_immigrants[data_raw$Q23 >500000] <- 1
table(data_clean$how_many_immigrants)

## Q29 ----------------------- attachment to church ----------------------------

attributes(data_raw$Q29)
attributes(data_raw$Q28)
table(data_raw$Q29)
data_clean$religion_attached_to_church_all <- NA
data_clean$religion_attached_to_church_all <- (data_raw$Q29 - 1) / 4
data_clean$religion_attached_to_church_all[data_raw$Q28 == 1] <- 0
sum(table(data_clean$religion_attached_to_church_all))

## -------------- attachment to church for religious people --------------------

data_clean$religion_attached_to_church_religious <- NA
data_clean$religion_attached_to_church_religious <- (data_raw$Q29 - 1) / 4
table(data_clean$religion_attached_to_church_religious)
sum(table(data_clean$religion_attached_to_church_religious))

## ---------------------- Religious or not -------------------------------------

data_clean$religion_bin <- NA
data_clean$religion_bin[data_raw$Q28 == 1] <- 0
data_clean$religion_bin[data_raw$Q28 != 1] <- 1
table(data_clean$religion_bin)
sum(table(data_clean$religion_bin))

## Q32 ------------------------ sexual orientation -----------------------------

attributes(data_raw$Q32)
table(data_raw$Q32)
data_clean$ses_sexual_orientation <- NA
data_clean$ses_sexual_orientation[data_raw$Q32 == 1] <- "heterosexual"
data_clean$ses_sexual_orientation[data_raw$Q32 == 2] <- "homomosexual"
data_clean$ses_sexual_orientation[data_raw$Q32 == 3] <- "bisexual"
data_clean$ses_sexual_orientation[data_raw$Q32 == 4] <- "other"
table(data_clean$ses_sexual_orientation)

## Q33 ------------------------ marital status ---------------------------------

attributes(data_raw$Q33)
table(data_raw$Q33)
data_clean$ses_marital_status <- NA
data_clean$ses_marital_status[data_raw$Q33 == 1] <- "married"
data_clean$ses_marital_status[data_raw$Q33 == 2] <- "living_with_partner"
data_clean$ses_marital_status[data_raw$Q33 == 3] <- "single"
data_clean$ses_marital_status[data_raw$Q33 == 4] <- "divorced"
data_clean$ses_marital_status[data_raw$Q33 == 5] <- "separated"
data_clean$ses_marital_status[data_raw$Q33 == 6] <- "widowed"
data_clean$ses_marital_status[data_raw$Q33 == 7] <- "never_married"
data_clean$ses_marital_status[data_raw$Q33 == 8] <- "dk"
data_clean$ses_marital_status <- factor(data_clean$ses_marital_status, levels = c("married", "living_with_partner", "single", "divorced", "separated", "widowed", "never_married", "dk"))
table(data_clean$ses_marital_status)

## Q34 ----------------------- ethnicity ---------------------------------------

attributes(data_raw$Q34)
table(data_raw$Q34)
data_clean$ses_ethnicity <- NA
data_clean$ses_ethnicity[data_raw$Q34 == 1] <- "white"
data_clean$ses_ethnicity[data_raw$Q34 == 2] <- "black"
data_clean$ses_ethnicity[data_raw$Q34 == 3] <- "native"
data_clean$ses_ethnicity[data_raw$Q34 == 4] <- "asian"
data_clean$ses_ethnicity[data_raw$Q34 == 5] <- "hispanic"
data_clean$ses_ethnicity[data_raw$Q34 == 6] <- "arab"
table(data_clean$ethnicity)

## Q48 -------------------- born in canada -------------------------------------

attributes(data_raw$Q48)
table(data_raw$Q48)
data_clean$ses_born_canada <- NA
data_clean$ses_born_canada[data_raw$Q48 == 1] <- 1
data_clean$ses_born_canada[data_raw$Q48 != 1] <- 0
table(data_clean$ses_born_canada)

## Q36 ------------------------ education --------------------------------------

attributes(data_raw$Q36)
table(data_raw$Q36)
data_clean$ses_education <- NA
data_clean$ses_education[data_raw$Q36 == 1] <- "before_high_school"
data_clean$ses_education[data_raw$Q36 == 2] <- "before_high_school"
data_clean$ses_education[data_raw$Q36 == 3] <- "before_high_school"
data_clean$ses_education[data_raw$Q36 == 4] <- "before_high_school"
data_clean$ses_education[data_raw$Q36 == 5] <- "high_school"
data_clean$ses_education[data_raw$Q36 == 6] <- "high_school"
data_clean$ses_education[data_raw$Q36 == 7] <- "cegep_college"
data_clean$ses_education[data_raw$Q36 == 8] <- "cegep_college"
data_clean$ses_education[data_raw$Q36 == 9] <- "undergraduate"
data_clean$ses_education[data_raw$Q36 == 10] <- "graduate"
data_clean$ses_education[data_raw$Q36 == 11] <- "graduate"
data_clean$ses_education <- factor(data_clean$ses_education, levels = c("before_high_school", "high_school", "cegep_college", "undergraduate", "graduate"))
table(data_clean$ses_education)

## Q39 ---------------------------- occupation ---------------------------------

attributes(data_raw$Q39)
table(data_raw$Q39)
data_clean$ses_occupation <- NA
data_clean$ses_occupation[data_raw$Q39 == 1] <- "employed"
data_clean$ses_occupation[data_raw$Q39 == 2] <- "employed"
data_clean$ses_occupation[data_raw$Q39 == 3] <- "student"
data_clean$ses_occupation[data_raw$Q39 == 4] <- "retired"
data_clean$ses_occupation[data_raw$Q39 == 5] <- "unemployed"
data_clean$ses_occupation[data_raw$Q39 == 6] <- "unemployed"
table(data_clean$ses_occupation)

# covid ----------------------------- covid ------------------------------------

data_clean$during_covid <- 0

# survey ----------------- which survey ----------------------------------------

data_clean$survey_name <- "sondage_2014"

# create an empty weights variable
data_clean$weight <- NA

## Save ------------------------------------------------------------------------

saveRDS(data_clean, "_SharedFolder_article_religion-magie/Data/data_clean/data_2014.rds")
