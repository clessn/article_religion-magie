# Load les données 

cora_cpep <- readRDS("_SharedFolder_article_religion-magie/Data/religiosity_historic/warehouse/individual/cora-cpep.rds")

democracy_checkup_2019 <- readRDS("_SharedFolder_article_religion-magie/Data/religiosity_historic/warehouse/individual/democracy_checkup_2019.rds")

democracy_checkup_2020 <- readRDS("_SharedFolder_article_religion-magie/Data/religiosity_historic/warehouse/individual/democracy_checkup_2020.rds")

democracy_checkup_2021 <- readRDS("_SharedFolder_article_religion-magie/Data/religiosity_historic/warehouse/individual/democracy_checkup_2021.rds")

democracy_checkup_2022 <- readRDS("_SharedFolder_article_religion-magie/Data/religiosity_historic/warehouse/individual/democracy_checkup_2022.rds")

environics_2011 <- readRDS("_SharedFolder_article_religion-magie/Data/religiosity_historic/warehouse/individual/environics_2011.rds")

general_social_survey_2010 <- readRDS("_SharedFolder_article_religion-magie/Data/religiosity_historic/warehouse/individual/general_social_survey_2010.rds")

general_social_survey_2011 <- readRDS("_SharedFolder_article_religion-magie/Data/religiosity_historic/warehouse/individual/general_social_survey_2011.rds")

general_social_survey_2013 <- readRDS("_SharedFolder_article_religion-magie/Data/religiosity_historic/warehouse/individual/general_social_survey_2013.rds")

general_social_survey_2014 <- readRDS("_SharedFolder_article_religion-magie/Data/religiosity_historic/warehouse/individual/general_social_survey_2014.rds")

general_social_survey_2015 <- readRDS("_SharedFolder_article_religion-magie/Data/religiosity_historic/warehouse/individual/general_social_survey_2015.rds")

general_social_survey_2016 <- readRDS("_SharedFolder_article_religion-magie/Data/religiosity_historic/warehouse/individual/general_social_survey_2016.rds")

general_social_survey_2017 <- readRDS("_SharedFolder_article_religion-magie/Data/religiosity_historic/warehouse/individual/general_social_survey_2017.rds")

general_social_survey_2018 <- readRDS("_SharedFolder_article_religion-magie/Data/religiosity_historic/warehouse/individual/general_social_survey_2018.rds")

general_social_survey_2020 <- readRDS("_SharedFolder_article_religion-magie/Data/religiosity_historic/warehouse/individual/general_social_survey_2020.rds")


# r.bin() les dataframes 

combined_df <- rbind(cora_cpep, democracy_checkup_2019, democracy_checkup_2020, democracy_checkup_2021, democracy_checkup_2022, environics_2011, general_social_survey_2010, general_social_survey_2011, general_social_survey_2013, general_social_survey_2014, general_social_survey_2015, general_social_survey_2016, general_social_survey_2017, general_social_survey_2018, general_social_survey_2020)


saveRDS(combined_df, "")

