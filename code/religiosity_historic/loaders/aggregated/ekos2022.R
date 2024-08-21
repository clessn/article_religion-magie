df <- readODS::read_ods("_SharedFolder_article_religion-magie/Data/religiosity_historic/warehouse/aggregated/ekos2022.ods")

df_clean <- df

df_clean$choice[df$choice == "Very important"] <- 1
df_clean$choice[df$choice == "Somewhat important"] <- 0.67
df_clean$choice[df$choice == "Not too important"] <- 0.33
df_clean$choice[df$choice == "Not at all important"] <- 0
df_clean$choice[df$choice == "Don’t know/No Response/Skip"] <- NA
df_clean$choice[df$choice == "More than once"] <- 1
df_clean$choice[df$choice == "Once"] <- 0.5
df_clean$choice[df$choice == "Never"] <- 0
df_clean$choice[df$choice == "No response/skip"] <- NA
df_clean$choice[df$choice == "1"] <- 1
df_clean$choice[df$choice == "0"] <- 1

df_clean$choice <- as.numeric(df_clean$choice)

df_clean$survey_id <- "ekos2022"

saveRDS(df_clean, "_SharedFolder_article_religion-magie/Data/religiosity_historic/warehouse/aggregated/ekos2022.rds")
