df <- readODS::read_ods("_SharedFolder_article_religion-magie/Data/religiosity_historic/warehouse/aggregated/researchco22.ods")

# Clean the 'choice' variable
df_clean <- df

# Recode the 'choice' variable
df_clean$choice[df$choice == "Very important"] <- 1
df_clean$choice[df$choice == "Moderately important"] <- 0.67
df_clean$choice[df$choice == "Not too important"] <- 0.33
df_clean$choice[df$choice == "Not important at all"] <- 0
df_clean$choice[df$choice == "Very spiritual"] <- 1
df_clean$choice[df$choice == "Moderately spiritual"] <- 0.67
df_clean$choice[df$choice == "Not too spiritual"] <- 0.33
df_clean$choice[df$choice == "Not spiritual at all"] <- 0
df_clean$choice[df$choice == "Not sure"] <- NA
df_clean$choice[df$choice == "More than once a week"] <- 1
df_clean$choice[df$choice == "Once a week"] <- 0.8
df_clean$choice[df$choice == "Monthly"] <- 0.6
df_clean$choice[df$choice == "Less than monthly"] <- 0.4
df_clean$choice[df$choice == "Only for special events (e.g. weddings, funerals, baptisms, etc.)"] <- 0.2
df_clean$choice[df$choice == "Never"] <- 0
df_clean$choice[df$choice == "1"] <- 1
df_clean$choice[df$choice == "0"] <- 0

# Ensure 'choice' is numeric
df_clean$choice <- as.numeric(df_clean$choice)

df_clean$survey_id <- "researchco22"

saveRDS(df_clean, "_SharedFolder_article_religion-magie/Data/religiosity_historic/warehouse/aggregated/researchco22.rds")
