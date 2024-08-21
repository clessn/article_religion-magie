df_years_ref <- readODS::read_ods("_SharedFolder_article_religion-magie/Data/religiosity_historic/years_reference.ods")

df_years_ref$year[df_years_ref$year == "2011-2014"] <- "2014"

df <- readRDS("_SharedFolder_article_religion-magie/Data/religiosity_historic/mart/data.rds")

# Check for orphaned survey_ids in df
orphaned_ids <- setdiff(df$survey_id, df_years_ref$survey_id)

# If there are orphaned survey_ids, print them
if (length(orphaned_ids) > 0) {
  message("The following survey_ids in df do not have a matching entry in df_years_ref:")
  print(orphaned_ids)
} else {
  message("All survey_ids in df have a matching entry in df_years_ref.")
}

# Merge the two dataframes based on 'survey_id'
df <- merge(df, df_years_ref[, c("survey_id", "year")], by = "survey_id", all.x = TRUE)

# Check if the merge introduced any NAs in the year column
na_years <- df[is.na(df$year), "survey_id"]

# If there are NAs in the year column, identify the cause
if (length(na_years) > 0) {
  message("The following survey_ids resulted in NA for the 'year' column after the merge:")
  print(na_years)
  message("Possible causes could be: missing 'survey_id' in df_years_ref or incorrect 'survey_id' in df.")
} else {
  message("No NAs were introduced in the 'year' column after the merge.")
}

df_final <- df

df_final$variable_id[df$variable_id == "attend" | 
                     df$variable_id == "participate" |
                     df$variable_id == "participatation"] <- "participation"

df_final <- df_final |> 
  dplyr::filter(variable_id != "spirituality")

saveRDS(df, "_SharedFolder_article_religion-magie/Data/religiosity_historic/mart/data.rds")
