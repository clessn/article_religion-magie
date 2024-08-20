# Define the paths to the warehouse directories
path_individual <- "_SharedFolder_article_religion-magie/Data/religiosity_historic/warehouse/individual/"
path_aggregated <- "_SharedFolder_article_religion-magie/Data/religiosity_historic/warehouse/aggregated/"

# Get a list of all RDS files in both directories
files_individual <- list.files(path_individual, pattern = "\\.rds$", full.names = TRUE)
files_aggregated <- list.files(path_aggregated, pattern = "\\.rds$", full.names = TRUE)

# Combine the lists of files
files <- c(files_individual, files_aggregated)

# Read all RDS files and combine them into a single dataframe
combined_df <- do.call(rbind, lapply(files, readRDS))

# Save the combined dataframe to an RDS file
saveRDS(combined_df, "_SharedFolder_article_religion-magie/Data/religiosity_historic/mart/data.rds")
