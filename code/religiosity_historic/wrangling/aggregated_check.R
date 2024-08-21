# Define the paths to the warehouse directories
path_individual <- "_SharedFolder_article_religion-magie/Data/religiosity_historic/warehouse/individual/"
path_aggregated <- "_SharedFolder_article_religion-magie/Data/religiosity_historic/warehouse/aggregated/"

# Get a list of all RDS files in both directories
files_individual <- list.files(path_individual, pattern = "\\.rds$", full.names = TRUE)
files_aggregated <- list.files(path_aggregated, pattern = "\\.rds$", full.names = TRUE)

# Combine the lists of files
files <- c(files_individual, files_aggregated)

# Function to check the number of columns in each file
check_columns <- function(file) {
  df <- readRDS(file)
  return(ncol(df))
}

# Get the number of columns for each file
columns_info <- sapply(files, check_columns)

# Print the column counts to identify mismatches
print(columns_info)

# Optionally, you can identify files with mismatched column counts
unique_column_counts <- unique(columns_info)
mismatched_files <- files[columns_info != unique_column_counts[1]]
print(mismatched_files)
