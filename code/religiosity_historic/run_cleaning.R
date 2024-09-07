
# Define the paths to the main directory and the aggregated subfolder
path_to_main <- "code/religiosity_historic/loaders/"
path_to_aggregated <- "code/religiosity_historic/loaders/aggregated/"

# List all R scripts in the main directory, excluding 'template.R'
r_scripts_main <- list.files(path_to_main, pattern = "\\.R$", full.names = TRUE)
r_scripts_main <- r_scripts_main[!grepl("template.R", r_scripts_main)]

# List all R scripts in the aggregated subfolder
r_scripts_aggregated <- list.files(path_to_aggregated, pattern = "\\.R$", full.names = TRUE)

# Combine the two lists of R scripts
all_r_scripts <- c(r_scripts_main, r_scripts_aggregated)

# Loop over each script and source it
for (script in all_r_scripts) {
  message(paste("Running:", script))
  
  # Source each script
  source(script)
  
  # Check if the script is from the main folder (i.e., needs n column)
  if (script %in% r_scripts_main && exists("raw_data") && exists("clean_data")) {
    # Add the number of observations from raw_data to clean_data
    clean_data$n <- nrow(raw_data)
    message(paste("Number of observations for", script, ":", nrow(raw_data)))
    
    # Ensure that the `n` column is carried over to the `output` if it exists
    if (exists("output")) {
      output$n <- nrow(raw_data)
    }
  }
  
  # Save the modified `clean_data` or `output` depending on what needs saving
  if (exists("output")) {
    saveRDS(output, paste0("_SharedFolder_article_religion-magie/Data/religiosity_historic/warehouse/individual/", survey_id, ".rds"))
  } else if (exists("clean_data")) {
    saveRDS(clean_data, paste0("_SharedFolder_article_religion-magie/Data/religiosity_historic/warehouse/individual/", survey_id, ".rds"))
  } else {
    message(paste("Skipping n column calculation for aggregated folder script:", script))
  }
}

message("All scripts have been executed.")

