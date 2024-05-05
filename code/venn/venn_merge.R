library(dplyr)

data_quorum_1 <- haven::read_sav("_SharedFolder_article_religion-magie/Data/quorum_1/ULAQUORUM.Sav", encoding = "latin1")
data_quorum_2 <- haven::read_sav("_SharedFolder_article_religion-magie/Data/quorum_2/QUORUM2.Sav")
data_quorum_3 <- haven::read_sav("_SharedFolder_article_religion-magie/Data/quorum_3/ULA003.Sav")
data_2014 <- haven::read_sav("_SharedFolder_article_religion-magie/Data/MJU001 - PONDÉRÉ N=2000.Sav")
data_2024 <- haven::read_sav("_SharedFolder_article_religion-magie/Data/Immigration Spicy - UL_April 18, 2024_06.51.sav")   

columns_to_remove <- c("SAV_NUM", "CASEID", "PROV", "CP", "REG_1", "REG_2", "REG_3", "REG_4", "REGION", "REG_ADM", "REGIONAD", "SEXE", "YOB", "AGE", "LANGU", "AGESE", "AGESEQ", "REGPQ", "LANGUPQ", "POIDQ", "REGP", "REGPC", "POICAN", "POIDS", "TOTALDUR", "QUEST", "StartDate", "EndDate", "Status", "IPAddress", "Progress", "Duration__in_seconds_", "Finished", "RecordedDate", "ResponseId", "RecipientLastName", "RecipientFirstName", "RecipientEmail", "ExternalReference", "LocationLatitude", "LocationLongitude", "DistributionChannel", "UserLanguage", "Q_LANG", "genre", "orientation", "orientation_4_TEXT", "age", "origines_ethniques", "revenu", "education", "religion", "religion_11_TEXT", "attachement_religion", "occupation", "milieu_vie", "ses_code_postal", "province", "language", "language_3_TEXT", "know_language_1", "know_language_2", "langue_maternelle", "langue_maternelle_3_TEXT", "langue_travail", "langue_travail_3_TEXT", "french_daily", "english_daily", "origine_canada", "pays_origine", "parent_origine", "cate_immigrant", "cate_immgrant_12_TEXT", "annee_canada", "pn_identificaion_1", "pn_identificaion_2", "pn_identificaion_3", "pn_identificaion_4", "pn_identificaion_5", "pn_nation", "pn_bispirit", "sophis_pol_1", "intrRel_pplFeelRelIm_1", "intrRel_liveLifeRel", "extrRel_lostRelCivDo", "subjRel_moreReligiou", "intrRel_helpSenseLif", "sciBelief_moonAffect", "sciBelief_horoscopeP", "intrRel_thinkLargerF", "sciBelief_relAlwaysR", "list_every", "list_aboriginals", "list_asian", "list_french", "direct_every", "direct_aboriginals", "direct_asian", "direct_french", "alberta", "bc", "ipe", "manitoba", "nb", "ne", "ontario", "quebec", "sk", "tnl", "tno", "nunavut", "yukon")

columns_to_remove_quorum_1 <- columns_to_remove[columns_to_remove %in% names(data_quorum_1)]
columns_to_remove_quorum_2 <- columns_to_remove[columns_to_remove %in% names(data_quorum_2)]
columns_to_remove_quorum_3 <- columns_to_remove[columns_to_remove %in% names(data_quorum_3)]
columns_to_remove_2014 <- columns_to_remove[columns_to_remove %in% names(data_2014)]
columns_to_remove_2024 <- columns_to_remove[columns_to_remove %in% names(data_2024)]

data_quorum_1 <- data_quorum_1 %>% 
  select(-all_of(columns_to_remove_quorum_1))

data_quorum_2 <- data_quorum_2 %>% 
  select(-all_of(columns_to_remove_quorum_2))

data_quorum_3 <- data_quorum_3 %>%
  select(-all_of(columns_to_remove_quorum_3))

data_2014 <- data_2014 %>%
  select(-all_of(columns_to_remove_2014))

data_2024 <- data_2024 %>%
  select(-all_of(columns_to_remove_2024))

labels_quorum_1 <- (sondr::sav_to_codebook(data_quorum_1))$question
labels_quorum_2 <- (sondr::sav_to_codebook(data_quorum_2))$question
labels_quorum_3 <- (sondr::sav_to_codebook(data_quorum_3))$question
labels_2014 <- (sondr::sav_to_codebook(data_2014))$question
labels_2024 <- (sondr::sav_to_codebook(data_2024))$question

# Convert vectors to a single string separated by commas
formatted_labels_1 <- paste(unique(labels_quorum_1), collapse = ", ")
formatted_labels_2 <- paste(unique(labels_quorum_2), collapse = ", ")
formatted_labels_3 <- paste(unique(labels_quorum_3), collapse = ", ")
formatted_labels_2014 <- paste(unique(labels_2014), collapse = ", ")
formatted_labels_2024 <- paste(unique(labels_2024), collapse = ", ")

system <- "Your role is to detect which survey questions are duplicates between 5 different surveys."

# Construct the full prompt
prompt <- paste0("I will give you the questions from 5 different surveys and your role is to detect which questions are duplicates between the surveys. Output the duplicated questions into JSON format. Output the question in a Question field and the surveys associated with this question in a surveys field. Be exhaustive. It's ok if the answer is extremely long. Don't cut corners. Analyse 100% of the material and be completely exhaustive. Output only the json and nothing else. Here is an example of the structure:\n\n",
"{\n",
"  \"Questions\": [\n",
"    {\n",
"      \"Question\": \"What is your age?\",\n",
"      \"Surveys\": [\"survey_name_1\", \"survey_name_2\"]\n",
"    },\n",
"    {\n",
"      \"Question\": \"What is your income?\",\n",
"      \"surveys\": [\"survey_name_4\", \"survey_name_5\", \"survey_name_6\"]\n",
"    }\n",
"  ]\n",
"}\n\n",
"Here are the questions from the 5 surveys:\n\n",
"Questions from data_quorum_1: ", formatted_labels_1, "\n\n",
"Questions from data_quorum_2: ", formatted_labels_2, "\n\n",
"Questions from data_quorum_3: ", formatted_labels_3, "\n\n",
"Questions from data_2014: ", formatted_labels_2014, "\n\n",
"Questions from data_2024: ", formatted_labels_2024)

# API call
chat_prompt <- openai::create_chat_completion(
    model = "gpt-4-turbo",
    messages = list(
        list(role = "system", content = system),
        list(role = "user", content = prompt)
    )
)

output <- chat_prompt$choices$message.content

input_tokens <- (chat_prompt$usage$prompt_tokens / 1000000) * 10
  output_tokens <- (chat_prompt$usage$completion_tokens / 1000000) * 30
  price <- input_tokens + output_tokens
  total_price <- price
  print(paste0("Iteration price: $", price,". Total price: $", total_price))

# Find the position of the first occurrence of "{"

json_start <- regexpr("\\{", output)
      
# Find all occurrences of "}" and select the last one

all_json_ends <- gregexpr("\\}", output)
last_json_end <- if (length(all_json_ends[[1]]) > 0) max(all_json_ends[[1]]) else -1
      
# Check if we have valid start and end positions for the JSON content

if (json_start > 0 && last_json_end > 0) {

    # Extract the JSON part from the output using the start and end positions

    json_content <- substr(output, json_start, last_json_end)
} else {
    json_content <- ""  # Default to an empty string if positions are not valid
}

initial_duplicates <- json_content
current_duplicates <- initial_duplicates

  # Construct the prompt
  prompt <- paste0("I will give you the questions from 5 different surveys and your role is to detect which questions are duplicates between the surveys. You must use a standard JSON structure to output your findings. Here is an example of the structure:\n\n",
"{\n",
"  \"Questions\": [\n",
"    {\n",
"      \"Question\": \"What is your age?\",\n",
"      \"Surveys\": [\"survey_name_1\", \"survey_name_2\"]\n",
"    },\n",
"    {\n",
"      \"Question\": \"What is your income?\",\n",
"      \"surveys\": [\"survey_name_4\", \"survey_name_5\", \"survey_name_6\"]\n",
"    }\n",
"  ]\n",
"}\n\n",
"Here is the current state of detected duplicates in JSON format:\n\n", 
                   current_duplicates, "\n\n",
                   "Please create a JSON list with any new duplicates you can find. Use the provided structure and stick to it. Keep your questions formatted in JSON as per the structure of the list you were provided. Don't include any duplicates from the list you were provided earlier. Only output new findings. Here are the questions from the 5 surveys:\n\n",
                   "Questions from data_quorum_1: ", formatted_labels_1, "\n\n",
                   "Questions from data_quorum_2: ", formatted_labels_2, "\n\n",
                   "Questions from data_quorum_3: ", formatted_labels_3, "\n\n",
                   "Questions from data_2014: ", formatted_labels_2014, "\n\n",
                   "Questions from data_2024: ", formatted_labels_2024)

  # API call
  chat_prompt <- openai::create_chat_completion(
    model = "gpt-4-turbo",
    messages = list(
        list(role = "system", content = system),
        list(role = "user", content = prompt)
    )
  )
  # Parse new duplicates from response
  output <- chat_prompt$choices$message.content

  # Find the position of the first occurrence of "{"

  json_start <- regexpr("\\{", output)
  
  # Find all occurrences of "}" and select the last one

  all_json_ends <- gregexpr("\\}", output)
  last_json_end <- if (length(all_json_ends[[1]]) > 0) max(all_json_ends[[1]]) else -1
        
  # Check if we have valid start and end positions for the JSON content

  if (json_start > 0 && last_json_end > 0) {

    # Extract the JSON part from the output using the start and end positions

    json_content <- substr(output, json_start, last_json_end)
  } else {
    json_content <- ""  # Default to an empty string if positions are not valid
  }

  new_duplicates <- json_content
  
  input_tokens <- (chat_prompt$usage$prompt_tokens / 1000000) * 10
  output_tokens <- (chat_prompt$usage$completion_tokens / 1000000) * 30
  price <- input_tokens + output_tokens
  price
  total_price <- total_price + price
  print(paste0("Iteration price: $", price,". Total price: $", total_price))


# Parse the JSON content to extract the detected duplicates

if (nzchar(current_duplicates)) {
    parsed_output <- tryCatch({
        jsonlite::fromJSON(current_duplicates)
    }, error = function(e) {
        message("JSON parsing error: ", e$message)
        NULL
    })
} else {
    message("No JSON content found.")
    parsed_output <- NULL
}