df <- read.csv("_SharedFolder_article_religion-magie/Data/venn/duplicates.csv")

data_quorum_1 <- haven::read_sav("_SharedFolder_article_religion-magie/Data/quorum_1/ULAQUORUM.Sav", encoding = "latin1")
data_quorum_2 <- haven::read_sav("_SharedFolder_article_religion-magie/Data/quorum_2/QUORUM2.Sav")
data_quorum_3 <- haven::read_sav("_SharedFolder_article_religion-magie/Data/quorum_3/ULA003.Sav")
data_2014 <- haven::read_sav("_SharedFolder_article_religion-magie/Data/MJU001 - PONDÉRÉ N=2000.Sav")
data_2024 <- haven::read_sav("_SharedFolder_article_religion-magie/Data/Immigration Spicy - UL_April 18, 2024_06.51.sav")

labels_quorum_1 <- (sondr::sav_to_codebook(data_quorum_1))$question
labels_quorum_2 <- (sondr::sav_to_codebook(data_quorum_2))$question
labels_quorum_3 <- (sondr::sav_to_codebook(data_quorum_3))$question
labels_2014 <- (sondr::sav_to_codebook(data_2014))$question
labels_2024 <- (sondr::sav_to_codebook(data_2024))$question

# Convert vectors to a single string separated by commas
formatted_labels_1 <- paste(labels_quorum_1, collapse = ", ")
formatted_labels_2 <- paste(labels_quorum_2, collapse = ", ")
formatted_labels_3 <- paste(labels_quorum_3, collapse = ", ")
formatted_labels_2014 <- paste(labels_2014, collapse = ", ")
formatted_labels_2024 <- paste(labels_2024, collapse = ", ")

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

iterations <- 5
for (i in 1:iterations){
  # Construct the prompt
  prompt <- paste0("I will give you the questions from 5 different surveys and your role is to detect which questions are duplicates between the surveys. Here is the current state of detected duplicates in JSON format:\n", 
                   current_duplicates, "\n",
                   "Please update this JSON with any new duplicates. Here are the questions from the 5 surveys:\n\n",
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
  print("let's go")
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
  
  current_duplicates <- new_duplicates


  input <- (chat_prompt$usage$prompt_tokens / 1000000) * 10
  output <- (chat_prompt$usage$completion_tokens / 1000000) * 30
  price <- input + output
  price
  total_price <- total_price + price
  print(paste0("Iteration price: $", price,". Total price: $", total_price))
}

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