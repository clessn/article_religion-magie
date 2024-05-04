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

max_length <- max(length(labels_quorum_1), length(labels_quorum_2), length(labels_quorum_3), length(labels_2024))

labels_quorum_1 <- c(labels_quorum_1, rep(NA, max_length - length(labels_quorum_1)))
labels_quorum_2 <- c(labels_quorum_2, rep(NA, max_length - length(labels_quorum_2)))
labels_quorum_3 <- c(labels_quorum_3, rep(NA, max_length - length(labels_quorum_3)))
labels_2024 <- c(labels_2024, rep(NA, max_length - length(labels_2024)))

codebook_rest <- data.frame(
  quorum_1 = labels_quorum_1, 
  quorum_2 = labels_quorum_2, 
  quorum_3 = labels_quorum_3, 
  survey_2024 = labels_2024
)

write.csv(codebook_rest, file = "_SharedFolder_article_religion-magie/Data/venn/ven_coding_rest.csv")

formatted_labels_1 <- paste(unique(labels_quorum_1), collapse = ", ")
formatted_labels_2 <- paste(unique(labels_quorum_2), collapse = ", ")
formatted_labels_3 <- paste(unique(labels_quorum_3), collapse = ", ")
formatted_labels_2014 <- paste(unique(labels_2014), collapse = ", ")
formatted_labels_2024 <- paste(unique(labels_2024), collapse = ", ")

codebook_2014 <- sondr::sav_to_codebook(data_2014)

write.csv(codebook_2014, file = "_SharedFolder_article_religion-magie/Data/venn/ven_coding.csv")

codebook_2014$quorum_1 <- NA
codebook_2014$quorum_2 <- NA
codebook_2014$quorum_3 <- NA
codebook_2014$survey_2024 <- NA

price <- 0
total_price <- 0

for (i in 55:length(codebook_2014$question)) {
  system <- "Your role is to read a survey question and detect if it's present within 4 different surveys"

# Construct the full prompt
prompt <- paste0("I will give you a survey question: ,", codebook_2014$question[i],"Your role is to detect if that question is present within 4 different surveys: quorum_1, quorum_2, quorum_3, and survey_2024. Questions can be in english or in french. Using JSON format, your role is to output a 1 if the question is present in a survey or a 0 if it's not present. Here is an example of the structure: {\"Questions\":[{\"Survey Question\":\"What is your age?\",\"Surveys\":{\"quorum_1\":1,\"quorum_2\":1,\"quorum_3\":1,\"survey_2024\":1}},{\"Survey Question\":\"What is your income?\",\"Surveys\":{\"quorum_1\":1,\"quorum_2\":1,\"quorum_3\":1,\"survey_2024\":1}}]}. Here is the question you need to find across surveys: ", codebook_2014$question[i],". Here are the questions present in the 4 different surveys: Questions from data_quorum_1: ", formatted_labels_1, ", \n\nQuestions from data_quorum_2: ", formatted_labels_2, ", \n\nQuestions from data_quorum_3: ", formatted_labels_3, ", \n\nQuestions from survey_2024: ", formatted_labels_2024)

# API call
chat_prompt <- openai::create_chat_completion(
    model = "gpt-4-turbo",
    messages = list(
        list(role = "system", content = system),
        list(role = "user", content = prompt)
    )
)

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

if (nzchar(json_content)) {
          parsed_output <- tryCatch({
              jsonlite::fromJSON(json_content)
          }, error = function(e) {
              message("JSON parsing error: ", e$message)
              NULL
          })
      } else {
          message("No JSON content found.")
          parsed_output <- NULL
      }

print(paste0("The question: ", codebook_2014$question[i] , " is present in the following surveys: ", if(parsed_output$Questions$Surveys$quorum_1 == 1) "quorum_1" else "", if(parsed_output$Questions$Surveys$quorum_2 == 1) ", quorum_2" else "", if(parsed_output$Questions$Surveys$quorum_3 == 1) ", quorum_3" else "", if(parsed_output$Questions$Surveys$survey_2024 == 1) ", survey_2024" else "NO SURVEY"))

codebook_2014$quorum_1[i] <- parsed_output$Questions$Surveys$quorum_1
codebook_2014$quorum_2[i] <- parsed_output$Questions$Surveys$quorum_2
codebook_2014$quorum_3[i] <- parsed_output$Questions$Surveys$quorum_3
codebook_2014$survey_2024[i] <- parsed_output$Questions$Surveys$survey_2024


input_tokens <- (chat_prompt$usage$prompt_tokens / 1000000) * 10
  output_tokens <- (chat_prompt$usage$completion_tokens / 1000000) * 30
  price <- input_tokens + output_tokens
  total_price <- price
  print(paste0("Iteration ", i, " price: $", price,". Total price: $", total_price))
}
