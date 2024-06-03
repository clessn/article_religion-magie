library(dplyr)
library(ggplot2)

data <- readRDS("_SharedFolder_article_religion-magie/Data/data_clean/data_religion_magie.rds")

data_quorum <- data  %>% 
    filter(survey_name == "quorum_1")

data_quorum_roc <- data_quorum %>% 
    filter(ses_province != "qc")

data_quorum_qc <- data_quorum %>%
    filter(ses_province == "qc")

# Calculate the density of points
data_quorum_density <- data_quorum %>%
    group_by(covid_afraid_of_dying, religion_attached_to_church_religious) %>%
    summarise(n = n())

# Find the maximum count for setting limits
max_count <- max(data_quorum_density$n)

# Plot with varying alpha based on point density
ggplot(data_quorum_density, aes(x = covid_afraid_of_dying, y = religion_attached_to_church_religious)) +
    geom_tile(aes(alpha = n), fill = "#000000") +
    geom_smooth(data = data_quorum, method = "loess", se = FALSE, color = "#ff00dd") +
    scale_alpha_continuous(range = c(0.1, 10), limits = c(0, max_count)) +
    labs(title = "Relationship between Fear of Dying from COVID-19 and Religious Attachment",
         x = "Fear of Dying from COVID-19",
         y = "Religious Attachment to Church",
         alpha = "Number of Points") +
    clessnize::theme_clean_light()


table(data_quorum$covid_afraid_of_dying)
table(data_quorum$religion_attached_to_church_all)

ggplot(data_quorum_qc, aes(x = covid_afraid_of_dying, y = religion_attached_to_church_all)) +
    geom_point() +
    geom_smooth(method = "loess", se = FALSE) +
    labs(title = "Relationship between Fear of Dying from COVID-19 and Religious Attachment in Quebec",
         x = "Fear of Dying from COVID-19",
         y = "Religious Attachment to Church") +
    theme_minimal()

ggplot(data_quorum_roc, aes(x = covid_afraid_of_dying, y = religion_attached_to_church_all)) +
    geom_point() +
    geom_smooth(method = "loess", se = FALSE) +
    labs(title = "Relationship between Fear of Dying from COVID-19 and Religious Attachment in Rest of Canada",
         x = "Fear of Dying from COVID-19",
         y = "Religious Attachment to Church") +
    theme_minimal()


ggplot(data_quorum_qc, aes(x = religion_attached_to_church_all)) +
    geom_histogram(bins = 10) +
    labs(title = "Distribution of Religious Attachment in Quebec",
         x = "Religious Attachment to Church",
         y = "Frequency") +
    theme_minimal()

ggplot(data_quorum_roc, aes(x = religion_attached_to_church_all)) +
    geom_histogram(bins = 5) +
    labs(title = "Distribution of Religious Attachment in Rest of Canada",
         x = "Religious Attachment to Church",
         y = "Frequency") +
    theme_minimal()


