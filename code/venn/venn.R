library(ggvenn)

data <- read.csv("_SharedFolder_article_religion-magie/Data/venn/venn_done.csv")

data_venn <- data  %>% 
filter(quorum_1 == 1 | quorum_2 == 1 | quorum_3 == 1 | survey_2024 == 1)

list_data <- list(
    quorum_1 = which(data_venn$quorum_1 == 1),
    quorum_2 = which(data_venn$quorum_2 == 1),
    quorum_3 = which(data_venn$quorum_3 == 1),
    spicy = which(data_venn$survey_2024 == 1)
)

ggvenn(list_data, show_elements = TRUE)

ggsave("_SharedFolder_article_religion-magie/Data/venn/venn.png")

write.csv(data_venn, "_SharedFolder_article_religion-magie/Data/venn/venn.csv")

