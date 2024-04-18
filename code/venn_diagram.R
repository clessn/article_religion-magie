### Sondage 2014

data_2014 <- haven::read_sav("/Users/camillepelletier/Dropbox/CamillePelletier/Article-ScienceReligion/Data/MJU001 - PONDÉRÉ N=2000.sav")

codebook_2014 <- sondr::sav_to_codebook(data_2014)

write.csv(codebook_2014, "/Users/camillepelletier/Dropbox/CamillePelletier/Article-ScienceReligion/Data/codebook_2014.csv")



### Sondage Quorum 1

data_quorum_1 <- haven::read_sav("/Users/camillepelletier/Dropbox/article_religion-magie/_SharedFolder_article_religion-magie/Data/quorum_1/ULAQUORUM.Sav", encoding = "latin1")

codebook_quorum_1 <- sondr::sav_to_codebook(data_quorum_1)

write.csv(codebook_quorum_1, "/Users/camillepelletier/Dropbox/article_religion-magie/_SharedFolder_article_religion-magie/Data/codebook_quorum_1")



### Sondage Quorum 2

data_quorum_2 <- haven::read_sav("/Users/camillepelletier/Dropbox/article_religion-magie/_SharedFolder_article_religion-magie/Data/quorum_2/QUORUM2.Sav", encoding = "latin1")

codebook_quorum_2 <- sondr::sav_to_codebook(data_quorum_2)

write.csv(codebook_quorum_2, "/Users/camillepelletier/Dropbox/article_religion-magie/_SharedFolder_article_religion-magie/Data/codebook_quorum_2")



### Sondage Quorum 3

data_quorum_3 <- haven::read_sav("/Users/camillepelletier/Dropbox/article_religion-magie/_SharedFolder_article_religion-magie/Data/quorum_3/ULA003.Sav", encoding = "latin1")

codebook_quorum_3 <- sondr::sav_to_codebook(data_quorum_3)

write.csv(codebook_quorum_3, "/Users/camillepelletier/Dropbox/article_religion-magie/_SharedFolder_article_religion-magie/Data/codebook_quorum_3")




### Sondage 2024

data_2024 <- haven::read_sav("/Users/camillepelletier/Dropbox/git/article_religion-magie/Immigration Spicy - UL_April 18, 2024_06.51.sav")

codebook_2024 <- sondr::sav_to_codebook(data_2024) 

write.csv(codebook_2024, "/Users/camillepelletier/Dropbox/article_religion-magie/_SharedFolder_article_religion-magie/Data/codebook_2024")



### Diagramme de venn

codebook_venn <- rbind(codebook_2024, codebook_quorum_3, codebook_quorum_2, codebook_quorum_1, codebook_2014)

write.csv(codebook_venn, "/Users/camillepelletier/Dropbox/CamillePelletier/Article-ScienceReligion/Data/codebook_venn.csv")




