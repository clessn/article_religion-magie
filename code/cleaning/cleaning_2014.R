data <- haven::read_sav("_SharedFolder_article_religion-magie/Data/MJU001 - PONDÉRÉ N=2000.Sav")

codebook <- sondr::sav_to_codebook(data)

data <- data %>% 
  select(PROV, CP, SEXE, YOB, AGE, LANGU, Q2_5, Q23, Q28, Q29, Q32, Q33, Q34, Q48, Q36, Q39)


## Q2_5 ------------------------------------------------------------------------