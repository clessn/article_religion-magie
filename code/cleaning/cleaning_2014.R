data <- haven::read_sav("_SharedFolder_article_religion-magie/Data/MJU001 - PONDÉRÉ N=2000.Sav")

codebook <- sondr::sav_to_codebook(data)
