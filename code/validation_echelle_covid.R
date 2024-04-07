library(dplyr)

data <- haven::read_sav("_SharedFolder_article_religion-magie/Data/quorum_2/QUORUM2.Sav", encoding = "latin1")

codebook <- sondr::sav_to_codebook(data)
sondr::codebook_to_catalog(codebook, "codebook_quorum2.md", "Quorum 2")

# En regardant la liste suivante de recommandations de la santÃ© publique, quelle(s) recommandation(s) respectez-vous sans faute?Vous pouvez choisir plus dâ€™une rÃ©ponse. 1 = Respecter les mesures de distanciation physique en lien avec le COVID-19à

data$N1AC1

# La santÃ© publique recommande de respecter les mesures de distanciation  physique en lien avec le COVID-19. Respectez-vous sans faute cette recommandation?

data$Q100

# ÃŠtes-vous en accord avec lâ€™utilisation dâ€™une application utilisant [SELN2.TEXT] pour faciliter le traÃ§age des gens qui ont Ã©tÃ© en contact avec une personne infectÃ©e par la COVID-19 et ainsi aider Ã  lutter contre la propagation du COVID-19?

data$N2

# Ãtes-vous en accord ou en dÃ©saccord avec les Ã©noncÃ©s suivants : - Les personnes infectÃ©es par la COVID-19 devraient obligatoirement partager toutes informations personnelles considÃ©rÃ©es comme

data$Q2B_N8

# Ãtes-vous en accord ou en dÃ©saccord avec les Ã©noncÃ©s suivants : - Je n'ai pas particuliÃ¨rement peur de mourir de la COVID-19.

data$Q17_3
data$Q17_3_inv <- sondr::finverser(data$Q17_3)

# Ãtes-vous en accord ou en dÃ©saccord avec les Ã©noncÃ©s suivants : - La COVID-19 menace la survie de l'humanitÃ©.

data$Q17_N6

data$Q17_N2
data$Q17_N3
data$Q17_N3_inv <- sondr::finverser(data$Q17_N3)
data$Q17_N4

# Ãtes-vous en accord ou en dÃ©saccord avec les Ã©noncÃ©s suivants : - La vie ne sera plus jamais pareille aprÃ¨s la pandÃ©mie de la COVID-19.

data$Q17_N7

# Ãtes-vous en accord ou en dÃ©saccord avec les Ã©noncÃ©s suivants : - La pandÃ©mie de la COVID-19 est une revanche de la nature sur l'humanitÃ©.

data$Q17_7

# Ãtes-vous en accord ou en dÃ©saccord avec les Ã©noncÃ©s suivants : - La pandÃ©mie de la COVID-19 n'a aucun lien avec les problÃ¨mes environnementaux.

data$Q17_N10
data$Q17_N10_inv <- sondr::finverser(data$Q17_N10)

# Ãtes-vous en accord ou en dÃ©saccord avec les Ã©noncÃ©s suivants : - Au final, seuls les plus forts survivront Ã  la COVID-19.

data$Q17_N11

# Ãtes-vous en accord ou en dÃ©saccord avec les Ã©noncÃ©s suivants : - J'ai confiance que l'Ãªtre humain va trouver des solutions pour sortir plus fort de la pandÃ©mie de COVID19

data$Q17_N12
data$Q17_N12_inv <- sondr::finverser(data$Q17_N12)


# Comment dÃ©cririez-vous votre sentiment gÃ©nÃ©ral concernant la pandÃ©mie de COVID-19? - Totalement pessimiste

data$Q23

# Selon vous, quelles sont les chances que vous contractiez la COVID-19 d'ici la fin de la pandÃ©mie? - Aucune chance

data$Q29
data$Q29_inv <- sondr::finverser(data$Q29)

#  Si vous aviez Ã  prÃ©dire, dans combien de mois la crise de la COVID-19 sera-t-elle terminÃ©e?

data$Q30
data$Q30_inv <- sondr::finverser(data$Q30)

# Ãtes-vous en accord ou en dÃ©saccord avec les Ã©noncÃ©s suivants : - Jusqu'Ã  prÃ©sent, le gouvernement fÃ©dÃ©ral gÃ¨re la pandÃ©mie de la COVID-19 de faÃ§on satisfaisante.

data$Q37_1


data_facanal <- data  %>% 
    select(Q100, N2, Q2B_N8, Q17_3_inv, Q17_N6, Q17_N2, Q17_N3_inv, Q17_N4, Q17_N7, Q17_7, Q17_N10_inv, Q17_N11, Q17_N12_inv, Q23, Q29_inv, Q30_inv, Q37_1) %>% 
    na.omit()


sondr::topdown_fa(data_facanal)
