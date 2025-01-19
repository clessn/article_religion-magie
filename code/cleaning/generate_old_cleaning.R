
#### BEGINNING OF THE DOCUMENT ####

## Library used ##
library(foreign) # For reading Stata files
library(tidyverse)
library(haven)

#********************************#
####_____ 0. LOADING DATA_____####
#********************************#  


## Loading/Reading a Datafile ##

QRM20 <- read.spss("_SharedFolder_article_religion-magie/Data/quorum_1/ULAQUORUM.Sav", to.data.frame=TRUE)

#### 0.1 Creating a clean empty dataframe ####
CleanData <- data.frame(year=seq(2020, 2020, length=nrow(QRM20)))

#### 0.2 Year ####
CleanData$year <- seq(2020, 2020,length=nrow(QRM20))

#### 0.3 Country ####
CleanData$country <- paste0("CAN")


#######

#attributes(QRM20)$variable.labels[109]
#table(QRM20$PROV)

CleanData$caseID <- NA
CleanData$caseID <- QRM20$CASEID

## In which province or territory do you live?
CleanData$alberta <- NA
CleanData$alberta[as.numeric(QRM20$PROV) == 2] <- 1
CleanData$alberta[as.numeric(QRM20$PROV) != 2] <- 0

CleanData$bc <- NA
CleanData$bc[as.numeric(QRM20$PROV) == 3] <- 1
CleanData$bc[as.numeric(QRM20$PROV) != 3] <- 0

CleanData$mb <- NA
CleanData$mb[as.numeric(QRM20$PROV) == 4] <- 1
CleanData$mb[as.numeric(QRM20$PROV) != 4] <- 0

CleanData$nb <- NA
CleanData$nb[as.numeric(QRM20$PROV) == 5] <- 1
CleanData$nb[as.numeric(QRM20$PROV) != 5] <- 0

CleanData$nfld <- NA
CleanData$nfld[as.numeric(QRM20$PROV) == 6] <- 1
CleanData$nfld[as.numeric(QRM20$PROV) != 6] <- 0

CleanData$ns <- NA
CleanData$ns[as.numeric(QRM20$PROV) == 7] <- 1
CleanData$ns[as.numeric(QRM20$PROV) != 7] <- 0

CleanData$nwt <- NA
CleanData$nwt[as.numeric(QRM20$PROV) == 8] <- 1 # none
CleanData$nwt[as.numeric(QRM20$PROV) != 8] <- 0

CleanData$nt <- NA
CleanData$nt[as.numeric(QRM20$PROV) == 9] <- 1 # none
CleanData$nt[as.numeric(QRM20$PROV) != 9] <- 0

CleanData$ontario <- NA
CleanData$ontario[as.numeric(QRM20$PROV) == 10] <- 1
CleanData$ontario[as.numeric(QRM20$PROV) != 10] <- 0

CleanData$quebec <- NA
CleanData$quebec[as.numeric(QRM20$PROV) == 11] <- 1
CleanData$quebec[as.numeric(QRM20$PROV) != 11] <- 0

CleanData$sask <- NA
CleanData$sask[as.numeric(QRM20$PROV) == 5] <- 1
CleanData$sask[as.numeric(QRM20$PROV) != 5] <- 0

CleanData$yukon <- NA
CleanData$yukon[as.numeric(QRM20$PROV) == 5] <- 1
CleanData$yukon[as.numeric(QRM20$PROV) != 5] <- 0

## Region selon code postal

CleanData$basStLaurent <- NA
CleanData$basStLaurent[as.numeric(QRM20$REGIONAD) == 2] <- 1
CleanData$basStLaurent[as.numeric(QRM20$REGIONAD) != 2] <- 0

CleanData$sagLacStJean <- NA
CleanData$sagLacStJean[as.numeric(QRM20$REGIONAD) == 3] <- 1
CleanData$sagLacStJean[as.numeric(QRM20$REGIONAD) != 3] <- 0

CleanData$quebecCity <- NA
CleanData$quebecCity[as.numeric(QRM20$REGIONAD) == 4] <- 1
CleanData$quebecCity[as.numeric(QRM20$REGIONAD) != 4] <- 0

CleanData$mauricie <- NA
CleanData$mauricie[as.numeric(QRM20$REGIONAD) == 5] <- 1
CleanData$mauricie[as.numeric(QRM20$REGIONAD) != 5] <- 0

CleanData$estrie <- NA
CleanData$estrie[as.numeric(QRM20$REGIONAD) == 6] <- 1
CleanData$estrie[as.numeric(QRM20$REGIONAD) != 6] <- 0

CleanData$mtlCity <- NA
CleanData$mtlCity[as.numeric(QRM20$REGIONAD) == 7] <- 1
CleanData$mtlCity[as.numeric(QRM20$REGIONAD) != 7] <- 0

CleanData$outaouais <- NA
CleanData$outaouais[as.numeric(QRM20$REGIONAD) == 8] <- 1
CleanData$outaouais[as.numeric(QRM20$REGIONAD) != 8] <- 0

CleanData$abitibi <- NA
CleanData$abitibi[as.numeric(QRM20$REGIONAD) == 9] <- 1
CleanData$abitibi[as.numeric(QRM20$REGIONAD) != 9] <- 0

CleanData$coteNord <- NA
CleanData$coteNord[as.numeric(QRM20$REGIONAD) == 10] <- 1
CleanData$coteNord[as.numeric(QRM20$REGIONAD) != 10] <- 0

CleanData$nordDuQC <- NA
CleanData$nordDuQC[as.numeric(QRM20$REGIONAD) == 11] <- 1
CleanData$nordDuQC[as.numeric(QRM20$REGIONAD) != 11] <- 0

CleanData$gaspesie <- NA
CleanData$gaspesie[as.numeric(QRM20$REGIONAD) == 12] <- 1
CleanData$gaspesie[as.numeric(QRM20$REGIONAD) != 12] <- 0

CleanData$chaudiereApp <- NA
CleanData$chaudiereApp[as.numeric(QRM20$REGIONAD) == 13] <- 1
CleanData$chaudiereApp[as.numeric(QRM20$REGIONAD) != 13] <- 0

CleanData$laval <- NA
CleanData$laval[as.numeric(QRM20$REGIONAD) == 14] <- 1
CleanData$laval[as.numeric(QRM20$REGIONAD) != 14] <- 0

CleanData$lanaudiere <- NA
CleanData$lanaudiere[as.numeric(QRM20$REGIONAD) == 15] <- 1
CleanData$lanaudiere[as.numeric(QRM20$REGIONAD) != 15] <- 0

CleanData$laurentides <- NA
CleanData$laurentides[as.numeric(QRM20$REGIONAD) == 16] <- 1
CleanData$laurentides[as.numeric(QRM20$REGIONAD) != 16] <- 0

CleanData$laurentides <- NA
CleanData$laurentides[as.numeric(QRM20$REGIONAD) == 17] <- 1
CleanData$laurentides[as.numeric(QRM20$REGIONAD) != 17] <- 0

CleanData$centreDuQC <- NA
CleanData$centreDuQC[as.numeric(QRM20$REGIONAD) == 18] <- 1
CleanData$centreDuQC[as.numeric(QRM20$REGIONAD) != 18] <- 0

# What is your sex?

CleanData$female <- NA
CleanData$female[as.numeric(QRM20$SEXE) == 3] <- 1
CleanData$female[as.numeric(QRM20$SEXE) != 3] <- 0
CleanData$female[as.numeric(QRM20$SEXE) == 4] <- NA

# In what year were you born?

CleanData$age34m <- NA
CleanData$age34m[as.numeric(QRM20$AGE) == 2 | as.numeric(QRM20$AGE) == 3] <- 1
CleanData$age34m[as.numeric(QRM20$AGE) != 2 | as.numeric(QRM20$AGE) != 3] <- 0

CleanData$age35p54 <- NA
CleanData$age35p54[as.numeric(QRM20$AGE) == 4 | as.numeric(QRM20$AGE) == 5] <- 1
CleanData$age35p54[as.numeric(QRM20$AGE) != 4 | as.numeric(QRM20$AGE) != 5] <- 0

CleanData$age34m <- NA
CleanData$age34m[as.numeric(QRM20$AGE) == 6 | as.numeric(QRM20$AGE) == 7] <- 1
CleanData$age34m[as.numeric(QRM20$AGE) != 6 | as.numeric(QRM20$AGE) != 7] <- 0

# What language do you speak most often at home?

CleanData$langFR <- NA
CleanData$langFR[as.numeric(QRM20$LANGU) == 2] <- 1
CleanData$langFR[as.numeric(QRM20$LANGU) == 3] <- 0
CleanData$langFR[as.numeric(QRM20$LANGU) == 4] <- NA

CleanData$langEN <- NA
CleanData$langEN[as.numeric(QRM20$LANGU) == 2] <- 1
CleanData$langEN[as.numeric(QRM20$LANGU) == 3] <- 0
CleanData$langEN[as.numeric(QRM20$LANGU) == 4] <- NA

# What is the most important issue to you personally?
CleanData$issAllEcnMI <- NA
CleanData$issAllEcnMI[as.numeric(QRM20$Q1) == 2 | as.numeric(QRM20$Q1) == 3 | 
                      as.numeric(QRM20$Q1) == 10 | as.numeric(QRM20$Q1) == 12 |
                      as.numeric(QRM20$Q1) == 12 | as.numeric(QRM20$Q1) == 4] <- 1
CleanData$issAllEcnMI[as.numeric(QRM20$Q1) != 2 & as.numeric(QRM20$Q1) != 3 & 
                      as.numeric(QRM20$Q1) != 10 & as.numeric(QRM20$Q1) != 12 &
                      as.numeric(QRM20$Q1) != 12 & as.numeric(QRM20$Q1) != 4] <- 0

CleanData$issEnvMI <- NA
CleanData$issEnvMI[as.numeric(QRM20$Q1) == 8] <- 1
CleanData$issEnvMI[as.numeric(QRM20$Q1) != 8] <- 0

CleanData$issHlthMI <- NA
CleanData$issHlthMI[as.numeric(QRM20$Q1) == 5 | as.numeric(QRM20$Q1) == 15] <- 1
CleanData$issHlthMI[as.numeric(QRM20$Q1) != 5 & as.numeric(QRM20$Q1) != 15] <- 0

CleanData$issAllMoralMI <- NA
CleanData$issAllMoralMI[as.numeric(QRM20$Q1) == 7] <- 1
CleanData$issAllMoralMI[as.numeric(QRM20$Q1) != 7] <- 0

CleanData$issImmgMI <- NA
CleanData$issImmgMI[as.numeric(QRM20$Q1) == 6] <- 1
CleanData$issImmgMI[as.numeric(QRM20$Q1) != 6] <- 0

CleanData$issCrimMI <- NA
CleanData$issCrimMI[as.numeric(QRM20$Q1) == 16] <- 1
CleanData$issCrimMI[as.numeric(QRM20$Q1) != 16] <- 0

CleanData$issAllSocMI <- NA
CleanData$issAllSocMI[as.numeric(QRM20$Q1) == 9 | as.numeric(QRM20$Q1) == 11] <- 1
CleanData$issAllSocMI[as.numeric(QRM20$Q1) != 9 & as.numeric(QRM20$Q1) != 11] <- 0

# Do you agree or disagree with the following statements: - In times of major crisis, 
# opposition parties and the media should temper their criticism of the government.
CleanData$crisisOppTemper <- NA
CleanData$crisisOppTemper[as.numeric(QRM20$Q2A_1) == 2] <- 1
CleanData$crisisOppTemper[as.numeric(QRM20$Q2A_1) == 3] <- 0.75
CleanData$crisisOppTemper[as.numeric(QRM20$Q2A_1) == 4] <- 0.5
CleanData$crisisOppTemper[as.numeric(QRM20$Q2A_1) == 5] <- 0.25
CleanData$crisisOppTemper[as.numeric(QRM20$Q2A_1) == 6] <- 0


# Do you agree or disagree with the following statements: - 
# Even in times of major crisis, debates in Parliament should never be suspended.

CleanData$crisisNoParlSuspend <- NA
CleanData$crisisNoParlSuspend[as.numeric(QRM20$Q2A_2) == 2] <- 1
CleanData$crisisNoParlSuspend[as.numeric(QRM20$Q2A_2) == 3] <- 0.75
CleanData$crisisNoParlSuspend[as.numeric(QRM20$Q2A_2) == 4] <- 0.5
CleanData$crisisNoParlSuspend[as.numeric(QRM20$Q2A_2) == 5] <- 0.25
CleanData$crisisNoParlSuspend[as.numeric(QRM20$Q2A_2) == 6] <- 0

# Do you agree or disagree with the following statements: - Authoritarian regimes manage major crises better.
CleanData$crisisAuthIsBetter <- NA
CleanData$crisisAuthIsBetter[as.numeric(QRM20$Q2A_3) == 2] <- 1
CleanData$crisisAuthIsBetter[as.numeric(QRM20$Q2A_3) == 3] <- 0.75
CleanData$crisisAuthIsBetter[as.numeric(QRM20$Q2A_3) == 4] <- 0.5
CleanData$crisisAuthIsBetter[as.numeric(QRM20$Q2A_3) == 5] <- 0.25
CleanData$crisisAuthIsBetter[as.numeric(QRM20$Q2A_3) == 6] <- 0

# Do you agree or disagree with the following statements: - The law should always be obeyed even if a law is wrong.
CleanData$alwaysObeyLaws <- NA
CleanData$alwaysObeyLaws[as.numeric(QRM20$Q2A_4) == 2] <- 1
CleanData$alwaysObeyLaws[as.numeric(QRM20$Q2A_4) == 3] <- 0.75
CleanData$alwaysObeyLaws[as.numeric(QRM20$Q2A_4) == 4] <- 0.5
CleanData$alwaysObeyLaws[as.numeric(QRM20$Q2A_4) == 5] <- 0.25
CleanData$alwaysObeyLaws[as.numeric(QRM20$Q2A_4) == 6] <- 0

# Do you agree or disagree with the following statements: - In times of major crisis, censorship of journalists should be allowed.
CleanData$crisisMediaCensorOk <- NA
CleanData$crisisMediaCensorOk[as.numeric(QRM20$Q2A_5) == 2] <- 1
CleanData$crisisMediaCensorOk[as.numeric(QRM20$Q2A_5) == 3] <- 0.75
CleanData$crisisMediaCensorOk[as.numeric(QRM20$Q2A_5) == 4] <- 0.5
CleanData$crisisMediaCensorOk[as.numeric(QRM20$Q2A_5) == 5] <- 0.25
CleanData$crisisMediaCensorOk[as.numeric(QRM20$Q2A_5) == 6] <- 0

#Do you agree or disagree with the following statements: - Extreme political opinions should be excluded from public discourse.
CleanData$excludeExtrmOps <- NA
CleanData$excludeExtrmOps[as.numeric(QRM20$Q2A_6) == 2] <- 1
CleanData$excludeExtrmOps[as.numeric(QRM20$Q2A_6) == 3] <- 0.75
CleanData$excludeExtrmOps[as.numeric(QRM20$Q2A_6) == 4] <- 0.5
CleanData$excludeExtrmOps[as.numeric(QRM20$Q2A_6) == 5] <- 0.25
CleanData$excludeExtrmOps[as.numeric(QRM20$Q2A_6) == 6] <- 0

# Do you agree or disagree with the following statements: - All political parties are alike; there is no real choice.
CleanData$allPartiesALike <- NA
CleanData$allPartiesALike[as.numeric(QRM20$Q2A_A7) == 2] <- 1
CleanData$allPartiesALike[as.numeric(QRM20$Q2A_A7) == 3] <- 0.75
CleanData$allPartiesALike[as.numeric(QRM20$Q2A_A7) == 4] <- 0.5
CleanData$allPartiesALike[as.numeric(QRM20$Q2A_A7) == 5] <- 0.25
CleanData$allPartiesALike[as.numeric(QRM20$Q2A_A7) == 6] <- 0

# Do you agree or disagree with the following statements: - People like me have no say in government actions.
CleanData$PPLNoSayGovt <- NA
CleanData$PPLNoSayGovt[as.numeric(QRM20$Q2A_A8) == 2] <- 1
CleanData$PPLNoSayGovt[as.numeric(QRM20$Q2A_A8) == 3] <- 0.75
CleanData$PPLNoSayGovt[as.numeric(QRM20$Q2A_A8) == 4] <- 0.5
CleanData$PPLNoSayGovt[as.numeric(QRM20$Q2A_A8) == 5] <- 0.25
CleanData$PPLNoSayGovt[as.numeric(QRM20$Q2A_A8) == 6] <- 0

# Do you agree or disagree with the following statements: - 
# Most of the time, we can trust that the people in government will do what is right.

CleanData$trustGovtRight <- NA
CleanData$trustGovtRight[as.numeric(QRM20$Q2B_1) == 2] <- 1
CleanData$trustGovtRight[as.numeric(QRM20$Q2B_1) == 3] <- 0.75
CleanData$trustGovtRight[as.numeric(QRM20$Q2B_1) == 4] <- 0.5
CleanData$trustGovtRight[as.numeric(QRM20$Q2B_1) == 5] <- 0.25
CleanData$trustGovtRight[as.numeric(QRM20$Q2B_1) == 6] <- 0

# Do you agree or disagree with the following statements: - Elections are a way of holding governments accountable for their actions.
CleanData$electionsGovtAccount <- NA
CleanData$electionsGovtAccount[as.numeric(QRM20$Q2B_2) == 2] <- 1
CleanData$electionsGovtAccount[as.numeric(QRM20$Q2B_2) == 3] <- 0.75
CleanData$electionsGovtAccount[as.numeric(QRM20$Q2B_2) == 4] <- 0.5
CleanData$electionsGovtAccount[as.numeric(QRM20$Q2B_2) == 5] <- 0.25
CleanData$electionsGovtAccount[as.numeric(QRM20$Q2B_2) == 6] <- 0

# Do you agree or disagree with the following statements: - Most politicians are corrupt.
CleanData$corruptPol <- NA
CleanData$corruptPol[as.numeric(QRM20$Q2B_3) == 2] <- 1
CleanData$corruptPol[as.numeric(QRM20$Q2B_3) == 3] <- 0.75
CleanData$corruptPol[as.numeric(QRM20$Q2B_3) == 4] <- 0.5
CleanData$corruptPol[as.numeric(QRM20$Q2B_3) == 5] <- 0.25
CleanData$corruptPol[as.numeric(QRM20$Q2B_3) == 6] <- 0

# Do you agree or disagree with the following statements: - Most politicians are controlled by lobbyists.
CleanData$lobbyCntrlPol <- NA
CleanData$lobbyCntrlPol[as.numeric(QRM20$Q2B_4) == 2] <- 1
CleanData$lobbyCntrlPol[as.numeric(QRM20$Q2B_4) == 3] <- 0.75
CleanData$lobbyCntrlPol[as.numeric(QRM20$Q2B_4) == 4] <- 0.5
CleanData$lobbyCntrlPol[as.numeric(QRM20$Q2B_4) == 5] <- 0.25
CleanData$lobbyCntrlPol[as.numeric(QRM20$Q2B_4) == 6] <- 0

# Do you agree or disagree with the following statements: - Limiting our rights and freedoms is necessary for public health protection.
CleanData$hlthB4Rights <- NA
CleanData$hlthB4Rights[as.numeric(QRM20$Q2B_5) == 2] <- 1
CleanData$hlthB4Rights[as.numeric(QRM20$Q2B_5) == 3] <- 0.75
CleanData$hlthB4Rights[as.numeric(QRM20$Q2B_5) == 4] <- 0.5
CleanData$hlthB4Rights[as.numeric(QRM20$Q2B_5) == 5] <- 0.25
CleanData$hlthB4Rights[as.numeric(QRM20$Q2B_5) == 6] <- 0

# Do you agree or disagree with the following statements: - 
# Businesses should have the right to check the temperature of their customers before letting them in.
CleanData$busiCheckTemp <- NA
CleanData$busiCheckTemp[as.numeric(QRM20$Q2B_6) == 2] <- 1
CleanData$busiCheckTemp[as.numeric(QRM20$Q2B_6) == 3] <- 0.75
CleanData$busiCheckTemp[as.numeric(QRM20$Q2B_6) == 4] <- 0.5
CleanData$busiCheckTemp[as.numeric(QRM20$Q2B_6) == 5] <- 0.25
CleanData$busiCheckTemp[as.numeric(QRM20$Q2B_6) == 6] <- 0

# Do you agree or disagree with the following statements: - All those infected with COVID-19 
# should be forced to use a tracing device to help authorities monitor the spread of the outbr
CleanData$covidTrackers <- NA
CleanData$covidTrackers[as.numeric(QRM20$Q2B_7) == 2] <- 1
CleanData$covidTrackers[as.numeric(QRM20$Q2B_7) == 3] <- 0.75
CleanData$covidTrackers[as.numeric(QRM20$Q2B_7) == 4] <- 0.5
CleanData$covidTrackers[as.numeric(QRM20$Q2B_7) == 5] <- 0.25
CleanData$covidTrackers[as.numeric(QRM20$Q2B_7) == 6] <- 0

# Do you agree or disagree with the following statements: - 
# People infected with COVID-19 should share any personal information considered useful by the authorities.
CleanData$covidPersoInfo <- NA
CleanData$covidPersoInfo[as.numeric(QRM20$Q2B_8) == 2] <- 1
CleanData$covidPersoInfo[as.numeric(QRM20$Q2B_8) == 3] <- 0.75
CleanData$covidPersoInfo[as.numeric(QRM20$Q2B_8) == 4] <- 0.5
CleanData$covidPersoInfo[as.numeric(QRM20$Q2B_8) == 5] <- 0.25
CleanData$covidPersoInfo[as.numeric(QRM20$Q2B_8) == 6] <- 0

# How many immigrants should Canada admit?
CleanData$issImmgAdmit <- NA
CleanData$issImmgAdmit[as.numeric(QRM20$Q4) == 6] <- 1 # More
CleanData$issImmgAdmit[as.numeric(QRM20$Q4) == 5] <- 0.75
CleanData$issImmgAdmit[as.numeric(QRM20$Q4) == 4] <- 0.5
CleanData$issImmgAdmit[as.numeric(QRM20$Q4) == 3] <- 0.25
CleanData$issImmgAdmit[as.numeric(QRM20$Q4) == 2] <- 0 # Less

# Generally, how satisfied are you with the way democracy works in Canada?
# Bug in this question 
CleanData$demSat <- NA

# Do you agree or disagree with the following statement:<br><br>No crisis can justify the limitation of democratic rights.
CleanData$crisisCantJustLimitRights <- NA
CleanData$crisisCantJustLimitRights[as.numeric(QRM20$Q6) == 2] <- 1 # Agree
CleanData$crisisCantJustLimitRights[as.numeric(QRM20$Q6) == 3] <- 0.75
CleanData$crisisCantJustLimitRights[as.numeric(QRM20$Q6) == 4] <- 0.5
CleanData$crisisCantJustLimitRights[as.numeric(QRM20$Q6) == 5] <- 0.25
CleanData$crisisCantJustLimitRights[as.numeric(QRM20$Q6) == 6] <- 0 # Disagree

# Would you be ready to call the authorities if someone in your neighbourhood did not comply with public health directives?
CleanData$snitchAbtHlth <- NA
CleanData$snitchAbtHlth[as.numeric(QRM20$Q7) == 1] <- 1
CleanData$snitchAbtHlth[as.numeric(QRM20$Q7) == 3] <- 0.5
CleanData$snitchAbtHlth[as.numeric(QRM20$Q7) == 2] <- 0

# How informed about digital tracing applications would you consider yourself?
CleanData$informedDTA <- as.numeric(QRM20$Q8)/10

# How familiar are you with the concept of artificial intelligence (AI)?
CleanData$informedAI <- NA
CleanData$informedAI[as.numeric(QRM20$Q9) == 2] <- 1 # A lot
CleanData$informedAI[as.numeric(QRM20$Q9) == 3] <- 0.66
CleanData$informedAI[as.numeric(QRM20$Q9) == 4] <- 0.33
CleanData$informedAI[as.numeric(QRM20$Q9) == 5] <- 0 # Not at all

# Do you agree or disagree with the following statements: - 
# I will never give up my right to confidentiality even if it jeopardized my health." 
# Question lacking answers
CleanData$privacyB4Hlth <- NA


# "Do you agree or disagree with the following statements: - 
# The people and organizations that develop computational artificial intelligence can be trusted." 
CleanData$AITrustOrgPpl <- NA
CleanData$AITrustOrgPpl[as.numeric(QRM20$Q10A_A2) == 2] <- 1 # Agree
CleanData$AITrustOrgPpl[as.numeric(QRM20$Q10A_A2) == 3] <- 0.75
CleanData$AITrustOrgPpl[as.numeric(QRM20$Q10A_A2) == 4] <- 0.5
CleanData$AITrustOrgPpl[as.numeric(QRM20$Q10A_A2) == 5] <- 0.25
CleanData$AITrustOrgPpl[as.numeric(QRM20$Q10A_A2) == 6] <- 0 # Disagree

# Do you agree or disagree with the following statements: - 
# I would hate the idea of computational artifical intelligence making judgments about political decisions.
CleanData$AIPolDecisions <- NA
CleanData$AIPolDecisions[as.numeric(QRM20$Q10A_A3) == 2] <- 1 # Agree
CleanData$AIPolDecisions[as.numeric(QRM20$Q10A_A3) == 3] <- 0.75
CleanData$AIPolDecisions[as.numeric(QRM20$Q10A_A3) == 4] <- 0.5
CleanData$AIPolDecisions[as.numeric(QRM20$Q10A_A3) == 5] <- 0.25
CleanData$AIPolDecisions[as.numeric(QRM20$Q10A_A3) == 6] <- 0 # Disagree


# "Do you agree or disagree with the following statements: -
# In the future, society will be dominated by computational artificial intelligence." 
# Question lacking answers
CleanData$AIDomSociety <- NA

# "Do you agree or disagree with the following statements: - I fear that one day, humans will be under the control of computational artificial intelligence." 
CleanData$AIDomHumans <- NA
CleanData$AIDomHumans[as.numeric(QRM20$Q10A_A5) == 2] <- 1 # Agree
CleanData$AIDomHumans[as.numeric(QRM20$Q10A_A5) == 3] <- 0.75
CleanData$AIDomHumans[as.numeric(QRM20$Q10A_A5) == 4] <- 0.5
CleanData$AIDomHumans[as.numeric(QRM20$Q10A_A5) == 5] <- 0.25
CleanData$AIDomHumans[as.numeric(QRM20$Q10A_A5) == 6] <- 0 # Disagree

# "Do you agree or disagree with the following statements: - I take great care to protect my personal data." 
CleanData$protectPersoData <- NA
CleanData$protectPersoData[as.numeric(QRM20$Q10A_A6) == 2] <- 1 # Agree
CleanData$protectPersoData[as.numeric(QRM20$Q10A_A6) == 3] <- 0.75
CleanData$protectPersoData[as.numeric(QRM20$Q10A_A6) == 4] <- 0.5
CleanData$protectPersoData[as.numeric(QRM20$Q10A_A6) == 5] <- 0.25
CleanData$protectPersoData[as.numeric(QRM20$Q10A_A6) == 6] <- 0 # Disagree

# "Do you agree or disagree with the following statements: -
# Technology will weaken the core aspects of democracy in the next decade." 
CleanData$demWeakenByTech <- NA
CleanData$demWeakenByTech[as.numeric(QRM20$Q10A_A7) == 2] <- 1 # Agree
CleanData$demWeakenByTech[as.numeric(QRM20$Q10A_A7) == 3] <- 0.75
CleanData$demWeakenByTech[as.numeric(QRM20$Q10A_A7) == 4] <- 0.5
CleanData$demWeakenByTech[as.numeric(QRM20$Q10A_A7) == 5] <- 0.25
CleanData$demWeakenByTech[as.numeric(QRM20$Q10A_A7) == 6] <- 0 # Disagree

# "Do you agree or disagree with the following statements: -
# I know enough about artificial intelligence to understand its potential impact on my private life." 
CleanData$AIImpactPrivLife <- NA
CleanData$AIImpactPrivLife[as.numeric(QRM20$Q10A_A8) == 2] <- 1 # Agree
CleanData$AIImpactPrivLife[as.numeric(QRM20$Q10A_A8) == 3] <- 0.75
CleanData$AIImpactPrivLife[as.numeric(QRM20$Q10A_A8) == 4] <- 0.5
CleanData$AIImpactPrivLife[as.numeric(QRM20$Q10A_A8) == 5] <- 0.25
CleanData$AIImpactPrivLife[as.numeric(QRM20$Q10A_A8) == 6] <- 0 # Disagree

# "Do you agree or disagree with the following statements: - 
# It is more important to form your own ideas than to learn from what scientists say." 
CleanData$freeThinkBetter <- NA
CleanData$freeThinkBetter[as.numeric(QRM20$Q10A_A9) == 2] <- 1 # Agree
CleanData$freeThinkBetter[as.numeric(QRM20$Q10A_A9) == 3] <- 0.75
CleanData$freeThinkBetter[as.numeric(QRM20$Q10A_A9) == 4] <- 0.5
CleanData$freeThinkBetter[as.numeric(QRM20$Q10A_A9) == 5] <- 0.25
CleanData$freeThinkBetter[as.numeric(QRM20$Q10A_A9) == 6] <- 0 # Disagree

# "Do you agree or disagree with the following statements: 
# - Personalized horoscopes created by professionals can accurately predict the future." 
# Question lacking answers
CleanData$horoscopePredictFuture <- NA

# "Do you agree or disagree with the following statements: 
# - Science always provides better answers to problems than personal intuition." 

CleanData$scieBttrThanIntuition <- NA
CleanData$scieBttrThanIntuition[as.numeric(QRM20$Q10B_A2) == 2] <- 1 # Agree
CleanData$scieBttrThanIntuition[as.numeric(QRM20$Q10B_A2) == 3] <- 0.75
CleanData$scieBttrThanIntuition[as.numeric(QRM20$Q10B_A2) == 4] <- 0.5
CleanData$scieBttrThanIntuition[as.numeric(QRM20$Q10B_A2) == 5] <- 0.25
CleanData$scieBttrThanIntuition[as.numeric(QRM20$Q10B_A2) == 6] <- 0 # Disagree

# "Do you agree or disagree with the following statements: 
# - Things are simpler than most scientific experts would have you believe." 
# Question lacking answers
CleanData$thingsSimplerThanSciSays <- NA

# "Do you agree or disagree with the following statements: - 
# I am very concerned with technological changes like computational artificial intelligence." 
CleanData$AIConcernsMe <- NA
CleanData$AIConcernsMe[as.numeric(QRM20$Q10B_A4) == 2] <- 1 # Agree
CleanData$AIConcernsMe[as.numeric(QRM20$Q10B_A4) == 3] <- 0.75
CleanData$AIConcernsMe[as.numeric(QRM20$Q10B_A4) == 4] <- 0.5
CleanData$AIConcernsMe[as.numeric(QRM20$Q10B_A4) == 5] <- 0.25
CleanData$AIConcernsMe[as.numeric(QRM20$Q10B_A4) == 6] <- 0 # Disagree

# "Do you agree or disagree with the following statements: -
# I fear that computational artificial intelligence will attack and harm humans." 
CleanData$AIHurtHumans <- NA
CleanData$AIHurtHumans[as.numeric(QRM20$Q10B_A5) == 2] <- 1 # Agree
CleanData$AIHurtHumans[as.numeric(QRM20$Q10B_A5) == 3] <- 0.75
CleanData$AIHurtHumans[as.numeric(QRM20$Q10B_A5) == 4] <- 0.5
CleanData$AIHurtHumans[as.numeric(QRM20$Q10B_A5) == 5] <- 0.25
CleanData$AIHurtHumans[as.numeric(QRM20$Q10B_A5) == 6] <- 0 # Disagree

# "Do you agree or disagree with the following statements: -
# Widespread use of computational artificial intelligence would take away jobs from people." 
# Question lacking answers
CleanData$AITakeJobs <- NA

# "Do you agree or disagree with the following statements: - 
# Reliance on technology lowers the ability of people to think on their own." 
CleanData$relianceTechNoThinkOwn <- NA
CleanData$relianceTechNoThinkOwn[as.numeric(QRM20$Q10B_A7) == 2] <- 1 # Agree
CleanData$relianceTechNoThinkOwn[as.numeric(QRM20$Q10B_A7) == 3] <- 0.75
CleanData$relianceTechNoThinkOwn[as.numeric(QRM20$Q10B_A7) == 4] <- 0.5
CleanData$relianceTechNoThinkOwn[as.numeric(QRM20$Q10B_A7) == 5] <- 0.25
CleanData$relianceTechNoThinkOwn[as.numeric(QRM20$Q10B_A7) == 6] <- 0 # Disagree

# "Do you agree or disagree with the following statements: -
# Artificial intelligence will one day surpass all aspects of human intelligence." 
CleanData$AISurpassHI <- NA
CleanData$AISurpassHI[as.numeric(QRM20$Q10B_A8) == 2] <- 1 # Agree
CleanData$AISurpassHI[as.numeric(QRM20$Q10B_A8) == 3] <- 0.75
CleanData$AISurpassHI[as.numeric(QRM20$Q10B_A8) == 4] <- 0.5
CleanData$AISurpassHI[as.numeric(QRM20$Q10B_A8) == 5] <- 0.25
CleanData$AISurpassHI[as.numeric(QRM20$Q10B_A8) == 6] <- 0 # Disagree


# "Do you agree or disagree with the following statements: - Other people come to me for advice on new technologies." 
CleanData$helpPplNewTech <- NA
CleanData$helpPplNewTech[as.numeric(QRM20$Q10B_A9) == 2] <- 1 # Agree
CleanData$helpPplNewTech[as.numeric(QRM20$Q10B_A9) == 3] <- 0.75
CleanData$helpPplNewTech[as.numeric(QRM20$Q10B_A9) == 4] <- 0.5
CleanData$helpPplNewTech[as.numeric(QRM20$Q10B_A9) == 5] <- 0.25
CleanData$helpPplNewTech[as.numeric(QRM20$Q10B_A9) == 6] <- 0 
# 
# # "Do you agree or disagree with the following statements: - New technologies contribute to a better quality of life." 
# CleanData$newTechGood <- NA
# CleanData$newTechGood[as.numeric(QRM20$Q12_1) == 2] <- 1 # Agree
# CleanData$newTechGood[as.numeric(QRM20$Q12_1) == 3] <- 0.75
# CleanData$newTechGood[as.numeric(QRM20$Q12_1) == 4] <- 0.5
# CleanData$newTechGood[as.numeric(QRM20$Q12_1) == 5] <- 0.25
# CleanData$newTechGood[as.numeric(QRM20$Q12_1) == 6] <- 0 
# 
# #"Do you agree or disagree with the following statements: - 
# # If it's necessary for the sake of public health, the sharing of personal geolocation data should be mandatory for all." 
# CleanData$geolocMandatory <- NA
# CleanData$geolocMandatory[as.numeric(QRM20$Q12_2) == 2] <- 1 # Agree
# CleanData$geolocMandatory[as.numeric(QRM20$Q12_2) == 3] <- 0.75
# CleanData$geolocMandatory[as.numeric(QRM20$Q12_2) == 4] <- 0.5
# CleanData$geolocMandatory[as.numeric(QRM20$Q12_2) == 5] <- 0.25
# CleanData$geolocMandatory[as.numeric(QRM20$Q12_2) == 6] <- 0 
# 
# #

# "How irritated do you feel these days?" 
CleanData$feelIrritated <- NA
CleanData$feelIrritated[as.numeric(QRM20$Q20) == 2] <- 1
CleanData$feelIrritated[as.numeric(QRM20$Q20) == 3] <- 0.66
CleanData$feelIrritated[as.numeric(QRM20$Q20) == 4] <- 0.33
CleanData$feelIrritated[as.numeric(QRM20$Q20) == 5] <- 0

# "Thinking about your own future, how concerned are you about your financial situation?" 
CleanData$concernPersoFinance <- NA 
CleanData$concernPersoFinance[as.numeric(QRM20$Q21) == 1] <- 0
CleanData$concernPersoFinance[as.numeric(QRM20$Q21) == 2] <- 0.1
CleanData$concernPersoFinance[as.numeric(QRM20$Q21) == 3] <- 0.2
CleanData$concernPersoFinance[as.numeric(QRM20$Q21) == 4] <- 0.3
CleanData$concernPersoFinance[as.numeric(QRM20$Q21) == 5] <- 0.4
CleanData$concernPersoFinance[as.numeric(QRM20$Q21) == 6] <- 0.5
CleanData$concernPersoFinance[as.numeric(QRM20$Q21) == 7] <- 0.6
CleanData$concernPersoFinance[as.numeric(QRM20$Q21) == 8] <- 0.7
CleanData$concernPersoFinance[as.numeric(QRM20$Q21) == 9] <- 0.8
CleanData$concernPersoFinance[as.numeric(QRM20$Q21) == 10] <- 0.9
CleanData$concernPersoFinance[as.numeric(QRM20$Q21) == 11] <- 1

# "In your everyday life, how concerned are you about global warming?"
CleanData$concernGlobalWarm[as.numeric(QRM20$Q22) == 1] <- 0
CleanData$concernGlobalWarm[as.numeric(QRM20$Q22) == 2] <- 0.1
CleanData$concernGlobalWarm[as.numeric(QRM20$Q22) == 3] <- 0.2
CleanData$concernGlobalWarm[as.numeric(QRM20$Q22) == 4] <- 0.3
CleanData$concernGlobalWarm[as.numeric(QRM20$Q22) == 5] <- 0.4
CleanData$concernGlobalWarm[as.numeric(QRM20$Q22) == 6] <- 0.5
CleanData$concernGlobalWarm[as.numeric(QRM20$Q22) == 7] <- 0.6
CleanData$concernGlobalWarm[as.numeric(QRM20$Q22) == 8] <- 0.7
CleanData$concernGlobalWarm[as.numeric(QRM20$Q22) == 9] <- 0.8
CleanData$concernGlobalWarm[as.numeric(QRM20$Q22) == 10] <- 0.9
CleanData$concernGlobalWarm[as.numeric(QRM20$Q22) == 11] <- 1

# "How would you describe your general feelings regarding the current COVID-19 pandemic?" 
CleanData$covid19Feel[as.numeric(QRM20$Q23) == 1] <- 0
CleanData$covid19Feel[as.numeric(QRM20$Q23) == 2] <- 0.1
CleanData$covid19Feel[as.numeric(QRM20$Q23) == 3] <- 0.2
CleanData$covid19Feel[as.numeric(QRM20$Q23) == 4] <- 0.3
CleanData$covid19Feel[as.numeric(QRM20$Q23) == 5] <- 0.4
CleanData$covid19Feel[as.numeric(QRM20$Q23) == 6] <- 0.5
CleanData$covid19Feel[as.numeric(QRM20$Q23) == 7] <- 0.6
CleanData$covid19Feel[as.numeric(QRM20$Q23) == 8] <- 0.7
CleanData$covid19Feel[as.numeric(QRM20$Q23) == 9] <- 0.8
CleanData$covid19Feel[as.numeric(QRM20$Q23) == 10] <- 0.9
CleanData$covid19Feel[as.numeric(QRM20$Q23) == 11] <- 1

# "To what extent has your life been affected by the COVID-19 pandemic?" 
CleanData$covid19Affected[as.numeric(QRM20$Q24) == 1] <- 0
CleanData$covid19Affected[as.numeric(QRM20$Q24) == 2] <- 0.1
CleanData$covid19Affected[as.numeric(QRM20$Q24) == 3] <- 0.2
CleanData$covid19Affected[as.numeric(QRM20$Q24) == 4] <- 0.3
CleanData$covid19Affected[as.numeric(QRM20$Q24) == 5] <- 0.4
CleanData$covid19Affected[as.numeric(QRM20$Q24) == 6] <- 0.5
CleanData$covid19Affected[as.numeric(QRM20$Q24) == 7] <- 0.6
CleanData$covid19Affected[as.numeric(QRM20$Q24) == 8] <- 0.7
CleanData$covid19Affected[as.numeric(QRM20$Q24) == 9] <- 0.8
CleanData$covid19Affected[as.numeric(QRM20$Q24) == 10] <- 0.9
CleanData$covid19Affected[as.numeric(QRM20$Q24) == 11] <- 1

# "To what extent do you respect the lockdown rules?" 
CleanData$covid19RspctRules <- NA
CleanData$covid19RspctRules[as.numeric(QRM20$Q25) == 2] <- 1
CleanData$covid19RspctRules[as.numeric(QRM20$Q25) == 3] <- 0.66
CleanData$covid19RspctRules[as.numeric(QRM20$Q25) == 4] <- 0.33
CleanData$covid19RspctRules[as.numeric(QRM20$Q25) == 5] <- 0

# "Do you think that a year from now... - your province's economy will be better off, worse off, or about the same as now?" 
CleanData$provEcnBORW <- NA
CleanData$provEcnBORW[as.numeric(QRM20$Q26_1) == 2] <- 1
CleanData$provEcnBORW[as.numeric(QRM20$Q26_1) == 3] <- 0.5
CleanData$provEcnBORW[as.numeric(QRM20$Q26_1) == 4] <- 0

#"Do you think that a year from now... - your financial situation will be better, worse or about the same as it is today?" 
CleanData$persoEcnBORW <- NA
CleanData$persoEcnBORW[as.numeric(QRM20$Q26_2) == 2] <- 1
CleanData$persoEcnBORW[as.numeric(QRM20$Q26_2) == 3] <- 0.5
CleanData$persoEcnBORW[as.numeric(QRM20$Q26_1) == 4] <- 0

# "Over the next five years, do you think that the average standard of living for
# citizens in your province will rise, or fall, or stay about the same?" 
# Question lacking answers

# In your opinion, what are the chances that you will contract COVID-19 by the end of the pandemic?
CleanData$covid19Contract[as.numeric(QRM20$Q29) == 1] <- 0
CleanData$covid19Contract[as.numeric(QRM20$Q29) == 2] <- 0.1
CleanData$covid19Contract[as.numeric(QRM20$Q29) == 3] <- 0.2
CleanData$covid19Contract[as.numeric(QRM20$Q29) == 4] <- 0.3
CleanData$covid19Contract[as.numeric(QRM20$Q29) == 5] <- 0.4
CleanData$covid19Contract[as.numeric(QRM20$Q29) == 6] <- 0.5
CleanData$covid19Contract[as.numeric(QRM20$Q29) == 7] <- 0.6
CleanData$covid19Contract[as.numeric(QRM20$Q29) == 8] <- 0.7
CleanData$covid19Contract[as.numeric(QRM20$Q29) == 9] <- 0.8
CleanData$covid19Contract[as.numeric(QRM20$Q29) == 10] <- 0.9
CleanData$covid19Contract[as.numeric(QRM20$Q29) == 11] <- 1

# "Make a prediction: in how many days do you think the COVID-19 crisis will be over?" 
CleanData$covid19DaysLeft <- NA
CleanData$covid19DaysLeft <- QRM20$Q30

# "How often do you follow the following recommendations?&nbsp; - Lockdown to contain COVID-19." 
# Question lacking answers

saveRDS(CleanData, "_SharedFolder_article_religion-magie/Data/data_clean/old_cleaning.rds")
