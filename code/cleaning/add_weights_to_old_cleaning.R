#### 0. Loading packages ####
library(tidyverse) # for dplyr
library(anesrake) # for raking using the ANES's method

#### 1. Loading data ####
Civimetre1 <- readRDS("_SharedFolder_article_religion-magie/Data/data_clean/old_cleaning.rds")

#### 2. Loading functions ####
source("code/functions/old_quorum_weights.R")

#### 3. Creating and rescaling relevant variables ####
#### 3.1 Pessimist-optimist scale ####
Civimetre1$sante_pub_pessimism_2 <- NA
Civimetre1$sante_pub_pessimism_2[Civimetre1$sante_pub_pessimism > 0] <- 0
Civimetre1$sante_pub_pessimism_2[Civimetre1$sante_pub_pessimism == 0] <- 1
# I am not particularly discouraged about the future
Civimetre1$sante_pub_covid19Contract_rev <- 1 - Civimetre1$sante_pub_covid19Contract
# In your opinion, what are the chances that you will contract COVID-19 by the end of the pandemic?
# (reversed)
Civimetre1$scale_PessimistOptimist <- c(
  Civimetre1$sante_pub_pessimism_2 + Civimetre1$sante_pub_covid19Contract_rev +
    Civimetre1$sante_pub_afraidDying + # I am not particularly afraid of dying of COVID-19.
    Civimetre1$sante_pub_covid19Feel) / 4 # How would you describe your general feelings regarding
# the current COVID-19 pandemic? 0 = most pessimistic, 1 = most optimistic

#### 3.2 Technocritical-technophile scale ####
Civimetre1$scale_TechnocriticalTechnophile <- c(
  (1 - Civimetre1$sci_tech_AIConcernsMe) + (1 - Civimetre1$sci_tech_AIHurtHumans) +
    (1 - Civimetre1$dem_tech_demWeakenByTech)) / 3 # 0 = most technocritical, 1 = most technophile

#### 3.3 Contentious-disciplined scale ####
Civimetre1$scale_ContentiousDisciplined <- c(
  (1 - Civimetre1$ia_sante_dem_covidPersoInfo) + (1 - Civimetre1$ia_sante_dem_hlthB4Rights) +
    (1 - Civimetre1$geolocMandatory)) / 3 # 0 = most contentious, 1 = most disciplined

#### 3.4 Authoritarian-pluralist scale ####
Civimetre1$scale_AuthoritarianPluralist <- c(
  (1 - Civimetre1$dem_excludeExtrmOps) + (1 - Civimetre1$dem_crisisOppTemper) +
    (1 - Civimetre1$dem_crisisMediaCensorOk)) / 3 # 0 = most authoritarian, 1 = most pluralistic

#### 3.5 Creating unified socio-demographic variables ####
Civimetre1 <- Civimetre1 %>% mutate(
  # technocritical-technophile scale
  scale_TechnocriticalTechnophile_2cat = case_when(
    scale_TechnocriticalTechnophile < 0.5 ~ 0,
    scale_TechnocriticalTechnophile >= 0.5 ~ 1),
  # contentious-disciplined scale
  scale_ContentiousDisciplined_2cat = case_when(
    scale_ContentiousDisciplined < 0.5 ~ 0,
    scale_ContentiousDisciplined >= 0.5 ~ 1),
  # authoritarian-pluralist scale
  scale_AuthoritarianPluralist_2cat = case_when(
    scale_AuthoritarianPluralist < 0.5 ~ 0,
    scale_AuthoritarianPluralist >= 0.5 ~ 1),
  # pessimism-optimism scale
  scale_PessimistOptimist_2cat = case_when(
    scale_PessimistOptimist < 0.5 ~ 0,
    scale_PessimistOptimist >= 0.5 ~ 1),
  # extroversion scale
  ses_extroverted_enthusiastic_2cat = case_when(
    ses_extroverted_enthusiastic < 0.5 ~ 0,
    ses_extroverted_enthusiastic >= 0.5 ~ 1),
  # health scale
  ses_health_2cat = case_when(
    ses_health < 0.5 ~ 0,
    ses_health >= 0.5 ~ 1),
  # sex
  ses_female = case_when(ses_female == 0 ~ 1,
                         ses_female == 1 ~ 2),
  # age
  ses_age = case_when(ses_age34m   == 1 ~ 1,
                      ses_age35p54 == 1 ~ 2,
                      ses_age55p   == 1 ~ 3),
  # language
  ses_lang = case_when(ses_langFR    == 1 ~ 1,
                       ses_langEN    == 1 ~ 2,
                       ses_langOther == 1 ~ 3),
  # education
  ses_educ = case_when(ses_educ_BHS        == 1 ~ 1,
                       ses_educ_coll       == 1 ~ 2,
                       ses_educ_univBA     == 1 ~ 3, # merge both university education categories
                       ses_educ_univOverBA == 1 ~ 3),
  # income
  ses_income = case_when(ses_income_low  == 1 ~ 1,
                         ses_income_mid  == 1 ~ 2,
                         ses_income_high == 1 ~ 3,
                         is.na(ses_income_high) ~ 4)) # assign missing values to value 4
Civimetre1Alt <- Civimetre1 %>% select(ses_female, ses_age, ses_lang, ses_educ, ses_income,
                                    scale_PessimistOptimist)
saveRDS(Civimetre1Alt, "_SharedFolder_article_religion-magie/Data/data_clean/old_cleaning_civimetre_weights.rds")
#### 4. Create biased subsets ####
#### 4.1 Very technophile subset ####
VeryTechnophileSubset <- createBiasedSubset_2cat(
  data = Civimetre1,
  variable = scale_TechnocriticalTechnophile_2cat,
  value1 = 0, chosenProp1 = .05,
  value2 = 1, chosenProp2 = .5)

#### 4.2 Very contentious subset ####
VeryContentiousSubset <- createBiasedSubset_2cat(
  data = Civimetre1,
  variable = scale_ContentiousDisciplined_2cat,
  value1 = 0, chosenProp1 = .5,
  value2 = 1, chosenProp2 = .05)

#### 4.3 Very authoritarian subset ####
VeryAuthoritarianSubset <- createBiasedSubset_2cat(
  data = Civimetre1,
  variable = scale_AuthoritarianPluralist_2cat,
  value1 = 0, chosenProp1 = .5,
  value2 = 1, chosenProp2 = .05)

#### 4.4 Very religious subset ####
VeryReligiousSubset <- createBiasedSubset_2cat(
  data = Civimetre1,
  variable = ses_rel,
  value1 = 0, chosenProp1 = .05,
  value2 = 1, chosenProp2 = .5)

#### 4.5 Very extrovert subset ####
VeryExtrovertSubset <- createBiasedSubset_2cat(
  data = Civimetre1,
  variable = ses_extroverted_enthusiastic_2cat,
  value1 = 0, chosenProp1 = .05,
  value2 = 1, chosenProp2 = .5)

#### 4.6 Very healthy subset ####
VeryHealthySubset <- createBiasedSubset_2cat(
  data = Civimetre1,
  variable = ses_health_2cat,
  value1 = 0, chosenProp1 = .05,
  value2 = 1, chosenProp2 = .5)

#### 4.7 Very feminine subset ####
VeryFeminineSubset <- createBiasedSubset_2cat(
  data = Civimetre1,
  variable = ses_female,
  value1 = 1, chosenProp1 = .05,
  value2 = 2, chosenProp2 = .5)

#### 4.7 Very pessimist subset ####
VeryPessimistSubset <- createBiasedSubset_2cat(
  data = Civimetre1,
  variable = scale_PessimistOptimist_2cat,
  value1 = 0, chosenProp1 = .5,
  value2 = 1, chosenProp2 = .05)

#### 4.8 Very educated subset ####
VeryEducatedSubset <- createBiasedSubset_3cat(
  data = Civimetre1,
  variable = ses_educ,
  value1 = 1, chosenProp1 = .3,
  value2 = 2, chosenProp2 = .5,
  value3 = 3, chosenProp3 = .7)

#### 5. Raking ####
#### 5.1 Very technophile subset ####
VeryTechnophileSubset <- addRakingWeightsColumn_5var(
  popData = Civimetre1,
  sampleData = VeryTechnophileSubset,
  variable1 = ses_female, # calculate weights based on sex, education, language, age and income
  variable2 = ses_educ,
  variable3 = ses_lang,
  variable4 = ses_age,
  variable5 = ses_income)

#### 5.2 Very contentious subset ####
VeryContentiousSubset <- addRakingWeightsColumn_5var(
  popData = Civimetre1,
  sampleData = VeryContentiousSubset,
  variable1 = ses_female, # calculate weights based on sex, education, language, age and income
  variable2 = ses_educ,
  variable3 = ses_lang,
  variable4 = ses_age,
  variable5 = ses_income)

#### 5.3 Very authoritarian subset ####
VeryAuthoritarianSubset <- addRakingWeightsColumn_5var(
  popData = Civimetre1,
  sampleData = VeryAuthoritarianSubset,
  variable1 = ses_female, # calculate weights based on sex, education, language, age and income
  variable2 = ses_educ,
  variable3 = ses_lang,
  variable4 = ses_age,
  variable5 = ses_income)

#### 5.4 Very religious subset ####
VeryReligiousSubset <- addRakingWeightsColumn_5var(
  popData = Civimetre1,
  sampleData = VeryReligiousSubset,
  variable1 = ses_female, # calculate weights based on sex, education, language, age and income
  variable2 = ses_educ,
  variable3 = ses_lang,
  variable4 = ses_age,
  variable5 = ses_income)

#### 5.5 Very extrovert subset ####
VeryExtrovertSubset <- addRakingWeightsColumn_5var(
  popData = Civimetre1,
  sampleData = VeryExtrovertSubset,
  variable1 = ses_female, # calculate weights based on sex, education, language, age and income
  variable2 = ses_educ,
  variable3 = ses_lang,
  variable4 = ses_age,
  variable5 = ses_income)

#### 5.6 Very healthy subset ####
VeryHealthySubset <- addRakingWeightsColumn_5var(
  popData = Civimetre1,
  sampleData = VeryHealthySubset,
  variable1 = ses_female, # calculate weights based on sex, education, language, age and income
  variable2 = ses_educ,
  variable3 = ses_lang,
  variable4 = ses_age,
  variable5 = ses_income)

#### 5.7 Very feminine subset ####
VeryFeminineSubsetSES <- addRakingWeightsColumn_5var(
  popData = Civimetre1,
  sampleData = VeryFeminineSubset,
  variable1 = ses_female, # calculate weights based on sex, education, language, age and income
  variable2 = ses_educ,
  variable3 = ses_lang,
  variable4 = ses_age,
  variable5 = ses_income)

#### 5.7.1 Very feminine subset biased on health ####
VeryFeminineSubsetHealth <- addRakingWeightsColumn_5var(
  popData = Civimetre1,
  sampleData = VeryFeminineSubset,
  variable1 = ses_female, # calculate weights based on sex, education, health, age and income
  variable2 = ses_educ,
  variable3 = ses_health_2cat,
  variable4 = ses_age,
  variable5 = ses_income)

#### 5.8 Very pessimist subset ####
VeryPessimistSubset <- addRakingWeightsColumn_5var(
  popData = Civimetre1,
  sampleData = VeryPessimistSubset,
  variable1 = ses_female, # calculate weights based on sex, education, language, age and income
  variable2 = ses_educ,
  variable3 = ses_lang,
  variable4 = ses_age,
  variable5 = ses_income)

#### 5.9 Very educated subset ####
VeryEducatedSubset <- addRakingWeightsColumn_5var(
  popData = Civimetre1,
  sampleData = VeryEducatedSubset,
  variable1 = ses_female, # calculate weights based on sex, education, language, age and income
  variable2 = ses_educ,
  variable3 = ses_lang,
  variable4 = ses_age,
  variable5 = ses_income)

#### 5.9.1 Very educated subset biased on health ####
VeryEducatedSubsetHealth <- addRakingWeightsColumn_5var(
  popData = Civimetre1,
  sampleData = VeryEducatedSubset,
  variable1 = ses_female, # calculate weights based on sex, education, health, age and income
  variable2 = ses_educ,
  variable3 = ses_health_2cat,
  variable4 = ses_age,
  variable5 = ses_income)

#### 5.9.2 Very educated subset biased on 2 variables ####
VeryEducatedSubset_2var <- addRakingWeightsColumn_5var(
  popData = Civimetre1,
  sampleData = VeryEducatedSubset,
  variable1 = ses_female,
  variable2 = ses_educ)

#### 5.9.3 Very educated subset biased on 3 variables ####
VeryEducatedSubset_3var <- addRakingWeightsColumn_5var(
  popData = Civimetre1,
  sampleData = VeryEducatedSubset,
  variable1 = ses_female,
  variable2 = ses_educ,
  variable3 = ses_age)

#### 5.9.4 Very educated subset biased on 4 variables ####
VeryEducatedSubset_4var <- addRakingWeightsColumn_5var(
  popData = Civimetre1,
  sampleData = VeryEducatedSubset,
  variable1 = ses_female,
  variable2 = ses_educ,
  variable3 = ses_age,
  variable4 = ses_income)

#### 5.9.5 Very educated subset biased on 4 variables ####
VeryEducatedSubsetLang_4var <- addRakingWeightsColumn_5var(
  popData = Civimetre1,
  sampleData = VeryEducatedSubset,
  variable1 = ses_female,
  variable2 = ses_educ,
  variable3 = ses_age,
  variable4 = ses_lang)

#### 6. Post-stratification ####
#### 6.1 Very technophile subset ####
VeryTechnophileSubset <- addPostStratWeightsColumn_5var(
  popData = Civimetre1,
  sampleData = VeryTechnophileSubset,
  variable1 = ses_female, # calculate weights based on sex, education, language, age and income
  variable2 = ses_educ,
  variable3 = ses_lang,
  variable4 = ses_age,
  variable5 = ses_income)

#### 6.2 Very contentious subset ####
VeryContentiousSubset <- addPostStratWeightsColumn_5var(
  popData = Civimetre1,
  sampleData = VeryContentiousSubset,
  variable1 = ses_female, # calculate weights based on sex, education, language, age and income
  variable2 = ses_educ,
  variable3 = ses_lang,
  variable4 = ses_age,
  variable5 = ses_income)

#### 6.3 Very authoritarian subset ####
VeryAuthoritarianSubset <- addPostStratWeightsColumn_5var(
  popData = Civimetre1,
  sampleData = VeryAuthoritarianSubset,
  variable1 = ses_female, # calculate weights based on sex, education, language, age and income
  variable2 = ses_educ,
  variable3 = ses_lang,
  variable4 = ses_age,
  variable5 = ses_income)

#### 6.4 Very religious subset ####
VeryReligiousSubset <- addPostStratWeightsColumn_5var(
  popData = Civimetre1,
  sampleData = VeryReligiousSubset,
  variable1 = ses_female, # calculate weights based on sex, education, language, age and income
  variable2 = ses_educ,
  variable3 = ses_lang,
  variable4 = ses_age,
  variable5 = ses_income)

#### 6.5 Very extrovert subset ####
VeryExtrovertSubset <- addPostStratWeightsColumn_5var(
  popData = Civimetre1,
  sampleData = VeryExtrovertSubset,
  variable1 = ses_female, # calculate weights based on sex, education, language, age and income
  variable2 = ses_educ,
  variable3 = ses_lang,
  variable4 = ses_age,
  variable5 = ses_income)

#### 6.6 Very healthy subset ####
VeryHealthySubset <- addPostStratWeightsColumn_5var(
  popData = Civimetre1,
  sampleData = VeryHealthySubset,
  variable1 = ses_female, # calculate weights based on sex, education, language, age and income
  variable2 = ses_educ,
  variable3 = ses_lang,
  variable4 = ses_age,
  variable5 = ses_income)

#### 6.7 Very feminine subset ####
VeryFeminineSubsetSES <- addPostStratWeightsColumn_5var(
  popData = Civimetre1,
  sampleData = VeryFeminineSubsetSES,
  variable1 = ses_female, # calculate weights based on sex, education, language, age and income
  variable2 = ses_educ,
  variable3 = ses_lang,
  variable4 = ses_age,
  variable5 = ses_income)

#### 6.7.1 Very feminine subset biased on health ####
VeryFeminineSubsetHealth <- addPostStratWeightsColumn_5var(
  popData = Civimetre1,
  sampleData = VeryFeminineSubsetHealth,
  variable1 = ses_female, # calculate weights based on sex, education, health, age and income
  variable2 = ses_educ,
  variable3 = ses_health_2cat,
  variable4 = ses_age,
  variable5 = ses_income)

#### 6.8 Very pessimist subset ####
VeryPessimistSubset <- addPostStratWeightsColumn_5var(
  popData = Civimetre1,
  sampleData = VeryPessimistSubset,
  variable1 = ses_female, # calculate weights based on sex, education, language, age and income
  variable2 = ses_educ,
  variable3 = ses_lang,
  variable4 = ses_age,
  variable5 = ses_income)

#### 6.9 Very educated subset ####
VeryEducatedSubset <- addPostStratWeightsColumn_5var(
  popData = Civimetre1,
  sampleData = VeryEducatedSubset,
  variable1 = ses_female, # calculate weights based on sex, education, language, age and income
  variable2 = ses_educ,
  variable3 = ses_lang,
  variable4 = ses_age,
  variable5 = ses_income)

#### 6.9.1 Very educated subset biased on health ####
VeryEducatedSubsetHealth <- addPostStratWeightsColumn_5var(
  popData = Civimetre1,
  sampleData = VeryEducatedSubsetHealth,
  variable1 = ses_female, # calculate weights based on sex, education, health, age and income
  variable2 = ses_educ,
  variable3 = ses_health_2cat,
  variable4 = ses_age,
  variable5 = ses_income)

#### 6.9.2 Very educated subset biased on 2 variables ####
VeryEducatedSubset_2var <- addPostStratWeightsColumn_5var(
  popData = Civimetre1,
  sampleData = VeryEducatedSubset,
  variable1 = ses_female,
  variable2 = ses_educ)

#### 6.9.3 Very educated subset biased on 3 variables ####
VeryEducatedSubset_3var <- addPostStratWeightsColumn_5var(
  popData = Civimetre1,
  sampleData = VeryEducatedSubset,
  variable1 = ses_female,
  variable2 = ses_educ,
  variable3 = ses_age)

#### 6.9.4 Very educated subset biased on 4 variables ####
VeryEducatedSubset_4var <- addPostStratWeightsColumn_5var(
  popData = Civimetre1,
  sampleData = VeryEducatedSubset,
  variable1 = ses_female,
  variable2 = ses_educ,
  variable3 = ses_age,
  variable4 = ses_income)

#### 6.9.5 Very educated subset biased on 4 variables ####
VeryEducatedSubsetLang_4var <- addPostStratWeightsColumn_5var(
  popData = Civimetre1,
  sampleData = VeryEducatedSubset,
  variable1 = ses_female,
  variable2 = ses_educ,
  variable3 = ses_age,
  variable4 = ses_lang)

#### 7. Compare proportions in population, sample, raked sample and post-stratified sample ####
#### 7.1 Very technophile subset ####
VeryTechnophilePercentages <- comparePopSampleRakedPostStratPercentages(
  popData = Civimetre1,
  sampleData = VeryTechnophileSubset, # n = 703
  variable = scale_TechnocriticalTechnophile_2cat)
summary(VeryTechnophilePercentages$rakingImprovesBy) # improvement by -0.04 percentage points on average
summary(VeryTechnophilePercentages$postStratImprovesBy) # improvement by 0.65 percentage points on average

#### 7.2 Very contentious subset ####
VeryContentiousPercentages <- comparePopSampleRakedPostStratPercentages(
  popData = Civimetre1,
  sampleData = VeryContentiousSubset, # n = 866
  variable = scale_ContentiousDisciplined_2cat)
summary(VeryContentiousPercentages$rakingImprovesBy) # improvement by 0.21 percentage points on average
summary(VeryContentiousPercentages$postStratImprovesBy) # improvement by -0.09 percentage points

#### 7.3 Very authoritarian subset ####
VeryAuthoritarianPercentages <- comparePopSampleRakedPostStratPercentages(
  popData = Civimetre1,
  sampleData = VeryAuthoritarianSubset, # n = 530
  variable = scale_AuthoritarianPluralist_2cat)
summary(VeryAuthoritarianPercentages$rakingImprovesBy) # improvement by 1.37 percentage points on average
summary(VeryAuthoritarianPercentages$postStratImprovesBy) # improvement by 1.13 points on average

#### 7.4 Very religious subset ####
VeryReligiousPercentages <- comparePopSampleRakedPostStratPercentages(
  popData = Civimetre1,
  sampleData = VeryReligiousSubset, # n = 391
  variable = ses_rel)
summary(VeryReligiousPercentages$rakingImprovesBy) # improvement by 2.71 percentage points on average
summary(VeryReligiousPercentages$postStratImprovesBy) # improvement by 3.26 percentage points on average

#### 7.5 Very extrovert subset ####
VeryExtrovertPercentages <- comparePopSampleRakedPostStratPercentages(
  popData = Civimetre1,
  sampleData = VeryExtrovertSubset, # n = 736
  variable = ses_extroverted_enthusiastic_2cat)
summary(VeryExtrovertPercentages$rakingImprovesBy) # improvement by 0.12 percentage points on average
summary(VeryExtrovertPercentages$postStratImprovesBy) # improvement by -0.16 percentage points on average

#### 7.6 Very healthy subset ####
VeryHealthyPercentages <- comparePopSampleRakedPostStratPercentages(
  popData = Civimetre1,
  sampleData = VeryHealthySubset, # n = 1087
  variable = ses_health_2cat)
summary(VeryHealthyPercentages$rakingImprovesBy) # improvement by 0.04 percentage points on average
summary(VeryHealthyPercentages$postStratImprovesBy) # improvement by -0.04 percentage points on average

#### 7.7 Very feminine subset ####
VeryFemininePercentages <- comparePopSampleRakedPostStratPercentages(
  popData = Civimetre1,
  sampleData = VeryFeminineSubsetSES, # n = 709
  variable = ses_female)
summary(VeryFemininePercentages$rakingImprovesBy) # improvement by 33.29 percentage points on average
summary(VeryFemininePercentages$postStratImprovesBy) # improvement by 29.6 percentage points on average

VeryFemininePercentagesHealth <- comparePopSampleRakedPostStratPercentages(
  popData = Civimetre1,
  sampleData = VeryFeminineSubsetHealth, # n = 709
  variable = ses_female)
summary(VeryFemininePercentagesHealth$rakingImprovesBy) # improvement by 33.29 percentage points
summary(VeryFemininePercentagesHealth$postStratImprovesBy) # improvement by 32.78 percentage points

#### 7.7.1 Effect on optimism ####
VeryFeminineOptimistPercentages <- comparePopSampleRakedPostStratPercentages(
  popData = Civimetre1,
  sampleData = VeryFeminineSubsetSES, # n = 709
  variable = scale_PessimistOptimist_2cat)
summary(VeryFeminineOptimistPercentages$rakingImprovesBy) # improvement by -3.87 percentage points
summary(VeryFeminineOptimistPercentages$postStratImprovesBy) # improvement by -3.63 percentage points

VeryFeminineOptimistPercentagesHealth <- comparePopSampleRakedPostStratPercentages(
  popData = Civimetre1,
  sampleData = VeryFeminineSubsetHealth, # n = 709
  variable = scale_PessimistOptimist_2cat)
summary(VeryFeminineOptimistPercentagesHealth$rakingImprovesBy) # improvement by -3.34 percentage points
summary(VeryFeminineOptimistPercentagesHealth$postStratImprovesBy) # improvement by -5.95 points

#### 7.8 Very pessimist subset ####
VeryPessimistPercentages <- comparePopSampleRakedPostStratPercentages(
  popData = Civimetre1,
  sampleData = VeryPessimistSubset, # n = 574
  variable = scale_PessimistOptimist_2cat)
summary(VeryPessimistPercentages$rakingImprovesBy) # improvement by 0.67 percentage points
summary(VeryPessimistPercentages$postStratImprovesBy) # improvement by 1.35 percentage points

#### 7.9 Very educated subset ####
VeryEducatedPercentages <- comparePopSampleRakedPostStratPercentages(
  popData = Civimetre1,
  sampleData = VeryEducatedSubset, # n = 1400
  variable = ses_educ)
summary(VeryEducatedPercentages$rakingImprovesBy) # improvement by 8.21 percentage points
summary(VeryEducatedPercentages$postStratImprovesBy) # improvement by 7.91 percentage points

VeryEducatedPercentagesHealth <- comparePopSampleRakedPostStratPercentages(
  popData = Civimetre1,
  sampleData = VeryEducatedSubsetHealth, # n = 1400
  variable = ses_educ)
summary(VeryEducatedPercentagesHealth$rakingImprovesBy) # improvement by 8.21 percentage points
summary(VeryEducatedPercentagesHealth$postStratImprovesBy) # improvement by 7.5 percentage points

comparePopSampleRakedPostStratPercentages(
  popData = Civimetre1,
  sampleData = VeryEducatedSubset_2var,
  variable = ses_lang)

comparePopSampleRakedPostStratPercentages(
  popData = Civimetre1,
  sampleData = VeryEducatedSubset_2var,
  variable = ses_age)

comparePopSampleRakedPostStratPercentages(
  popData = Civimetre1,
  sampleData = VeryEducatedSubset_2var,
  variable = ses_female)

comparePopSampleRakedPostStratPercentages(
  popData = Civimetre1,
  sampleData = VeryEducatedSubset_2var,
  variable = ses_income)

comparePopSampleRakedPostStratPercentages(
  popData = Civimetre1,
  sampleData = VeryEducatedSubset_3var,
  variable = ses_lang)

comparePopSampleRakedPostStratPercentages(
  popData = Civimetre1,
  sampleData = VeryEducatedSubset_3var,
  variable = ses_age)

comparePopSampleRakedPostStratPercentages(
  popData = Civimetre1,
  sampleData = VeryEducatedSubset_3var,
  variable = ses_female)

comparePopSampleRakedPostStratPercentages(
  popData = Civimetre1,
  sampleData = VeryEducatedSubset_3var,
  variable = ses_income)

comparePopSampleRakedPostStratPercentages(
  popData = Civimetre1,
  sampleData = VeryEducatedSubset_4var,
  variable = ses_lang)

comparePopSampleRakedPostStratPercentages(
  popData = Civimetre1,
  sampleData = VeryEducatedSubset_4var,
  variable = ses_age)

comparePopSampleRakedPostStratPercentages(
  popData = Civimetre1,
  sampleData = VeryEducatedSubset_4var,
  variable = ses_female)

comparePopSampleRakedPostStratPercentages(
  popData = Civimetre1,
  sampleData = VeryEducatedSubset_4var,
  variable = ses_income)

#### 7.9.1 Effect on optimism ####
VeryEducatedOptimistPercentages <- comparePopSampleRakedPostStratPercentages(
  popData = Civimetre1,
  sampleData = VeryEducatedSubset, # n = 1400
  variable = scale_PessimistOptimist_2cat)
summary(VeryEducatedOptimistPercentages$rakingImprovesBy) # improvement by 0.44 percentage points
summary(VeryEducatedOptimistPercentages$postStratImprovesBy) # improvement by 0.52 percentage points

VeryEducatedOptimistPercentagesHealth <- comparePopSampleRakedPostStratPercentages(
  popData = Civimetre1,
  sampleData = VeryEducatedSubsetHealth, # n = 1400
  variable = scale_PessimistOptimist_2cat)
summary(VeryEducatedOptimistPercentagesHealth$rakingImprovesBy) # improvement by 0.44 percentage points
summary(VeryEducatedOptimistPercentagesHealth$postStratImprovesBy) # improvement by 0.43 percentage points
