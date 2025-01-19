#### Create all functions ####
bias2 <- function(data, variable, cutoff, prop1, prop2) {
  set.seed(123)
  dataA  <- data %>% 
    filter({{variable}} < {{cutoff}}) %>%
    slice_sample(prop = {{prop1}}, replace = F)
  dataB <- data %>% 
    filter({{variable}} >= {{cutoff}}) %>%
    slice_sample(prop = {{prop2}}, replace = F)
  dataC <- bind_rows(dataA, dataB)
  return(dataC)
}

bias3 <- function(data, variable, cutoff1, cutoff2, prop1, prop2, prop3) {
  set.seed(123)
  dataA  <- data %>% 
    filter({{variable}} < {{cutoff1}}) %>%
    slice_sample(prop = {{prop1}}, replace = F)
  dataB <- data %>% 
    filter({{variable}} >= {{cutoff1}} & {{variable}} <= {{cutoff2}}) %>%
    slice_sample(prop = {{prop2}}, replace = F)
  dataC <- data %>% 
    filter({{variable}} > {{cutoff2}}) %>%
    slice_sample(prop = {{prop3}}, replace = F)
  dataD <- bind_rows(dataA, dataB, dataC)
  return(dataD)
}

createBiasedSubset_2cat <- function(data, variable, value1, value2, chosenProp1, chosenProp2) {
  set.seed(123) # keep the random aspect of slice_sample constant for replication
  dataA <- data %>%
    filter({{variable}} == {{value1}}) %>% # often 0
    slice_sample(prop = {{chosenProp1}}, replace = F) # enable multiple random selections of 1 observation
  dataB <- data %>% 
    filter({{variable}} == {{value2}}) %>% # often 1
    slice_sample(prop = {{chosenProp2}}, replace = F) # enable multiple random selections of 1 observation
  dataC <- bind_rows(dataA, dataB)
  return(dataC)
}

createBiasedSubset_3cat <- function(data, variable, value1, value2, value3, chosenProp1, chosenProp2,
                                    chosenProp3) {
  set.seed(123) # keep the random aspect of slice_sample constant for replication
  dataA <- data %>%
    filter({{variable}} == {{value1}}) %>% # often 0
    slice_sample(prop = {{chosenProp1}}, replace = F) # enable multiple random selections of 1 observation
  dataB <- data %>% 
    filter({{variable}} == {{value2}}) %>% # often 1
    slice_sample(prop = {{chosenProp2}}, replace = F) # enable multiple random selections of 1 observation
  dataC <- data %>% 
    filter({{variable}} == {{value3}}) %>% # often 1
    group_by({{variable}}) %>%
    slice_sample(prop = {{chosenProp3}}, replace = F) # enable multiple random selections of 1 observation
  dataD <- bind_rows(dataA, dataB, dataC)
  return(dataD)
}

calculateUnweightedProps <- function(data, variable) {
  data %>% # calculate proportions for one variable
    select({{variable}}) %>%
    group_by({{variable}}) %>%
    dplyr::summarize(n = n()) %>%
    na.omit() %>%
    mutate(prop = n / sum(n))
}

calculate5UnweightedProps <- function(data, variable1, variable2, variable3, variable4, variable5) {
  Prop1 <- calculateUnweightedProps(data, variable = {{variable1}}) # calculate proportions for variable 1
  Prop2 <- calculateUnweightedProps(data, variable = {{variable2}})
  Prop3 <- calculateUnweightedProps(data, variable = {{variable3}})
  Prop4 <- calculateUnweightedProps(data, variable = {{variable4}})
  Prop5 <- calculateUnweightedProps(data, variable = {{variable5}})
  dataProp <- bind_rows(Prop1, Prop2, Prop3, Prop4, Prop5) %>%
    gather(key, value, -n, -prop) %>% # key = variable name, value = variable value
    na.omit() %>%
    select(key, value, n, prop)
  return(dataProp) # calculate proportions for multiple variables
}

addRakingWeightsColumn_5var <- function(popData, sampleData, variable1, variable2, variable3, variable4,
                                        variable5) {
  popProps <- calculate5UnweightedProps(data = popData, # population data
                                        variable1 = {{variable1}},
                                        variable2 = {{variable2}},
                                        variable3 = {{variable3}},
                                        variable4 = {{variable4}},
                                        variable5 = {{variable5}})
  targets <- unstack(popProps, form = prop ~ key) # transform data.frame into list (needed for anesrake)
  sampleData$mergeId <- 1:nrow(sampleData) # add a variable for row number
  subsetRaking <- sampleData %>% # keep only relevant variables
    select(mergeId, {{variable1}}, {{variable2}}, {{variable3}}, {{variable4}}, {{variable5}}) %>%
    as.data.frame() # transform into data.frame
  raking <- anesrake(inputter = targets, # target proportions from the population
                     dataframe = subsetRaking, # sample data
                     caseid = subsetRaking$mergeId,
                     cap = 5, # maximum value the weight variable is allowed to take
                     type = "pctlim", # among the 5 SES variables, only those whose proportions deviate
                     # enough from the population proportion are included
                     pctlim = 5, # the "enough" on the previous line is set to 5 percentage points
                     choosemethod = "total") # this 5 points applies to all variable values added together
  sampleData$weightRaking <- raking$weightvec # add raking weights column to sample
  return(sampleData)
  ### Tests pour vérifier que l'ordre des colonnes demeure le même en utilisant un merge ID.
  ### Tests fructueux, donc inutile de les intégrer dans la fonction
  # Merge new weight variable with the larger dataset
  #Rakingdata <- data.frame(mergeID = raking$caseid, weightRaking = raking$weightvec)
  #SubsetXnew <- left_join(sampleData, Rakingdata)
  #subsetX$weightnewNew <- unlist(raking[1]) # same thing
  #return(SubsetXnew)
}

addPostStratWeightsColumn_5var <- function(popData, sampleData, variable1, variable2, variable3,
                                           variable4, variable5) {
  popPropsData <- popData %>% # calculate proportions in population data
    select({{variable1}}, {{variable2}}, {{variable3}}, {{variable4}}, {{variable5}}) %>%
    na.omit() %>%
    ungroup() %>%
    count({{variable1}}, {{variable2}}, {{variable3}}, {{variable4}}, {{variable5}}) %>%
    mutate(popProps = n / sum(n)) %>%
    select(-c(n))
  samplePropsData <- sampleData %>% # calculate proportions in sample data
    select({{variable1}}, {{variable2}}, {{variable3}}, {{variable4}}, {{variable5}}) %>%
    na.omit() %>%
    ungroup() %>%
    count({{variable1}}, {{variable2}}, {{variable3}}, {{variable4}}, {{variable5}}) %>%
    mutate(sampleProps = n / sum(n)) %>%
    select(-c(n))
  weightData <- inner_join(popPropsData, samplePropsData) %>%
    mutate(weightPostStrat = popProps / sampleProps) %>% # create weights variable
    select(-c(popProps, sampleProps))
  sampleData <- left_join(sampleData, weightData) # merge weights variable to sample data
  return(sampleData)
}

comparePopSampleRakedPostStratPercentages <- function(popData, sampleData, variable) {
  popPercentagesData <- popData %>% # calculate proportions in population data
    select({{variable}}) %>%
    na.omit() %>%
    ungroup() %>%
    count({{variable}}) %>%
    mutate(popPercentages = 100 * n / sum(n)) %>%
    select(-c(n))
  samplePercentagesData <- sampleData %>% # calculate percentages in sample data
    select({{variable}}) %>%
    na.omit() %>%
    ungroup() %>%
    count({{variable}}) %>%
    mutate(samplePercentages = 100 * n / sum(n))
  rakedPercentagesData <- sampleData %>% # calculate percentages in raked data
    select({{variable}}, weightRaking) %>%
    na.omit() %>%
    ungroup() %>%
    count({{variable}}, wt = weightRaking) %>%
    mutate(rakedSamplePercentages = 100 * n / sum(n))
  postStratifiedPercentagesData <- sampleData %>% # calculate percentages in post-stratified data
    select({{variable}}, weightPostStrat) %>%
    na.omit() %>%
    ungroup() %>%
    count({{variable}}, wt = weightPostStrat) %>%
    mutate(postStratSamplePercentages = 100 * n / sum(n)) %>%
    select(-c(n))
  percentagesData <- inner_join(popPercentagesData, samplePercentagesData)
  percentagesData$rakedSamplePercentages <- rakedPercentagesData$rakedSamplePercentages
  percentagesData$postStratSamplePercentages <- postStratifiedPercentagesData$postStratSamplePercentages
  percentagesData$diffPopSample <- abs(percentagesData$popPercentages - # absolute value (no negatives)
                                         percentagesData$samplePercentages)
  percentagesData$diffPopRaking <- abs(percentagesData$popPercentages -
                                         percentagesData$rakedSamplePercentages)
  percentagesData$diffPopPostStrat <- abs(percentagesData$popPercentages -
                                            percentagesData$postStratSamplePercentages)
  percentagesData$rakingImprovesBy <- percentagesData$diffPopSample -
    percentagesData$diffPopRaking # create a vector where positive scores are associated with
  # better performance of raked estimates relative to non-weighted estimates
  percentagesData$postStratImprovesBy <- percentagesData$diffPopSample -
    percentagesData$diffPopPostStrat # create a vector where positive scores are associated with
  # better performance of post-stratified estimates relative to non-weighted estimates
  return(percentagesData)
}
