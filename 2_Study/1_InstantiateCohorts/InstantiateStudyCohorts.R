# instantiate cancer cohorts
info(logger, "- getting cancer definitions")


# get concept sets from cohorts----
cancer_concepts <- CodelistGenerator::codesFromCohort(
  path = here::here("1_InstantiateCohorts", "Cohorts" ) ,
  cdm = cdm,
  withConceptDetails = FALSE)


# instantiate the cohorts with no prior history 
cdm <- CDMConnector::generateConceptCohortSet(
  cdm,
  conceptSet = cancer_concepts,
  name = "outcome",
  limit = "first",
  requiredObservation = c(0, 0),
  end = "observation_period_end_date",
  overwrite = TRUE )



if(priorhistory == TRUE){
  
  # add in prior history
  cdm$outcome <- cdm$outcome %>% 
    PatientProfiles::addPriorObservation(
      cdm = cdm,
      indexDate = "cohort_start_date")
  
  #for those with prior history remove those with less than 365 days of prior history
  cdm$outcome <- cdm$outcome %>% 
    filter(prior_observation >= 365) %>% 
    select(-c(prior_observation))
  
}


info(logger, "- got cancer cohorts")

