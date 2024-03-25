# instantiate cancer cohorts
cli::cli_alert_info("- Getting cancer definitions")

# get concept sets from cohorts----
cancer_concepts_inc <- CodelistGenerator::codesFromCohort(
  path = here::here("2_Study", "1_InstantiateCohorts", "Cohorts", "incidence" ) ,
  cdm = cdm,
  withConceptDetails = FALSE)


# instantiate the cohorts with no prior history 
cdm <- CDMConnector::generateConceptCohortSet(
  cdm,
  conceptSet = cancer_concepts_inc,
  name = "outcome",
  limit = "first",
  requiredObservation = c(0, 0),
  end = "observation_period_end_date",
  overwrite = TRUE )

if(isTRUE(run_prevalence)){
  
# instantiate 
cancer_concepts_p <- CodelistGenerator::codesFromCohort(
  path = here::here("2_Study", "1_InstantiateCohorts", "Cohorts", "prevalence" ) ,
  cdm = cdm,
  withConceptDetails = FALSE)


# instantiate the cohorts with no prior history 
cdm <- CDMConnector::generateConceptCohortSet(
  cdm,
  conceptSet = cancer_concepts_p,
  name = "outcome_p",
  limit = "first",
  requiredObservation = c(0, 0),
  end = "observation_period_end_date",
  overwrite = TRUE )

}


cli::cli_alert_success("- Got cancer definitions")