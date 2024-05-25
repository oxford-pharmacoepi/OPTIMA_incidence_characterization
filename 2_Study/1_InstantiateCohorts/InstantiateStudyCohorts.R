# instantiate cancer cohorts
cli::cli_alert_info("- Getting cancer definitions")

# get concept sets from cohorts----
cancer_concepts_inc <- CDMConnector::readCohortSet(
  path = here::here("1_InstantiateCohorts", "Cohorts", "incidence" ))

# instantiate the cohorts with no prior history 
cdm <- CDMConnector::generateCohortSet(
  cdm,
  cohortSet = cancer_concepts_inc,
  name = "outcome",
  overwrite = TRUE)

if(isTRUE(run_prevalence)){
  
# instantiate 
cancer_concepts_p <- CDMConnector::readCohortSet(
  path = here::here("1_InstantiateCohorts", "Cohorts", "prevalence" ))

# instantiate the cohorts
cdm <- CDMConnector::generateCohortSet(
  cdm,
  cohortSet = cancer_concepts_p,
  name = "outcome_p",
  overwrite = TRUE )

}


cli::cli_alert_success("- Got cancer definitions")