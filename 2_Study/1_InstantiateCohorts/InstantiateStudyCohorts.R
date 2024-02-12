# instantiate cancer cohorts
cli::cli_alert_info("- Getting cancer definitions")

# get concept sets from cohorts----
cancer_concepts <- CodelistGenerator::codesFromCohort(
  path = here::here("2_Study", "1_InstantiateCohorts", "Cohorts" ) ,
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

cli::cli_alert_success("Got cancer cohorts")

