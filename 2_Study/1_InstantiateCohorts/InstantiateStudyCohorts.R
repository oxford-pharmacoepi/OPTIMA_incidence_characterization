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


# only run analysis where we have counts more than 200 ----
cancer_cohorts_inc <- CDMConnector::cohortSet(cdm$outcome) %>%
  dplyr::inner_join(CDMConnector::cohortCount(cdm$outcome), by = "cohort_definition_id") %>%
  dplyr::arrange(cohort_definition_id) %>% 
  dplyr::filter(number_subjects >= 200)

# filter the data to cohorts that have more than 200 patients
id <- cohortCount(cdm$outcome) %>% dplyr::filter(number_subjects >= 200) %>% dplyr::pull("cohort_definition_id")

cdm$outcome <- cdm$outcome %>% filter(cohort_definition_id %in% id)

cdm$outcome <- CDMConnector::recordCohortAttrition(cohort = cdm$outcome,
                                                     reason="Removing cancer cohorts from analysis with less than 200 patients" )

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



# only run analysis where we have counts more than 200 ----
cancer_cohorts_prev <- CDMConnector::cohortSet(cdm$outcome_p) %>%
  dplyr::inner_join(CDMConnector::cohortCount(cdm$outcome_p), by = "cohort_definition_id") %>%
  dplyr::arrange(cohort_definition_id) %>% 
  dplyr::filter(number_subjects >= 200)

# filter the data to cohorts that have more than 200 patients
id <- cohortCount(cdm$outcome_p) %>% dplyr::filter(number_subjects >= 200) %>% dplyr::pull("cohort_definition_id")
cdm$outcome_p <- cdm$outcome_p %>% filter(cohort_definition_id %in% id)

#update the attrition
cdm$outcome_p <- CDMConnector::recordCohortAttrition(cohort = cdm$outcome_p,
                                                   reason="Removing cancer cohorts from analysis with less than 200 patients" )

}


cli::cli_alert_success("- Got cancer definitions")