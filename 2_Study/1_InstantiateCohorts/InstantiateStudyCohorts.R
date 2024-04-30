# instantiate cancer cohorts
cli::cli_alert_info("- Getting cancer definitions")

# get concept sets from cohorts----
cancer_concepts_inc <- CDMConnector::readCohortSet(
  path = here::here("2_Study", "1_InstantiateCohorts", "Cohorts", "incidence" ))

# instantiate the cohorts with no prior history 
cdm <- CDMConnector::generateCohortSet(
  cdm,
  cohortSet = cancer_concepts_inc,
  name = "outcome",
  overwrite = TRUE)

# # only run analysis where we have counts more than 200 ----
# cancer_cohorts_inc <- CDMConnector::settings(cdm$outcome) %>%
#   dplyr::inner_join(CDMConnector::cohortCount(cdm$outcome), by = "cohort_definition_id") %>%
#   dplyr::arrange(cohort_definition_id) %>% 
#   dplyr::filter(number_subjects >= 200)
# 
# # filter the data to cohorts that have more than 200 patients
# id <- cohortCount(cdm$outcome) %>% dplyr::filter(number_subjects >= 200) %>% dplyr::pull("cohort_definition_id")
# 
# cdm$outcome <- cdm$outcome %>% filter(cohort_definition_id %in% id)
# 
# cdm$outcome <- CDMConnector::recordCohortAttrition(cohort = cdm$outcome,
#                                                      reason="Removing cancer cohorts from analysis with less than 200 patients" )

if(isTRUE(run_prevalence)){
  
# instantiate 
cancer_concepts_p <- CDMConnector::readCohortSet(
  path = here::here("2_Study", "1_InstantiateCohorts", "Cohorts", "prevalence" ))

# instantiate the cohorts
cdm <- CDMConnector::generateCohortSet(
  cdm,
  cohortSet = cancer_concepts_p,
  name = "outcome_p",
  overwrite = TRUE )

# # only run analysis where we have counts more than 200 ----
# cancer_cohorts_prev <- CDMConnector::settings(cdm$outcome_p) %>%
#   dplyr::inner_join(CDMConnector::cohortCount(cdm$outcome_p), by = "cohort_definition_id") %>%
#   dplyr::arrange(cohort_definition_id) %>% 
#   dplyr::filter(number_subjects >= 200)
# 
# # filter the data to cohorts that have more than 200 patients
# id <- cohortCount(cdm$outcome_p) %>% dplyr::filter(number_subjects >= 200) %>% dplyr::pull("cohort_definition_id")
# cdm$outcome_p <- cdm$outcome_p %>% filter(cohort_definition_id %in% id)
# 
# #update the attrition
# cdm$outcome_p <- CDMConnector::recordCohortAttrition(cohort = cdm$outcome_p,
#                                                    reason="Removing cancer cohorts from analysis with less than 200 patients" )
# 
# }


cli::cli_alert_success("- Got cancer definitions")