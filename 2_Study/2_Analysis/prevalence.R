# # code for estimating prevalence below

if(isTRUE(run_prevalence)){
  
# if user has not run incidence the denominator will need to be run
  if(isFALSE(run_incidence)){
# #get denominator ------
cli::cli_alert_info("- Getting denominator")
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator" ,
  cohortDateRange = c(as.Date(study_start), as.Date("2023-01-01")),
  requirementInteractions = TRUE,
  ageGroup =list(
    c(18, 150),
    c(18, 49),
    c(50, 59),
    c(60, 69),
    c(70, 79),
    c(80, 150)
  ),
  sex = c("Male", "Female", "Both"),
  daysPriorObservation = 365
)
cli::cli_alert_success("- Got denominator")
  
  }
  
}


# Estimate prevalence -------
cli::cli_alert_info("- Getting prevalence")

# prevalence until end of observation (total) and partial prevalence at 2 and 5 years
prev <- estimatePeriodPrevalence(
  cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome_p",
  interval = "years",
  completeDatabaseIntervals = TRUE,
  fullContribution = FALSE,
  strata = list(),
  minCellCount = 0,
  returnParticipants = FALSE
)

cli::cli_alert_success("- Got prevalence")


# # Export the results -----
cli::cli_alert_info("- Getting prevalence attrition")
write.csv(attrition(prev), here::here("Results", paste0(db_name, "/", cdmName(cdm), "_prevalence_attrition.csv")), row.names = FALSE)

cli::cli_alert_info("- Getting prevalence settings")
write.csv(settings(prev), here::here("Results", paste0(db_name, "/", cdmName(cdm), "_prevalence_settings.csv")), row.names = FALSE)

cli::cli_alert_info("- Getting prevalence results")
write.csv(prev, here::here("Results", paste0(db_name, "/", cdmName(cdm), "_prevalence_estimates.csv")), row.names = FALSE)

cli::cli_alert_success("Prevalence Analysis Complete")