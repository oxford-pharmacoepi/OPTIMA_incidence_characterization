# # code for estimating prevalence below

if(isFALSE(run_prevalence)){
# #get denominator ------
cli::cli_alert_info("- Getting denominator")
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator" ,
  cohortDateRange = c(as.Date("2002-12-31"), as.Date("2022-12-31")),
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

# Estimate total prevalence -------
cli::cli_alert_info("- Getting prevalence")

# total prevalence
total_prevalence <- estimatePeriodPrevalence(
  cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
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