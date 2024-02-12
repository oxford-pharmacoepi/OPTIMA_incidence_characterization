# code for getting denominator and estimating incidence below

#get denominator ------
cli::cli_alert_info("- Getting denominator")
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator" ,
  cohortDateRange = c(as.Date("2000-01-01"), as.Date("2022-01-01")),
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
  daysPriorObservation = 365,
  overwrite = TRUE
)
cli::cli_alert_info("- Got denominator")

# Estimate incidence -------
cli::cli_alert_info("- Getting incidence")

#incidence
inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  denominatorCohortId = NULL,
  interval = c("years", "overall"),
  outcomeWashout = Inf,
  repeatedEvents = FALSE,
  completeDatabaseIntervals = TRUE,
  minCellCount = 5,
  temporary = TRUE,
  returnParticipants = FALSE
)


cli::cli_alert_info("- Getting participants from incidence")
#getting participants for survival analysis
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator_parts" ,
  cohortDateRange = c(as.Date("2000-01-01"), as.Date("2022-01-01")),
  ageGroup =list(
    c(18, 150)),
  sex = c("Both"),
  daysPriorObservation = 365,
  overwrite = TRUE
)

inc_overall_parts <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator_parts",
  outcomeTable = "outcome",
  denominatorCohortId = NULL,
  interval = c("overall"),
  outcomeWashout = Inf,
  repeatedEvents = FALSE,
  completeDatabaseIntervals = TRUE,
  minCellCount = 0,
  temporary = FALSE,
  returnParticipants = TRUE
)

# Export the results -----
cli::cli_alert_info("- Getting incidence attrition")
write.csv(IncidencePrevalence::incidenceAttrition(inc), here::here("Results", paste0(db_name, "/", cdmName(cdm), "_incidence_attrition.csv")), row.names = FALSE)

cli::cli_alert_info("- Getting incidence settings")
write.csv(IncidencePrevalence::incidenceSet(inc), here::here("Results", paste0(db_name, "/", cdmName(cdm), "_incidence_settings.csv")), row.names = FALSE)

cli::cli_alert_info("- Getting incidence results")
write.csv(inc, here::here("Results", paste0(db_name, "/", cdmName(cdm), "_incidence_estimates.csv")), row.names = FALSE)