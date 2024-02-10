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
  temporary = FALSE,
  returnParticipants = TRUE
)

# Export the results -----
cli::cli_alert_info("- Getting incidence attrition")
write.csv(IncidencePrevalence::incidenceAttrition(inc), here::here(paste0(output.folder,"/", db.name, "_incidence_attrition.csv")), row.names = FALSE)

cli::cli_alert_info("- Getting incidence settings")
write.csv(IncidencePrevalence::incidenceSet(inc), here::here(paste0(output.folder,"/", db.name, "_incidence_settings.csv")), row.names = FALSE)

cli::cli_alert_info("- Getting incidence results")
write.csv(inc, here::here(paste0(output.folder,"/", db.name, "_incidence_estimates.csv")), row.names = FALSE)
