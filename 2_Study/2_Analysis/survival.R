# KM survival analysis ---
cli::cli_alert_info("- Getting survival")

# add sex and age to cohorts ----
cli::cli_alert_info("Add demographics to cohort")
cdm$outcome <- cdm$outcome %>% 
  PatientProfiles::addSex(cdm) %>% 
  PatientProfiles::addAge(cdm,
                          ageName = "age",
                          ageGroup = list(
                            "age_group" =
                              list(
                                "18 to 49" = c(18, 49),
                                "50 to 39" = c(50, 59),
                                "60 to 59" = c(60, 69),
                                "70 to 79" = c(70, 79),
                                "80+" = c(80, 150)
                              )
                          ))

# add diagnosis year 
cdm$outcome <-cdm$outcome %>% 
  mutate(year = year(cohort_start_date)) %>% 
  
# create diagnosis age band groups
cdm$outcome <- cdm$outcome %>% 
  mutate(age_band = cut(year, breaks = c(2003, 2008, 2013, 2018, 2023), 
                        labels = c("2003-2007", "2008-2012", "2013-2017", "2018-2022"),
                        include.lowest = TRUE))

# add in exclusion criteria

# remove people with any history of cancer

# remove any people with cancer and death on same date


if(cdm$death %>% head(5) %>% count() %>% pull("n") > 0){
  # generate death cohort ----
  cli::cli_alert_info("Generating death cohort")
  cdm <- generateDeathCohortSet(cdm = cdm,
                                name = "cancer_death",
                                overwrite = TRUE)
  
  # estimate survival ----
  cli::cli_alert_info("Estimating survival")
  surv <- estimateSingleEventSurvival(cdm = cdm,
                                      followUpDays = 3650,
                                      censorOnCohortExit = FALSE ,
                                      censorOnDate = NULL ,
                                      timeGap = c(365) ,
                                      targetCohortTable = "outcome",
                                      outcomeCohortTable = "cancer_death",
                                      strata = list(c("sex"),
                                                    c("age_group")
                                                    
                                                    
                                                    ),
                                      minCellCount = 5)
  

  cli::cli_alert_info("Exporting survival attrition")
  write_csv(surv, here("Results", paste0(db.name, "/", cdmName(cdm), "_survival_attrition.csv"
  )))
  
  # export survival ----
  cli::cli_alert_info("Exporting survival results")
  write_csv(surv, here("Results", paste0(db.name, "/", cdmName(cdm), "_survival_estimates.csv"
            )))
  
  


  
}
