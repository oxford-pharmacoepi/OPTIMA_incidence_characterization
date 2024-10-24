# KM survival analysis ---

cli::cli_alert_info("- Getting participants for survival")
  
# add sex and age to cohorts ----
cli::cli_alert_info("Add demographics to cohort")
cdm$outcome <- cdm$outcome %>% 
  PatientProfiles::addDemographics(
                          ageGroup = list(
                            "age_group" =
                              list(
                                "18 to 49" = c(18, 49),
                                "50 to 59" = c(50, 59),
                                "60 to 69" = c(60, 69),
                                "70 to 79" = c(70, 79),
                                "80 to 89" = c(80, 89),
                                "90+" = c(90, 150)
                              )
                          )) %>% 
  mutate(year = year(cohort_start_date))
  
# create diagnosis age band groups
cdm$outcome <- cdm$outcome %>% 
  PatientProfiles::addCategories(
    variable = "cohort_start_date",
    categories = list("diag_yr_gp" = list(
      "2003 to 2006" = as.Date(c("2003-01-01", "2006-12-31")),
      "2007 to 2010" = as.Date(c("2007-01-01", "2010-12-31")),
      "2011 to 2014" = as.Date(c("2011-01-01", "2014-12-31")),
      "2015 to 2018" = as.Date(c("2015-01-01", "2018-12-31")),
      "2019 to 2022" = as.Date(c("2019-01-01", "2022-12-31"))
    )
    )
  )

# rename categories
cdm$outcome <- cdm$outcome %>% 
  mutate(diag_yr_gp = case_when(
    grepl("^2003-01-01 to 2003-01-01$", diag_yr_gp) ~ "2003-2006",
    grepl("^2007-01-01 to 2007-01-01$", diag_yr_gp) ~ "2007-2010",
    grepl("^2011-01-01 to 2011-01-01$", diag_yr_gp) ~ "2011-2014",
    grepl("^2015-01-01 to 2015-01-01$", diag_yr_gp) ~ "2015-2018",
    grepl("^2019-01-01 to 2019-01-01$", diag_yr_gp) ~ "2019-2022",
    TRUE ~ diag_yr_gp  # Keep the original value if it doesn't match the specific pattern
  ))


# remove those outside the study period ------
cdm$outcome <- cdm$outcome %>% 
  dplyr::filter(diag_yr_gp != "None") 

# make outcome a perm table and update the attrition
cdm$outcome <- cdm$outcome %>% 
  compute(name = "outcome", temporary = FALSE, overwrite = TRUE) %>% 
  recordCohortAttrition(reason="Exclude patients outside study period")

#exclude those under 18 years of age  -------
cdm$outcome <- cdm$outcome %>% 
  filter(age >= 18) 

# make outcome a perm table and update the attrition
cdm$outcome <- cdm$outcome %>% 
  compute(name = "outcome", temporary = FALSE, overwrite = TRUE) %>% 
  CDMConnector::recordCohortAttrition(reason="Excluded patients younger than 18 years of age" )

#for those with prior history remove those with less than 365 days of prior history -------
cdm$outcome <- cdm$outcome %>% 
  filter(prior_observation >= 365) 

# make outcome a perm table and update the attrition
cdm$outcome <- cdm$outcome %>% 
  compute(name = "outcome", temporary = FALSE, overwrite = TRUE) %>% 
  CDMConnector::recordCohortAttrition(reason="Excluded patients with less than 365 prior history" )


#remove people with no sex ----
cdm$outcome <- cdm$outcome %>% 
  filter(sex != "Male" | sex != "Female" ) 

cdm$outcome <- cdm$outcome %>% 
  compute(name = "outcome", temporary = FALSE, overwrite = TRUE) %>% 
  CDMConnector::recordCohortAttrition(reason="Excluded patients with no sex recorded" )

if(cdm$death %>% head(5) %>% count() %>% pull("n") > 0){
  # generate death cohort ----
  cli::cli_alert_info("Generating death cohort")
  
  cdm <- generateDeathCohortSet(cdm = cdm,
                                name = "cancer_death")
  # estimate survival ----
  cli::cli_alert_info("Estimating survival")
  
  # Analysis 1 
  # this is on a cohort of patients with only patients whose DOD == Cancer diagnosis
  suppressWarnings(
    surv1 <- estimateSingleEventSurvival(cdm = cdm,
                                        targetCohortTable = "outcome",
                                        outcomeCohortTable = "cancer_death",
                                        outcomeWashout = Inf,
                                        censorOnCohortExit = TRUE ,
                                        censorOnDate = as.Date("2023-01-01") ,
                                        strata = list(c("sex"),
                                                      c("age_group"),
                                                      c("age_group", "sex"),
                                                      c("diag_yr_gp"),
                                                      c("diag_yr_gp", "sex")),
                                        eventGap = c(365) ,
                                        estimateGap = c(1) ,
                                        restrictedMeanFollowUp = 1825,
                                        minimumSurvivalDays = 1,
                                        minCellCount = 0,
                                        returnParticipants = FALSE) )
  
  
  # pull out survival estimates at certain times (0.5  to 20 years) -----
  times_sur <- c(183, 365, 730, 1095, 1460, 1825, 3650, 5475, 7300)

  # survival results from analysis with no exclusions
  surv1_result <- tableSurvival(
    surv1,
    times = times_sur ,
    timeScale = "days",
    splitStrata = TRUE,
    header = c("estimate"),
    type = "tibble"
  )
  

  cli::cli_alert_info("Exporting survival attrition")
  
  attrition1 <- attrition(cdm$outcome) %>% 
    mutate(cdm_name = db_name) %>% 
    dplyr::inner_join(settings(cdm$outcome) %>%   
                        select("cohort_definition_id",  "cohort_name"), by ="cohort_definition_id" ) %>% 
    rename(outcome_cohort_name = cohort_name) 
  
  
  # attrition from survival with no cancer related attrition
  attrition1a <- attributes(surv1)$attrition %>% 
    rename(cohort_definition_id  = exposure_id) %>% 
    select(-c(outcome_id)) %>% 
    dplyr::inner_join(settings(cdm$outcome) %>%   
                        select("cohort_definition_id",  "cohort_name"), by ="cohort_definition_id" ) %>% 
    mutate(cdm_name = db_name) %>% 
    rename(outcome_cohort_name = cohort_name) 
  
  combined_attrition <- bind_rows(attrition1, attrition1a)
  
  combined_attrition1 <- combined_attrition %>%
    group_by(outcome_cohort_name) %>%
    mutate(reason_id = row_number()) %>%
    ungroup()
  
  #write the results
  write_csv(combined_attrition1, here("Results", paste0(db_name, "/", cdmName(cdm), "_survival_attrition_analysis1.csv"
  )))
  
  # analysis 1 #
  # export survival estimates ----
  cli::cli_alert_info("Exporting survival results")
  omopgenerics::exportSummarisedResult(surv1,
                                       fileName = paste0(cdmName(cdm), "_survival_results_analysis1.csv"),
                                       path = here("Results",db_name))  
  # n events n risk for study
  readr::write_csv(attr(surv1 %>% asSurvivalResult(), "events"), paste0(here("Results", db_name), paste0("/", cdmName(cdm), "_survival_events_analysis1.csv")))
  
  
  # summary table
  readr::write_csv(surv1_result, paste0(here("Results", db_name), paste0("/", cdmName(cdm), "_survival_summary_analysis1.csv")))
  
  # export survival summary ----
  cli::cli_alert_info("Exporting survival summary")

  cli::cli_alert_success("Survival Analysis Complete")

}
