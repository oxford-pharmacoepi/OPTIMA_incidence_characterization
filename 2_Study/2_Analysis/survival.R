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
                                "80+" = c(80, 150)
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
      "2019 to 2022" = as.Date(c("2019-01-01", "2023-01-01"))
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


# create a new survival object to carry on with survival we want to carry out characterisation on outcome table
cdm$survival <- cdm$outcome %>% 
  compute(name = "survival", temporary = FALSE, overwrite = TRUE) 

# remove people with any history of cancer (apart from skin cancer) -------
codelistExclusion <- CodelistGenerator::codesFromConceptSet(here::here("1_InstantiateCohorts", "Exclusion"), cdm)
# get codelists for cancers in question
cancer_codes_inc <- CodelistGenerator::codesFromCohort(here::here("1_InstantiateCohorts", "Cohorts", "incidence"), cdm)
# add cancer concepts to exclusion concepts to make sure we capture all exclusions
codelistExclusion <- list(unique(Reduce(union_all, c(cancer_codes_inc, codelistExclusion))))

#rename list of concepts
names(codelistExclusion) <- "anymalignancy"

# instaniate the exclusion cohort
cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm,
                                              conceptSet = codelistExclusion,
                                              name = "exclusion",
                                              overwrite = TRUE)

# create a flag of anyone with MALIGNANT NEOPLASTIC DISEASE (excluding skin cancer) prior to cancer diagnoses in our cohorts
cdm$survival <- cdm$survival %>%
  PatientProfiles::addCohortIntersectFlag(
    targetCohortTable = "exclusion",
    targetStartDate = "cohort_start_date",
    targetEndDate = "cohort_end_date",
    window = list(c(-Inf, -1))
  )

# remove those with any a prior malignancy (apart from skin cancer in prior history)
cdm$survival <- cdm$survival %>%
  dplyr::filter(anymalignancy_minf_to_m1 != 1)

# make a perm table and update the attrition
cdm$survival <- cdm$survival %>%
  compute(name = "survival", temporary = FALSE, overwrite = TRUE) %>% 
  recordCohortAttrition(reason="Exclude patients with any prior history of maglinancy (ex skin cancer)")


# remove any patients with other cancers on same date not in our list of cancers -----
# get the any malignancy codelist
codelistExclusion1 <- CodelistGenerator::codesFromConceptSet(here::here("1_InstantiateCohorts", "Exclusion"), cdm)

# merge all concepts for all cancers together
codes2remove <- list(unique(Reduce(union_all, c(cancer_codes_inc))))
names(codes2remove) <- "allmalignancy"

# remove lists from our cancers of interest from the any malignancy list
codes2remove <- list(codelistExclusion1$cancerexcludnonmelaskincancer[!codelistExclusion1$cancerexcludnonmelaskincancer %in% codes2remove$allmalignancy])
names(codes2remove) <- "allmalignancy"

#instantiate any malignancy codes minus our cancers of interest
cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm,
                                              conceptSet = codes2remove ,
                                              name = "allmalignancy",
                                              overwrite = TRUE)

# create a flag of anyone with MALIGNANT NEOPLASTIC DISEASE (excluding skin cancer) ON cancer diagnosis date but removing our codes of interest
# in doing so we are capturing people with other cancers on the same day and wont exclude everyone
cdm$survival <- cdm$survival %>%
  PatientProfiles::addCohortIntersectFlag(
    targetCohortTable = "allmalignancy",
    targetStartDate = "cohort_start_date",
    targetEndDate = "cohort_end_date",
    window = list(c(0, 0))
  )

# cdm$survival <- cdm$survival %>%
#   dplyr::distinct(subject_id, .keep_all = TRUE)

cdm$survival <- cdm$survival %>%
  dplyr::filter(allmalignancy_0_to_0 != 1)

#update the attrition
cdm$survival <- cdm$survival %>%
  compute(name = "survival", temporary = FALSE, overwrite = TRUE) %>% 
  CDMConnector::recordCohortAttrition(reason="Exclude patients with multiple cancers on different sites diagnosed on same day" )

  
#subset the cdm with final study population
cdm <- cdmSubsetCohort(cdm, cohortTable = "survival")

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
  
  # Analysis 2
  # this is on a cohort of patients with only patients whose DOD == Cancer diagnosis, prior history of cancer, multiple cancers on the same day
  suppressWarnings(
    surv2 <- estimateSingleEventSurvival(cdm = cdm,
                                        targetCohortTable = "survival",
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
  
  
  # survival results from analysis with exclusions
  surv2_result <- tableSurvival(
    surv2,
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
  
  combined_attrition <- combined_attrition %>%
    group_by(outcome_cohort_name) %>%
    mutate(reason_id = row_number()) %>%
    ungroup()
  
  
  # attrition (ie removing people with multiple cancers, previous history of cancer)
  attrition2 <- attrition(cdm$survival) %>% 
    mutate(cdm_name = db_name) %>% 
    dplyr::inner_join(settings(cdm$outcome) %>%   
                        select("cohort_definition_id",  "cohort_name"), by ="cohort_definition_id" ) %>% 
    rename(outcome_cohort_name = cohort_name) 

  # attrition from survival with cancer related attrition
  attrition2a <- attributes(surv2)$attrition %>% 
    rename(cohort_definition_id  = exposure_id) %>% 
    select(-c(outcome_id)) %>% 
    dplyr::inner_join(settings(cdm$outcome) %>%   
                        select("cohort_definition_id",  "cohort_name"), by ="cohort_definition_id" ) %>% 
    mutate(cdm_name = db_name) %>% 
    rename(outcome_cohort_name = cohort_name) 
  
  combined_attrition2 <- bind_rows(attrition2, attrition2a)
  
  combined_attrition2 <- combined_attrition2 %>%
    group_by(outcome_cohort_name) %>%
    mutate(reason_id = row_number()) %>%
    ungroup()
  
  
  
  #write the results
  write_csv(combined_attrition1, here("Results", paste0(db_name, "/", cdmName(cdm), "_survival_attrition_analysis1.csv"
  )))
  
  write_csv(combined_attrition2, here("Results", paste0(db_name, "/", cdmName(cdm), "_survival_attrition_analysis2.csv"
  )))
  
  
  # analysis 1 ###########
  # export survival estimates ----
  cli::cli_alert_info("Exporting survival results")
  omopgenerics::exportSummarisedResult(surv1,
                                       fileName = paste0(cdmName(cdm), "_survival_results_analysis1.csv"),
                                       path = here("Results",db_name))  
  # n events n risk for study
  readr::write_csv(attr(surv1 %>% asSurvivalResult(), "events"), paste0(here("Results", db_name), paste0("/", cdmName(cdm), "_survival_events_analysis1.csv")))
  
  
  # summary table
  readr::write_csv(surv1_result, paste0(here("Results", db_name), paste0("/", cdmName(cdm), "_survival_summary_analysis1.csv")))
  
  
  # analysis 2 ###########
  # export survival estimates ----
  omopgenerics::exportSummarisedResult(surv2,
                                       fileName = paste0(cdmName(cdm), "_survival_results_analysis2.csv"),
                                       path = here("Results",db_name))  
  
  # n events n risk for study
  readr::write_csv(attr(surv2 %>% asSurvivalResult(), "events"), paste0(here("Results", db_name), paste0("/", cdmName(cdm), "_survival_events_analysis2.csv")))
  

  # summary table
  readr::write_csv(surv2_result, paste0(here("Results", db_name), paste0("/", cdmName(cdm), "_survival_summary_analysis2.csv")))
  
  
  
  # export survival summary ----
  cli::cli_alert_info("Exporting survival summary")

  cli::cli_alert_success("Survival Analysis Complete")

}
