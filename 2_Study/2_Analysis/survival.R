# KM survival analysis ---

cli::cli_alert_info("- Getting participants for survival")
  
if(isTRUE(run_incidence)){
  
# get participants from incidence analysis to feed into survival analysis
cdm$outcome_participants <- participants(inc_overall_parts, 1) %>% 
  select("subject_id", "outcome_start_date") %>% 
  filter(!is.na(outcome_start_date)) %>% 
  rename("cohort_start_date" = "outcome_start_date") %>% 
  compute(name = "outcome_participants")

# filter out participants not present in this and record in attrition
cdm$outcome <- cdm$outcome %>% 
  dplyr::right_join(cdm$outcome_participants %>%
                      select("subject_id") %>% 
                      distinct(),
                    by = c("subject_id")) %>%
  dplyr::compute()

cdm$outcome <- cdm$outcome %>% 
  compute(name = "outcome", temporary = FALSE, overwrite = TRUE) %>% 
  recordCohortAttrition(reason="Excluding cases from excluded from incidence analysis")

cli::cli_alert_info("Add demographics to cohort")
cdm$outcome <- cdm$outcome %>% 
  PatientProfiles::addDemographics(
    ageGroup = list(
      "age_group" =
        list(
          "18 to 49" = c(18, 49),
          "50 to 39" = c(50, 59),
          "60 to 59" = c(60, 69),
          "70 to 79" = c(70, 79),
          "80 +" = c(80, 150)
        )
    )) %>% 
  mutate(year = year(cohort_start_date))

# create diagnosis age band groups
cdm$outcome <- cdm$outcome %>%
  mutate(diag_yr_gp = cut(year,
                        breaks = c(2003, 2007, 2011, 2015, 2019, 2024),
                        labels = c("2003-2006", "2007-2010", "2011-2014", "2015-2018", "2019-2023"),
                        include.lowest = TRUE))


# remove people with any history of cancer (apart from skin cancer) -------
codelistExclusion <- CodelistGenerator::codesFromConceptSet(here::here("2_Study" ,  "1_InstantiateCohorts", "Exclusion"), cdm)
# get codelists for cancers in question
cancer_codes_inc <- CodelistGenerator::codesFromCohort(here::here("2_Study" ,  "1_InstantiateCohorts", "Cohorts", "incidence"), cdm)

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
cdm$outcome <- cdm$outcome %>%
  PatientProfiles::addCohortIntersectFlag(
    targetCohortTable = "exclusion",
    targetStartDate = "cohort_start_date",
    targetEndDate = "cohort_end_date",
    window = list(c(-Inf, -1))
  )

# remove those with any a prior malignancy (apart from skin cancer in prior history)
cdm$outcome <- cdm$outcome %>%
  dplyr::filter(anymalignancy_minf_to_m1 != 1)

# make outcome a perm table and update the attrition
cdm$outcome <- cdm$outcome %>% 
  compute(name = "outcome", temporary = FALSE, overwrite = TRUE) %>% 
  recordCohortAttrition(reason="Exclude patients with any prior history of maglinancy (ex skin cancer)")

# remove any patients with other cancers on same date not in our list of cancers -----
# get the any malignancy codelist
codelistExclusion1 <- CodelistGenerator::codesFromConceptSet(here::here("2_Study" ,  "1_InstantiateCohorts", "Exclusion"), cdm)

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
cdm$outcome <- cdm$outcome %>%
  PatientProfiles::addCohortIntersectFlag(
    targetCohortTable = "allmalignancy",
    targetStartDate = "cohort_start_date",
    targetEndDate = "cohort_end_date",
    window = list(c(0, 0))
  )


cdm$outcome <- cdm$outcome %>%
  dplyr::distinct(subject_id, .keep_all = TRUE)

cdm$outcome <- cdm$outcome %>%
  dplyr::filter(allmalignancy_0_to_0 != 1)

#update the attrition
cdm$outcome <- cdm$outcome %>% 
  compute(name = "outcome", temporary = FALSE, overwrite = TRUE) %>% 
  CDMConnector::recordCohortAttrition(reason="Exclude patients with multiple cancers on different sites diagnosed on same day" )

#remove people with date of death outside of their observation period end -----
cdm$outcome <- cdm$outcome %>% 
  dplyr::left_join(cdm$death %>%
                     select("person_id",  "death_date") %>%
                     distinct(),
                   by = c("subject_id"= "person_id")) %>%
  dplyr::left_join(cdm$observation_period %>%
                     select("person_id",  "observation_period_end_date") %>%
                     distinct(),
                   by = c("subject_id"= "person_id")) %>%
  dplyr::compute()


cdm$outcome <- cdm$outcome %>% 
  filter(is.na(death_date) | death_date <= observation_period_end_date)

#update the attrition
cdm$outcome <- cdm$outcome %>% 
  compute(name = "outcome", temporary = FALSE, overwrite = TRUE) %>% 
  CDMConnector::recordCohortAttrition(reason="Exclude patients where death occurs outside of observation end date" )


} else {
# add sex and age to cohorts ----
cli::cli_alert_info("Add demographics to cohort")
cdm$outcome <- cdm$outcome %>% 
  PatientProfiles::addDemographics(
                          ageGroup = list(
                            "age_group" =
                              list(
                                "18 to 49" = c(18, 49),
                                "50 to 39" = c(50, 59),
                                "60 to 59" = c(60, 69),
                                "70 to 79" = c(70, 79),
                                "80+" = c(80, 150)
                              )
                          )) %>% 
  mutate(year = year(cohort_start_date))
  
# create diagnosis age band groups
cdm$outcome <- cdm$outcome %>%
  mutate(diag_yr_gp = cut(year,
                          breaks = c(2003, 2007, 2011, 2015, 2019, 2024),
                          labels = c("2003-2006", "2007-2010", "2011-2014", "2015-2018", "2019-2023"),
                          include.lowest = TRUE))


# remove those outside the study period ------
cdm$outcome <- cdm$outcome %>%
  dplyr::filter(!is.na(diag_yr_gp)) %>% 
  dplyr::filter(cohort_start_date <= as.Date("2023-01-01"))

# make outcome a perm table and update the attrition
cdm$outcome <- cdm$outcome %>% 
  compute(name = "outcome", temporary = FALSE, overwrite = TRUE) %>% 
  recordCohortAttrition(reason="Exclude patients outside study period")

#exclude those under 18 years of age  -------
cdm$outcome <- cdm$outcome %>% 
  filter(age >= 18) 

# make outcome a perm table and update the attrition
cdm$outcome <- CDMConnector::recordCohortAttrition(cohort = cdm$outcome,
                                                   reason="Excluded patients younger than 18 years of age" )

#for those with prior history remove those with less than 365 days of prior history -------
cdm$outcome <- cdm$outcome %>% 
  filter(prior_observation >= 365) 

# make outcome a perm table and update the attrition
cdm$outcome <- CDMConnector::recordCohortAttrition(cohort = cdm$outcome,
                                                   reason="Excluded patients with less than 365 prior history" )


# remove people with any history of cancer (apart from skin cancer) -------
codelistExclusion <- CodelistGenerator::codesFromConceptSet(here::here("2_Study" ,  "1_InstantiateCohorts", "Exclusion"), cdm)
# get codelists for cancers in question
cancer_codes_inc <- CodelistGenerator::codesFromCohort(here::here("2_Study" ,  "1_InstantiateCohorts", "Cohorts", "incidence"), cdm)
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
cdm$outcome <- cdm$outcome %>%
  PatientProfiles::addCohortIntersectFlag(
    targetCohortTable = "exclusion",
    targetStartDate = "cohort_start_date",
    targetEndDate = "cohort_end_date",
    window = list(c(-Inf, -1))
  )

# remove those with any a prior malignancy (apart from skin cancer in prior history)
cdm$outcome <- cdm$outcome %>%
  dplyr::filter(anymalignancy_minf_to_m1 != 1)

# make outcome a perm table and update the attrition
cdm$outcome <- cdm$outcome %>% 
  compute(name = "outcome", temporary = FALSE, overwrite = TRUE) %>% 
  recordCohortAttrition(reason="Exclude patients with any prior history of maglinancy (ex skin cancer)")


# remove any patients with other cancers on same date not in our list of cancers -----
# get the any malignancy codelist
codelistExclusion1 <- CodelistGenerator::codesFromConceptSet(here::here("2_Study" ,  "1_InstantiateCohorts", "Exclusion"), cdm)

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
cdm$outcome <- cdm$outcome %>%
  PatientProfiles::addCohortIntersectFlag(
    targetCohortTable = "allmalignancy",
    targetStartDate = "cohort_start_date",
    targetEndDate = "cohort_end_date",
    window = list(c(0, 0))
  )

cdm$outcome <- cdm$outcome %>%
  dplyr::distinct(subject_id, .keep_all = TRUE)

cdm$outcome <- cdm$outcome %>%
  dplyr::filter(allmalignancy_0_to_0 != 1)

#update the attrition
cdm$outcome <- cdm$outcome %>% 
  compute(name = "outcome", temporary = FALSE, overwrite = TRUE) %>% 
  CDMConnector::recordCohortAttrition(reason="Exclude patients with multiple cancers on different sites diagnosed on same day" )


#remove people with date of death outside of their observation period end -----
cdm$outcome <- cdm$outcome %>% 
  dplyr::left_join(cdm$death %>%
                     select("person_id",  "death_date") %>%
                     distinct(),
                   by = c("subject_id"= "person_id")) %>%
  dplyr::left_join(cdm$observation_period %>%
                     select("person_id",  "observation_period_end_date") %>%
                     distinct(),
                   by = c("subject_id"= "person_id")) %>%
  dplyr::compute()


cdm$outcome <- cdm$outcome %>% 
  filter(is.na(death_date) | death_date <= observation_period_end_date)

#update the attrition
cdm$outcome <- cdm$outcome %>% 
  compute(name = "outcome", temporary = FALSE, overwrite = TRUE) %>% 
  CDMConnector::recordCohortAttrition(reason="Exclude patients where death occurs outside of observation end date" )

}
  
#subset the cdm with final study population
cdm <- cdmSubsetCohort(cdm, cohortTable = "outcome")

if(cdm$death %>% head(5) %>% count() %>% pull("n") > 0){
  # generate death cohort ----
  cli::cli_alert_info("Generating death cohort")
  cdm <- generateDeathCohortSet(cdm = cdm,
                                name = "cancer_death")
  # estimate survival ----
  cli::cli_alert_info("Estimating survival")
  
  # Analysis 1 
  # this creates survival stratified by diagnosis groups with follow up truncated at 5 years
  suppressWarnings(
    
    surv <- estimateSingleEventSurvival(cdm = cdm,
                                        targetCohortTable = "outcome",
                                        outcomeCohortTable = "cancer_death",
                                        outcomeWashout = Inf,
                                        censorOnCohortExit = TRUE ,
                                        censorOnDate = as.Date("2023-01-01") ,
                                        followUpDays = 1825,
                                        strata = list(c("sex"),
                                                      c("age_group"),
                                                      c("age_group", "sex"),
                                                      c("diag_yr_gp"),
                                                      c("diag_yr_gp", "sex")),
                                        eventGap = c(30) ,
                                        estimateGap = c(1) ,
                                        restrictedMeanFollowUp = NULL,
                                        minimumSurvivalDays = 1,
                                        minCellCount = 5,
                                        returnParticipants = FALSE) )
  
  # Analysis 2 
  # follow up not truncated and not carrying out stratification by diagnosis year
  # also includes restricted mean survival
  suppressWarnings(
    
    surv1 <- estimateSingleEventSurvival(cdm = cdm,
                                         followUpDays = Inf,
                                         censorOnCohortExit = TRUE ,
                                         censorOnDate = as.Date("2023-01-01") ,
                                         eventGap = c(365) ,
                                         estimateGap = c(1) ,
                                         restrictedMeanFollowUp = 1825,
                                         minimumSurvivalDays = 1,
                                         targetCohortTable = "outcome",
                                         outcomeCohortTable = "cancer_death",
                                         strata = list(c("sex"),
                                                       c("age_group"),
                                                       c("age_group", "sex")),
                                         minCellCount = 5)
  )
  

  cli::cli_alert_info("Exporting survival attrition")
  
  # initial attrition (ie removing people with multiple cancers, previous history of cancer)
  attrition1 <- attrition(cdm$outcome) %>% 
    mutate(cdm_name = db_name) %>% 
    dplyr::inner_join(settings(cdm$outcome) %>%   
                        select("cohort_definition_id",  "cohort_name"), by ="cohort_definition_id" ) %>% 
    rename(outcome_cohort_name = cohort_name) 
  
  # will need to duplicate based on how many outcomes AND will need to update the numbers of the attrition so it runs from 1-10 not 1-6 then 1-3
  attrition2 <- attributes(surv)$attrition %>% 
    rename(cohort_definition_id  = outcome_id) %>% 
    select(-c(exposure_id)) %>% 
    dplyr::inner_join(settings(cdm$outcome) %>%   
                        select("cohort_definition_id",  "cohort_name"), by ="cohort_definition_id" ) %>% 
    mutate(cdm_name = db_name)
  
  #bind attrition together
  attrition_final <- bind_rows(attrition1, attrition2)
  
  #write the results
  write_csv(attrition_final, here("Results", paste0(db_name, "/", cdmName(cdm), "_survival_attrition.csv"
  )))
  
  # export survival results ----
  # this will be exported as a using omopgenerics exportSummarisedResult argument
  # then in the shiny we can read this in and then use TableSurvival to make a nice table
  # analysis 1
  cli::cli_alert_info("Exporting survival results")
  omopgenerics::exportSummarisedResult(surv,
                                       fileName = paste0(cdmName(cdm), "_survival_results_analysis1.csv"),
                                       path = here("Results",db_name))  
  
  # analysis 2
  cli::cli_alert_info("Exporting survival results")
  omopgenerics::exportSummarisedResult(surv1,
                                       fileName = paste0(cdmName(cdm), "_survival_results_analysis2.csv"),
                                       path = here("Results",db_name))  
  
  # export survival summary ----
  cli::cli_alert_info("Exporting survival summary")

  cli::cli_alert_success("Survival Analysis Complete")

}
