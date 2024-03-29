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
                          breaks = c(2003, 2008, 2013, 2018, 2023),
                          labels = c("2003-2007", "2008-2012", "2013-2017", "2018-2022"),
                          include.lowest = TRUE)) 


# remove people with any history of cancer (apart from skin cancer) -------
codelistExclusion <- CodelistGenerator::codesFromConceptSet(here::here("2_Study" ,  "1_InstantiateCohorts", "Exclusion"), cdm)
# add cancer concepts to exclusion concepts to make sure we capture all exclusions
codelistExclusion <- list(unique(Reduce(union_all, c(cancer_concepts_inc, codelistExclusion))))

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
codes2remove <- list(unique(Reduce(union_all, c(cancer_concepts_inc))))
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
                          breaks = c(2003, 2008, 2013, 2018, 2024),
                          labels = c("2003-2007", "2008-2012", "2013-2017", "2018-2022"),
                          include.lowest = TRUE)) 


# remove those outside the study period ------
cdm$outcome <- cdm$outcome %>%
  dplyr::filter(!is.na(diag_yr_gp)) %>% 
  dplyr::filter(cohort_start_date <= as.Date("2023-01-01"))

# make outcome a perm table and update the attrition
cdm$outcome <- cdm$outcome %>% 
  compute(name = "outcome", temporary = FALSE, overwrite = TRUE) %>% 
  recordCohortAttrition(reason="Exclude patients outside study period")


#for those with prior history remove those with less than 365 days of prior history -------
cdm$outcome <- cdm$outcome %>% 
  filter(prior_observation >= 365) 

cdm$outcome <- CDMConnector::recordCohortAttrition(cohort = cdm$outcome,
                                                   reason="Excluded patients with less than 365 prior history" )


# remove people with any history of cancer (apart from skin cancer) -------
codelistExclusion <- CodelistGenerator::codesFromConceptSet(here::here("2_Study" ,  "1_InstantiateCohorts", "Exclusion"), cdm)
# add cancer concepts to exclusion concepts to make sure we capture all exclusions
codelistExclusion <- list(unique(Reduce(union_all, c(cancer_concepts_inc, codelistExclusion))))

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
codes2remove <- list(unique(Reduce(union_all, c(cancer_concepts_inc))))
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
                                name = "cancer_death",
                                overwrite = TRUE)
  
  # estimate survival ----
  cli::cli_alert_info("Estimating survival")
  
  suppressWarnings(
  surv <- estimateSingleEventSurvival(cdm = cdm,
                                      followUpDays = 1827,
                                      censorOnCohortExit = TRUE ,
                                      censorOnDate = as.Date("2023-01-01") ,
                                      eventGap = c(5) ,
                                      estimateGap = c(5) ,
                                      targetCohortTable = "outcome",
                                      outcomeCohortTable = "cancer_death",
                                      strata = list(c("sex"),
                                                    c("age_group"),
                                                    c("age_group", "sex"),
                                                    c("diag_yr_gp"),
                                                    c("diag_yr_gp", "sex")),
                                      minCellCount = 5)
  )
  

  cli::cli_alert_info("Exporting survival attrition")
  write_csv(attrition(cdm$outcome) %>% 
              mutate(cdm_name = db_name) %>% 
              dplyr::inner_join(settings(cdm$outcome) %>%   
                                  select("cohort_definition_id",  "cohort_name"), by ="cohort_definition_id" ) %>% 
              rename(outcome_cohort_name = cohort_name)
            , here("Results", paste0(db_name, "/", cdmName(cdm), "_survival_attrition.csv"
  )))
  

  # export survival estimates ----
  cli::cli_alert_info("Exporting survival results")
  write_csv(surv, here("Results", paste0(db_name, "/", cdmName(cdm), "_survival_estimates.csv"
            )))
  
  # export survival summary ----
  cli::cli_alert_info("Exporting survival summary")
  write_csv(survivalSummary(surv), here("Results", paste0(db_name, "/", cdmName(cdm), "_survival_summary.csv"
  ))) 

  cli::cli_alert_success("Survival Analysis Complete")

}