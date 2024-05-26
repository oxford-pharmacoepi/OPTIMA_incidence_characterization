
if(isTRUE(run_characterisation)){

# demographics ----
cli::cli_alert_info("Summarising Demographics")

  # if the survival is not run (FALSE) and incidence has been run (TRUE)
  if(isFALSE(run_survival) & isTRUE(run_incidence)){  
    
    # get participants from incidence analysis
    # make a list to put each set of participants
    pops <- list()
    
    # loop over each outcome pulling out the participants
    for (i in 1:nrow(inc_overall_parts)){
      
      #extract the participants for each cancer
      pops[[i]] <- participants(inc_overall_parts, as.numeric(inc_overall_parts$analysis_id[i])) %>% 
        select("subject_id", "outcome_start_date", "cohort_end_date") %>%
        filter(!is.na(outcome_start_date)) %>% 
        collect()
      
      pops[[i]] <- pops[[i]]  %>% 
        mutate(cohort_definition_id = inc_overall_parts$outcome_cohort_id[i]) %>%
        mutate(cohort_name = inc_overall_parts$outcome_cohort_name[i]) %>%
        rename(cohort_start_date = outcome_start_date) %>%
        collect()
      
    }  
    
    #bind the participants for all outcomes
    pops_all <- bind_rows(pops) 
    
    #create a new cdm ref object of participants
    cdm <- insertTable(cdm = cdm, name = "outcome_participants", table = pops_all, overwrite = TRUE)
    
    #create new cohort table
    cdm$outcome_participants <- newCohortTable(table = cdm$outcome_participants,
                                               cohortSetRef = tibble(cancer_concepts_inc[,c(1,2)])
    )
    
    cli::cli_alert_info("Add demographics to cohort")
    cdm$outcome <- cdm$outcome_participants %>% 
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
    
    suppressWarnings(
      
      summaryDemographics <- cdm$outcome %>%
        CohortCharacteristics::summariseCharacteristics(
          strata = list(c("sex"),
                        c("age_group"),
                        c("age_group", "sex")),
          ageGroup = list( "18 to 49" = c(18, 49),
                           "50 to 59" = c(50, 59),
                           "60 to 69" = c(60, 69),
                           "70 to 79" = c(70, 79),
                           "80 +" = c(80, 150))
        )
    )
    
  }
  
  
  # if survival has been run (TRUE) (does not matter if incidence has been run)
  if(isTRUE(run_survival) & isTRUE(run_incidence) |
     isTRUE(run_survival) & isFALSE(run_incidence)){  
    
    suppressWarnings(
      
      summaryDemographics <- cdm$outcome %>%
        CohortCharacteristics::summariseCharacteristics(
          strata = list(c("diag_yr_gp", "sex"),
                        c("diag_yr_gp"),
                        c("sex"),
                        c("age_group"),
                        c("age_group", "sex"),
                        c("year"),
                        c("year", "sex"),
                        c("year", "sex", "age_group")),
          ageGroup = list( "18 to 49" = c(18, 49),
                           "50 to 59" = c(50, 59),
                           "60 to 69" = c(60, 69),
                           "70 to 79" = c(70, 79),
                           "80 +" = c(80, 150))
        )
      
    )
    
    
  }
  
  
  if(isFALSE(run_survival) & isFALSE(run_incidence)
     & isFALSE(run_prevalence) & isTRUE(run_characterisation)){  
      
      # remove people outside the study period
      cdm$outcome <- cdm$outcome %>%
        dplyr::filter(cohort_start_date >= as.Date(study_start)) %>% 
        dplyr::filter(cohort_start_date <= as.Date("2023-01-01"))
      
      # make outcome a perm table and update the attrition
      cdm$outcome <- cdm$outcome %>% 
        compute(name = "outcome", temporary = FALSE, overwrite = TRUE) %>% 
        recordCohortAttrition(reason="Exclude patients outside study period")
      
      # add demographics
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
      
suppressWarnings(
      summaryDemographics <- cdm$outcome %>%
        CohortCharacteristics::summariseCharacteristics(
          strata = list(c("sex"),
                        c("age_group"),
                        c("age_group", "sex")),
          ageGroup = list( "18 to 49" = c(18, 49),
                           "50 to 59" = c(50, 59),
                           "60 to 69" = c(60, 69),
                           "70 to 79" = c(70, 79),
                           "80 +" = c(80, 150))
        )
      )
      
    }
  
  
  
cli::cli_alert_info("Exporting demographics characteristics results")

omopgenerics::exportSummarisedResult(summaryDemographics,
                                       minCellCount = 5,
                                       path = here("Results",db_name),
                                       fileName = paste0(cdmName(cdm),
                                                         "_summary_demographics.csv")
  )

cli::cli_alert_success("Summarising Demographics Complete")

# comorbidities --------
cli::cli_alert_info("Instantiating Comorbidities")

codelistConditions <- CodelistGenerator::codesFromConceptSet(here("1_InstantiateCohorts", "Conditions"), cdm)

cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm, 
                                              conceptSet = codelistConditions,
                                              name = "conditions",
                                              overwrite = TRUE)

cli::cli_alert_info("Summarising Comorbidities")

# run if survival not run but incidence has been run OR if only running characterisation
if(isFALSE(run_survival) & isTRUE(run_incidence) |
   isFALSE(run_survival) & isFALSE(run_incidence)
   & isFALSE(run_prevalence) & isTRUE(run_characterisation)){  
  
  
  suppressWarnings(
    
    summaryComorbidity <- cdm$outcome %>%
      CohortCharacteristics::summariseCharacteristics(
        strata = list(c("sex"),
                      c("age_group"),
                      c("age_group", "sex"),
                      c("year"),
                      c("year", "sex"),
                      c("year", "sex", "age_group")),
        ageGroup = list( "18 to 49" = c(18, 49),
                         "50 to 59" = c(50, 59),
                         "60 to 69" = c(60, 69),
                         "70 to 79" = c(70, 79),
                         "80 +" = c(80, 150)),
        cohortIntersectFlag = list(
          "Conditions prior to index date" = list(
            targetCohortTable = "conditions",
            window = c(-Inf, -1)
          ),
          "Conditions prior and up to 365 days before index date" = list(
            targetCohortTable = "conditions",
            window = c(-Inf, -366)
          ),
          "Conditions 365 and up to 31 days before index date" = list(
            targetCohortTable = "conditions",
            window = c(-365, -31)
          ),
          "Conditions 30 and up to 1 day before index date" = list(
            targetCohortTable = "conditions",
            window = c(-31, -1)
          ),
          "Conditions on index date" = list(
            targetCohortTable = "conditions",
            window = c(0, 0)
          )
        )
      )
  )
  
}
  
# runs if survival has been run
if(isTRUE(run_survival) & isTRUE(run_incidence) | 
   isTRUE(run_survival) & isFALSE(run_incidence)){ 
  
  suppressWarnings(
    
    summaryComorbidity <- cdm$outcome %>%
      CohortCharacteristics::summariseCharacteristics(
        strata = list(c("diag_yr_gp", "sex"),
                      c("diag_yr_gp"),
                      c("sex"),
                      c("age_group"),
                      c("age_group", "sex"),
                      c("year"),
                      c("year", "sex"),
                      c("year", "sex", "age_group")),
        ageGroup = list( "18 to 49" = c(18, 49),
                         "50 to 59" = c(50, 59),
                         "60 to 69" = c(60, 69),
                         "70 to 79" = c(70, 79),
                         "80 +" = c(80, 150)),
        cohortIntersectFlag = list(
          "Conditions prior to index date" = list(
            targetCohortTable = "conditions",
            window = c(-Inf, -1)
          ),
          "Conditions prior and up to 365 days before index date" = list(
            targetCohortTable = "conditions",
            window = c(-Inf, -366)
          ),
          "Conditions 365 and up to 31 days before index date" = list(
            targetCohortTable = "conditions",
            window = c(-365, -31)
          ),
          "Conditions 30 and up to 1 day before index date" = list(
            targetCohortTable = "conditions",
            window = c(-31, -1)
          ),
          "Conditions on index date" = list(
            targetCohortTable = "conditions",
            window = c(0, 0)
          )
        )
      )
  )
  
}

cli::cli_alert_info("Exporting comorbidities characteristics results")
omopgenerics::exportSummarisedResult(summaryComorbidity,
            minCellCount = 5,
            path = here("Results",db_name),
            fileName = paste0(cdmName(cdm),
              "_summary_comorbidity.csv")
            )

cli::cli_alert_success("Summarising Comorbidities Complete")

# medications -----
cli::cli_alert_info("Summarising Medications")

# instantiate medications
codelistMedications <- CodelistGenerator::codesFromConceptSet(here("1_InstantiateCohorts", "Medications"), cdm)

cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(cdm = cdm, 
                                                         conceptSet = codelistMedications, 
                                                         name = "medications")

cli::cli_alert_info("Summarising Medications")
# runs if survival has not been run but incidence has
if(isFALSE(run_survival) & isTRUE(run_incidence) |
   isFALSE(run_survival) & isFALSE(run_incidence)
   & isFALSE(run_prevalence) & isTRUE(run_characterisation)){  
  
  suppressWarnings(
    
    summaryMedications <- cdm$outcome %>%
      CohortCharacteristics::summariseCharacteristics(
        strata = list(c("sex"),
                      c("age_group"),
                      c("age_group", "sex"),
                      c("year"),
                      c("year", "sex"),
                      c("year", "sex", "age_group")),
        ageGroup = list( "18 to 49" = c(18, 49),
                         "50 to 59" = c(50, 59),
                         "60 to 69" = c(60, 69),
                         "70 to 79" = c(70, 79),
                         "80 +" = c(80, 150)),
        cohortIntersectFlag = list(
          "Medications 365 days prior to index date" = list(
            targetCohortTable = "medications",
            window = c(-365, -1)
          ),
          "Medications 365 to 31 days prior to index date" = list(
            targetCohortTable = "medications",
            window = c(-365, -31)
          ),
          "Medications 30 to 1 day prior to index date" = list(
            targetCohortTable = "medications",
            window = c(-30, -1)
          ),
          "Medications on index date" = list(
            targetCohortTable = "medications",
            window = c(0, 0)
          ),
          "Medications 1 to 30 days after index date" = list(
            targetCohortTable = "medications",
            window = c(1, 30)
          ),
          "Medications 1 to 30 days after index date" = list(
            targetCohortTable = "medications",
            window = c(1, 90)
          ),
          "Medications 1 to 30 days after index date" = list(
            targetCohortTable = "medications",
            window = c(1, 365)
          )
          
        )
      )
  )

  
}

# runs if survival has been run 
if(isTRUE(run_survival) & isTRUE(run_incidence) | 
   isTRUE(run_survival) & isFALSE(run_incidence)){  
  
  suppressWarnings(
    
    summaryMedications <- cdm$outcome %>%
      CohortCharacteristics::summariseCharacteristics(
        strata = list(c("diag_yr_gp", "sex"),
                      c("diag_yr_gp"),
                      c("sex"),
                      c("age_group"),
                      c("age_group", "sex"),
                      c("year"),
                      c("year", "sex"),
                      c("year", "sex", "age_group")),
        ageGroup = list( "18 to 49" = c(18, 49),
                         "50 to 59" = c(50, 59),
                         "60 to 69" = c(60, 69),
                         "70 to 79" = c(70, 79),
                         "80 +" = c(80, 150)),
        cohortIntersectFlag = list(
          "Medications 365 days prior to index date" = list(
            targetCohortTable = "medications",
            window = c(-365, -1)
          ),
          "Medications 365 to 31 days prior to index date" = list(
            targetCohortTable = "medications",
            window = c(-365, -31)
          ),
          "Medications 30 to 1 day prior to index date" = list(
            targetCohortTable = "medications",
            window = c(-30, -1)
          ),
          "Medications on index date" = list(
            targetCohortTable = "medications",
            window = c(0, 0)
          ),
          "Medications 1 to 30 days after index date" = list(
            targetCohortTable = "medications",
            window = c(1, 30)
          ),
          "Medications 1 to 30 days after index date" = list(
            targetCohortTable = "medications",
            window = c(1, 90)
          ),
          "Medications 1 to 30 days after index date" = list(
            targetCohortTable = "medications",
            window = c(1, 365)
          )
          
        )
      )
  )
  
}

cli::cli_alert_info("Exporting medications characteristics results")
omopgenerics::exportSummarisedResult(summaryMedications,
                                     minCellCount = 5,
                                     path = here("Results",db_name),
                                     fileName = paste0(cdmName(cdm),
                                                       "_summary_medications.csv")
)

cli::cli_alert_success("Summarising Medications Complete")

cli::cli_alert_success("Characterisation Analysis Complete")

}