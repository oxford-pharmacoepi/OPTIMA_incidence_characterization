
if(isTRUE(run_characterisation)){

# demographics ----
cli::cli_alert_info("Summarising Table One Demographics")
  
      
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
        )) 
    

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
    
    
    #only keeping those with sex recorded
    cdm$outcome <- cdm$outcome %>% 
      filter(sex %in% c("Male", "Female"))
    
    # make outcome a perm table and update the attrition
    cdm$outcome <- cdm$outcome %>% 
      compute(name = "outcome", temporary = FALSE, overwrite = TRUE) %>% 
      CDMConnector::recordCohortAttrition(reason="Excluded patients with no sex recorded" )
      

    
    
cli::cli_alert_info("Creating matched cohorts for large scale characteristics")
    
    cdm$outcome_matched <- matchCohorts(
      cohort = cdm$outcome,
      matchSex = TRUE,
      matchYearOfBirth = TRUE,
      ratio = 1,
      keepOriginalCohorts = TRUE,
      name = "outcome_matched"
    )
    
    
# add in demographics again (generates a warning but have suppressed it)
    
suppressWarnings(
    
    cdm$outcome_matched <- cdm$outcome_matched %>% 
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
        ))  
    
    )
 
       
suppressWarnings(
  
  summaryDemographics <- cdm$outcome_matched %>%
    CohortCharacteristics::summariseCharacteristics(
      strata = list(c("sex"),
                    c("age_group"),
                    c("age_group", "sex")),
      ageGroup = list( "18 to 49" = c(18, 49),
                       "50 to 59" = c(50, 59),
                       "60 to 69" = c(60, 69),
                       "70 to 79" = c(70, 79),
                       "80 to 89" = c(80, 89),
                       "90+" = c(90, 150))
    )
  
)

  
  
cli::cli_alert_info("Exporting demographics table one characteristics results")

omopgenerics::exportSummarisedResult(summaryDemographics,
                                       minCellCount = 5,
                                       path = here("Results",db_name),
                                       fileName = paste0(cdmName(cdm),
                                                         "_summary_characteristics_demographics.csv")
  )

cli::cli_alert_success("Summarising table one Demographics Complete")

# comorbidities --------
cli::cli_alert_info("Instantiating table one Comorbidities")

codelistConditions <- CodelistGenerator::codesFromConceptSet(here("1_InstantiateCohorts", "Conditions"), cdm)

cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm, 
                                              conceptSet = codelistConditions,
                                              name = "conditions",
                                              overwrite = TRUE)

cli::cli_alert_info("Summarising table one Comorbidities")
  
  suppressWarnings(
    
    summaryComorbidity <- cdm$outcome_matched %>%
      CohortCharacteristics::summariseCharacteristics(
        strata = list(c("sex"),
                      c("age_group"),
                      c("age_group", "sex")),
        ageGroup = list( "18 to 49" = c(18, 49),
                         "50 to 59" = c(50, 59),
                         "60 to 69" = c(60, 69),
                         "70 to 79" = c(70, 79),
                         "80 to 89" = c(80, 89),
                         "90+" = c(90, 150)),
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
            window = c(-30, -1)
          ),
          "Conditions on index date" = list(
            targetCohortTable = "conditions",
            window = c(0, 0)
          ),
          "Conditions 1 to 30 days after index date" = list(
            targetCohortTable = "conditions",
            window = c(1, 30)
          ),
          "Conditions 31 to 365 days after index date" = list(
            targetCohortTable = "conditions",
            window = c(31, 365)
          ),
          "Conditions 1 to 365 days after index date" = list(
            targetCohortTable = "conditions",
            window = c(1, 365)
          )
          
        )
      )
  )
  

cli::cli_alert_info("Exporting comorbidities table one characteristics results")
omopgenerics::exportSummarisedResult(summaryComorbidity,
            minCellCount = 5,
            path = here("Results",db_name),
            fileName = paste0(cdmName(cdm),
              "_summary_characteristics_comorbidity.csv")
            )

cli::cli_alert_success("Summarising table one Comorbidities Complete")

# medications -----
cli::cli_alert_info("Instantiating table one Medications")

# instantiate medications
codelistMedications <- CodelistGenerator::codesFromConceptSet(here("1_InstantiateCohorts", "Medications"), cdm)

cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(cdm = cdm, 
                                                         conceptSet = codelistMedications, 
                                                         name = "medications")

cli::cli_alert_info("Summarising table one Medications")
  
  suppressWarnings(
    
    summaryMedications <- cdm$outcome_matched %>%
      CohortCharacteristics::summariseCharacteristics(
        strata = list(c("sex"),
                      c("age_group"),
                      c("age_group", "sex")),
        ageGroup = list( "18 to 49" = c(18, 49),
                         "50 to 59" = c(50, 59),
                         "60 to 69" = c(60, 69),
                         "70 to 79" = c(70, 79),
                         "80 to 89" = c(80, 89),
                         "90+" = c(90, 150)),
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
          "Medications 31 to 365 days after index date" = list(
            targetCohortTable = "medications",
            window = c(31, 365)
          ),
          
          "Medications 1 to 365 days after index date" = list(
            targetCohortTable = "medications",
            window = c(1, 365)
          )
          
        )
      )
  )

cli::cli_alert_info("Exporting medications table one characteristics results")
omopgenerics::exportSummarisedResult(summaryMedications,
                                     minCellCount = 5,
                                     path = here("Results",db_name),
                                     fileName = paste0(cdmName(cdm),
                                                       "_summary_characteristics_medications.csv")
)

cli::cli_alert_success("Summarising table one Medications Complete")

cli::cli_alert_success("Table one Characterisation Analysis Complete")



cli::cli_alert_info("Starting large scale characteristics")


lsc <- cdm$outcome_matched %>% 
  summariseLargeScaleCharacteristics(
    window = list(c(-Inf, -366), c(-365, -31),
                  c(-30, -1), c(0, 0),
                  c(1, 30), c(31, 365),
                  c(366, Inf)),
    eventInWindow = c("condition_occurrence", 
                      "visit_occurrence",
                      "measurement", 
                      "procedure_occurrence",
                      "observation"),
    episodeInWindow = c("drug_era"),
    minimumFrequency = 0.0005,
    includeSource = TRUE)
    


omopgenerics::exportSummarisedResult(lsc,
                                     minCellCount = 5,
                                     path = here("Results",db_name),
                                     fileName = paste0(cdmName(cdm),
                                                       "_large_scale_characteristics.csv")
)

cli::cli_alert_success("Large scale characteristics complete")

}