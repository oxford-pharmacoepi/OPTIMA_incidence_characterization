
if(isTRUE(run_characterisation)){

# demographics ----
cli::cli_alert_info("Summarising Demographics")
  
  
  if(isFALSE(run_survival)){  
      
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
    
    
    #only keeping those with sex recorded
    cdm$outcome <- cdm$outcome %>% 
      filter(sex %in% c("Male", "Female"))
    
    # make outcome a perm table and update the attrition
    cdm$outcome <- cdm$outcome %>% 
      compute(name = "outcome", temporary = FALSE, overwrite = TRUE) %>% 
      CDMConnector::recordCohortAttrition(reason="Excluded patients with no sex recorded" )
      
  }



suppressWarnings(
  
  summaryDemographics <- cdm$outcome %>%
    CohortCharacteristics::summariseCharacteristics(
      strata = list(c("diag_yr_gp", "sex"),
                    c("diag_yr_gp"),
                    c("sex"),
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

  
  
  
cli::cli_alert_info("Exporting demographics characteristics results")

omopgenerics::exportSummarisedResult(summaryDemographics,
                                       minCellCount = 5,
                                       path = here("Results",db_name),
                                       fileName = paste0(cdmName(cdm),
                                                         "_summary_demographics_analysis1.csv")
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
  
  suppressWarnings(
    
    summaryComorbidity <- cdm$outcome %>%
      CohortCharacteristics::summariseCharacteristics(
        strata = list(c("diag_yr_gp", "sex"),
                      c("diag_yr_gp"),
                      c("sex"),
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
  

cli::cli_alert_info("Exporting comorbidities characteristics results")
omopgenerics::exportSummarisedResult(summaryComorbidity,
            minCellCount = 5,
            path = here("Results",db_name),
            fileName = paste0(cdmName(cdm),
              "_summary_comorbidity_analysis1.csv")
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
  
  suppressWarnings(
    
    summaryMedications <- cdm$outcome %>%
      CohortCharacteristics::summariseCharacteristics(
        strata = list(c("diag_yr_gp", "sex"),
                      c("diag_yr_gp"),
                      c("sex"),
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

cli::cli_alert_info("Exporting medications characteristics results")
omopgenerics::exportSummarisedResult(summaryMedications,
                                     minCellCount = 5,
                                     path = here("Results",db_name),
                                     fileName = paste0(cdmName(cdm),
                                                       "_summary_medications_analysis1.csv")
)

cli::cli_alert_success("Summarising Medications Complete")

cli::cli_alert_success("Characterisation Analysis Complete")



# run for the subset of survival patients removing cases with any prior cancers
if(isTRUE(run_survival)){  
  
  
  suppressWarnings(
    
    summaryDemographics <- cdm$outcome %>%
      CohortCharacteristics::summariseCharacteristics(
        strata = list(c("diag_yr_gp", "sex"),
                      c("diag_yr_gp"),
                      c("sex"),
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
  
  
  
  
  cli::cli_alert_info("Exporting demographics characteristics results")
  
  omopgenerics::exportSummarisedResult(summaryDemographics,
                                       minCellCount = 5,
                                       path = here("Results",db_name),
                                       fileName = paste0(cdmName(cdm),
                                                         "_summary_demographics_analysis2.csv")
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
  
  suppressWarnings(
    
    summaryComorbidity <- cdm$survival %>%
      CohortCharacteristics::summariseCharacteristics(
        strata = list(c("diag_yr_gp", "sex"),
                      c("diag_yr_gp"),
                      c("sex"),
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
            window = c(-31, -1)
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
  
  
  cli::cli_alert_info("Exporting comorbidities characteristics results")
  omopgenerics::exportSummarisedResult(summaryComorbidity,
                                       minCellCount = 5,
                                       path = here("Results",db_name),
                                       fileName = paste0(cdmName(cdm),
                                                         "_summary_comorbidity_analysis2.csv")
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
  
  suppressWarnings(
    
    summaryMedications <- cdm$survival %>%
      CohortCharacteristics::summariseCharacteristics(
        strata = list(c("diag_yr_gp", "sex"),
                      c("diag_yr_gp"),
                      c("sex"),
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
  
  cli::cli_alert_info("Exporting medications characteristics results")
  omopgenerics::exportSummarisedResult(summaryMedications,
                                       minCellCount = 5,
                                       path = here("Results",db_name),
                                       fileName = paste0(cdmName(cdm),
                                                         "_summary_medications_analysis2.csv")
  )
  
  cli::cli_alert_success("Summarising Medications Complete")
  
  cli::cli_alert_success("Characterisation Analysis Complete") 
  
  
}



}