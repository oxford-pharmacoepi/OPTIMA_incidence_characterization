# demographics ----
cli::cli_alert_info("Summarising demographics")
summaryDemographics <- cdm$outcome %>%
  summariseCharacteristics(
    strata = list(c("sex"),
                  c("age_group"),
                  c("age_group", "sex"),
                  c("diag_yr_gp"),
                  c("diag_yr_gp", "sex")),
    tableIntersect = list()
  )

write_csv(summaryDemographics, here("Results", paste0(db.name, "/", cdmName(cdm), "_summaryDemographics.csv"
)))

# comorbidities --------
cli::cli_alert_info("Instantiating comorbidities")

codelistConditions <- CodelistGenerator::codesFromConceptSet(here("2_Study", "1_InstantiateCohorts", "Conditions"), cdm)

cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm, 
                                              conceptSet = codelistConditions,
                                              name = "conditions",
                                              overwrite = TRUE)


cli::cli_alert_info("Summarising comorbidities")
summaryComorbidity <- cdm$outcome %>%
  summariseCharacteristics(
    strata = list(c("sex"),
                  c("age_group"),
                  c("age_group", "sex"),
                  c("diag_yr_gp"),
                  c("diag_yr_gp", "sex")),
    tableIntersect = list(),
    cohortIntersect = list("Comorbidities" = list(
      targetCohortTable = "conditions",
      value = "flag",
      window = list(c(-999999, -1) ,
                   c(-999999, -366),
                    c(-365, -31),
                    c(-30, -1),
                    c(0, 0))
    )
    )
  )

# instantiate obesity using diagnosis and measurements
cli::cli_alert_info("Instantiating obesity using diagnosis and measurements")

obesity_cohorts <- CDMConnector::readCohortSet(here::here(
  "2_Study" ,
  "1_InstantiateCohorts",
  "Obesity" 
))

cdm <- CDMConnector::generateCohortSet(cdm = cdm, 
                                       cohortSet = obesity_cohorts, 
                                       name = "obesity",
                                       computeAttrition = TRUE,
                                       overwrite = TRUE)

cli::cli_alert_info("Summarising obesity")
summaryObesity <- cdm$outcome %>%
  summariseCharacteristics(
    strata = list(c("sex"),
                  c("age_group"),
                  c("age_group", "sex"),
                  c("diag_yr_gp"),
                  c("diag_yr_gp", "sex")),
    tableIntersect = list(),
    cohortIntersect = list("Obesity_charybdis" = list(
      targetCohortTable = "obesity",
      value = "flag",
      window = list(c(-999999, -1) ,
                    c(-999999, -366),
                    c(-365, -31),
                    c(-30, -1),
                    c(0, 0))
    )
    )
  )


summaryComorbidity <- bind_rows(summaryComorbidity,
                                summaryObesity)

write_csv(summaryComorbidity %>%
            suppressCounts(minCellCount = 5), here("Results", paste0(cdmName(cdm),
              "_summaryComorbidity.csv"
            )))


# medications -----
cli::cli_alert_info("Summarising medications")

# instantiate medications
codelistMedications <- CodelistGenerator::codesFromConceptSet(here("2_Study" ,"1_InstantiateCohorts", "Medications"), cdm)

cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(cdm = cdm, 
                                                         conceptSet = codelistMedications, 
                                                         name = "medications")


summaryMedications <- cdm$outcome %>%
  summariseCharacteristics(
    strata = list(c("sex"),
                  c("age_group"),
                  c("age_group", "sex"),
                  c("diag_yr_gp"),
                  c("diag_yr_gp", "sex")),
    tableIntersect = list(),
    cohortIntersect = list(
      "Medications" = list(
        targetCohortTable = "medications",
        value = "flag",
        window = list(c(-365, -1),
                      c(-365, -31),
                      c(-30, -1),
                      c(0, 0),
                      c(1, 30),
                      c(1, 90),
                      c(1, 365))
      )),
    minCellCount = 5
  )

write_csv(summaryMedications %>%
            suppressCounts(minCellCount = 5),
          here("Results", paste0(cdmName(cdm),
            "_summaryMedications.csv"
          )))
