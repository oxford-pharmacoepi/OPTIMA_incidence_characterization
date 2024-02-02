# demographics ----
cli::cli_alert_info("Summarising demographics")
summaryDemographics <- cdm$outcome %>%
  summariseCharacteristics(
    ageGroup = list(c(18,49),
                    c(50,59),
                    c(60,69),
                    c(70,79),
                    c(80,150)),
    tableIntersect = list(),
    minCellCount = 5
  )

write_csv(summaryDemographics, here("Results", paste0(db.name, "/", cdmName(cdm), "_summaryDemographics.csv"
)))

# comorbidities --------
cli::cli_alert_info("Summarising comorbidities")
summaryComorbidity <- cdm$mm_cohort %>%
  filter(cohort_definition_id == mm_p_cohort_id) %>%
  summariseCharacteristics(
    strata =  list(c("age_group"),
                   c("sex")),
    tableIntersect = list(),
    cohortIntersect = list("Comorbidities" = list(
      targetCohortTable = "mm_cohort_cond",
      value = "flag",
      window = list(c(-999999, -366),
                    c(-365, -31),
                    c(-30, -1),
                    c(0, 0))
    )
    ),
    minCellCount = 5
  )
write_csv(summaryComorbidity %>%
            suppressCounts(minCellCount = 5), here("Results", paste0(
              "summaryComorbidity_", cdmName(cdm), ".csv"
            )))


# medications -----
cli::cli_alert_info("Summarising medications")
summaryMedications <- cdm$mm_cohort %>%
  filter(cohort_definition_id == mm_p_cohort_id) %>%
  summariseCharacteristics(
    strata =  list(c("age_group"),
                   c("sex")),
    tableIntersect = list(),
    cohortIntersect = list(
      "Medications" = list(
        targetCohortTable = "mm_cohort_meds",
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
          here("Results", paste0(
            "summaryMedications_", cdmName(cdm), ".csv"
          )))

# treatments --------
cli::cli_alert_info("Summarising treatments")
summaryTreatments <- cdm$mm_cohort %>%
  filter(cohort_definition_id == mm_f_cohort_id) %>%
  summariseCharacteristics(
    strata =  list(c("age_group"),
                   c("sex")),
    tableIntersect = list(),
    cohortIntersect = list("Treatments" = list(
      targetCohortTable = "mm_cohort_treatments",
      value = "flag",
      window = list(c(1, 30),
                    c(1, 90),
                    c(1, 365))
    )
    ),
    minCellCount = 5
  )

write_csv(summaryTreatments %>%
            suppressCounts(minCellCount = 5),
          here("Results", paste0(
            "summaryTreatments_", cdmName(cdm), ".csv"
          )))

# large scale --------
cli::cli_alert_info("Running large scale characterisation")
ls_condition_occurrence <- cdm$mm_cohort %>%
  filter(cohort_definition_id == mm_p_cohort_id) %>%
  PatientProfiles::summariseLargeScaleCharacteristics(
    strata =  list(c("age_group"),
                   c("sex")),
    window = list(c(-999999, -366),
                  c(-365, -31),
                  c(-30, -1),
                  c(0, 0)),
    episodeInWindow = "condition_occurrence")
ls_drug_exposure <- cdm$mm_cohort %>%
  filter(cohort_definition_id == mm_p_cohort_id) %>%
  PatientProfiles::summariseLargeScaleCharacteristics(
    strata =  list(c("age_group"),
                   c("sex")),
    window = list(c(-365, -1),
                  c(-365, -31),
                  c(-30, -1),
                  c(0, 0),
                  c(1, 30),
                  c(1, 90),
                  c(1, 365)),
    episodeInWindow = "drug_exposure")

ls_characteristics <- ls_condition_occurrence %>%
  bind_rows(ls_drug_exposure)

write_csv(ls_characteristics, here("Results", paste0(
  "ls_characteristics_", cdmName(cdm), ".csv"
)))


