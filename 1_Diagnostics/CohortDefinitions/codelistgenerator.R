# Manage project dependencies ------
# the following will prompt you to install the various packages used in the study
# install.packages("renv")
# renv::activate()
renv::restore()

# packages ---
library(Capr)
library(here)
library(DBI)
library(CDMConnector)
library(dplyr)
library(CodelistGenerator)

# db with vocab ----
server_dbi <- Sys.getenv("DB_SERVER_cdm_ukbiobank_202003_dbi") #ukb
#server_dbi <- Sys.getenv("DB_SERVER_cdm_gold_202207_dbi") #GOLD
user       <- Sys.getenv("DB_USER")
password   <- Sys.getenv("DB_PASSWORD")
port       <- Sys.getenv("DB_PORT")
host       <- Sys.getenv("DB_HOST")

# connect
db <- DBI::dbConnect(RPostgres::Postgres(),
                     dbname = server_dbi,
                     port = port,
                     host = host,
                     user = user,
                     password = password)

cdm <- CDMConnector::cdmFromCon(con = db,
                  cdmSchema = "public",
                  writeSchema = "results")


# check patient numbers
cdm$person %>%
  tally()

# check vocab version
# getVocabVersion(cdm = cdm)


getConceptClassId(cdm,
                  standardConcept = "Standard")

# [1] "Clinical Finding"  "Context-dependent" "HCPCS Modifier"    "ICDO Condition"
# [5] "Procedure"

# all lung cancer together
lungcancer_codes <- getCandidateCodes(
  cdm = cdm,
  keywords = "malignant neoplasm of lung",
  exclude = c("melanoma", "lymphoma", "secondary") ,
  domains = "Condition"
)

# add more exclusion criteria's
lungcancer_codes1 <- getCandidateCodes(
  cdm = cdm,
  keywords = "malignant neoplasm of lung",
  exclude = c("melanoma", "lymphoma", "sarcoma", "secondary") ,
  domains = "Condition"
)

# condition and observation
lungcancer_codes_cond_obs <- getCandidateCodes(
  cdm = cdm,
  keywords = "malignant neoplasm of lung",
  exclude = c("melanoma", "lymphoma", "secondary") ,
  domains = c("Condition", "Observation")
)


# small cell lung cancer
smallcell_lungcancer_codes <- getCandidateCodes(
  cdm = cdm,
  keywords = "Small cell carcinoma of lung",
  exclude = c("melanoma", "Non-small", "secondary") ,
  domains = "Condition"
)

# small cell lung cancer condition + observation
smallcell_lungcancer_codes_condition_obs <- getCandidateCodes(
  cdm = cdm,
  keywords = "Small cell carcinoma of lung",
  exclude = c("melanoma", "Non-small", "secondary") ,
  domains = c("Condition", "Observation")
)

# Non-small cell lung cancer
nonsmallcell_lungcancer_codes <- getCandidateCodes(
  cdm = cdm,
  keywords = c("Non-small cell lung cancer" ),
  exclude = c("melanoma", "lymphoma", "secondary") ,
  domains = "Condition"
)


nonsmallcell_lungcancer_codes_cond_obs <- getCandidateCodes(
  cdm = cdm,
  keywords = c("Non-small cell lung cancer" ),
  exclude = c("melanoma", "lymphoma", "secondary") ,
  domains = c("Condition", "Observation")
)

# Multiple myeloma -----
# original concepts ----
# mm_narrow_concepts <- c(
#   4258135, 4094548, 4111355, 4111356, 4112310,
#   4259972, 4188299, 4197600, 4082464, 4210177,
#   437233, 436059, 4214660, 4019477, 4079684,
#   4137510, 133154, 4028859, 760936, 133158,
#   4190641, 4190642, 4163558, 4024874, 4216139,
#   4300702, 764229, 4184985
# )
# mm_broad_concepts <- c(
#   4224628, 4258135, 4043447, 4094548, 46270015,
#   37209514, 4111355, 4111356, 4112310, 4259972,
#   4188299, 4197600, 4082464, 37016161, 437233,
#   4210177, 436059, 4214660, 4019477, 4137433,
#   4043713, 4079684, 42538151, 4137510, 133154,
#   4028859, 760936, 133158, 4190641, 4190642,
#   4163558, 4216139, 4024874, 4300702, 764229,
#   4184985, 4145040
# )

# add ICDO3 concepts ----
mm_icdo3_mappings <- cdm$concept_relationship %>%
  inner_join(cdm$concept %>%
               filter(vocabulary_id == "ICDO3")  %>%
               filter(substr(concept_code, 8L, 10L) %in%
                        c("C34")) ,
             by = c("concept_id_1"= "concept_id")) %>%
  filter(relationship_id == 'Maps to') %>%
  select(concept_id_2) %>%
  rename("concept_id" = "concept_id_2") %>%
  distinct() %>%
  inner_join(cdm$concept %>%
              filter(vocabulary_id == "ICDO3"),
            by = "concept_id") %>%
  collect()

mm_icdo3_mappings1 <- cdm$concept_relationship %>%
  inner_join(cdm$concept %>%
               filter(vocabulary_id == "ICDO3")  %>%
               filter(substr(concept_code, 1L, 10L) %in%
                        c("8041/3-C34")) ,
             by = c("concept_id_1"= "concept_id")) %>%
  filter(relationship_id == 'Maps to') %>%
  select(concept_id_2) %>%
  rename("concept_id" = "concept_id_2") %>%
  distinct() %>%
  inner_join(cdm$concept %>%
               filter(vocabulary_id == "ICDO3"),
             by = "concept_id") %>%
  collect()


# add to previous concept ids (which were snomed only)
mm_narrow_concepts <- c(mm_narrow_concepts, mm_icdo3_mappings$concept_id)
mm_broad_concepts <- c(mm_broad_concepts, mm_icdo3_mappings$concept_id)

# 1) mm narrow - no restrictions ----
mm_narrow <- cohort(
  entry = entry(
    conditionOccurrence(cs(mm_narrow_concepts, name = "mm_narrow_cond")),
    observation(cs(mm_narrow_concepts, name = "mm_narrow_obs")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)
writeCohort(mm_narrow, here::here("cohorts",
                                  "mm_narrow.json"))

# 2) mm narrow - 365 days prior observation ----
mm_narrow_365_prior<- cohort(
  entry = entry(
    conditionOccurrence(cs(mm_narrow_concepts, name = "mm_narrow_cond")),
    observation(cs(mm_narrow_concepts, name = "mm_narrow_obs")),
    observationWindow = continuousObservation(365L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)
writeCohort(mm_narrow_365_prior, here::here("cohorts",
                                            "mm_narrow_365_prior.json"))

# 3) mm narrow - 30 days post observation ----
mm_narrow_30_post<- cohort(
  entry = entry(
    conditionOccurrence(cs(mm_narrow_concepts, name = "mm_narrow_cond")),
    observation(cs(mm_narrow_concepts, name = "mm_narrow_obs")),
    observationWindow = continuousObservation(0L, 30L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)
writeCohort(mm_narrow_30_post, here::here("cohorts",
                                            "mm_narrow_30_post.json"))



# 4) mm broad - no restrictions ----
mm_broad <- cohort(
  entry = entry(
    conditionOccurrence(cs(mm_broad_concepts, name = "mm_broad_cond")),
    observation(cs(mm_broad_concepts, name = "mm_broad_obs")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)
writeCohort(mm_broad, here::here("cohorts",
                                  "mm_broad.json"))

# 5) mm broad - 365 days prior observation ----
mm_broad_365_prior<- cohort(
  entry = entry(
    conditionOccurrence(cs(mm_broad_concepts, name = "mm_broad_cond")),
    observation(cs(mm_broad_concepts, name = "mm_broad_obs")),
    observationWindow = continuousObservation(365L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)
writeCohort(mm_broad_365_prior, here::here("cohorts",
                                            "mm_broad_365_prior.json"))

# 6) mm broad - 30 days post observation ----
mm_broad_30_post<- cohort(
  entry = entry(
    conditionOccurrence(cs(mm_broad_concepts, name = "mm_broad_cond")),
    observation(cs(mm_broad_concepts, name = "mm_broad_obs")),
    observationWindow = continuousObservation(0L, 30L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)
writeCohort(mm_broad_30_post, here::here("cohorts",
                                          "mm_broad_30_post.json"))




# Characterisation concept sets -----

Capr::writeConceptSet(x = cs(descendants(317009), name = "asthma"),
                      path = here("characterisation_cohorts", "asthma.json"))
Capr::writeConceptSet(x = cs(descendants(255573), name = "chronic_obstructive_lung_disease"),
                      path = here("characterisation_cohorts", "chronic_obstructive_lung_disease.json"))
Capr::writeConceptSet(x = cs(descendants(4212540), name = "chronic_iver_disease"),
                      path = here("characterisation_cohorts", "chronic_iver_disease.json"))
Capr::writeConceptSet(x = cs(descendants(201606), name = "crohns_disease"),
                      path = here("characterisation_cohorts", "crohns_disease.json"))
Capr::writeConceptSet(x = cs(descendants(201820), name = "diabetes_mellitus"),
                      path = here("characterisation_cohorts", "diabetes_mellitus.json"))
Capr::writeConceptSet(x = cs(descendants(318800), name = "gastroesophageal_reflux_disease"),
                      path = here("characterisation_cohorts", "gastroesophageal_reflux_disease.json"))
Capr::writeConceptSet(x = cs(descendants(192671), name = "gastrointestinal_hemorrhage"),
                      path = here("characterisation_cohorts", "gastrointestinal_hemorrhage.json"))
Capr::writeConceptSet(x = cs(descendants(439727), name = "human_immunodeficiency_virus_infection"),
                      path = here("characterisation_cohorts", "human_immunodeficiency_virus_infection.json"))
Capr::writeConceptSet(x = cs(descendants(432867), name = "hyperlipidemia"),
                      path = here("characterisation_cohorts", "hyperlipidemia.json"))
Capr::writeConceptSet(x = cs(descendants(316866), name = "hypertensive_disorder"),
                      path = here("characterisation_cohorts", "hypertensive_disorder.json"))
Capr::writeConceptSet(x = cs(descendants(433736), name = "obesity"),
                      path = here("characterisation_cohorts", "obesity.json"))
Capr::writeConceptSet(x = cs(descendants(80180), name = "osteoarthritis"),
                      path = here("characterisation_cohorts", "osteoarthritis.json"))
Capr::writeConceptSet(x = cs(descendants(255848), name = "pneumonia"),
                      path = here("characterisation_cohorts", "pneumonia.json"))
Capr::writeConceptSet(x = cs(descendants(140168), name = "psoriasis"),
                      path = here("characterisation_cohorts", "psoriasis.json"))
Capr::writeConceptSet(x = cs(descendants(4030518), name = "renal_impairment"),
                      path = here("characterisation_cohorts", "renal_impairment.json"))
Capr::writeConceptSet(x = cs(descendants(81893), name = "ulcerative_collitis"),
                      path = here("characterisation_cohorts", "ulcerative_collitis.json"))
Capr::writeConceptSet(x = cs(descendants(81902), name = "urinary_tract_infection"),
                      path = here("characterisation_cohorts", "urinary_tract_infection.json"))
Capr::writeConceptSet(x = cs(descendants(4291005), name = "viral_hepatitis"),
                      path = here("characterisation_cohorts", "viral_hepatitis.json"))
Capr::writeConceptSet(x = cs(descendants(435783), name = "schizophrenia"),
                      path = here("characterisation_cohorts", "schizophrenia.json"))
Capr::writeConceptSet(x = cs(descendants(4182210), name = "dementia"),
                      path = here("characterisation_cohorts", "dementia.json"))
Capr::writeConceptSet(x = cs(descendants(381270), name = "parkinson_disease"),
                      path = here("characterisation_cohorts", "parkinson_disease.json"))
Capr::writeConceptSet(x = cs(descendants(440383), name = "depressive_disorder"),
                      path = here("characterisation_cohorts", "depressive_disorder.json"))
Capr::writeConceptSet(x = cs(descendants(441542), name = "anxiety"),
                      path = here("characterisation_cohorts", "anxiety.json"))
Capr::writeConceptSet(x = cs(descendants(438409), name = "attention_deficit_hyperactivity_disorder"),
                      path = here("characterisation_cohorts", "attention_deficit_hyperactivity_disorder.json"))
Capr::writeConceptSet(x = cs(descendants(443392), name = "malignant_neoplastic_disease"),
                      path = here("characterisation_cohorts", "malignant_neoplastic_disease.json"))








