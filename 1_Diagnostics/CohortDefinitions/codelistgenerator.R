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
library(tidyr)
library(CodelistGenerator)
library(ggplot2)

# db with vocab ----
server_dbi <- Sys.getenv("DB_SERVER_cdm_thin_fr_202308_dbi")
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

# Set database details -----
# The name of the schema that contains the OMOP CDM with patient-level data
cdm_database_schema <- "public"

# The name of the schema that contains the vocabularies
# (often this will be the same as cdm_database_schema)
vocabulary_database_schema <- cdm_database_schema

# The name of the schema where results tables will be created
results_database_schema <- "results"

# database metadata and connection details -----
# The name/ acronym for the database
db_name<-"THIN_fr"

# Name of outcome table in the result table where the outcome cohorts will be stored
# Note, if there is an existing table in your results schema with the same names
# it will be overwritten
table_stem <- "dnclgenoptima"

# create cdm reference ----
cdm <- CDMConnector::cdm_from_con(con = db,
                                  cdm_schema = cdm_database_schema,
                                  write_schema = c("schema" = results_database_schema,
                                                   "prefix" = table_stem),
                                  cdm_name = db_name,
                                  achilles_schema = results_database_schema )


# check patient numbers
cdm$person %>%
  tally()

# check vocab version
# getVocabVersion(cdm = cdm)

getConceptClassId(cdm,
                  standardConcept = "Standard")

# [1] "Clinical Finding"  "Context-dependent" "HCPCS Modifier"    "ICDO Condition"
# [5] "Procedure"

# Broad lung cancer including cancers of trachea and bronchus and lower respiratory tract
lungcancer_codes <- getCandidateCodes(
  cdm = cdm,
  keywords = c("malignant neoplasm of lung",
               "malignant neoplasm of trachea",
               "Primary malignant neoplasm of bronchus",
               "Oat cell carcinoma of lung",
               "Oat cell carcinoma of trachea",
               "Oat cell carcinoma of main bronchus" ,
               "malignant neoplasm of lower respiratory tract") ,
  exclude = c("melanoma",
              "lymphoma",
              "sarcoma",
              "secondary",
              "metastasis",
              "lymphocytic",
              "benign",
              "hodgkin",
              "neuroendocrine",
              "rhabdomyosarcoma",
              "angiomyosarcoma",
              "fibrosarcoma",
              "leiomyosarcoma",
              "hemangiosarcoma",
              "pseudosarcomatous",
              "carcinosarcoma",
              "leukemia",
              "blastoma",
              "T-cell",
              "atelectasis",
              "plasmacytoma",
              "mesenchymoma",
              "heavy chain disease" ,
              "ectomesenchymoma",
              "myeloproliferative",
              "sezary",
              "lymphoid",
              "epithelioid hemangioendothelioma"

  ) ,
  domains = c("Condition", "Observation"),
  standardConcept = "Standard",
  searchInSynonyms = FALSE,
  searchNonStandard = FALSE,
  includeDescendants = TRUE,
  includeAncestor = TRUE
)

write.csv(lungcancer_codes, here::here("preliminary_cohorts" ,
                                       paste0(cdmName(cdm), "_lungCancer.csv")), row.names = FALSE)

#trying out orphan codes for lung cancer
lungcancer_orphan_codes <- CodelistGenerator::summariseOrphanCodes(x = list("lung_cancer" = lungcancer_codes$concept_id),
                                cdm = cdm)


lungcancer_orphan_codes <- lungcancer_orphan_codes %>%
  separate(additional_level, into = c("Description", "Concept ID"), sep = " ; ")

write.csv(lungcancer_orphan_codes, here::here("preliminary_cohorts" ,
                                       paste0(cdmName(cdm), "_lungCancer_orphan.csv")), row.names = FALSE)


# Creating cohort files ------------

# read in reviewed list of codelists
reviewed_code_list <- read.csv(here::here("preliminary_cohorts" , "reviewed" ,
                                   paste0(cdmName(cdm), "_lungCancer_reviewed.csv")))

# lung cancer all incidence (everything plus SCLC) ----
all_inc <- reviewed_code_list %>%
  filter(lung_cancer_all == "y") %>%
  filter(domain_id == "Condition") %>%
  pull(concept_id)

# Broad lung cancer incidence --- excludes SCLC and includes neoplasm of respiratory tract
broad_inc <- reviewed_code_list %>%
  filter(lung_cancer_broad == "y") %>%
  filter(domain_id == "Condition") %>%
  pull(concept_id)

# narrow lung cancer incidence - as above but remove neoplasm of respiratory tract
narrow_inc <- reviewed_code_list %>%
  filter(lung_cancer_narrow == "y") %>%
  filter(domain_id == "Condition") %>%
  pull(concept_id)

# all lung cancer prevalence
all_prev <- reviewed_code_list %>%
  filter(lung_cancer_all_prev == "y") %>%
  filter(domain_id == "Condition") %>%
  pull(concept_id)

# Broad lung cancer prevalence
broad_prev <- reviewed_code_list %>%
  filter(lung_cancer_broad_prev == "y") %>%
  filter(domain_id == "Condition") %>%
  pull(concept_id)

# narrow lung cancer prevalence
narrow_prev <- reviewed_code_list %>%
  filter(lung_cancer_narrow_prev == "y") %>%
  filter(domain_id == "Condition") %>%
  pull(concept_id)

# small cell lung cancer
sclc <- reviewed_code_list %>%
  filter(small_cell_lung_cancer == "y") %>%
  filter(domain_id == "Condition") %>%
  pull(concept_id)

# create cohorts

# 4 definitions
# All lung cancer
# Broad lung cancer exc SCLC and malignant neoplasm of resp tract (excluding codes SCLC)
# Narrow lung cancer exc SCLC and malignant neoplasm of resp tract (excluding codes for SCLC)
# Small cell

# INCIDENCE ---------
# 1 lung cancer all incidence
lung_cancer_incident_all <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(all_inc,
                                                name = "lung_cancer_all_inc"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(lung_cancer_incident_all, here::here("preliminary_cohorts",
                                                   "lung_cancer_incident_all.json"))

# 2 broad incidence
lung_cancer_incident_broad <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(broad_inc,
                                                name = "lung_cancer_inc_broad",
                                                exclude(sclc)
                                                ), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(lung_cancer_incident_broad, here::here("preliminary_cohorts",
                                                     "lung_cancer_incident_broad.json"))

# 3 narrow incidence
lung_cancer_incident_narrow <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(narrow_inc,
                                                name = "lung_cancer_narrow_inc",
                                                exclude(sclc)), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(lung_cancer_incident_narrow, here::here("preliminary_cohorts",
                                                   "lung_cancer_incident_narrow.json"))


# 4 sclc incidence
small_cell_lung_cancer <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(sclc, name = "small_cell_lung_cancer"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(small_cell_lung_cancer, here::here("preliminary_cohorts",
                                                    "small_cell_lung_cancer.json"))

# PREV to end ---------
# 1 all
total_prev_lung_cancer_all <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(all_prev, name = "all_lung_cancer_end"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(total_prev_lung_cancer_all, here::here("preliminary_cohorts",
                                                     "all_lung_cancer_end.json"))

# 2 broad prev end
total_prev_lung_cancer_broad <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(broad_prev,
                                                name = "broad_lung_cancer_end",
                                                exclude(sclc)), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(total_prev_lung_cancer_broad, here::here("preliminary_cohorts",
                                                     "broad_lung_cancer_end.json"))


# 3 narrow prev end
total_prev_lung_cancer_narrow <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(narrow_prev,
                                                name = "narrow_lung_cancer_end",
                                                exclude(sclc)), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(total_prev_lung_cancer_narrow, here::here("preliminary_cohorts",
                                                      "narrow_lung_cancer_end.json"))


# PREV to 2 years ---------

# 1 all prev 2 year
partial_prev2y_lung_cancer_all <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(all_prev,
                                                name = "all_lung_cancer_2y"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(fixedExit("startDate", 720L)
  )
)

writeCohort(partial_prev2y_lung_cancer_all, here::here("preliminary_cohorts",
                                                         "all_lung_cancer_2y.json"))


# 2 broad prev 2 year
partial_prev2y_lung_cancer_broad <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(broad_prev,
                                                name = "broad_lung_cancer_2y",
                                                exclude(sclc)
                                                ), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(fixedExit("startDate", 720L)
  )
)

writeCohort(partial_prev2y_lung_cancer_broad, here::here("preliminary_cohorts",
                                                         "broad_lung_cancer_2y.json"))



# 3 narrow prev 2 year
partial_prev2y_lung_cancer_narrow <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(narrow_prev,
                                                name = "narrow_lung_cancer_2y",
                                                exclude(sclc)
                                                ), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(fixedExit("startDate", 720L)
  )
)

writeCohort(partial_prev2y_lung_cancer_narrow, here::here("preliminary_cohorts",
                                                          "narrow_lung_cancer_2y.json"))


# 4 sclc prev 2 year
partial_prev2y_sclc <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(sclc, name = "sclc_2y"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(fixedExit("startDate", 720L)
  )
)

writeCohort(partial_prev2y_sclc, here::here("preliminary_cohorts",
                                            "small_cell_lung_cancer_2y.json"))


# PREV to 5 years ---------

# 1 all prev 5 year
partial_prev5y_lung_cancer_all <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(all_prev, name = "all_lung_cancer_5y"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(fixedExit("startDate", 1825L)
  )
)

writeCohort(partial_prev5y_lung_cancer_all, here::here("preliminary_cohorts",
                                                         "all_lung_cancer_5y.json"))

# 2 broad prev 5 year
partial_prev5y_lung_cancer_broad <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(broad_prev, name = "broad_lung_cancer_5y",
                                                exclude(sclc)), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(fixedExit("startDate", 1825L)
  )
)

writeCohort(partial_prev5y_lung_cancer_broad, here::here("preliminary_cohorts",
                                                         "broad_lung_cancer_5y.json"))


# 3 narrow prev 5 year
partial_prev5y_lung_cancer_narrow <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(narrow_prev, name = "narrow_lung_cancer_5y",
                                                exclude(sclc)), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(fixedExit("startDate", 1825L)
  )
)

writeCohort(partial_prev5y_lung_cancer_narrow, here::here("preliminary_cohorts",
                                                         "narrow_lung_cancer_5y.json"))



# 4 sclc prev 5 year
partial_prev5y_sclc <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(sclc, name = "sclc_5y"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(fixedExit("startDate", 1825L)
  )
)

writeCohort(partial_prev5y_sclc, here::here("preliminary_cohorts",
                                                          "small_cell_lung_cancer_5y.json"))

