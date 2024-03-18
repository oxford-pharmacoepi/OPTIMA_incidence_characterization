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
db_name<-"UKB"

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

# getConceptClassId(cdm,
#                   standardConcept = "Standard")

# [1] "Clinical Finding"  "Context-dependent" "HCPCS Modifier"    "ICDO Condition"
# [5] "Procedure"

# all lung cancer together
lungcancer_codes <- getCandidateCodes(
  cdm = cdm,
  keywords = "malignant neoplasm of lung",
  exclude = c("melanoma",
              "lymphoma",
              "sarcoma",
              "secondary",
              "metastasis",
              "benign",
              "hodgkin",
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
  domains = "Condition"
)


#trying out orphan codes for lung cancer
lungcancer_orphan_codes <- findOrphanCodes(x = list("lung_cancer" = lungcancer_codes$concept_id),
                                cdm = cdm,
                                domains = "Condition",
                                standardConcept = "Standard",
                                searchInSynonyms = FALSE,
                                searchNonStandard = FALSE,
                                includeDescendants = TRUE,
                                includeAncestor = TRUE)

# get the counts from codes used in the database from achilles
#test <- achillesCodeUse(list(lc = lungcancer_codes$concept_id), cdm, countBy = c("person"), minCellCount = 0)


# small cell lung cancer
smallcell_lungcancer_codes <- getCandidateCodes(
  cdm = cdm,
  keywords = c("Small cell carcinoma of lung",
               "Oat cell carcinoma of lung",
               "small cell malignant neoplasm of lung"),
  exclude = c("melanoma",
              "metastasis",
              "Non-small" ,
              "secondary",
              "benign",
              "hodgkin",
              "sarcoma",
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
              "epithelioid hemangioendothelioma") ,
  domains = "Condition"
)

smallcell_lungcancer_codes_orphan_codes <- findOrphanCodes(x = list("sc_lung_cancer" = smallcell_lungcancer_codes$concept_id),
                                           cdm = cdm,
                                           domains = "Condition",
                                           standardConcept = "Standard",
                                           searchInSynonyms = FALSE,
                                           searchNonStandard = FALSE,
                                           includeDescendants = TRUE,
                                           includeAncestor = TRUE)


# according to this NSCLC consists of squamous, non squamous, adencarcinoma and large cell
#https://www.nature.com/articles/nrdp20159
# Non-small cell lung cancer
nonsmallcell_lungcancer_codes <- getCandidateCodes(
  cdm = cdm,
  keywords = c("Non-small cell lung cancer",
               "malignant neoplasm of lung",
               "Squamous non-small cell lung cancer",
               "Nonsquamous nonsmall cell neoplasm of lung",
               "Primary acinar cell carcinoma of lung",
               "Primary adenocarcinoma of lung",
               "Primary solid carcinoma of lung",
               "Large cell carcinoma of lung"

               ),
  exclude = c("melanoma",
              "lymphoma",
              "secondary",
              "small cell carcinoma",
              "Oat cell carcinoma",
              "metastasis",
              "secondary",
              "benign",
              "hodgkin",
              "sarcoma",
              "small cell malignant neoplasm",
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
              "epithelioid hemangioendothelioma") ,
  domains = "Condition"
)


nonsmallcell_lungcancer_codes_orphan_codes <- findOrphanCodes(x = list("nsc_lung_cancer" = nonsmallcell_lungcancer_codes$concept_id),
                                                           cdm = cdm,
                                                           domains = "Condition",
                                                           standardConcept = "Standard",
                                                           searchInSynonyms = FALSE,
                                                           searchNonStandard = FALSE,
                                                           includeDescendants = TRUE,
                                                           includeAncestor = TRUE)




#staging codes 1 NSCLC
stage1_nsclungcancer_codes <- getCandidateCodes(
  cdm = cdm,
  keywords = c("Non-small cell carcinoma of lung, TNM stage 1",
               "Large cell carcinoma of lung, TNM stage 1",
               "Squamous cell carcinoma of lung, TNM stage 1",
               "Adenocarcinoma of lung, stage I"
               ),
  exclude = c("melanoma",
              "lymphoma",
              "secondary",
              "small cell carcinoma",
              "metastasis",
              "secondary",
              "benign",
              "hodgkin",
              "sarcoma",
              "small cell malignant neoplasm",
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
              "epithelioid hemangioendothelioma") ,
  domains = "Condition"
)


nsclc_stage1_orphan_codes <- findOrphanCodes(x = list("lung_cancer" = stage1_nsclungcancer_codes$concept_id),
                                                              cdm = cdm,
                                                              domains = "Condition",
                                                              standardConcept = "Standard",
                                                              searchInSynonyms = FALSE,
                                                              searchNonStandard = FALSE,
                                                              includeDescendants = TRUE,
                                                              includeAncestor = TRUE)


#staging codes 2 NSCLC
stage2_nsclungcancer_codes <- getCandidateCodes(
  cdm = cdm,
  keywords = c("Non-small cell carcinoma of lung, TNM stage 2",
               "Large cell carcinoma of lung, TNM stage 2",
               "Squamous cell carcinoma of lung, TNM stage 2",
               "Adenocarcinoma of lung, stage II"
  ),
  exclude = c("melanoma",
              "lymphoma",
              "secondary",
              "small cell carcinoma",
              "metastasis",
              "secondary",
              "benign",
              "hodgkin",
              "sarcoma",
              "small cell malignant neoplasm",
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
              "epithelioid hemangioendothelioma") ,
  domains = "Condition"
)

nsclc_stage2_orphan_codes <- findOrphanCodes(x = list("lung_cancer" = stage2_nsclungcancer_codes$concept_id),
                                             cdm = cdm,
                                             domains = "Condition",
                                             standardConcept = "Standard",
                                             searchInSynonyms = FALSE,
                                             searchNonStandard = FALSE,
                                             includeDescendants = TRUE,
                                             includeAncestor = TRUE)



#staging codes 3 NSCLC
stage3_nsclungcancer_codes <- getCandidateCodes(
  cdm = cdm,
  keywords = c("Non-small cell carcinoma of lung, TNM stage 3",
               "Large cell carcinoma of lung, TNM stage 3",
               "Squamous cell carcinoma of lung, TNM stage 3",
               "Adenocarcinoma of lung, stage III"
  ),
  exclude = c("melanoma",
              "lymphoma",
              "secondary",
              "small cell carcinoma",
              "metastasis",
              "secondary",
              "benign",
              "hodgkin",
              "sarcoma",
              "small cell malignant neoplasm",
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
              "epithelioid hemangioendothelioma") ,
  domains = "Condition"
)

nsclc_stage3_orphan_codes <- findOrphanCodes(x = list("lung_cancer" = stage3_nsclungcancer_codes$concept_id),
                                             cdm = cdm,
                                             domains = "Condition",
                                             standardConcept = "Standard",
                                             searchInSynonyms = FALSE,
                                             searchNonStandard = FALSE,
                                             includeDescendants = TRUE,
                                             includeAncestor = TRUE)




#staging codes 4 NSCLC
stage4_nsclungcancer_codes <- getCandidateCodes(
  cdm = cdm,
  keywords = c("Non-small cell carcinoma of lung, TNM stage 4",
               "Large cell carcinoma of lung, TNM stage 4",
               "Squamous cell carcinoma of lung, TNM stage 4",
               "Adenocarcinoma of lung, stage IV"
  ),
  exclude = c("melanoma",
              "lymphoma",
              "secondary",
              "small cell carcinoma",
              "metastasis",
              "secondary",
              "benign",
              "hodgkin",
              "sarcoma",
              "small cell malignant neoplasm",
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
              "epithelioid hemangioendothelioma") ,
  domains = "Condition"
)

nsclc_stage4_orphan_codes <- findOrphanCodes(x = list("lung_cancer" = stage4_nsclungcancer_codes$concept_id),
                                             cdm = cdm,
                                             domains = "Condition",
                                             standardConcept = "Standard",
                                             searchInSynonyms = FALSE,
                                             searchNonStandard = FALSE,
                                             includeDescendants = TRUE,
                                             includeAncestor = TRUE)




#staging codes 1 SCLC
stage1_sclungcancer_codes <- getCandidateCodes(
  cdm = cdm,
  keywords = c("Primary small cell malignant neoplasm of lung, TNM stage 1"),
  exclude = c("melanoma",
              "lymphoma",
              "secondary",
              "metastasis",
              "secondary",
              "benign",
              "hodgkin",
              "sarcoma",
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
              "epithelioid hemangioendothelioma") ,
  domains = "Condition"
)

sclc_stage1_orphan_codes <- findOrphanCodes(x = list("lung_cancer" = stage1_sclungcancer_codes$concept_id),
                                             cdm = cdm,
                                             domains = "Condition",
                                             standardConcept = "Standard",
                                             searchInSynonyms = FALSE,
                                             searchNonStandard = FALSE,
                                             includeDescendants = TRUE,
                                             includeAncestor = TRUE)



#staging codes 2 SCLC
stage2_sclungcancer_codes <- getCandidateCodes(
  cdm = cdm,
  keywords = c("Primary small cell malignant neoplasm of lung, TNM stage 2"),
  exclude = c("melanoma",
              "lymphoma",
              "secondary",
              "metastasis",
              "secondary",
              "benign",
              "hodgkin",
              "sarcoma",
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
              "epithelioid hemangioendothelioma") ,
  domains = "Condition"
)

sclc_stage2_orphan_codes <- findOrphanCodes(x = list("lung_cancer" = stage2_sclungcancer_codes$concept_id),
                                            cdm = cdm,
                                            domains = "Condition",
                                            standardConcept = "Standard",
                                            searchInSynonyms = FALSE,
                                            searchNonStandard = FALSE,
                                            includeDescendants = TRUE,
                                            includeAncestor = TRUE)


#staging codes 3 SCLC
stage3_sclungcancer_codes <- getCandidateCodes(
  cdm = cdm,
  keywords = c("Primary small cell malignant neoplasm of lung, TNM stage 3"),
  exclude = c("melanoma",
              "lymphoma",
              "secondary",
              "metastasis",
              "secondary",
              "benign",
              "hodgkin",
              "sarcoma",
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
              "epithelioid hemangioendothelioma") ,
  domains = "Condition"
)

sclc_stage3_orphan_codes <- findOrphanCodes(x = list("lung_cancer" = stage3_sclungcancer_codes$concept_id),
                                            cdm = cdm,
                                            domains = "Condition",
                                            standardConcept = "Standard",
                                            searchInSynonyms = FALSE,
                                            searchNonStandard = FALSE,
                                            includeDescendants = TRUE,
                                            includeAncestor = TRUE)



#staging codes 4 SCLC
stage4_sclungcancer_codes <- getCandidateCodes(
  cdm = cdm,
  keywords = c("Primary small cell malignant neoplasm of lung, TNM stage 4"),
  exclude = c("melanoma",
              "lymphoma",
              "secondary",
              "metastasis",
              "secondary",
              "benign",
              "hodgkin",
              "sarcoma",
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
              "epithelioid hemangioendothelioma") ,
  domains = "Condition"
)

sclc_stage4_orphan_codes <- findOrphanCodes(x = list("lung_cancer" = stage4_sclungcancer_codes$concept_id),
                                            cdm = cdm,
                                            domains = "Condition",
                                            standardConcept = "Standard",
                                            searchInSynonyms = FALSE,
                                            searchNonStandard = FALSE,
                                            includeDescendants = TRUE,
                                            includeAncestor = TRUE)



# Creating cohort files ------------

# # add to previous concept ids (which were snomed only)

# creates a cohort where patients exit at the end of observation or end of study broad lung cancer
total_prev_lung_cancer_broad <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(lungcancer_codes$concept_id, name = "lung_cancer_end"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(total_prev_lung_cancer_broad, here::here("preliminary_cohorts",
                                                     "lung_cancer_end.json"))


# creates a cohort where patients exit after 2 years for broad lung cancer
partial_prev2y_lung_cancer_broad <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(lungcancer_codes$concept_id, name = "lung_cancer_2y"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(fixedExit("startDate", 720L)
  )
)

writeCohort(partial_prev2y_lung_cancer_broad, here::here("preliminary_cohorts",
                                                     "lung_cancer_2y.json"))

# creates a cohort where patients exit after 5 years for broad lung cancer
partial_prev5y_lung_cancer_broad <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(lungcancer_codes$concept_id, name = "lung_cancer_5y"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(fixedExit("startDate", 1825L)
  )
)

writeCohort(partial_prev5y_lung_cancer_broad, here::here("preliminary_cohorts",
                                  "lung_cancer_5y.json"))



