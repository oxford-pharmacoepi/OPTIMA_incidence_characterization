# Manage project dependencies ------
# the following will prompt you to install the various packages used in the study
# install.packages("renv")
# renv::activate()
renv::restore()

# install.packages("remotes")
# remotes::install_github("darwin-eu-dev/CodelistGenerator@exclusion")

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

# condition and observation
lungcancer_codes_cond_obs1 <- getCandidateCodes(
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
              "epithelioid hemangioendothelioma") ,
  domains = c("Condition", "Observation")
)


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



# Non-small cell lung cancer
nonsmallcell_lungcancer_codes <- getCandidateCodes(
  cdm = cdm,
  keywords = c("Non-small cell lung cancer",
               "malignant neoplasm of lung"),
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

#staging codes
stage1_lungcancer_codes <- getCandidateCodes(
  cdm = cdm,
  keywords = c("Non-small cell carcinoma of lung, TNM stage 1"),
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

#trying out orphan codes for small cell lung cancer
orphan_codes <- findOrphanCodes(x = list("lung_cancer" = smallcell_lungcancer_codes$concept_id),
                                cdm = cdm,
                                domains = "Condition",
                                standardConcept = "Standard",
                                searchInSynonyms = FALSE,
                                searchNonStandard = FALSE,
                                includeDescendants = TRUE,
                                includeAncestor = FALSE)



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
# mm_icdo3_mappings <- cdm$concept_relationship %>%
#   inner_join(cdm$concept %>%
#                filter(vocabulary_id == "ICDO3")  %>%
#                filter(substr(concept_code, 8L, 10L) %in%
#                         c("C34")) ,
#              by = c("concept_id_1"= "concept_id")) %>%
#   filter(relationship_id == 'Maps to') %>%
#   select(concept_id_2) %>%
#   rename("concept_id" = "concept_id_2") %>%
#   distinct() %>%
#   inner_join(cdm$concept %>%
#               filter(vocabulary_id == "ICDO3"),
#             by = "concept_id") %>%
#   collect()
#
# mm_icdo3_mappings1 <- cdm$concept_relationship %>%
#   inner_join(cdm$concept %>%
#                filter(vocabulary_id == "ICDO3")  %>%
#                filter(substr(concept_code, 1L, 10L) %in%
#                         c("8041/3-C34")) ,
#              by = c("concept_id_1"= "concept_id")) %>%
#   filter(relationship_id == 'Maps to') %>%
#   select(concept_id_2) %>%
#   rename("concept_id" = "concept_id_2") %>%
#   distinct() %>%
#   inner_join(cdm$concept %>%
#                filter(vocabulary_id == "ICDO3"),
#              by = "concept_id") %>%
#   collect()
#
#
# # add to previous concept ids (which were snomed only)
# mm_narrow_concepts <- c(mm_narrow_concepts, mm_icdo3_mappings$concept_id)
# mm_broad_concepts <- c(mm_broad_concepts, mm_icdo3_mappings$concept_id)
#
# # 1) mm narrow - no restrictions ----
# mm_narrow <- cohort(
#   entry = entry(
#     conditionOccurrence(cs(mm_narrow_concepts, name = "mm_narrow_cond")),
#     observation(cs(mm_narrow_concepts, name = "mm_narrow_obs")),
#     observationWindow = continuousObservation(0L, 0L),
#     primaryCriteriaLimit = "First"
#   ),
#   exit = exit(
#     endStrategy = observationExit()
#   )
# )
# writeCohort(mm_narrow, here::here("cohorts",
#                                   "mm_narrow.json"))



