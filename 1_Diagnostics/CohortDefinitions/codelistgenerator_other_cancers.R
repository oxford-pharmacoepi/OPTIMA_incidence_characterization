# additional cancer phenotyping

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


# Bladder ---------------
bladdercancer_codes <- getCandidateCodes(
  cdm = cdm,
  keywords = c("malignant neoplasm of bladder") ,
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
  domains = c("Condition", "Observation")
)

write.csv(bladdercancer_codes, here::here("preliminary_cohorts" , "other_cancers",
                                       paste0(cdmName(cdm), "_bladderCancerBroad.csv")), row.names = FALSE)


# Cervix ----------------
cervixcancer_codes <- getCandidateCodes(
  cdm = cdm,
  keywords = c("malignant tumor of cervix") ,
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
  domains = c("Condition", "Observation")
)

write.csv(cervixcancer_codes, here::here("preliminary_cohorts" , "other_cancers",
                                          paste0(cdmName(cdm), "_cervicalCancerBroad.csv")), row.names = FALSE)




# Thyroid ----------------
thyroidcancer_codes <- getCandidateCodes(
  cdm = cdm,
  keywords = c("malignant tumor of thyroid gland") ,
  exclude = c("melanoma",
              "lymphoma",
              "parathyroid",
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
  domains = c("Condition", "Observation")
)

write.csv(thyroidcancer_codes, here::here("preliminary_cohorts" , "other_cancers",
                                           paste0(cdmName(cdm), "_thyroidCancerBroad.csv")), row.names = FALSE)




# Anus ----------------
analcancer_codes <- getCandidateCodes(
  cdm = cdm,
  keywords = c("malignant neoplasm of anus") ,
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
  domains = c("Condition", "Observation")
)

write.csv(analcancer_codes, here::here("preliminary_cohorts" , "other_cancers",
                                          paste0(cdmName(cdm), "_analCancerBroad.csv")), row.names = FALSE)



# Kidney ----------------
kidneycancer_codes <- getCandidateCodes(
  cdm = cdm,
  keywords = c("malignant tumor of kidney",
               "renal cell carcinoma") ,
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
  domains = c("Condition", "Observation")
)

write.csv(kidneycancer_codes, here::here("preliminary_cohorts" , "other_cancers",
                                       paste0(cdmName(cdm), "_kidneyCancerBroad.csv")), row.names = FALSE)


# Uterus ----------------
uteruscancer_codes <- getCandidateCodes(
  cdm = cdm,
  keywords = c("malignant neoplasm of uterus") ,
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
  domains = c("Condition", "Observation")
)

write.csv(uteruscancer_codes, here::here("preliminary_cohorts" , "other_cancers",
                                         paste0(cdmName(cdm), "_uterusCancerBroad.csv")), row.names = FALSE)


# Ovary ----------------
ovariancancer_codes <- getCandidateCodes(
  cdm = cdm,
  keywords = c("malignant tumor of ovary") ,
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
  domains = c("Condition", "Observation")
)

write.csv(ovariancancer_codes, here::here("preliminary_cohorts" , "other_cancers",
                                         paste0(cdmName(cdm), "_ovarianCancerBroad.csv")), row.names = FALSE)


# head and neck cancers --------
hancancer_codes <- getCandidateCodes(
  cdm = cdm,
  keywords = c("malignant tumor of head and neck",
               "malignant neoplasm of lip, oral cavity and pharynx"
  ) ,
  exclude = c("melanoma",
              "lymphoma",
              "sarcoma",
              "face",
              "facial",
              "eye",
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
              "brain",
              "jaw",
              "blastoma",
              "T-cell",
              "atelectasis",
              "plasmacytoma",
              "mesenchymoma",
              "heavy chain disease" ,
              "skin",
              "middle ear",
              "external ear",
              "yolk sac tumor",
              "thyroid gland",
              "germinoma",
              "ganglioglioma",
              "thyroid carcinoma",
              "thyroid cancer",
              "parathyroid" ,
              "ectomesenchymoma",
              "myeloproliferative",
              "sezary",
              "lymphoid",
              "epithelioid hemangioendothelioma"

  ) ,
  domains = c("Condition", "Observation")
)

write.csv(hancancer_codes, here::here("preliminary_cohorts" , "other_cancers",
                                      paste0(cdmName(cdm), "_hanCancerBroad.csv")), row.names = FALSE)


# Breast ---------------
breastcancer_codes <- getCandidateCodes(
  cdm = cdm,
  keywords = c("malignant neoplasm of breast",
               "malignant tumor of breast"
               ) ,
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
  domains = c("Condition", "Observation")
)

write.csv(breastcancer_codes, here::here("preliminary_cohorts" , "other_cancers",
                                          paste0(cdmName(cdm), "_breastCancerBroad.csv")), row.names = FALSE)

# Colorectal ---------------
colorectalcancer_codes <- getCandidateCodes(
  cdm = cdm,
  keywords = c("malignant tumor of colon",
               "malignant tumor of rectum",
               "malignant neoplasm of colon",
               "malignant neoplasm of rectum") ,
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
  domains = c("Condition", "Observation")
)

write.csv(colorectalcancer_codes, here::here("preliminary_cohorts" , "other_cancers",
                                         paste0(cdmName(cdm), "_colorectalCancerBroad.csv")), row.names = FALSE)

# esophageal ---------------
esophagealcancer_codes <- getCandidateCodes(
  cdm = cdm,
  keywords = c("malignant tumor of esophagus",
               "malignant neoplasm of esophagus") ,
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
  domains = c("Condition", "Observation")
)

write.csv(esophagealcancer_codes, here::here("preliminary_cohorts" , "other_cancers",
                                         paste0(cdmName(cdm), "_esophagealCancerBroad.csv")), row.names = FALSE)

# liver ---------------
livercancer_codes <- getCandidateCodes(
  cdm = cdm,
  keywords = c("malignant neoplasm of liver",
               "malignant tumor of liver",
               "hepatocellular carcinoma") ,
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
  domains = c("Condition", "Observation")
)

write.csv(livercancer_codes, here::here("preliminary_cohorts" , "other_cancers",
                                             paste0(cdmName(cdm), "_liverCancerBroad.csv")), row.names = FALSE)


# liver ---------
livercancer_orphan_codes <- findOrphanCodes(x = list("liver_cancer" = livercancer_codes$concept_id),
                                            cdm = cdm,
                                            domains = c("Condition", "Observation"),
                                            standardConcept = "Standard",
                                            searchInSynonyms = FALSE,
                                            searchNonStandard = FALSE,
                                            includeDescendants = TRUE,
                                            includeAncestor = TRUE)


livercancer_orphan_codes <- livercancer_orphan_codes %>%
  separate(additional_level, into = c("Description", "Concept ID"), sep = " ; ")

write.csv(livercancer_orphan_codes, here::here("preliminary_cohorts" , "other_cancers" ,
                                               paste0(cdmName(cdm), "_liverCancer_orphan.csv")), row.names = FALSE)

# pancreatic ---------------
pancreascancer_codes <- getCandidateCodes(
  cdm = cdm,
  keywords = c("malignant neoplasm of pancreas",
               "malignant tumor of pancreas") ,
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
  domains = c("Condition", "Observation")
)

write.csv(pancreascancer_codes, here::here("preliminary_cohorts" , "other_cancers",
                                        paste0(cdmName(cdm), "_pancreasCancerBroad.csv")), row.names = FALSE)


# prostate ---------------
prostatecancer_codes <- getCandidateCodes(
  cdm = cdm,
  keywords = c("malignant neoplasm of prostate",
               "malignant tumor of prostate") ,
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
  domains = c("Condition", "Observation")
)

write.csv(prostatecancer_codes, here::here("preliminary_cohorts" , "other_cancers",
                                           paste0(cdmName(cdm), "_prostateCancerBroad.csv")), row.names = FALSE)

# Stomach ---------------
stomachcancer_codes <- getCandidateCodes(
  cdm = cdm,
  keywords = c("malignant neoplasm of stomach",
               "malignant tumor of stomach") ,
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
  domains = c("Condition", "Observation")
)

write.csv(stomachcancer_codes, here::here("preliminary_cohorts" , "other_cancers",
                                           paste0(cdmName(cdm), "_stomachCancerBroad.csv")), row.names = FALSE)

################### getting orphan codes for extra cancers

# bladder ---------
bladdercancer_orphan_codes <- findOrphanCodes(x = list("bladder_cancer" = bladdercancer_codes$concept_id),
                                              cdm = cdm,
                                              domains = c("Condition", "Observation"),
                                              standardConcept = "Standard",
                                              searchInSynonyms = FALSE,
                                              searchNonStandard = FALSE,
                                              includeDescendants = TRUE,
                                              includeAncestor = TRUE)


bladdercancer_orphan_codes <- bladdercancer_orphan_codes %>%
  separate(additional_level, into = c("Description", "Concept ID"), sep = " ; ")

write.csv(bladdercancer_orphan_codes, here::here("preliminary_cohorts" , "other_cancers" ,
                                              paste0(cdmName(cdm), "_bladderCancer_orphan.csv")), row.names = FALSE)


# cervical ----------------
cervicalcancer_orphan_codes <- findOrphanCodes(x = list("cervical_cancer" = cervixcancer_codes$concept_id),
                                              cdm = cdm,
                                              domains = c("Condition", "Observation"),
                                              standardConcept = "Standard",
                                              searchInSynonyms = FALSE,
                                              searchNonStandard = FALSE,
                                              includeDescendants = TRUE,
                                              includeAncestor = TRUE)


cervicalcancer_orphan_codes <- cervicalcancer_orphan_codes %>%
  separate(additional_level, into = c("Description", "Concept ID"), sep = " ; ")

write.csv(cervicalcancer_orphan_codes, here::here("preliminary_cohorts" , "other_cancers" ,
                                                 paste0(cdmName(cdm), "_cervicalCancer_orphan.csv")), row.names = FALSE)





# thyroid ------
thyroidcancer_orphan_codes <- findOrphanCodes(x = list("thyroid_cancer" = thyroidcancer_codes$concept_id),
                                              cdm = cdm,
                                              domains = c("Condition", "Observation"),
                                              standardConcept = "Standard",
                                              searchInSynonyms = FALSE,
                                              searchNonStandard = FALSE,
                                              includeDescendants = TRUE,
                                              includeAncestor = TRUE)

thyroidcancer_orphan_codes <- thyroidcancer_orphan_codes %>%
  separate(additional_level, into = c("Description", "Concept ID"), sep = " ; ")

write.csv(thyroidcancer_orphan_codes, here::here("preliminary_cohorts" , "other_cancers" ,
                                                 paste0(cdmName(cdm), "_thyroidCancer_orphan.csv")), row.names = FALSE)





# anal ------
analcancer_orphan_codes <- findOrphanCodes(x = list("anal_cancer" = analcancer_codes$concept_id),
                                              cdm = cdm,
                                              domains = c("Condition", "Observation"),
                                              standardConcept = "Standard",
                                              searchInSynonyms = FALSE,
                                              searchNonStandard = FALSE,
                                              includeDescendants = TRUE,
                                              includeAncestor = TRUE)


analcancer_orphan_codes <- analcancer_orphan_codes %>%
  separate(additional_level, into = c("Description", "Concept ID"), sep = " ; ")

write.csv(analcancer_orphan_codes, here::here("preliminary_cohorts" , "other_cancers" ,
                                                 paste0(cdmName(cdm), "_analCancer_orphan.csv")), row.names = FALSE)



# kidney ------
kidneycancer_orphan_codes <- findOrphanCodes(x = list("kidney_cancer" = kidneycancer_codes$concept_id),
                                              cdm = cdm,
                                             domains = c("Condition", "Observation"),
                                              standardConcept = "Standard",
                                              searchInSynonyms = FALSE,
                                              searchNonStandard = FALSE,
                                              includeDescendants = TRUE,
                                              includeAncestor = TRUE)

kidneycancer_orphan_codes <- kidneycancer_orphan_codes %>%
  separate(additional_level, into = c("Description", "Concept ID"), sep = " ; ")

write.csv(kidneycancer_orphan_codes, here::here("preliminary_cohorts" , "other_cancers" ,
                                                 paste0(cdmName(cdm), "_kidneyCancer_orphan.csv")), row.names = FALSE)




# uterus ---------
uteruscancer_orphan_codes <- findOrphanCodes(x = list("uterus_cancer" = uteruscancer_codes$concept_id),
                                              cdm = cdm,
                                              domains = c("Condition", "Observation"),
                                              standardConcept = "Standard",
                                              searchInSynonyms = FALSE,
                                              searchNonStandard = FALSE,
                                              includeDescendants = TRUE,
                                              includeAncestor = TRUE)

uteruscancer_orphan_codes <- uteruscancer_orphan_codes %>%
  separate(additional_level, into = c("Description", "Concept ID"), sep = " ; ")

write.csv(uteruscancer_orphan_codes, here::here("preliminary_cohorts" , "other_cancers" ,
                                                 paste0(cdmName(cdm), "_uterusCancer_orphan.csv")), row.names = FALSE)




# ovary ------
ovariancancer_orphan_codes <- findOrphanCodes(x = list("ovarian_cancer" = ovariancancer_codes$concept_id),
                                              cdm = cdm,
                                              domains = c("Condition", "Observation"),
                                              standardConcept = "Standard",
                                              searchInSynonyms = FALSE,
                                              searchNonStandard = FALSE,
                                              includeDescendants = TRUE,
                                              includeAncestor = TRUE)



ovariancancer_orphan_codes <- ovariancancer_orphan_codes %>%
  separate(additional_level, into = c("Description", "Concept ID"), sep = " ; ")

write.csv(ovariancancer_orphan_codes, here::here("preliminary_cohorts" , "other_cancers" ,
                                                 paste0(cdmName(cdm), "_ovarianCancer_orphan.csv")), row.names = FALSE)


# breast ---------
breastcancer_orphan_codes <- findOrphanCodes(x = list("breast_cancer" = breastcancer_codes$concept_id),
                                              cdm = cdm,
                                              domains = c("Condition", "Observation"),
                                              standardConcept = "Standard",
                                              searchInSynonyms = FALSE,
                                              searchNonStandard = FALSE,
                                              includeDescendants = TRUE,
                                              includeAncestor = TRUE)


breastcancer_orphan_codes <- breastcancer_orphan_codes %>%
  separate(additional_level, into = c("Description", "Concept ID"), sep = " ; ")

write.csv(breastcancer_orphan_codes, here::here("preliminary_cohorts" , "other_cancers" ,
                                                 paste0(cdmName(cdm), "_breastCancer_orphan.csv")), row.names = FALSE)

# colorectal ---------
colorectalcancer_orphan_codes <- findOrphanCodes(x = list("colorectal_cancer" = colorectalcancer_codes$concept_id),
                                             cdm = cdm,
                                             domains = c("Condition", "Observation"),
                                             standardConcept = "Standard",
                                             searchInSynonyms = FALSE,
                                             searchNonStandard = FALSE,
                                             includeDescendants = TRUE,
                                             includeAncestor = TRUE)


colorectalcancer_orphan_codes <- colorectalcancer_orphan_codes %>%
  separate(additional_level, into = c("Description", "Concept ID"), sep = " ; ")

write.csv(colorectalcancer_orphan_codes, here::here("preliminary_cohorts" , "other_cancers" ,
                                                paste0(cdmName(cdm), "_colorectalCancer_orphan.csv")), row.names = FALSE)
# esophagus ---------
esophagealcancer_codescancer_orphan_codes <- findOrphanCodes(x = list("esophageal_cancer" = esophagealcancer_codes$concept_id),
                                             cdm = cdm,
                                             domains = c("Condition", "Observation"),
                                             standardConcept = "Standard",
                                             searchInSynonyms = FALSE,
                                             searchNonStandard = FALSE,
                                             includeDescendants = TRUE,
                                             includeAncestor = TRUE)

esophagealcancer_orphan_codes <- esophagealcancer_orphan_codes %>%
  separate(additional_level, into = c("Description", "Concept ID"), sep = " ; ")

write.csv(esophagealcancer_orphan_codes, here::here("preliminary_cohorts" , "other_cancers" ,
                                                paste0(cdmName(cdm), "_esophagealCancer_orphan.csv")), row.names = FALSE)
# liver ---------
livercancer_orphan_codes <- findOrphanCodes(x = list("liver_cancer" = livercancer_codes$concept_id),
                                             cdm = cdm,
                                             domains = c("Condition", "Observation"),
                                             standardConcept = "Standard",
                                             searchInSynonyms = FALSE,
                                             searchNonStandard = FALSE,
                                             includeDescendants = TRUE,
                                             includeAncestor = TRUE)


livercancer_orphan_codes <- livercancer_orphan_codes %>%
  separate(additional_level, into = c("Description", "Concept ID"), sep = " ; ")

write.csv(livercancer_orphan_codes, here::here("preliminary_cohorts" , "other_cancers" ,
                                                paste0(cdmName(cdm), "_liverCancer_orphan.csv")), row.names = FALSE)
# pancreas ---------
pancreascancer_orphan_codes <- findOrphanCodes(x = list("pancreas_cancer" = pancreascancer_codes$concept_id),
                                             cdm = cdm,
                                             domains = c("Condition", "Observation"),
                                             standardConcept = "Standard",
                                             searchInSynonyms = FALSE,
                                             searchNonStandard = FALSE,
                                             includeDescendants = TRUE,
                                             includeAncestor = TRUE)


pancreascancer_orphan_codes <- pancreascancer_orphan_codes %>%
  separate(additional_level, into = c("Description", "Concept ID"), sep = " ; ")

write.csv(pancreascancer_orphan_codes, here::here("preliminary_cohorts" , "other_cancers" ,
                                                paste0(cdmName(cdm), "_pancreasCancer_orphan.csv")), row.names = FALSE)
# prostate ---------
prostatecancer_orphan_codes <- findOrphanCodes(x = list("prostate_cancer" = prostatecancer_codes$concept_id),
                                             cdm = cdm,
                                             domains = c("Condition", "Observation"),
                                             standardConcept = "Standard",
                                             searchInSynonyms = FALSE,
                                             searchNonStandard = FALSE,
                                             includeDescendants = TRUE,
                                             includeAncestor = TRUE)


prostatecancer_orphan_codes <- prostatecancer_orphan_codes %>%
  separate(additional_level, into = c("Description", "Concept ID"), sep = " ; ")

write.csv(prostatecancer_orphan_codes, here::here("preliminary_cohorts" , "other_cancers" ,
                                                paste0(cdmName(cdm), "_prostateCancer_orphan.csv")), row.names = FALSE)

# stomach  ---------
stomachcancer_orphan_codes <- findOrphanCodes(x = list("stomach_cancer" = stomachcancer_codes$concept_id),
                                             cdm = cdm,
                                             domains = c("Condition", "Observation"),
                                             standardConcept = "Standard",
                                             searchInSynonyms = FALSE,
                                             searchNonStandard = FALSE,
                                             includeDescendants = TRUE,
                                             includeAncestor = TRUE)


stomachcancer_orphan_codes <- stomachcancer_orphan_codes %>%
  separate(additional_level, into = c("Description", "Concept ID"), sep = " ; ")

write.csv(stomachcancer_orphan_codes, here::here("preliminary_cohorts" , "other_cancers" ,
                                                paste0(cdmName(cdm), "_stomachCancer_orphan.csv")), row.names = FALSE)

################### getting orphan codes ----

# han ---------
hancancer_orphan_codes <- findOrphanCodes(x = list("han_cancer" = hancancer_codes$concept_id),
                                              cdm = cdm,
                                              domains = c("Condition", "Observation"),
                                              standardConcept = "Standard",
                                              searchInSynonyms = FALSE,
                                              searchNonStandard = FALSE,
                                              includeDescendants = TRUE,
                                              includeAncestor = TRUE)


hancancer_orphan_codes <- hancancer_orphan_codes %>%
  separate(additional_level, into = c("Description", "Concept ID"), sep = " ; ")

write.csv(hancancer_orphan_codes, here::here("preliminary_cohorts" , "other_cancers" ,
                                                 paste0(cdmName(cdm), "_hanCancer_orphan.csv")), row.names = FALSE)


######################################################

# create cohorts

# anal
reviewed_code_list_anal_cancer <- read.csv(here::here("preliminary_cohorts" ,"other_cancers", "reviewed" ,
                                                  paste0(cdmName(cdm), "_analCancerBroad_reviewed.csv")))

anal_cancer <- reviewed_code_list_anal_cancer %>%
  filter(include == "y" &
           domain_id == "Condition"
           ) %>%
  pull(concept_id)

# create cohorts
anal_cancer <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(anal_cancer, name = "anal_cancer"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(anal_cancer, here::here("preliminary_cohorts", "other_cancers", "reviewed", "anal_cancer.json"))





# bladder
reviewed_code_list_bladder_cancer <- read.csv(here::here("preliminary_cohorts" ,"other_cancers", "reviewed" ,
                                                      paste0(cdmName(cdm), "_bladderCancerBroad_reviewed.csv")))

bladder_cancer <- reviewed_code_list_bladder_cancer %>%
  filter(include == "y" &
           domain_id == "Condition"
  ) %>%
  pull(concept_id)

# create cohorts
bladder_cancer <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(bladder_cancer, name = "bladder_cancer"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(bladder_cancer, here::here("preliminary_cohorts", "other_cancers", "reviewed", "bladder_cancer.json"))



# breast
reviewed_code_list_breast_cancer <- read.csv(here::here("preliminary_cohorts" ,"other_cancers", "reviewed" ,
                                                         paste0(cdmName(cdm), "_breastCancerBroad_reviewed.csv")))

breast_cancer <- reviewed_code_list_breast_cancer %>%
  filter(include == "y" &
           domain_id == "Condition"
  ) %>%
  pull(concept_id)

# create cohorts
breast_cancer <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(breast_cancer, name = "breast_cancer"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(breast_cancer, here::here("preliminary_cohorts", "other_cancers", "reviewed", "breast_cancer.json"))


# cervical
reviewed_code_list_cervical_cancer <- read.csv(here::here("preliminary_cohorts" ,"other_cancers", "reviewed" ,
                                                        paste0(cdmName(cdm), "_cervicalCancerBroad_reviewed.csv")))

cervical_cancer <- reviewed_code_list_cervical_cancer %>%
  filter(include == "y" &
           domain_id == "Condition"
  ) %>%
  pull(concept_id)

# create cohorts
cervical_cancer <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(cervical_cancer, name = "cervical_cancer"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(cervical_cancer, here::here("preliminary_cohorts", "other_cancers", "reviewed", "cervical_cancer.json"))


# colorectal
reviewed_code_list_colorectal_cancer <- read.csv(here::here("preliminary_cohorts" ,"other_cancers", "reviewed" ,
                                                          paste0(cdmName(cdm), "_colorectalCancerBroad_reviewed.csv")))

colorectal_cancer <- reviewed_code_list_colorectal_cancer %>%
  filter(include == "y" &
           domain_id == "Condition"
  ) %>%
  pull(concept_id)

colon_cancer <- reviewed_code_list_colorectal_cancer %>%
  filter(colon == "y" &
           domain_id == "Condition"
  ) %>%
  pull(concept_id)

rectal_cancer <- reviewed_code_list_colorectal_cancer %>%
  filter(rectum == "y" &
           domain_id == "Condition"
  ) %>%
  pull(concept_id)

# create cohorts
colorectal_cancer <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(colorectal_cancer, name = "colorectal_cancer"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(colorectal_cancer, here::here("preliminary_cohorts", "other_cancers", "reviewed", "colorectal_cancer.json"))


colon_cancer <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(colon_cancer, name = "colon_cancer"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(colon_cancer, here::here("preliminary_cohorts", "other_cancers", "reviewed", "colon_cancer.json"))


rectal_cancer <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(rectal_cancer, name = "rectal_cancer"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(rectal_cancer, here::here("preliminary_cohorts", "other_cancers", "reviewed", "rectal_cancer.json"))



# kidney
reviewed_code_list_kidney_cancer <- read.csv(here::here("preliminary_cohorts" ,"other_cancers", "reviewed" ,
                                                          paste0(cdmName(cdm), "_kidneyCancerBroad_reviewed.csv")))

kidney_cancer <- reviewed_code_list_kidney_cancer %>%
  filter(include == "y" &
           domain_id == "Condition"
  ) %>%
  pull(concept_id)

# create cohorts
kidney_cancer <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(kidney_cancer, name = "kidney_cancer"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(kidney_cancer, here::here("preliminary_cohorts", "other_cancers", "reviewed", "kidney_cancer.json"))




# liver
reviewed_code_list_liver_cancer <- read.csv(here::here("preliminary_cohorts" ,"other_cancers", "reviewed" ,
                                                        paste0(cdmName(cdm), "_liverCancerBroad_reviewed.csv")))

liver_cancer <- reviewed_code_list_liver_cancer %>%
  filter(include == "y" &
           domain_id == "Condition"
  ) %>%
  pull(concept_id)

# create cohorts
liver_cancer <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(liver_cancer, name = "liver_cancer"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(liver_cancer, here::here("preliminary_cohorts", "other_cancers", "reviewed", "liver_cancer.json"))



# ovarian
reviewed_code_list_ovarian_cancer <- read.csv(here::here("preliminary_cohorts" ,"other_cancers", "reviewed" ,
                                                       paste0(cdmName(cdm), "_ovarianCancerBroad_reviewed.csv")))

ovarian_cancer <- reviewed_code_list_ovarian_cancer %>%
  filter(include == "y" &
           domain_id == "Condition"
  ) %>%
  pull(concept_id)

# create cohorts
ovarian_cancer <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(ovarian_cancer, name = "ovarian_cancer"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(ovarian_cancer, here::here("preliminary_cohorts", "other_cancers", "reviewed", "ovarian_cancer.json"))


# pancreas
reviewed_code_list_pancreas_cancer <- read.csv(here::here("preliminary_cohorts" ,"other_cancers", "reviewed" ,
                                                         paste0(cdmName(cdm), "_pancreasCancerBroad_reviewed.csv")))

pancreas_cancer <- reviewed_code_list_pancreas_cancer %>%
  filter(include == "y" &
           domain_id == "Condition"
  ) %>%
  pull(concept_id)

# create cohorts
pancreas_cancer <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(pancreas_cancer, name = "pancreas_cancer"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(pancreas_cancer, here::here("preliminary_cohorts", "other_cancers", "reviewed", "pancreas_cancer.json"))



# prostate
reviewed_code_list_prostate_cancer <- read.csv(here::here("preliminary_cohorts" ,"other_cancers", "reviewed" ,
                                                          paste0(cdmName(cdm), "_prostateCancerBroad_reviewed.csv")))

prostate_cancer <- reviewed_code_list_prostate_cancer %>%
  filter(include == "y" &
           domain_id == "Condition"
  ) %>%
  pull(concept_id)

# create cohorts
prostate_cancer <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(prostate_cancer, name = "prostate_cancer"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(prostate_cancer, here::here("preliminary_cohorts", "other_cancers", "reviewed", "prostate_cancer.json"))





# stomach
reviewed_code_list_stomach_cancer <- read.csv(here::here("preliminary_cohorts" ,"other_cancers", "reviewed" ,
                                                          paste0(cdmName(cdm), "_stomachCancerBroad_reviewed.csv")))

stomach_cancer <- reviewed_code_list_stomach_cancer %>%
  filter(include == "y" &
           domain_id == "Condition"
  ) %>%
  pull(concept_id)

# create cohorts
stomach_cancer <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(stomach_cancer, name = "stomach_cancer"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(stomach_cancer, here::here("preliminary_cohorts", "other_cancers", "reviewed", "stomach_cancer.json"))



# thyroid
reviewed_code_list_thyroid_cancer <- read.csv(here::here("preliminary_cohorts" ,"other_cancers", "reviewed" ,
                                                         paste0(cdmName(cdm), "_thyroidCancerBroad_reviewed.csv")))

thyroid_cancer <- reviewed_code_list_thyroid_cancer %>%
  filter(include == "y" &
           domain_id == "Condition"
  ) %>%
  pull(concept_id)

# esophageal
reviewed_code_list_esophageal_cancer <- read.csv(here::here("preliminary_cohorts" ,"other_cancers", "reviewed" ,
                                                         paste0(cdmName(cdm), "_esophagealCancerBroad_reviewed.csv")))

esophageal_cancer <- reviewed_code_list_esophageal_cancer %>%
  filter(include == "y" &
           domain_id == "Condition"
  ) %>%
  pull(concept_id)


# create cohorts
thyroid_cancer <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(thyroid_cancer, name = "thyroid_cancer"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(thyroid_cancer, here::here("preliminary_cohorts", "other_cancers", "reviewed", "thyroid_cancer.json"))


# uterus
reviewed_code_list_uterus_cancer <- read.csv(here::here("preliminary_cohorts" ,"other_cancers", "reviewed" ,
                                                         paste0(cdmName(cdm), "_uterusCancerBroad_reviewed.csv")))

uterus_cancer <- reviewed_code_list_uterus_cancer %>%
  filter(include == "y" &
           domain_id == "Condition"
  ) %>%
  pull(concept_id)

# create cohorts
uterus_cancer <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(uterus_cancer, name = "uterus_cancer"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(uterus_cancer, here::here("preliminary_cohorts", "other_cancers", "reviewed", "uterus_cancer.json"))



# han
reviewed_code_list_han_cancer <- read.csv(here::here("preliminary_cohorts" ,"other_cancers", "reviewed" ,
                                                        paste0(cdmName(cdm), "_hanCancerBroad_reviewed.csv")))

han_cancer <- reviewed_code_list_han_cancer %>%
  filter(overall == "y" &
           domain_id == "Condition"
  ) %>%
  pull(concept_id)

oral_cavity_cancer <- reviewed_code_list_han_cancer %>%
  filter(`Oral.cavity..lip.C00..gum.floor.of.mouth.C03..C04..palate.C05..other.and.unspecified.parts.of.mouth.C06.` == "y" &
           domain_id == "Condition"
  ) %>%
  pull(concept_id)

nasal_cavity_sinuses_cancer <- reviewed_code_list_han_cancer %>%
  filter(`Nasal.cavity.and.sinuses...C30.C31.` == "y" &
           domain_id == "Condition"
  ) %>%
  pull(concept_id)

salivary_glands_cancer <- reviewed_code_list_han_cancer %>%
  filter(`Salivary.glands..Parotid.gland.C07..other.and.unspecified.major.salivary.glands.C08...` == "y" &
           domain_id == "Condition"
  ) %>%
  pull(concept_id)

larynx_cancer <- reviewed_code_list_han_cancer %>%
  filter(`Larynx..C32.` == "y" &
           domain_id == "Condition"
  ) %>%
  pull(concept_id)

oropharynx_cancer <- reviewed_code_list_han_cancer %>%
  filter(`Oropharynx..C10..C09.` == "y" &
           domain_id == "Condition"
  ) %>%
  pull(concept_id)

nasopharynx_cancer <- reviewed_code_list_han_cancer %>%
  filter(`Nasopharynx..C11.` == "y" &
           domain_id == "Condition"
  ) %>%
  pull(concept_id)

hypopharynx_cancer <- reviewed_code_list_han_cancer %>%
  filter(`Hypopharynx..C12..C13.` == "y" &
           domain_id == "Condition"
  ) %>%
  pull(concept_id)


# create cohorts
han_cancer <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(han_cancer, name = "han_cancer"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(han_cancer, here::here("preliminary_cohorts", "other_cancers", "reviewed", "han_cancer.json"))


oral_cavity_cancer <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(oral_cavity_cancer, name = "oral_cavity_cancer"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(oral_cavity_cancer, here::here("preliminary_cohorts", "other_cancers", "reviewed", "oral_cavity_cancer.json"))



nasal_cavity_sinuses_cancer <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(nasal_cavity_sinuses_cancer, name = "nasal_cavity_sinuses_cancer"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(nasal_cavity_sinuses_cancer, here::here("preliminary_cohorts", "other_cancers", "reviewed", "nasal_cavity_sinuses_cancer.json"))



salivary_glands_cancer <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(salivary_glands_cancer, name = "salivary_glands_cancer"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(salivary_glands_cancer, here::here("preliminary_cohorts", "other_cancers", "reviewed", "salivary_glands_cancer.json"))



larynx_cancer <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(larynx_cancer, name = "larynx_cancer"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(larynx_cancer, here::here("preliminary_cohorts", "other_cancers", "reviewed", "larynx_cancer.json"))



oropharynx_cancer <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(oropharynx_cancer, name = "oropharynx_cancer"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(oropharynx_cancer, here::here("preliminary_cohorts", "other_cancers", "reviewed", "oropharynx_cancer.json"))



nasopharynx_cancer <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(nasopharynx_cancer, name = "nasopharynx_cancer"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(nasopharynx_cancer, here::here("preliminary_cohorts", "other_cancers", "reviewed", "nasopharynx_cancer.json"))

hypopharynx_cancer <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(hypopharynx_cancer, name = "hypopharynx_cancer"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(hypopharynx_cancer, here::here("preliminary_cohorts", "other_cancers", "reviewed", "hypopharynx_cancer.json"))


esophageal_cancer <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(esophageal_cancer, name = "esophageal_cancer"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(esophageal_cancer, here::here("preliminary_cohorts", "other_cancers", "reviewed", "esophageal_cancer.json"))





