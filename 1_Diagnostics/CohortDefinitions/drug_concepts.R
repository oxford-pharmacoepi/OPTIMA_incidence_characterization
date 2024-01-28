library(DBI)
library(CDMConnector)
library(dplyr)
library(here)
library(snakecase)
library(Capr)

server_dbi<-"cdm_gold_202207"
user<-Sys.getenv("DB_USER")
password<- Sys.getenv("DB_PASSWORD")
port<-Sys.getenv("DB_PORT")
host<-"163.1.65.51"

db <- dbConnect(RPostgres::Postgres(),
                dbname = server_dbi,
                port = port,
                host = host,
                user = user,
                password = password)

cdm <- cdmFromCon(con = db,
                  cdmSchema = "public",
                  writeSchema = "results")

drug_names <- read.csv(here::here("drug_concepts", "drug_names.csv")) %>%
  mutate(concept_name = tolower(concept_name))

d_names <- drug_names %>%
  pull(concept_name)

drug_concepts <- cdm$concept %>%
  mutate(concept_name = tolower(concept_name)) %>%
  filter(concept_name %in% d_names) %>%
  filter(concept_class_id=="Ingredient")    %>%
  filter(standard_concept =="S") %>%
  collect()


drug_concepts <- drug_names %>%
  left_join(drug_concepts,
            by = "concept_name") %>%
  select("concept_name", "concept_id")


write.csv(drug_concepts,
          file = here("drug_concepts", "drug_concepts.csv"),
          row.names = FALSE)

# drug concept sets
drug_concepts$concept_name <- to_snake_case(drug_concepts$concept_name)

for(i in seq_along(drug_concepts$concept_name)){
  Capr::writeConceptSet(x = cs(descendants(drug_concepts$concept_id[[i]]),
                               name = drug_concepts$concept_name[[i]]),
                        path = here("drug_concepts",
                                   paste0("med_", drug_concepts$concept_name[[i]],
                                          ".json")))
}

# add class concept sets
Capr::writeConceptSet(x = cs(descendants(drug_concepts %>%
                                           filter(concept_name %in%
                                                    c("melphalan",
                                                      "bendamustine",
                                                      "doxorubicin",
                                                      "cisplatin",
                                                      "cyclophosphamide",
                                                      "etoposide",
                                                      "vincristine")) %>%
                                           pull("concept_id")),
                             name = "chemotherapies"),
                      path = here("drug_concepts",
                                  "class_chemotherapies.json"))

Capr::writeConceptSet(x = cs(descendants(drug_concepts %>%
                                           filter(concept_name %in%
                                                    c("thalidomide",
                                                      "lenalidomide",
                                                      "pomalidomide")) %>%
                                           pull("concept_id")),
                             name = "immunomodulatory_drugs"),
                      path = here("drug_concepts",
                                  "class_immunomodulatory_drugs.json"))

Capr::writeConceptSet(x = cs(descendants(drug_concepts %>%
                                           filter(concept_name %in%
                                                    c("bortezomib",
                                                      "carfilzomib",
                                                      "ixazomib",
                                                      "venetoclax")) %>%
                                           pull("concept_id")),
                             name = "proteasome_inhibitor"),
                      path = here("drug_concepts",
                                  "class_proteasome_inhibitor.json"))

Capr::writeConceptSet(x = cs(descendants(drug_concepts %>%
                                           filter(concept_name %in%
                                                    c("daratumumab",
                                                      "isatuximab",
                                                      "denosumab",
                                                      "elotuzumab")) %>%
                                           pull("concept_id")),
                             name = "monoclonal_antibodies"),
                      path = here("drug_concepts",
                                  "class_monoclonal_antibodies.json"))

Capr::writeConceptSet(x = cs(descendants(drug_concepts %>%
                                           filter(concept_name %in%
                                                    c("selinexor")) %>%
                                           pull("concept_id")),
                             name = "nuclear_export_inhibitor"),
                      path = here("drug_concepts",
                                  "class_nuclear_export_inhibitor.json"))

Capr::writeConceptSet(x = cs(descendants(c(drug_concepts %>%
                                           filter(concept_name %in%
                                                    c("lisocabtagene maraleucel",
                                                      "idecabtagene vicleucel",
                                                      "brexucabtagene autoleucel",
                                                      "axicabtagene ciloleucel",
                                                      "tisagenlecleucel")) %>%
                                           pull("concept_id"),
                                         35807448)), # also include Chimeric antigen receptor T-cell classification
                             name = "car_t_cell"),
                      path = here("drug_concepts",
                                  "class_car_t_cell.json"))

Capr::writeConceptSet(x = cs(descendants(drug_concepts %>%
                                           filter(concept_name %in%
                                                    c("dexamethasone",
                                                      "prednisone")) %>%
                                           pull("concept_id")),
                             name = "glucocorticoids"),
                      path = here("drug_concepts",
                                  "class_glucocorticoids.json"))

Capr::writeConceptSet(x = cs(descendants(drug_concepts %>%
                                           filter(concept_name %in%
                                                    c("zoledronic acid",
                                                      "pamidronate",
                                                      "clodronic acid",
                                                      "ibandronate",
                                                      "etidronate")) %>%
                                           pull("concept_id")),
                             name = "bisphosphonates"),
                      path = here("drug_concepts",
                                  "class_bisphosphonates.json"))

Capr::writeConceptSet(x = cs(descendants(drug_concepts %>%
                                           filter(concept_name %in%
                                                    c("panobinostat")) %>%
                                           pull("concept_id")),
                             name = "others"),
                      path = here("drug_concepts",
                                  "class_others.json"))

# other drugs of interest
other_drugs <- list(agents_ren_ang_system =21601782,
antibacterials =21602796,
antidepressants =21604686,
antiepileptics =21604389,
antiinf_and_antirh =21603932,
antineoplastic_agents =21601387,
antipsoriatics =21602028,
antithrombotics =21600960,
beta_blockers =21601664,
calcium_channel_blockers =21601744,
diuretics =21601461,
drugs_for_acid_rel_disord =21600046,
drugs_for_obs_airway =21603248,
drugs_used_in_diabetes =21600712,
immunosuppressants =21603890,
lipid_modifying_agents =21601853,
opioids =21604254,
psycholeptics =21604489,
psychostim_agents =21604752)

for(i in seq_along(other_drugs)){
  Capr::writeConceptSet(x = cs(descendants(other_drugs[[i]]),
                               name = names(other_drugs)[i]),
                        path = here("drug_concepts",
                                    paste0("drug_", names(other_drugs)[i],
                                           ".json")))

}


