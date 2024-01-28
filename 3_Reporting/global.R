# load packages -----
library(shiny)
library(shinydashboard)
library(dplyr)
library(readr)
library(here)
library(stringr)
library(PatientProfiles)
library(DT)
library(shinycssloaders)
library(shinyWidgets)
library(gt)
library(CohortSurvival)
library(scales)
library(kableExtra)
library(tidyr)
library(stringr)
library(ggplot2)

# functions ----
nice.num3<-function(x) {
  trimws(format(x,
                big.mark=",", nsmall = 3, digits=3, scientific=FALSE))}
nice.num1<-function(x) {
  trimws(format(round(x,2),
                big.mark=",", nsmall = 1, digits=1, scientific=FALSE))}
nice.num.count<-function(x) {
  trimws(format(x,
                big.mark=",", nsmall = 0, digits=1, scientific=FALSE))}

rename_db <- function(result){
  result<- result %>%
    mutate(cdm_name = recode(cdm_name, 
                             'Clinical DataWarehouse Bordeaux University Hospital' = 'CDWBordeaux', 
                             "German Disease Analyzer" = "IQVIA DA Germany",
                             "Institut Municipal Assistència Sanitària Information System" = "IMASIS",
                             "Netherlands Cancer Registry" = "NCR",
                             "Estonian_Biobank,_University_Tartu" = "EBB"))
  if("strata_level" %in% colnames(result)){
    result<- result %>%
      mutate(strata_level = if_else(strata_level == "70 to 150",
                                 ">=70", strata_level))
    
  }
  if("variable_level" %in% colnames(result)){
    result<- result %>%
      mutate(variable_level = if_else(variable_level == "70 to 150",
                                    ">=70", variable_level))
    
  }  
  
  
  
  result
  
}
# read results from data folder ----
results<-list.files(here("data"), full.names = TRUE)

# cdm snapshot ------
cdm_snapshot_files<-results[stringr::str_detect(results, ".csv")]
cdm_snapshot_files<-results[stringr::str_detect(results, "cdm_snapshot")]
cdm_snapshot <- list()
for(i in seq_along(cdm_snapshot_files)){
  cdm_snapshot[[i]]<-readr::read_csv(cdm_snapshot_files[[i]], 
                                     show_col_types = FALSE) %>% 
    select("cdm_name", "person_count", "observation_period_count" ,
           "vocabulary_version")
}
cdm_snapshot <- dplyr::bind_rows(cdm_snapshot)
cdm_snapshot <- rename_db(cdm_snapshot)
cdm_snapshot <- cdm_snapshot %>% 
  mutate(person_count = nice.num.count(person_count), 
         observation_period_count = nice.num.count(observation_period_count)) %>% 
  rename("Database name" = "cdm_name",
         "Persons in the database" = "person_count",
         "Number of observation periods" = "observation_period_count",
         "OMOP CDM vocabulary version" = "vocabulary_version")

# cohort_attrition ------
cohort_attrition_files<-results[stringr::str_detect(results, ".csv")]
cohort_attrition_files<-results[stringr::str_detect(results, "cohort_attrition")]
cohort_attrition <- list()
for(i in seq_along(cohort_attrition_files)){
  cohort_attrition[[i]]<-readr::read_csv(cohort_attrition_files[[i]], 
                                     show_col_types = FALSE)
}
cohort_attrition <- dplyr::bind_rows(cohort_attrition) 

cohort_attrition <- cohort_attrition %>% 
  arrange(cdm_name) %>% 
  select("cdm_name",  "cohort_name", "reason",
         "number_subjects") %>% 
  pivot_wider(names_from = "cdm_name", 
              values_from = c("number_subjects"))

# summaryDemographics ------
results_summaryDemographics_files<-results[stringr::str_detect(results, 
                                                              "summaryDemographics")]
results_summaryDemographics <- list()
for(i in seq_along(results_summaryDemographics_files)){
  results_summaryDemographics[[i]]<-readr::read_csv(results_summaryDemographics_files[[i]], 
                                                   show_col_types = FALSE) 
}
results_summaryDemographics <- dplyr::bind_rows(results_summaryDemographics)
results_summaryDemographics <- rename_db(results_summaryDemographics)

# summaryComorbidity ------
results_summaryComorbidity_files<-results[stringr::str_detect(results, 
                                                             "summaryComorbidity")]
results_summaryComorbidity <- list()
for(i in seq_along(results_summaryComorbidity_files)){
  results_summaryComorbidity[[i]]<-readr::read_csv(results_summaryComorbidity_files[[i]], 
                                     show_col_types = FALSE) 
}
results_summaryComorbidity <- dplyr::bind_rows(results_summaryComorbidity) %>% 
  filter(variable_type == "binary") %>% 
  pivot_wider(names_from = estimate_type,
              values_from = estimate) %>% 
  select(!c("result_type", "group_name", "variable_type",
            "strata_name")) %>% 
  rename("cohort_name"= "group_level") %>% 
  rename("time_window"= "variable") %>%
  filter(count != "<5") %>% 
  mutate(count=as.numeric(count)) %>% 
  mutate(percentage=round(as.numeric(percentage),2)) %>% 
  mutate(count_percentage = paste0(count, " (", percentage, "%)")) %>% 
  mutate(time_window = str_remove(time_window, "Comorbidities flag "))


results_summaryComorbidity <- results_summaryComorbidity %>%
  mutate(variable_level = recode(variable_level, 
                                 'Copd' = 'COPD', 
                                 "Adhd" = "Attention deficit hyperactivity disorder",
                                 "Uti" = "Urinary tract infection",
                                 "Gerd" = "Gastro-esophageal reflux disease (GERD)",
                                 "Gi hemorrhage" = "GI-Bleeding",
                                 "Hiv" = "Human Immunodeficiency Virus (HIV)",
                                 "Gerd" = "Gastro-esophageal reflux disease (GERD)",
                                 "Crohns disease"= "Crohn´s disease")) %>%
  mutate(variable_level = recode(variable_level, 
                                 'Cld' = 'Chronic liver disease', 
                                 "Collitis" = "Ulcerative collitis",
                                 "Crohns" = "Crohn´s disease",
                                 "Depress" = "Depressive disorder",
                                 "Diabetes" = "Diabetes mellitus",
                                 "Gi hem" = "GI-Bleeding",
                                 "Hyperlip" = "Hyperlipidemia",
                                 "Hyperten"= "Hypertension",
                                 "Malign" = "Malignancy",
                                 "Oa"="Osteoarthritis",
                                 "Parkinson"="Parkinson disease",
                                 "Pneum" = "Pneumonia",
                                 "Renal imp" = "Renal impairment" ,
                                 "Schizoph" = "Schizophrenia"))


results_summaryComorbidity <- rename_db(results_summaryComorbidity)


results_summaryComorbidity <- results_summaryComorbidity %>% 
  filter(time_window != "-inf to 0 days") %>%
  mutate(time_window = recode(time_window, 
                              "-999999 to -366 days" = "Any time prior to 366 days prior",
                              "-365 to -31 days"="365 days prior to 31 day prior",
                              "-30 to -1 days" = "30 days prior to 1 day prior",
                              "0 to 0" = "On index")) %>% 
  mutate(time_window = factor(time_window,
                              levels = c("Any time prior to 366 days prior",
                              "365 days prior to 31 day prior",
                              "30 days prior to 1 day prior",
                              "On index")))
                              


# summaryMedications ------
results_summaryMedications_files<-results[stringr::str_detect(results, 
                                                              "summaryMedications")]
results_summaryMedications <- list()
for(i in seq_along(results_summaryMedications_files)){
  results_summaryMedications[[i]]<-readr::read_csv(results_summaryMedications_files[[i]], 
                                                   show_col_types = FALSE) 
}
results_summaryMedications <- dplyr::bind_rows(results_summaryMedications) %>% 
  filter(variable_type == "binary") %>% 
  pivot_wider(names_from = estimate_type,
              values_from = estimate) %>% 
  select(!c("result_type", "group_name", "variable_type",
            "strata_name")) %>% 
  rename("cohort_name"= "group_level") %>% 
  rename("time_window"= "variable") %>%
  filter(count != "<5") %>% 
  mutate(count=as.numeric(count)) %>% 
  mutate(percentage=round(as.numeric(percentage),2)) %>% 
  mutate(count_percentage = paste0(count, " (", percentage, "%)")) %>% 
  mutate(time_window = str_remove(time_window, "Medications flag "))
results_summaryMedications <- rename_db(results_summaryMedications)

results_summaryMedications <- results_summaryMedications %>% 
  filter(time_window != "-365 to -1 days") %>%
  mutate(time_window = recode(time_window, 
                              "-365 to -31 days"="365 days prior to 31 day prior",
                              "-30 to -1 days" = "30 days prior to 1 day prior",
                              "0 to 0" = "On index",
                              "1 to 30" = "1 day post to 30 day post",
                              "1 to 90"= "1 day post to 90 day post",
                              "1 to 365"= "1 day post to 365 day post"
                              )) %>% 
  mutate(time_window = factor(time_window,
                              levels = c("365 days prior to 31 day prior",
                                         "30 days prior to 1 day prior",
                                         "On index",
                                         "1 day post to 30 day post",
                                         "1 day post to 90 day post",
                                         "1 day post to 365 day post")))

  table(results_summaryMedications$variable_level)

  results_summaryMedications <-  results_summaryMedications %>%
    mutate(variable_level = recode(variable_level, 
                                   'Drug ag ren ang' = 'Agents acting on the renin-angiotensin system', 
                                   'Drug ag ren' = 'Agents acting on the renin-angiotensin system',
                                   "Drug antibacterials" = "Antibacterials for systemic use",
                                   "Drug antibac" = "Antibacterials for systemic use",
                                   "Drug antidep" = "Antidepressants",
                                   "Drug antidepressants" = "Antidepressants",
                                   "Drug antiep"   ="Antiepileptics",
                                   "Drug antiepileptics" = "Antiepileptics",
                                   "Drug antiinf"="Antiinflammatory and antirheumatic products",
                                   "Drug antiinf antirh"="Antiinflammatory and antirheumatic products",
                                   "Drug antineopl"="Antineoplastic agents",
                                   "Drug antineoplastic"="Antineoplastic agents",
                                   "Drug antipsor"="Antipsoriatics",
                                   "Drug antipsoriatics"="Antipsoriatics",
                                   "Drug antithrom" ="Antithrombotic agents", 
                                   "Drug antithrombotics"="Antithrombotic agents", 
                                   "Drug beta blockers"  ="Beta blocking agents",       
                                   "Drug beta bs" ="Beta blocking agents", 
                                   "Drug cc blockers" = "Calcium channel blockers", 
                                   "Drug cc bs"="Calcium channel blockers", 
                                   "Drug diuretics" =  "Diuretics", 
                                   "Drug acid rel" =  "Drugs for acid related disorders", 
                                   "Drug acid rel disord" =  "Drugs for acid related disorders", 
                                   "Drug obs a" = "Drugs for obstructive airway diseases", 
                                   "Drug obs airway" = "Drugs for obstructive airway diseases", 
                                   "Drug in diab" = "Drugs used in diabetes", 
                                   "Drug immuno" = "Immunosuppressants", 
                                   "Drug lipid"="Lipid modifying agents", 
                                   "Drug lipid modifying"="Lipid modifying agents", 
                                   "Drug opioids"  ="Opioids", 
                                   "Drug psychol" ="Psycholeptics", 
                                   "Drug psycholeptics" ="Psycholeptics", 
                                   "Drug psychostim"= "Psychostimulants",
                                   "Drug psychost"= "Psychostimulants"))

  
  

# summaryTreatments ------
results_summaryTreatments_files<-results[stringr::str_detect(results, 
                                                              "summaryTreatments")]
results_summaryTreatments <- list()
if(length(results_summaryTreatments_files>0)){

for(i in seq_along(results_summaryTreatments_files)){
  results_summaryTreatments[[i]]<-readr::read_csv(results_summaryTreatments_files[[i]], 
                                                   show_col_types = FALSE) 
}
results_summaryTreatments <- dplyr::bind_rows(results_summaryTreatments) %>% 
  filter(variable_type == "binary") %>% 
  pivot_wider(names_from = estimate_type,
              values_from = estimate) %>% 
  select(!c("result_type", "group_name", "variable_type",
            "strata_name")) %>% 
  rename("cohort_name"= "group_level") %>% 
  rename("time_window"= "variable") %>%
  filter(count != "<5") %>% 
  mutate(count=as.numeric(count)) %>% 
  mutate(percentage=round(as.numeric(percentage),2)) %>% 
  mutate(count_percentage = paste0(count, " (", percentage, "%)")) %>% 
  mutate(time_window = str_remove(time_window, "Treatments flag "))

results_summaryTreatments <- results_summaryTreatments %>% 
  mutate(type = if_else(str_starts(variable_level, "Class"),
                        "Class", "Treatment")) %>% 
  mutate(variable_level    = str_remove(variable_level   , "Med ")) %>% 
  mutate(variable_level    = str_remove(variable_level   , "Class ")) %>% 
  mutate(variable_level    = str_to_sentence(variable_level))


# survival -----
results_survival_files<-results[stringr::str_detect(results, "survival")]
if(length(results_survival_files>0)){
results_survival <- list()
for(i in seq_along(results_survival_files)){
  results_survival[[i]]<-readr::read_csv(results_survival_files[[i]], 
                                          show_col_types = FALSE) 
}
results_survival <- dplyr::bind_rows(results_survival) 
results_survival <- rename_db(results_survival)


results_survival <- results_survival %>% 
  mutate(strata_name = if_else(strata_name == "age_group", "Age Group", strata_name)) %>% 
  mutate(strata_name = if_else(strata_name == "sex", "Sex", strata_name)) %>% 
  mutate(strata_name = if_else(strata_name == "study_period", "Study period", strata_name))

results_summary_survival <- results_survival %>% 
     filter(estimate_type == "Survival summary") %>% 
   filter(!is.na("n_start")) %>% 
filter(variable_type %in%  c("n_start", "events", "median_survival", 
                             "restricted_mean")) %>% 
  select(!c( "estimate_type", "group_name",
            "time", "analysis_type", "outcome", "variable", "variable_level"))
results_summary_survival <- results_summary_survival %>%
  pivot_wider(names_from = variable_type,
              values_from = estimate)
}
results_summary_survival<- results_summary_survival %>% select(!c("cohort_definition_id", "number_records" ,
                                       "number_subjects", "reason_id", "reason", "result_type",
                                       "excluded_records", "excluded_subjects"))

results_summary_survival <- results_summary_survival %>% 
  mutate(restricted_mean_year = restricted_mean/365.25,
         median_survival_year = median_survival/365.25) %>% 
  select(!c("restricted_mean", "median_survival")) %>% 
  rename("Restricted mean survival (years)" = "restricted_mean_year") %>% 
  rename("Median survival (years)" = "median_survival_year") 

y1_survival <-results_survival %>% 
  filter(time == 365) %>% 
  filter(estimate_type == "Survival probability") %>% 
  filter(variable_type %in%  c("estimate", "estimate_95CI_lower","estimate_95CI_upper")) %>% 
  pivot_wider(names_from = variable_type, values_from = estimate) %>% 
  mutate("1 year survival" = paste0(round(estimate,2), " (",round(estimate_95CI_lower,2), " to ",
                                    round(estimate_95CI_upper,2), ")")) %>% 
  select(cdm_name, strata_level, "1 year survival")
y3_survival <-results_survival %>% 
  filter(time == 1096) %>% 
  filter(estimate_type == "Survival probability") %>% 
  filter(variable_type %in%  c("estimate", "estimate_95CI_lower","estimate_95CI_upper")) %>% 
  pivot_wider(names_from = variable_type, values_from = estimate) %>% 
  mutate("3 year survival" = paste0(round(estimate,2), " (",round(estimate_95CI_lower,2), " to ",
                                    round(estimate_95CI_upper,2), ")")) %>% 
  select(cdm_name, strata_level, "3 year survival")
y5_survival <-results_survival %>% 
  filter(time == 1826) %>% 
  filter(estimate_type == "Survival probability") %>% 
  filter(variable_type %in%  c("estimate", "estimate_95CI_lower","estimate_95CI_upper")) %>% 
  pivot_wider(names_from = variable_type, values_from = estimate) %>% 
  mutate("5 year survival" = paste0(round(estimate,2), " (",round(estimate_95CI_lower,2), " to ",
                                    round(estimate_95CI_upper,2), ")")) %>% 
  select(cdm_name, strata_level, "5 year survival")
y10_survival <-results_survival %>% 
  filter(time == 3653) %>% 
  filter(estimate_type == "Survival probability") %>% 
  filter(variable_type %in%  c("estimate", "estimate_95CI_lower","estimate_95CI_upper")) %>% 
  pivot_wider(names_from = variable_type, values_from = estimate) %>% 
  mutate("10 year survival" = paste0(round(estimate,2), " (",round(estimate_95CI_lower,2), " to ",
                                    round(estimate_95CI_upper,2), ")")) %>% 
  select(cdm_name, strata_level, "10 year survival")


results_summary_survival<-results_summary_survival %>% 
  left_join(y1_survival) %>% 
  left_join(y3_survival) %>% 
  left_join(y5_survival)%>% 
  left_join(y10_survival)
                           