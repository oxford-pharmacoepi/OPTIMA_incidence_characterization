#### PACKAGES -----
library(shiny)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(readr)
library(here)
library(stringr)
library(DT)
library(shinycssloaders)
library(shinyWidgets)
library(gt)
library(scales)
library(kableExtra)
library(tidyr)
library(stringr)
library(ggplot2)
library(fresh)
library(plotly)
library(ggalt)
library(bslib)
library(PatientProfiles)


mytheme <- create_theme(
  adminlte_color(
    light_blue = "#605ca8"
  ),
  adminlte_sidebar(
    dark_bg = "#78B7C5", #  "#D8DEE9",
    dark_hover_bg = "#3B9AB2", #"#81A1C1",
    dark_color ="white" ,
    dark_submenu_bg = "#605ca8"
  ),
  adminlte_global(
    content_bg = "#eaebea"
    #content_bg = "white"
  ),
  adminlte_vars(
    border_color = "black",
    active_link_hover_bg = "#FFF",
    active_link_hover_color = "#112446",
    active_link_hover_border_color = "#112446",
    link_hover_border_color = "#112446",
    table_border_color = "black"

  )
)



# Data prep functions -----
# printing numbers with 1 decimal place and commas 
nice.num<-function(x) {
  trimws(format(round(x,1),
                big.mark=",", nsmall = 1, digits=1, scientific=FALSE))}
# printing numbers with 2 decimal place and commas 
nice.num2<-function(x) {
  trimws(format(round(x,2),
                big.mark=",", nsmall = 2, digits=2, scientific=FALSE))}
# printing numbers with 3 decimal place and commas 
nice.num3<-function(x) {
  trimws(format(round(x,3),
                big.mark=",", nsmall = 3, digits=3, scientific=FALSE))}
# printing numbers with 4 decimal place and commas 
nice.num4<-function(x) {
  trimws(format(round(x,4),
                big.mark=",", nsmall = 4, digits=4, scientific=FALSE))}
# for counts- without decimal place
nice.num.count<-function(x) {
  trimws(format(x,
                big.mark=",", nsmall = 0, digits=1, scientific=FALSE))}


# Load, prepare, and merge results -----
results <-list.files(here("data"), full.names = TRUE,
                     recursive = TRUE,
                     include.dirs = TRUE,
                     pattern = ".zip")

#unzip data
for (i in (1:length(results))) {
  utils::unzip(zipfile = results[[i]],
               exdir = here("data"))
}

#grab the results from the folders
results <- list.files(
  path = here("data"),
  pattern = ".csv",
  full.names = TRUE,
  recursive = TRUE,
  include.dirs = TRUE
)

# concept code lists -----
concepts_lists <- read_csv(here::here("www", "concept_list.csv"), show_col_types = FALSE) %>% 
  filter(Cancer == "Lung")

# incidence estimates not standardized -----
incidence_estimates_files <-results[stringr::str_detect(results, ".csv")]
incidence_estimates_files <-results[stringr::str_detect(results, "incidence_estimates")]
if(length(incidence_estimates_files > 0)){
  
incidence_estimates_files <-incidence_estimates_files[!(stringr::str_detect(incidence_estimates_files, "age_std_"))]

incidence_estimates <- list()

for(i in seq_along(incidence_estimates_files)){
  incidence_estimates[[i]]<-readr::read_csv(incidence_estimates_files[[i]], 
                                            show_col_types = FALSE)  
}
incidence_estimates <- dplyr::bind_rows(incidence_estimates)


# age standardized incidence estimates -----
incidence_estimates_files_std<-results[stringr::str_detect(results, ".csv")]
incidence_estimates_files_std<-results[stringr::str_detect(results, "incidence_estimates")]
incidence_estimates_files_std<-incidence_estimates_files_std[(stringr::str_detect(incidence_estimates_files_std, "age_std_"))]

incidence_estimates_std <- list()
for(i in seq_along(incidence_estimates_files_std)){
  incidence_estimates_std[[i]]<-readr::read_csv(incidence_estimates_files_std[[i]], 
                                            show_col_types = FALSE)  
}
incidence_estimates_std <- dplyr::bind_rows(incidence_estimates_std)

# incidence attrition -----
incidence_attrition_files<-results[stringr::str_detect(results, ".csv")]
incidence_attrition_files<-results[stringr::str_detect(results, "incidence_attrition")]
incidence_attrition <- list()
for(i in seq_along(incidence_attrition_files)){
  incidence_attrition[[i]]<-readr::read_csv(incidence_attrition_files[[i]], 
                                            show_col_types = FALSE)  
}
incidence_attrition <- dplyr::bind_rows(incidence_attrition)

# incidence settings ------
incidence_settings_files<-results[stringr::str_detect(results, ".csv")]
incidence_settings_files<-results[stringr::str_detect(results, "incidence_settings")]
incidence_settings <- list()
for(i in seq_along(incidence_settings_files)){
  incidence_settings[[i]]<-readr::read_csv(incidence_settings_files[[i]], 
                                            show_col_types = FALSE)  
}
incidence_settings <- dplyr::bind_rows(incidence_settings)

}

# survival estimates -------
survival_estimates_files <- results[stringr::str_detect(results, ".csv")]
survival_estimates_files <- results[stringr::str_detect(results, "survival_estimates")]
if(length(survival_estimates_files > 0)){

survival_estimates <- list()
for(i in seq_along(survival_estimates_files)){
  survival_estimates[[i]]<-readr::read_csv(survival_estimates_files[[i]],
                                           show_col_types = FALSE)
}
survival_estimates <- dplyr::bind_rows(survival_estimates) %>% 
  CohortSurvival::splitNameLevel(
    name = "additional_name",
    level = "additional_level",
    keep = FALSE,
    overall = FALSE) %>% 
  mutate(time = as.numeric(time)) %>% 
  pivot_wider(names_from = estimate_name, values_from = estimate_value) %>% 
  filter(estimate_type == "Survival probability")


# survival attrition ------
survival_attrition_files <- results[stringr::str_detect(results, ".csv")]
survival_attrition_files <- results[stringr::str_detect(results, "survival_attrition")]

survival_attrition <- list()
for(i in seq_along(survival_attrition_files)){
  survival_attrition[[i]]<-readr::read_csv(survival_attrition_files[[i]],
                                           show_col_types = FALSE)
}
survival_attrition <- dplyr::bind_rows(survival_attrition)


# survival summaries ------
survival_median_files <- results[stringr::str_detect(results, ".csv")]
survival_median_files <- results[stringr::str_detect(results, "survival_summary")]
survival_median_table <- list()
for(i in seq_along(survival_median_files)){
  survival_median_table[[i]]<-readr::read_csv(survival_median_files[[i]],
                                              show_col_types = FALSE)  
}
survival_median_table <- dplyr::bind_rows(survival_median_table)

}


# table one demographics------
tableone_demo_files <- results[stringr::str_detect(results, ".csv")]
tableone_demo_files <- results[stringr::str_detect(results, "demographics")]

if(length(tableone_demo_files > 0)){

tableone_demo <- list()
for(i in seq_along(tableone_demo_files)){
  tableone_demo[[i]] <- readr::read_csv(tableone_demo_files[[i]],
                                                 show_col_types = FALSE)  
}
demo_characteristics <- dplyr::bind_rows(tableone_demo)

# table one medications ------
tableone_med_files <- results[stringr::str_detect(results, ".csv")]
tableone_med_files <- results[stringr::str_detect(results, "medications")]
tableone_med <- list()
for(i in seq_along(tableone_med_files)){
  tableone_med[[i]] <- readr::read_csv(tableone_med_files[[i]],
                                        show_col_types = FALSE)  
}
med_characteristics <- dplyr::bind_rows(tableone_med)

# table one comorbidities ------
tableone_comorb_files <- results[stringr::str_detect(results, ".csv")]
tableone_comorb_files <- results[stringr::str_detect(results, "comorbidity")]
tableone_comorb <- list()
for(i in seq_along(tableone_comorb_files)){
  tableone_comorb[[i]] <- readr::read_csv(tableone_comorb_files[[i]],
                                       show_col_types = FALSE)  
}
comorb_characteristics <- dplyr::bind_rows(tableone_comorb)

}

# risk tables ----------
# survival_risk_table_files<-results[stringr::str_detect(results, ".csv")]
# survival_risk_table_files<-results[stringr::str_detect(results, "risk_table_results")]
# 
# survival_risk_table <- list()
# for(i in seq_along(survival_risk_table_files)){
#   survival_risk_table[[i]]<-readr::read_csv(survival_risk_table_files[[i]],
#                                             show_col_types = FALSE) %>%
#     mutate_if(is.double, as.character)
#   
# }
# 
# survival_risk_table <- dplyr::bind_rows(survival_risk_table)


# cdm snapshot ------
snapshot_files <- results[stringr::str_detect(results, ".csv")]
snapshot_files <- results[stringr::str_detect(results, "cdm_snapshot")]
snapshotcdm <- list()
for(i in seq_along(snapshot_files)){
  snapshotcdm[[i]] <- readr::read_csv(snapshot_files[[i]],
                                      show_col_types = FALSE) %>% 
    mutate_all(as.character)
  
}
snapshotcdm <- bind_rows(snapshotcdm) %>% 
  select("cdm_name", "person_count", "observation_period_count" ,
         "vocabulary_version", "cdm_version", "cdm_description",) %>% 
  mutate(person_count = nice.num.count(person_count), 
         observation_period_count = nice.num.count(observation_period_count)) %>% 
  dplyr::mutate(cdm_name = replace(cdm_name, cdm_name == "CPRD_GOLD", "CPRD GOLD")) %>% 
  rename("Database name" = "cdm_name",
         "Persons in the database" = "person_count",
         "Number of observation periods" = "observation_period_count",
         "OMOP CDM vocabulary version" = "vocabulary_version",
         "Database CDM Version" = "cdm_version",
         "Database Description" = "cdm_description" ) 

