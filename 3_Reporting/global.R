#### PACKAGES -----
renv::restore()

library(shiny)
library(shinydashboard)
library(shinythemes)
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
library(bslib)
library(PatientProfiles)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(CirceR)
library(rclipboard)
library(CodelistGenerator)
library(CohortCharacteristics)
library(rjson)
library(omopgenerics)
library(dplyr)
library(readr)


mytheme <- create_theme(
  adminlte_color(
    light_blue = "#E387EE"  # Pink for the top border
  ),
  adminlte_sidebar(
    dark_bg = "#7683D6",        # Purple for the sidebar
    dark_hover_bg = "#A9A9A9",  # Slightly darker grey for hover
    dark_color = "white"        # White text color
  ), 
  adminlte_global(
    content_bg = "#F5F5F5"      # Very light grey for the main background
  ),
  adminlte_vars(
    border_color = "#E387EE",   # Pink for the border color
    active_link_hover_bg = "#FFFFFF",  # White for active link hover background
    active_link_hover_color = "#E387EE", # Pink for active link hover color
    active_link_hover_border_color = "#E387EE", # Pink for active link hover border color
    link_hover_border_color = "#E387EE"  # Pink for link hover border color
  )
)

# format markdown
formatMarkdown <- function(x) {
  lines <- strsplit(x, "\r\n\r\n") |> unlist()
  getFormat <- function(line) {
    if (grepl("###", line)) {return(h3(gsub("###", "", line)))} 
    else {h4(line)} 
  }
  purrr::map(lines, ~ getFormat(.))
}

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

# cohort concept code lists -----
cohort_set <- CDMConnector::readCohortSet(here::here(
  "www", "Cohorts" , "incidence" ))

cohort_set$markdown <- ""

for (n in  row_number(cohort_set) ) {
  
  cohort <- cohort_set$cohort_name[n]  
  json <- paste0(cohort_set$json[n]  )
  cohortExpresion <- CirceR::cohortExpressionFromJson(json)
  markdown <- CirceR::cohortPrintFriendly(cohortExpresion)
  cohort_set$markdown[n] <-  markdown
  
} 


# Get concept ids from a provided path to cohort json files
# in dataframe
# Get a list of JSON files in the directory
json_files <- list.files(path = here("www", "Cohorts", "incidence"), pattern = "\\.json$", full.names = TRUE)
  concept_lists_temp <- list()
  concept_lists <- list()
  concept_sets <- list()
  
if(length(json_files > 0)){
  
  for(i in seq_along(json_files)){
    concept_lists_temp[[i]] <- fromJSON(file = json_files[[i]]) 
    
  } 

  
  for (i in 1:length(concept_lists_temp)) {
    
    for (k in 1:length(concept_lists_temp[[i]]$ConceptSets[[1]]$expression$items)) {  
      
      # Extract concept details
      concept_sets[[k]] <- bind_rows(concept_lists_temp[[i]]$ConceptSets[[1]]$expression$items[[k]]$concept)  
      
      # Extract isExcluded status
      is_excluded <- concept_lists_temp[[i]]$ConceptSets[[1]]$expression$items[[k]]$isExcluded
      
      # Combine concept info and isExcluded status into a single data frame
      concept_sets[[k]] <- concept_sets[[k]] %>% 
        mutate(isExcluded = is_excluded)
        
    }
    
    # Combine all concepts in the list and add the name field
    concept_lists[[i]] <- bind_rows(concept_sets) %>% 
      mutate(name = concept_lists_temp[[i]]$ConceptSets[[1]]$name)
    
    # Reset concept_sets for the next iteration
    concept_sets <- list()
  }
  
  
  
  concept_sets_final <- bind_rows(concept_lists)
  
}

  concept_sets_final <- concept_sets_final  %>% 
  mutate(name = ifelse(name == "lung_cancer_inc_broad", "lung_cancer_incident_broad", name)) %>%
  mutate(name = ifelse(name == "lung_cancer_narrow_inc", "lung_cancer_incident_narrow", name)) %>% 
  mutate(name = ifelse(name == "lung_cancer_all_inc", "lung_cancer_incident_all", name)) %>% 
  filter(isExcluded == "FALSE")
  
  
# incidence estimates -----
incidence_estimates_files <-results[stringr::str_detect(results, ".csv")]
incidence_estimates_files <-results[stringr::str_detect(results, "incidence_estimates")]

if(length(incidence_estimates_files > 0)){
  
incidence_estimates_files <-incidence_estimates_files[!(stringr::str_detect(incidence_estimates_files, "_age_std"))]

incidence_estimates <- list()

for(i in seq_along(incidence_estimates_files)) {
  incidence_estimates[[i]] <- read_csv(incidence_estimates_files[[i]], 
                                       show_col_types = FALSE) %>%
    mutate(incidence_start_date = as.Date(incidence_start_date))
}


incidence_estimates <- dplyr::bind_rows(incidence_estimates) %>% 
  mutate(cdm_name = case_when(
    cdm_name == "CPRD_GOLD_100k" ~ "CPRD_GOLD",
    TRUE ~ cdm_name
  )) %>% 
  mutate(cdm_name = str_replace_all(cdm_name, "_", " "))  %>% 
  mutate(outcome_cohort_name = case_when(
    outcome_cohort_name == "lung_cancer_incident_all" ~ "Lung Cancer All",
    outcome_cohort_name == "lung_cancer_incident_broad" ~ "Lung Cancer Broad",
    outcome_cohort_name == "lung_cancer_incident_narrow" ~ "Lung Cancer Narrow",
    outcome_cohort_name == "small_cell_lung_cancer" ~ "Small Cell Lung Cancer",
    TRUE ~ outcome_cohort_name
  )) %>% 
  mutate(cdm_name = case_when(
    cdm_name == "THIN es" ~ "THIN Spain",
    cdm_name == "THIN be" ~ "THIN Belgium",
    cdm_name == "THIN fr" ~ "THIN France",
    cdm_name == "THIN it" ~ "THIN Italy",
    cdm_name == "THIN ro" ~ "THIN Romania",
    cdm_name == "THIN uk" ~ "THIN UK",
    cdm_name == "German DA M202403" ~ "DA Germany",
    cdm_name == "GERMANY DA" ~ "DA Germany",
    cdm_name == "USONCEMR202112SRC" ~ "OncoEMR",
    TRUE ~ cdm_name
  )) %>% 
  # # Filter out rows where cdm_name starts with "THIN " and incidence_start_date == "2022-01-01" due to end of database cut affecting the denominator
  filter(!(str_starts(cdm_name, "THIN ") & incidence_start_date == "2022-01-01")) %>% 
  filter(!(str_starts(cdm_name, "DA ") & incidence_start_date < "2003-01-01")) %>% 
  filter(!(str_starts(cdm_name, "DA ") & incidence_start_date == "2022-01-01")) %>% 
# filter out rows where incidence is 0 i.e. for small cell lung cancer 
# Filter out rows for "Small Cell Lung Cancer" where all rows for the same incidence_start_date have events == 0
group_by(incidence_start_date, outcome_cohort_name) %>% 
  filter(!(outcome_cohort_name == "Small Cell Lung Cancer" & all(outcome_count == 0))) %>% 
  ungroup()




# age standardized incidence estimates -----
incidence_estimates_files_std<-results[stringr::str_detect(results, ".csv")]
incidence_estimates_files_std<-results[stringr::str_detect(results, "incidence_estimates")]
incidence_estimates_files_std<-incidence_estimates_files_std[(stringr::str_detect(incidence_estimates_files_std, "_age_std"))]

incidence_estimates_std <- list()

for(i in seq_along(incidence_estimates_files_std)){
  incidence_estimates_std[[i]]<-readr::read_csv(incidence_estimates_files_std[[i]], 
                                            show_col_types = FALSE) %>% 
    mutate(incidence_start_date = as.Date(incidence_start_date))
}

incidence_estimates_std <- dplyr::bind_rows(incidence_estimates_std) %>% 
  mutate(cdm_name = case_when(
    cdm_name == "CPRD_GOLD_100k" ~ "CPRD_GOLD",
    TRUE ~ cdm_name
  )) %>% 
  mutate(cdm_name = str_replace_all(cdm_name, "_", " ")) %>% 
  mutate(outcome_cohort_name = case_when(
    outcome_cohort_name == "lung_cancer_incident_all" ~ "Lung Cancer All",
    outcome_cohort_name == "lung_cancer_incident_broad" ~ "Lung Cancer Broad",
    outcome_cohort_name == "lung_cancer_incident_narrow" ~ "Lung Cancer Narrow",
    outcome_cohort_name == "small_cell_lung_cancer" ~ "Small Cell Lung Cancer",
    TRUE ~ outcome_cohort_name
  )) %>% 
  mutate(cdm_name = case_when(
    cdm_name == "THIN es" ~ "THIN Spain",
    cdm_name == "THIN be" ~ "THIN Belgium",
    cdm_name == "THIN fr" ~ "THIN France",
    cdm_name == "THIN it" ~ "THIN Italy",
    cdm_name == "THIN ro" ~ "THIN Romania",
    cdm_name == "THIN uk" ~ "THIN UK",
    cdm_name == "German DA M202403" ~ "DA Germany",
    cdm_name == "GERMANY DA" ~ "DA Germany",
    cdm_name == "USONCEMR202112SRC" ~ "OncoEMR",
    TRUE ~ cdm_name
  )) %>% 
  # Filter out rows where cdm_name starts with "THIN " and incidence_start_date == "2022-01-01" due to end of database cut affecting the denominator
  filter(!(str_starts(cdm_name, "THIN ") & incidence_start_date == "2022-01-01")) %>% 
  filter(!(str_starts(cdm_name, "DA ") & incidence_start_date < "2003-01-01")) %>% 
  filter(!(str_starts(cdm_name, "DA ") & incidence_start_date == "2022-01-01")) %>% 
  group_by(incidence_start_date, outcome_cohort_name) %>% 
  filter(!(outcome_cohort_name == "Small Cell Lung Cancer" & all(outcome_count == 0))) %>% 
  ungroup()

# incidence attrition -----
incidence_attrition_files<-results[stringr::str_detect(results, ".csv")]
incidence_attrition_files<-results[stringr::str_detect(results, "incidence_attrition")]
incidence_attrition <- list()

for(i in seq_along(incidence_attrition_files)){

    df <- readr::read_csv(incidence_attrition_files[[i]], show_col_types = FALSE)

    df <- df %>%
      rename_with(~ gsub("\\[header_name\\]Variable name\\n\\[header_level\\]", "", .)) %>%
      rename_with(~ gsub("\\[header_name\\]Variable name\\r\\n\\[header_level\\]", "", .))

    incidence_attrition[[i]] <- df
    
    
}

incidence_attrition <- dplyr::bind_rows(incidence_attrition) %>% 
  mutate(`CDM name` = case_when(
    `CDM name` == "CPRD_GOLD_100k" ~ "CPRD_GOLD",
    `CDM name` == "German DA M202403" ~ "DA Germany",
    `CDM name` == "GERMANY DA" ~ "DA Germany",
    `CDM name` == "USONCEMR202112SRC" ~ "OncoEMR",
    TRUE ~ `CDM name`
  )) %>% 
  mutate(`Outcome cohort name` = case_when(
    `Outcome cohort name` == "lung_cancer_incident_all" ~ "Lung Cancer All",
    `Outcome cohort name` == "lung_cancer_incident_broad" ~ "Lung Cancer Broad",
    `Outcome cohort name` == "lung_cancer_incident_narrow" ~ "Lung Cancer Narrow",
    `Outcome cohort name` == "small_cell_lung_cancer" ~ "Small Cell Lung Cancer",
    TRUE ~ `Outcome cohort name`
  )) %>% 
  mutate(`CDM name` = str_replace_all(`CDM name`, "_", " ")) %>% 
  rename_with(~ trimws(.)) %>% # Removes any leading or trailing whitespace
  rename_with(~ gsub(" ", "_", .))

}


# table one demographics------
  tableone_demo_files <- results[stringr::str_detect(results, ".csv")]
  tableone_demo_files <- results[stringr::str_detect(results, "demographics")]
  
  if(length(tableone_demo_files > 0)){
    
    tableone_demo <- list()
    
    for(i in seq_along(tableone_demo_files)){
      #read in the files

      tableone_demo[[i]] <- omopgenerics::importSummarisedResult(tableone_demo_files[[i]], recursive = FALSE)
      
      
    }
    
  }
  
  demo_characteristics <- Reduce(omopgenerics::bind, tableone_demo) %>%
    mutate(cdm_name = case_when(
      cdm_name == "CPRD_GOLD_100k" ~ "CPRD_GOLD",
      cdm_name == "German DA M202403" ~ "DA Germany",
      cdm_name == "GERMANY DA" ~ "DA Germany",
      cdm_name == "USONCEMR202112SRC" ~ "OncoEMR",
      TRUE ~ cdm_name
    )) %>% 
    mutate(cdm_name = str_replace_all(cdm_name, "_", " ")) %>% 
    mutate(group_level = case_when(
      group_level == "lung_cancer_incident_all" ~ "Lung Cancer All",
      group_level == "lung_cancer_incident_broad" ~ "Lung Cancer Broad",
      group_level == "lung_cancer_incident_narrow" ~ "Lung Cancer Narrow",
      group_level == "small_cell_lung_cancer" ~ "Small Cell Lung Cancer",
      group_level == "lung_cancer_incident_all_sampled" ~ "Lung Cancer All Sampled",
      group_level == "lung_cancer_incident_broad_sampled" ~ "Lung Cancer Broad Sampled",
      group_level == "lung_cancer_incident_narrow_sampled" ~ "Lung Cancer Narrow Sampled",
      group_level == "small_cell_lung_cancer_sampled" ~ "Small Cell Lung Cancer Sampled",
      group_level == "lung_cancer_incident_all_matched" ~ "Lung Cancer All Matched",
      group_level == "lung_cancer_incident_broad_matched" ~ "Lung Cancer Broad Matched",
      group_level == "lung_cancer_incident_narrow_matched" ~ "Lung Cancer Narrow Matched",
      group_level == "small_cell_lung_cancer_matched" ~ "Small Cell Lung Cancer Matched",
      TRUE ~ group_level
    )) %>% 
    
    mutate(cdm_name = case_when(
      cdm_name == "THIN es" ~ "THIN Spain",
      cdm_name == "THIN be" ~ "THIN Belgium",
      cdm_name == "THIN fr" ~ "THIN France",
      cdm_name == "THIN it" ~ "THIN Italy",
      cdm_name == "THIN ro" ~ "THIN Romania",
      cdm_name == "THIN uk" ~ "THIN UK",
      TRUE ~ cdm_name
    )) 
  
  rm(tableone_demo)

# table one medications ------
tableone_med_files <- results[stringr::str_detect(results, ".csv")]
tableone_med_files <- results[stringr::str_detect(results, "medications")]

if(length(tableone_med_files > 0)){

  tableone_med <- list()

  for(i in seq_along(tableone_med_files)){
    #read in the files
    tableone_med[[i]] <- omopgenerics::importSummarisedResult(tableone_med_files[[i]], recursive = FALSE)


  }

}

med_characteristics <- Reduce(omopgenerics::bind, tableone_med) %>%
  mutate(cdm_name = case_when(
    cdm_name == "CPRD_GOLD_100k" ~ "CPRD_GOLD",
    cdm_name == "German DA M202403" ~ "DA Germany",
    cdm_name == "GERMANY DA" ~ "DA Germany",
    cdm_name == "USONCEMR202112SRC" ~ "OncoEMR",
    TRUE ~ cdm_name
  )) %>% 
  mutate(cdm_name = str_replace_all(cdm_name, "_", " ")) %>% 
  mutate(group_level = case_when(
    group_level == "lung_cancer_incident_all" ~ "Lung Cancer All",
    group_level == "lung_cancer_incident_broad" ~ "Lung Cancer Broad",
    group_level == "lung_cancer_incident_narrow" ~ "Lung Cancer Narrow",
    group_level == "small_cell_lung_cancer" ~ "Small Cell Lung Cancer",
    group_level == "lung_cancer_incident_all_sampled" ~ "Lung Cancer All Sampled",
    group_level == "lung_cancer_incident_broad_sampled" ~ "Lung Cancer Broad Sampled",
    group_level == "lung_cancer_incident_narrow_sampled" ~ "Lung Cancer Narrow Sampled",
    group_level == "small_cell_lung_cancer_sampled" ~ "Small Cell Lung Cancer Sampled",
    group_level == "lung_cancer_incident_all_matched" ~ "Lung Cancer All Matched",
    group_level == "lung_cancer_incident_broad_matched" ~ "Lung Cancer Broad Matched",
    group_level == "lung_cancer_incident_narrow_matched" ~ "Lung Cancer Narrow Matched",
    group_level == "small_cell_lung_cancer_matched" ~ "Small Cell Lung Cancer Matched",
    TRUE ~ group_level
  )) %>% 
  
  mutate(cdm_name = case_when(
    cdm_name == "THIN es" ~ "THIN Spain",
    cdm_name == "THIN be" ~ "THIN Belgium",
    cdm_name == "THIN fr" ~ "THIN France",
    cdm_name == "THIN it" ~ "THIN Italy",
    cdm_name == "THIN ro" ~ "THIN Romania",
    cdm_name == "THIN uk" ~ "THIN UK",
    TRUE ~ cdm_name
  )) 

rm(tableone_med)

# table one comorbidities ------
tableone_comorb_files <- results[stringr::str_detect(results, ".csv")]
tableone_comorb_files <- results[stringr::str_detect(results, "comorbidity")]

if(length(tableone_comorb_files > 0)){

  tableone_comorb <- list()

  for(i in seq_along(tableone_comorb_files)){
    #read in the files
    
    tableone_comorb[[i]] <- omopgenerics::importSummarisedResult(tableone_comorb_files[[i]], recursive = FALSE)


  }

}

comorb_characteristics <- Reduce(omopgenerics::bind, tableone_comorb) %>%
  mutate(cdm_name = case_when(
    cdm_name == "CPRD_GOLD_100k" ~ "CPRD_GOLD",
    cdm_name == "German DA M202403" ~ "DA Germany",
    cdm_name == "GERMANY DA" ~ "DA Germany",
    cdm_name == "USONCEMR202112SRC" ~ "OncoEMR",
    TRUE ~ cdm_name
  )) %>% 
  mutate(cdm_name = str_replace_all(cdm_name, "_", " ")) %>% 
mutate(group_level = case_when(
  group_level == "lung_cancer_incident_all" ~ "Lung Cancer All",
  group_level == "lung_cancer_incident_broad" ~ "Lung Cancer Broad",
  group_level == "lung_cancer_incident_narrow" ~ "Lung Cancer Narrow",
  group_level == "small_cell_lung_cancer" ~ "Small Cell Lung Cancer",
  group_level == "lung_cancer_incident_all_sampled" ~ "Lung Cancer All Sampled",
  group_level == "lung_cancer_incident_broad_sampled" ~ "Lung Cancer Broad Sampled",
  group_level == "lung_cancer_incident_narrow_sampled" ~ "Lung Cancer Narrow Sampled",
  group_level == "small_cell_lung_cancer_sampled" ~ "Small Cell Lung Cancer Sampled",
  group_level == "lung_cancer_incident_all_matched" ~ "Lung Cancer All Matched",
  group_level == "lung_cancer_incident_broad_matched" ~ "Lung Cancer Broad Matched",
  group_level == "lung_cancer_incident_narrow_matched" ~ "Lung Cancer Narrow Matched",
  group_level == "small_cell_lung_cancer_matched" ~ "Small Cell Lung Cancer Matched",
  TRUE ~ group_level
)) %>% 
  
  mutate(cdm_name = case_when(
    cdm_name == "THIN es" ~ "THIN Spain",
    cdm_name == "THIN be" ~ "THIN Belgium",
    cdm_name == "THIN fr" ~ "THIN France",
    cdm_name == "THIN it" ~ "THIN Italy",
    cdm_name == "THIN ro" ~ "THIN Romania",
    cdm_name == "THIN uk" ~ "THIN UK",
    TRUE ~ cdm_name
  )) 

rm(tableone_comorb)


# large scale characteristics -------
lsc_files <- results[stringr::str_detect(results, ".csv")]
lsc_files <- results[stringr::str_detect(results, "_large_scale")]

if(length(lsc_files > 0)){
  
  table_lsc <- list()
  
  for(i in seq_along(lsc_files)){

    table_lsc[[i]] <- omopgenerics::importSummarisedResult(lsc_files[[i]], recursive = FALSE)
    
    
  }
  
}

lsc_characteristics <- Reduce(omopgenerics::bind, table_lsc) %>%
  mutate(cdm_name = case_when(
    cdm_name == "CPRD_GOLD_100k" ~ "CPRD_GOLD",
    cdm_name == "German DA M202403" ~ "DA Germany",
    cdm_name == "GERMANY DA" ~ "DA Germany",
    cdm_name == "USONCEMR202112SRC" ~ "OncoEMR",
    TRUE ~ cdm_name
  )) %>% 
  mutate(cdm_name = str_replace_all(cdm_name, "_", " ")) %>% 
  mutate(group_level = case_when(
    group_level == "lung_cancer_incident_all" ~ "Lung Cancer All",
    group_level == "lung_cancer_incident_broad" ~ "Lung Cancer Broad",
    group_level == "lung_cancer_incident_narrow" ~ "Lung Cancer Narrow",
    group_level == "small_cell_lung_cancer" ~ "Small Cell Lung Cancer",
    group_level == "lung_cancer_incident_all_sampled" ~ "Lung Cancer All Sampled",
    group_level == "lung_cancer_incident_broad_sampled" ~ "Lung Cancer Broad Sampled",
    group_level == "lung_cancer_incident_narrow_sampled" ~ "Lung Cancer Narrow Sampled",
    group_level == "small_cell_lung_cancer_sampled" ~ "Small Cell Lung Cancer Sampled",
    group_level == "lung_cancer_incident_all_matched" ~ "Lung Cancer All Matched",
    group_level == "lung_cancer_incident_broad_matched" ~ "Lung Cancer Broad Matched",
    group_level == "lung_cancer_incident_narrow_matched" ~ "Lung Cancer Narrow Matched",
    group_level == "small_cell_lung_cancer_matched" ~ "Small Cell Lung Cancer Matched",
    TRUE ~ group_level
  )) %>% 
  
  mutate(cdm_name = case_when(
    cdm_name == "THIN es" ~ "THIN Spain",
    cdm_name == "THIN be" ~ "THIN Belgium",
    cdm_name == "THIN fr" ~ "THIN France",
    cdm_name == "THIN it" ~ "THIN Italy",
    cdm_name == "THIN ro" ~ "THIN Romania",
    cdm_name == "THIN uk" ~ "THIN UK",
    TRUE ~ cdm_name
  )) 

rm(table_lsc)

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
  select("cdm_name", "person_count", "observation_period_count" , "start_date",
         "vocabulary_version", "cdm_version", "cdm_description",) %>%
  mutate(person_count = nice.num.count(person_count),
         observation_period_count = nice.num.count(observation_period_count)) %>%
  mutate(cdm_name = case_when(
    cdm_name == "CPRD_GOLD_100k" ~ "CPRD_GOLD",
    cdm_name == "German DA M202403" ~ "DA Germany",
    cdm_name == "GERMANY DA" ~ "DA Germany",
    cdm_name == "USONCEMR202112SRC" ~ "OncoEMR",
    TRUE ~ cdm_name
  )) %>% 
  mutate(cdm_name = str_replace_all(cdm_name, "_", " ")) %>%
  rename("Database name" = "cdm_name",
         "Study Start Date" = "start_date",
         "Persons in the database" = "person_count",
         "Number of observation periods" = "observation_period_count",
         "OMOP CDM vocabulary version" = "vocabulary_version",
         "Database CDM Version" = "cdm_version",
         "Database Description" = "cdm_description" )



# attrition functions ----
orderSummaryAttrition <- function(x) {
  vars <- c(
    "number_records", "number_subjects", "excluded_records", "excluded_subjects"
  )
  x |>
    dplyr::mutate(additional_level = as.numeric(.data$additional_level)) |>
    dplyr::inner_join(
      dplyr::tibble(variable_name = vars, var_id = seq_along(vars)),
      by = "variable_name"
    ) |>
    dplyr::arrange(.data$result_id, .data$additional_level, .data$var_id) |>
    dplyr::select(!"var_id") |>
    dplyr::mutate(additional_level = as.character(.data$additional_level))
}

summariseAttrition <- function(att,
                               set = NULL,
                               tname = "unknown",
                               cname = "unknown") {
  if (is.null(set)) {
    set <- att |>
      dplyr::select("cohort_definition_id") |>
      dplyr::distinct() |>
      dplyr::mutate(
        "result_id" = .data$cohort_definition_id,
        "cohort_name" = paste0("unknown_", .data$cohort_definition_id)
      )
  }
  att |>
    dplyr::inner_join(
      set |>
        dplyr::select("cohort_definition_id", "result_id", "cohort_name"),
      by = "cohort_definition_id"
    ) |>
    dplyr::select(-"cohort_definition_id") |>
    dplyr::mutate(dplyr::across(!"result_id", as.character)) |>
    tidyr::pivot_longer(
      cols = c(
        "number_records", "number_subjects", "excluded_records",
        "excluded_subjects"
      ),
      names_to = "variable_name",
      values_to = "estimate_value"
    ) |>
    dplyr::mutate(
      "estimate_name" = "count",
      "variable_level" = NA_character_,
      "estimate_type" = "integer",
      "cdm_name" = cname
    ) |>
    visOmopResults::uniteGroup("cohort_name") |>
    visOmopResults::uniteStrata("reason") |>
    visOmopResults::uniteAdditional("reason_id") |>
    orderSummaryAttrition() |>
    omopgenerics::newSummarisedResult(
      settings = set |>
        dplyr::select(!"cohort_name") |>
        dplyr::mutate(
          "result_type" = "summarise_cohort_attrition",
          "package_name" = "CohortCharacteristics",
          "package_version" = "0.5.1",
          "table_name" = tname
        ) |>
        dplyr::relocate(dplyr::all_of(c(
          "result_id", "result_type", "package_name", "package_version"
        ))) |>
        dplyr::mutate(
          cohort_definition_id = as.character(.data$cohort_definition_id)
        )
    )
}

# outcome attrition ------
outcome_attrition_files<-results[stringr::str_detect(results, ".csv")]
outcome_attrition_files<-results[stringr::str_detect(results, "outcome_attrition")]
outcome_attrition_temp <- list()
outcome_attrition <- list()

for(i in seq_along(outcome_attrition_files)){
  
  
  outcome_attrition_temp[[i]]<-readr::read_csv(outcome_attrition_files[[i]],
                                            show_col_types = FALSE)
  
  db_names <- str_extract(outcome_attrition_files[[i]], "(?<=data/)(.*?)(?=_outcome_attrition\\.csv)")
  
  
  # NEED TO ADD IN HERE IF THIN DATABASES SUPPRESS 10 OTHERS 5
  outcome_attrition[[i]] <- summariseAttrition(outcome_attrition_temp[[i]],
                                               cname = db_names                 
                                               ) 
  # %>% 
  #   omopgenerics::suppress(minCellCount = 5)
  
  
  
}


# bind the attrition cohorts and only keep main cohorts not matched ones
outcome_attrition_combined <- bind(outcome_attrition) %>% filter(
  group_level == "unknown_1" |
    group_level == "unknown_2" |
    group_level == "unknown_3" |
    group_level == "unknown_4" 
) %>% 
  filter(strata_level != "Cohort records collapsed") %>% 
  mutate(group_level = case_when(
    group_level == "unknown_1" ~ "Lung Cancer All",
    group_level == "unknown_2" ~ "Lung Cancer Broad",
    group_level == "unknown_3" ~ "Lung Cancer Narrow",
    group_level == "unknown_4" ~ "Small Cell Lung Cancer",
    TRUE ~ group_level
  )) %>% 
  mutate(cdm_name = case_when(
    cdm_name == "CPRD_GOLD" ~ "CPRD GOLD",
    cdm_name == "German DA M202403" ~ "DA Germany",
    cdm_name == "GERMANY DA" ~ "DA Germany",
    cdm_name == "USONCEMR202112SRC" ~ "OncoEMR",
    cdm_name == "THIN_es" ~ "THIN Spain",
    cdm_name == "THIN_be" ~ "THIN Belgium",
    cdm_name == "THIN_fr" ~ "THIN France",
    cdm_name == "THIN_it" ~ "THIN Italy",
    cdm_name == "THIN_ro" ~ "THIN Romania",
    cdm_name == "THIN_uk" ~ "THIN UK",
    TRUE ~ cdm_name
  ))




process_exclusion_summary <- function(df, group_level_value) {
  
  df_group <- df %>% filter(group_level == group_level_value)
  
  # Filtered data
  filtered_df <- subset(df_group, strata_level %in% c("Qualifying initial records", "Excluded patients outside study period"))
  exclusions_df <- subset(df_group, !(strata_level %in% c("Qualifying initial records", "Excluded patients outside study period")))
  
  # Sum exclusions
  excluded_records_sum <- sum(as.numeric(exclusions_df$estimate_value[exclusions_df$variable_name == "excluded_records"]), na.rm = TRUE)
  excluded_subjects_sum <- sum(as.numeric(exclusions_df$estimate_value[exclusions_df$variable_name == "excluded_subjects"]), na.rm = TRUE)
  
  # Template for exclusion summary rows
  template_row <- exclusions_df[1, ]
  
  # Create summary rows for exclusions
  summary_row_records <- template_row
  summary_row_records$strata_level <- "Excluded patients < 18 years or no sex recorded or < 1 year prior history"
  summary_row_records$variable_name <- "excluded_records"
  summary_row_records$estimate_value <- excluded_records_sum
  
  summary_row_subjects <- template_row
  summary_row_subjects$strata_level <- "Excluded patients < 18 years or no sex recorded or < 1 year prior history"
  summary_row_subjects$variable_name <- "excluded_subjects"
  summary_row_subjects$estimate_value <- excluded_subjects_sum
  
  # Combine summary rows into exclusions_df
  summary_rows <- bind_rows(summary_row_records, summary_row_subjects) %>%
    mutate(estimate_value = as.character(estimate_value))
  
  exclusions_df <- bind_rows(exclusions_df, summary_rows) %>%
    mutate(estimate_value = as.character(estimate_value))
  
  # Add summary rows to filtered_df
  filtered_df_add <- bind_rows(filtered_df, summary_rows)
  
  # Compute collapsed 'number_records'
  qualifying_nr <- as.numeric(filtered_df$estimate_value[filtered_df$strata_level == "Qualifying initial records" & filtered_df$variable_name == "number_records"])
  collapsed_nr <- qualifying_nr - excluded_records_sum
  
  # Summary row for collapsed 'number_records'
  template_row_nr <- filtered_df[filtered_df$strata_level == "Qualifying initial records" & filtered_df$variable_name == "number_records", ][1, ]
  summary_row_nr <- template_row_nr
  summary_row_nr$strata_level <- "Excluded patients < 18 years or no sex recorded or < 1 year prior history"
  summary_row_nr$estimate_value <- as.character(collapsed_nr)
  summary_row_nr$additional_level <- "3"
  
  # Add summary row to summary_rows and filtered_df
  summary_rows <- bind_rows(summary_rows, summary_row_nr)
  filtered_df_add <- bind_rows(filtered_df_add, summary_row_nr)
  
  # Compute collapsed 'number_subjects'
  qualifying_ns <- as.numeric(filtered_df$estimate_value[filtered_df$strata_level == "Qualifying initial records" & filtered_df$variable_name == "number_subjects"])
  collapsed_ns <- qualifying_ns - excluded_subjects_sum
  
  # Summary row for collapsed 'number_subjects'
  template_row_ns <- filtered_df[filtered_df$strata_level == "Qualifying initial records" & filtered_df$variable_name == "number_subjects", ][1, ]
  summary_row_ns <- template_row_ns
  summary_row_ns$strata_level <- "Excluded patients < 18 years or no sex recorded or < 1 year prior history"
  summary_row_ns$estimate_value <- as.character(collapsed_ns)
  summary_row_ns$additional_level <- "3"
  
  # Final merge
  summary_rows <- bind_rows(summary_rows, summary_row_ns)
  filtered_df_add <- bind_rows(filtered_df_add, summary_row_ns) %>% 
    mutate(
      additional_level = ifelse(
        strata_level == "Excluded patients < 18 years or no sex recorded or < 1 year prior history",
        "3",
        additional_level
      )
    ) %>%
    arrange(additional_level)
  
  return(filtered_df_add)
  
} 


final_result <- outcome_attrition_combined %>%
  split(.$cdm_name) %>%                 # Split by database
  lapply(function(db_df) {
    group_results <- lapply(unique(db_df$group_level), function(g) {
      process_exclusion_summary(db_df, g)
    })
    bind_rows(group_results)           # Combine all group_level results for one database
  }) %>%
  bind_rows()


outcome_attrition_combined <- final_result


