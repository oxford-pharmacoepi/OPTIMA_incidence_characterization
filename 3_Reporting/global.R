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
cohort_set <- CDMConnector::read_cohort_set(here::here(
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
  mutate(cdm_name = str_replace_all(cdm_name, "_", " "))  %>% 
  mutate(variable_level = case_when(
    variable_level == "lung_cancer_incident_all" ~ "Lung Cancer All",
    variable_level == "lung_cancer_incident_broad" ~ "Lung Cancer Broad",
    variable_level == "lung_cancer_incident_narrow" ~ "Lung Cancer Narrow",
    variable_level == "small_cell_lung_cancer" ~ "Small Cell Lung Cancer",
    TRUE ~ variable_level
  )) %>% 
  
  mutate(outcome_cohort_name = case_when(
    outcome_cohort_name == "lung_cancer_incident_all" ~ "Lung Cancer All",
    outcome_cohort_name == "lung_cancer_incident_broad" ~ "Lung Cancer Broad",
    outcome_cohort_name == "lung_cancer_incident_narrow" ~ "Lung Cancer Narrow",
    outcome_cohort_name == "small_cell_lung_cancer" ~ "Small Cell Lung Cancer",
    TRUE ~ outcome_cohort_name
  )) %>% 
  mutate(cdm_name = case_when(
    cdm_name == "THIN es" ~ "THIN Spain",
    cdm_name == "THIN be" ~ "THIN Belguim",
    cdm_name == "THIN fr" ~ "THIN France",
    cdm_name == "THIN it" ~ "THIN Italy",
    cdm_name == "THIN ro" ~ "THIN Romania",
    cdm_name == "THIN uk" ~ "THIN UK",
    TRUE ~ cdm_name
  )) 


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
    cdm_name == "THIN be" ~ "THIN Belguim",
    cdm_name == "THIN fr" ~ "THIN France",
    cdm_name == "THIN it" ~ "THIN Italy",
    cdm_name == "THIN ro" ~ "THIN Romania",
    cdm_name == "THIN uk" ~ "THIN UK",
    TRUE ~ cdm_name
  )) 

# incidence attrition -----
incidence_attrition_files<-results[stringr::str_detect(results, ".csv")]
incidence_attrition_files<-results[stringr::str_detect(results, "incidence_attrition")]
incidence_attrition <- list()

for(i in seq_along(incidence_attrition_files)){
  incidence_attrition[[i]]<-readr::read_csv(incidence_attrition_files[[i]], 
                                            show_col_types = FALSE)
  
}

incidence_attrition <- dplyr::bind_rows(incidence_attrition) %>% 
  mutate(`Variable level` = case_when(
    `Variable level` == "lung_cancer_incident_all" ~ "Lung Cancer All",
    `Variable level` == "lung_cancer_incident_broad" ~ "Lung Cancer Broad",
    `Variable level` == "lung_cancer_incident_narrow" ~ "Lung Cancer Narrow",
    `Variable level` == "small_cell_lung_cancer" ~ "Small Cell Lung Cancer",
    TRUE ~ `Variable level`
  )) %>% 
  mutate(`CDM name` = str_replace_all(`CDM name`, "_", " ")) %>% 
  rename_with(~ gsub("\\[header_name\\]Variable name\\n\\[header_level\\]", "", .)) %>%
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
    mutate(cdm_name = str_replace_all(cdm_name, "_", " "))
  
  rm(tableone_demo)

# table one medications ------
tableone_med_files <- results[stringr::str_detect(results, ".csv")]
tableone_med_files <- results[stringr::str_detect(results, "medications")]

if(length(tableone_med_files > 0)){

  tableone_med <- list()
  #settings_med <- list()

  for(i in seq_along(tableone_med_files)){
    #read in the files
    tableone_med[[i]] <- omopgenerics::importSummarisedResult(tableone_med_files[[i]], recursive = FALSE)


  }

}

med_characteristics <- Reduce(omopgenerics::bind, tableone_med) %>%
  mutate(cdm_name = str_replace_all(cdm_name, "_", " "))

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
  mutate(cdm_name = str_replace_all(cdm_name, "_", " "))

rm(tableone_comorb)


# large scale characteristics -------
lsc_files <- results[stringr::str_detect(results, ".csv")]
lsc_files <- results[stringr::str_detect(results, "large_scale")]

if(length(lsc_files > 0)){
  
  table_lsc <- list()
  
  for(i in seq_along(lsc_files)){

    table_lsc[[i]] <- omopgenerics::importSummarisedResult(lsc_files[[i]], recursive = FALSE)
    
    
  }
  
}

lsc_characteristics <- Reduce(omopgenerics::bind, table_lsc) %>%
  mutate(cdm_name = str_replace_all(cdm_name, "_", " ")) %>% 
  filter(!str_starts(group_level, "matched_to_"))


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
  mutate(cdm_name = str_replace_all(cdm_name, "_", " ")) %>%
  rename("Database name" = "cdm_name",
         "Study Start Date" = "start_date",
         "Persons in the database" = "person_count",
         "Number of observation periods" = "observation_period_count",
         "OMOP CDM vocabulary version" = "vocabulary_version",
         "Database CDM Version" = "cdm_version",
         "Database Description" = "cdm_description" )


# attrition functions ----
attritionChart <- function(x) {
  formatNum <- function(col) {
    col <- round(as.numeric(col))
    if_else(
      !is.na(col),
      gsub(" ", "", format(as.integer(col), big.mark=",")),
      as.character(col)
    )
  }
  
  xn <- x %>%
    arrange(reason_id) %>%
    mutate(
      number_subjects = formatNum(number_subjects),
      number_records = formatNum(number_records),
      excluded_subjects = formatNum(excluded_subjects),
      excluded_records = formatNum(excluded_records),
      label = paste0(
        "N subjects = ", number_subjects, "\nN records = ", number_records
      )
    )
  if (nrow(xn) == 1) {
    xn <- xn %>%
      mutate(label = paste0("Qualifying events", "\n", label)) %>%
      select(label)
  } else {
    att <- xn %>%
      filter(reason_id > min(reason_id)) %>%
      mutate(
        label = paste0(
          "N subjects = ", excluded_subjects, "\nN records = ", excluded_records
        )
      ) %>%
      select(reason, label)
    xn <- xn %>%
      mutate(
        label = if_else(
          reason_id == min(reason_id),
          paste0("Initial events", "\n", label),
          if_else(
            reason_id == max(reason_id),
            paste0("Final events", "\n", label),
            label
          )
        )
      ) %>%
      select(label)
  }
  n <- nrow(x)
  xg <- create_graph()
  
  for (k in seq_len(n)) {
    xg <- xg %>%
      add_node(
        label = xn$label[k],
        node_aes = node_aes(
          shape = "box",
          x = 1,
          width = 1.4,
          y = n + 1 - k + ifelse(k == 1, 0.1, 0) + ifelse(k == n, -0.1, 0),
          height = ifelse(k == 1 | k == n, 0.6, 0.4),
          fontsize = 10, fontcolor = "black", penwidth = ifelse(k == 1 | k == n, 2, 1), color = "black"
        )
      )
    if (k > 1) {
      xg <- xg %>%
        add_edge(from = k - 1, to = k, edge_aes = edge_aes(color = "black"))
    }
  }
  salt <- function(x) {
    s <- 50
    x <- strsplit(x = x, split = " ") |> unlist()
    nn <- (nchar(x) + c(0, rep(1, length(x)-1))) |> cumsum()
    id <- which(nn > s)
    if (length(id) > 0) {
      id <- id[1] - 1
      x <- paste0(paste0(x[1:id], collapse = " "), "\n", paste0(x[-(1:id)], collapse = " "))
    } else {
      x <- paste0(x, collapse = " ")
    }
    return(x)
  }
  if (n > 1) {
    for (k in seq_len(nrow(att))) {
      res <- att$reason[k]
      res <- salt(res)
      xg <- xg %>%
        add_node(
          label = att$label[k],
          node_aes = node_aes(
            shape = "box", x = 3.5, width = 1.2, y = n + 0.5 - k, height = 0.4,
            fontsize = 8, fillcolor = "grey", fontcolor = "black", color = "black"
          )
        ) %>%
        add_node(
          label = res,
          node_aes = node_aes(
            shape = "box", x = 1, width = 3.2, y = n + 0.5 - k, height = 0.35, fillcolor = "white", color = "black", fontcolor = "back"
          )
        ) %>%
        add_edge(
          from = 2*k + n, to = 2*k + n -1, edge_aes = edge_aes(color = "black")
        )
    }
  }
  
  return(xg)
}
